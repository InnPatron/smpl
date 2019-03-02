use std::collections::HashMap;

use petgraph::graph::{Graph, NodeIndex};

use super::error::TypeError;
use super::semantic_data::*;
use super::type_cons::TypeApp;

enum Node {
    Cons(TypeId),
    App(TypeApp),
}

type TypeGraph = Graph<Node, ()>;

pub fn cyclic_type_check(program: &Program, type_roots: Vec<TypeId>) -> Result<(), TypeError> {
    let mut type_node_map = HashMap::new();
    let mut type_graph = TypeGraph::new();

    let all_types = program.universe().static_types();

    // Insert all TypeIds in graph
    let all_types = all_types
        .into_iter()
        .map(|(type_id, type_cons)| {
            let node_id = type_graph.add_node(Node::Cons(type_id.clone()));
            type_node_map.insert(type_id.clone(), node_id);

            (node_id, type_cons)
        })
        .collect::<Vec<_>>();

    // Connect TypeIds by inspecting their type constructors
    for (ref current, ref type_cons) in all_types.iter() {
        use super::type_cons::TypeCons::*;
        match type_cons {
            UncheckedFunction {
                return_type: ref return_type,
                ..
            } => connect_app(&mut type_graph, &type_node_map, *current, return_type),

            Function {
                parameters: ref params,
                return_type: ref return_type,
                ..
            } => {
                connect_app(&mut type_graph, &type_node_map, *current, return_type);
                params
                    .iter()
                    .for_each(|app| connect_app(&mut type_graph, &type_node_map, *current, app));
            }

            Array {
                element_type: ref element_type,
                ..
            } => connect_app(&mut type_graph, &type_node_map, *current, element_type),

            Record {
                fields: ref fields, ..
            } => fields
                .values()
                .for_each(|f_app| connect_app(&mut type_graph, &type_node_map, *current, f_app)),

            _ => (),
        }
    }

    // petgraph cycle detection
    petgraph::algo::toposort(&type_graph, None)
        .map(|_| ())
        .map_err(|cycle| {
            let app = match type_graph.node_weight(cycle.node_id()).unwrap() {
                Node::Cons(ref c) => TypeApp::Applied {
                    type_cons: *c,
                    args: None,
                },
                Node::App(ref a) => a.clone(),
            };

            TypeError::CyclicType(app)
        })
}

fn connect_app(
    graph: &mut TypeGraph,
    map: &HashMap<TypeId, NodeIndex>,
    from: NodeIndex,
    to: &TypeApp,
) {
    match to {
        TypeApp::Applied {
            type_cons: ref type_cons_id,
            args: ref args,
        } => {
            args.as_ref()
                .map(|args| args.iter().for_each(|to| connect_app(graph, map, from, to)));

            // If not in the map, assume the type constructor is a generated one (like array or
            // anonymous function)
            if let Some(to_static_internal_cons) = map.get(type_cons_id) {
                graph.add_edge(from, *to_static_internal_cons, ());
            }

            let app_node_id = graph.add_node(Node::App(to.clone()));
            graph.add_edge(from, app_node_id, ());
        }

        _ => (),
    }
}
