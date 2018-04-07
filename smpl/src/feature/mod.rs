mod feature_list;

use std::collections::HashSet;
use std::collections::hash_set::Iter;

pub use self::feature_list::*;

#[derive(Debug, Clone)]
pub struct FeatureErr {
    pub requires: Option<Vec<FeatureReasoning>>,
    pub denies: Option<Vec<FeatureReasoning>>,
}

pub struct PresentFeatures {
    features: HashSet<String>,
}

impl PresentFeatures {

    pub fn new() -> PresentFeatures {
        PresentFeatures {
            features: HashSet::new(),
        }
    }

    pub fn add_feature(&mut self, f: &str) {
        self.features.insert(f.to_string());
    }

    pub fn has_feature(&self, f: &str) -> bool {
        self.features.contains(f)
    }
}

pub struct FeatureInfo {
    required: Vec<FeatureReasoning>,
    denied: Vec<FeatureReasoning>,
}

impl FeatureInfo {
    pub fn new(r: Vec<FeatureReasoning>, 
               d: Vec<FeatureReasoning>) -> FeatureInfo {
        FeatureInfo {
            required: r,
            denied: d,
        }
    }

    pub fn check(&self, pf: &PresentFeatures) -> Result<(), FeatureErr> {
        let mut rl = Vec::new();
        for r in self.required.iter() {
            if pf.has_feature(r.feature()) == false {
                rl.push(r.clone());
            }
        }

        let mut dl = Vec::new();
        for d in self.denied.iter() {
            if pf.has_feature(d.feature()) == true {
                dl.push(d.clone());
            }
        }

        let rl = if rl.len() == 0 {
            None
        } else {
            Some(rl)
        };

        let dl = if dl.len() == 0 {
            None
        } else {
            Some(dl)
        };

        if rl.is_some() || dl.is_some() {
            Err(FeatureErr {
                requires: rl,
                denies: dl
            })
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct FeatureReasoning {
    feature: String,
    reason: Option<String>,
}

impl FeatureReasoning {
    pub fn with_feature(f: &str) -> FeatureReasoning {
        FeatureReasoning {
            feature: f.to_string(),
            reason: None
        }
    }

    pub fn feature_with_reason(f: &str, r: &str) -> FeatureReasoning {
        FeatureReasoning {
            feature: f.to_string(),
            reason: Some(r.to_string())
        }
    }

    pub fn feature(&self) -> &str {
        &self.feature
    }

    pub fn reason(&self) -> Option<&str> {
        self.reason.as_ref().map(|s| s.as_str())
    }
}
