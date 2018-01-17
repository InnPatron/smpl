cd "${0%/*}" 

cargo build

if [ ! -d "./tmp_project" ]; then
	cargo init --bin tmp_project
fi

if [ ! -d "./target" ]; then
	mkdir ./target
fi

if [ -f "./target/sample_program.rs" ]; then
	rm ./target/sample_program.rs
fi

../target/debug/smplc --input sample_program.smpl --backend 0 --output ./target/

rm ./tmp_project/src/main.rs

touch ./tmp_project/src/main.rs

cat ./target/sample_program.rs > ./tmp_project/src/main.rs

cd tmp_project

cargo build 

cd ../
