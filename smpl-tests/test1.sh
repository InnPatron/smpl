cd "${0%/*}" 

cargo build

if [ ! -d "./target" ]; then
	mkdir ./target
fi

rm ./target/if_tests.rs
../target/debug/smplc --input if_tests.smpl --backend 0 --output ./target/

rm ./target/while_tests.rs
../target/debug/smplc --input while_tests.smpl --backend 0 --output ./target/
