cd "${0%/*}" 

cargo build

if [ ! -d "./target" ]; then
	mkdir ./target
fi

echo **if_tests**

echo

rm ./target/if_tests.rs
../target/debug/smplc --input if_tests.smpl --backend 0 --output ./target/

echo **while_tests**

echo

rm ./target/while_tests.rs
../target/debug/smplc --input while_tests.smpl --backend 0 --output ./target/
