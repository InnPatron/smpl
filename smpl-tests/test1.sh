cd "${0%/*}" 

cargo build

if [ ! -d "./target" ]; then
	mkdir ./target
fi

echo **if_tests**

echo

rm ./target/if_tests.rs
../target/debug/smplc --input if_tests.smpl --backend 0 --output ./target/if_tests.rs

echo **while_tests**

echo

rm ./target/while_tests.rs
../target/debug/smplc --input while_tests.smpl --backend 0 --output ./target/while_tests.rs

echo **fn_tests**

echo

rm ./target/fn_tests.rs > /dev/null
../target/debug/smplc --input fn_tests.smpl --backend 0 --output ./target/fn_tests.rs
