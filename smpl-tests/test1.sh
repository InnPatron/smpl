cd "${0%/*}" 

cargo build

if [ ! -d "./target" ]; then
	mkdir ./target
fi

rm ./target/test1.rs
../target/debug/smplc --input test1.smpl --backend 0 --output ./target/
