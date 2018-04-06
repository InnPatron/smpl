OUTPUT := ./target
SAMPLE_PROJECT := sample_program

.PHONY: clean setup

all: basic bin

basic: s_if s_while s_fn 

s_if: compiler if_tests.smpl
	./smplc --input if_tests.smpl --backend 0 --output $(OUTPUT)/if_tests.rs

s_while: compiler while_tests.smpl
	./smplc --input while_tests.smpl --backend 0 --output $(OUTPUT)/while_tests.rs

s_fn: compiler fn_tests.smpl
	./smplc --input fn_tests.smpl --backend 0 --output $(OUTPUT)/fn_tests.rs

s_array: compiler arrays.smpl
	./smplc --input arrays.smpl --backend 0 --output $(OUTPUT)/arrays.rs

bin: compiler sample_program.smpl
	./smplc --input sample_program.smpl sample_dependency.smpl --backend 0 --output $(OUTPUT)/sample_program.rs
	cp $(OUTPUT)/sample_program.rs $(OUTPUT)/$(SAMPLE_PROJECT)/src/main.rs
	cd $(OUTPUT)/$(SAMPLE_PROJECT);cargo build

compiler:
	cd ../;cargo build
	cp ../target/debug/smplc .

setup:
	mkdir -p $(OUTPUT)
	cd $(OUTPUT);cargo new --bin $(SAMPLE_PROJECT)

clean:
	rm -f smplc
	rm -r -f $(OUTPUT) 
