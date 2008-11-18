all: 
	@cd src;make
	@echo All Done

test: all
	@cd tests;make
	@echo Tests Done

clean:
	@cd src;make clean
	@cd tests;make clean
	@rm -rf ebin
	@rm -f erl_crash.dump
