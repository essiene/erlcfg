all: 
	@cd src;make
	@echo All Done

test:
	@cd tests;make
	@echo Tests Done

clean:
	@rm -rf ebin
	@rm -f erl_crash.dump
