EBIN_DIR= ../ebin
INCLUDE_DIR= ../include

ERLC_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR) -W -o $(EBIN_DIR)

$(EBIN_DIR)/%.beam: %.erl
	erlc $(ERLC_FLAGS) $<


TESTS=test_erlcfg_lexer \
	  test_erlcfg_parser \
	  test_node_addr \
	  test_erlcfg_node_find \
	  test_erlcfg_node \
	  test_erlcfg_interp \
	  test_erlcfg_data \
	  test_erlcfg


test: compile
	erl $(ERLC_FLAGS) -noshell -s test_erlcfg_lexer test -s init stop
	erl $(ERLC_FLAGS) -noshell -s test_erlcfg_parser test -s init stop
	erl $(ERLC_FLAGS) -noshell -s test_node_addr test -s init stop
	erl $(ERLC_FLAGS) -noshell -s test_erlcfg_node_find test -s init stop
	erl $(ERLC_FLAGS) -noshell -s test_erlcfg_node test -s init stop
	erl $(ERLC_FLAGS) -noshell -s test_erlcfg_interp test -s init stop
	erl $(ERLC_FLAGS) -noshell -s test_erlcfg_data test -s init stop
	erl $(ERLC_FLAGS) -noshell -s test_erlcfg test -s init stop


compile: ${TESTS:%=$(EBIN_DIR)/%.beam}
	@echo all done

clean:
	@rm -f *.beam
	@rm -f erl_crash.dump
