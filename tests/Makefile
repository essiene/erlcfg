EBIN_DIR= ../ebin
INCLUDE_DIR= ../include

ERLC_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR) -W -o $(EBIN_DIR)
ERL_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR)

$(EBIN_DIR)/%.beam: %.erl
	erlc $(ERLC_FLAGS) $<


OBJECTS=test_erlcfg_lexer \
	  test_schema_lexer \
	  test_erlcfg_parser \
	  test_schema_parser \
	  test_node_addr \
	  test_erlcfg_node_find \
	  test_erlcfg_node \
	  test_erlcfg_interp \
	  test_erlcfg_data \
	  test_erlcfg


test: compile
	erl $(ERL_FLAGS) -noshell $(foreach t, $(OBJECTS), -s $(t) test) -s init stop
	@echo finished running tests


compile: ${OBJECTS:%=$(EBIN_DIR)/%.beam}
	@echo all done compiling

clean:
	@rm -f *.beam
	@rm -f erl_crash.dump
