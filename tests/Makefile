EBIN_DIR= ../ebin
INCLUDE_DIR= ../include

ERLC_FLAGS= "+{parse_transform, pmod_pt}" -pa ../deps/pmod_transform/ebin -pa $(EBIN_DIR) -I $(INCLUDE_DIR) +debug_info -W -o $(EBIN_DIR)
ERL_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR)

FILES=$(patsubst %.erl,%,$(wildcard *.erl))
OBJECTS=$(FILES:%=$(EBIN_DIR)/%.beam)
SUITE=$(if $(suite),$(suite),$(FILES))
leftparen:=(
rightparen:=)
comma:=,
ALL_TESTS=$(foreach t,$(SUITE),-eval "io:format$(leftparen)\"Running suite ~-25w: \"$(comma) [$(t)]$(rightparen), $(t):test().")

test: compile
	@erl $(ERL_FLAGS) -noshell $(ALL_TESTS) -s erlang halt
	@echo "==> Finished running tests"

info:
	@echo "Objects: $(FILES)"

$(EBIN_DIR)/%.beam: %.erl
	@erlc $(ERLC_FLAGS) $< && echo "==> Compiled $<" || echo "==> Compilation of $< failed!"

compile: $(OBJECTS)

clean:
	@rm -f $(OBJECTS) erl_crash.dump
