EBIN_DIR= ../ebin
INCLUDE_DIR= ../include

ERLC_FLAGS= "+{parse_transform, pmod_pt}" -pa ../deps/pmod_transform/ebin -pa $(EBIN_DIR) -I $(INCLUDE_DIR) +debug_info -W -o $(EBIN_DIR)
ERL_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR)

FILES=$(patsubst %.erl,%,$(wildcard *.erl))
OBJECTS=$(FILES:%=$(EBIN_DIR)/%.beam)

test: compile
	for f in $(if $(suite),$(suite),$(FILES)); do \
    A="Running $$f suite:                 "; echo -en "$${A:0:35}"; \
    erl $(ERL_FLAGS) -noshell -s $$f test -s erlang halt; \
  done
	@echo ==> Finished running tests

info:
	@echo "Objects: $(FILES)"

$(EBIN_DIR)/%.beam: %.erl
	erlc $(ERLC_FLAGS) $<

compile: $(OBJECTS)

clean:
	@rm -f $(OBJECTS) erl_crash.dump
