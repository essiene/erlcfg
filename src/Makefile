EBIN_DIR=../ebin
INCLUDE_DIR=../include
ERLC=erlc
ERLC_FLAGS= -W -I $(INCLUDE_DIR) -o $(EBIN_DIR)
ERL=erl -noshell

$(EBIN_DIR)/%.erl: %.xrl
	@mkdir -p $(EBIN_DIR)
	@$(ERL) -eval "Filename = filename:basename(\"$<\", \".xrl\"), leex:file(Filename, [{outdir, \"$(EBIN_DIR)\"}]), halt()."

$(EBIN_DIR)/%.beam: $(EBIN_DIR)/%.erl
	@mkdir -p $(EBIN_DIR)
	$(ERLC) $(ERLC_FLAGS) $<

$(EBIN_DIR)/%.beam: %.erl
	@mkdir -p $(EBIN_DIR)
	$(ERLC) $(ERLC_FLAGS) $<


LEEX_OBJECTS=erlcfg_lexer

YEEC_OBJECTS=erlcfg_parser

OBJECTS=erlcfg_lexer


all: ${LEEX_OBJECTS:%=$(EBIN_DIR)/%.erl} ${OBJECTS:%=$(EBIN_DIR)/%.beam}
	@echo All Done


clean:
	@rm -rf $(EBIN_DIR)
	@rm -f erl_crash.dump
