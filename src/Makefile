EBIN_DIR=../ebin
INCLUDE_DIR=../include
ERLC=erlc
ERLC_FLAGS= -W -I $(INCLUDE_DIR) -o $(EBIN_DIR)
ERL=erl -noshell

%.erl: %.xrl
	@$(ERL) -eval "Filename = filename:basename(\"$<\", \".xrl\"), leex:file(Filename), halt()."

%.erl: %.yrl
	@$(ERL) -eval "Filename = filename:basename(\"$<\", \".yrl\"), yecc:file(Filename), halt()."

$(EBIN_DIR)/%.beam: %.erl
	@mkdir -p $(EBIN_DIR)
	$(ERLC) $(ERLC_FLAGS) $<

$(EBIN_DIR)/%.app: %.app
	@mkdir -p $(EBIN_DIR)
	@cp $< $@


LEEX_OBJECTS=erlcfg_lexer \
			 erlcfg_schema_lexer

YECC_OBJECTS=erlcfg_parser \
			 erlcfg_schema_parser

OBJECTS=erlcfg_lexer \
		erlcfg_parser \
		erlcfg_schema_lexer \
		erlcfg_schema_parser \
		node_addr \
		erlcfg_node \
		erlcfg_interp \
		erlcfg_data \
		erlcfg

APP=erlcfg


all: ${LEEX_OBJECTS:%=%.erl} ${YECC_OBJECTS:%=%.erl} ${OBJECTS:%=$(EBIN_DIR)/%.beam} ${APP:%=$(EBIN_DIR)/%.app}
	@echo All Done


clean:
	@rm -f *_lexer.erl
	@rm -f *_parser.erl
	@rm -rf $(EBIN_DIR)
	@rm -f erl_crash.dump
