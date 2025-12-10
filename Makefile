.PHONY: all compile xref eunit check_plt build_plt dialyzer doc callgraph graphviz clean distclean

REBAR := rebar3
APPS = erts kernel stdlib sasl crypto compiler inets mnesia public_key runtime_tools snmp syntax_tools tools xmerl
PLT_FILE = .leo_cache_dialyzer_plt
DOT_FILE = leo_cache.dot
CALL_GRAPH_FILE = leo_cache.png

all: compile xref eunit

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref

eunit:
	@$(REBAR) eunit

check_plt:
	@$(REBAR) compile
	dialyzer --check_plt --plt $(PLT_FILE) --apps $(APPS)

build_plt:
	@$(REBAR) compile
	dialyzer --build_plt --output_plt $(PLT_FILE) --apps $(APPS)

dialyzer:
	@$(REBAR) dialyzer

doc:
	@$(REBAR) edoc

callgraph: graphviz
	dot -Tpng -o$(CALL_GRAPH_FILE) $(DOT_FILE)

graphviz:
	$(if $(shell which dot),,$(error "To make the depgraph, you need graphviz installed"))

clean:
	@$(REBAR) clean

distclean: clean
	@rm -rf _build
