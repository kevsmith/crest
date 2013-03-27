DEPS = deps/jiffy deps/gproc deps/webmachine deps/proper deps/lager
GENERATED_SRC_FILES = ebin/crest_lexer.beam ebin/crest_parser.beam
DIALYZER = dialyzer -nn
DIALYZER_PLT = crest.plt

all: dialyzer

shell: compile
ifdef NORUN
	@/bin/echo "Starting VM ONLY"
	@/bin/sleep 1
	@erl -K true -pa ./deps/*/ebin -pa ./ebin -boot start_sasl
endif
ifndef NORUN
	@/bin/echo "Starting VM AND crest"
	@/bin/sleep 1
	@erl -K true -pa ./deps/*/ebin -pa ./ebin -eval "crest_app:manual_start()."
endif

use_locked_config = $(wildcard USE_REBAR_LOCKED)
ifeq ($(use_locked_config),USE_REBAR_LOCKED)
  rebar_config = rebar.config.lock
else
  rebar_config = rebar.config
endif
REBAR = rebar -C $(rebar_config)

clean:
	@$(REBAR) clean

allclean: depclean clean

depclean:
	@rm -rf deps

$(DEPS):
	@$(REBAR) get-deps

compile: $(DEPS)
	@$(REBAR) compile

compile_app:
	@$(REBAR) skip_deps=true compile

plt_clean:
	@$(DIALYZER) --build_plt --apps erts kernel stdlib crypto public_key ssl

$(DIALYZER_PLT): compile
	@$(DIALYZER) --build_plt deps/*/ebin --output_plt $(DIALYZER_PLT)

dialyzer: compile
	@rm -f $(GENERATED_SRC_FILES)
	@$(DIALYZER) --plts ${HOME}/.dialyzer_plt $(DIALYZER_PLT) -Wunderspecs -Werror_handling -Wrace_conditions -I deps -r ebin
	@rebar skip_deps=true compile >& /dev/null

eunit: compile
	@$(REBAR) -v skip_deps=true eunit

eunit_app: compile_app
	@$(REBAR) -v skip_deps=true eunit

test: eunit

tags:
	@find src deps -name "*.[he]rl" -print | etags -

distclean:
	@rm -rf deps
	@$(REBAR) clean
