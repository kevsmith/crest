DEPS := deps/jiffy deps/gproc deps/webmachine deps/proper deps/lager
DIALYZER_SRC := $(wildcard src/*.erl)
DIALYZER := dialyzer -nn

all: dialyzer compile all_tests

shell: compile
ifdef NORUN
	@/bin/echo "Starting VM ONLY"
	@/bin/sleep 1
	@erl -pa ./deps/*/ebin -pa ./ebin -boot start_sasl
endif
ifndef NORUN
	@/bin/echo "Starting VM AND crest"
	@/bin/sleep 1
	@erl -pa ./deps/*/ebin -pa ./ebin -eval "crest_app:manual_start()."
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

compile: $(DEPS)
	@$(REBAR) compile

compile_app:
	@$(REBAR) skip_deps=true compile

plt_clean:
	@$(DIALYZER) --build_plt --apps erts kernel stdlib crypto public_key ssl

plt:
	@$(DIALYZER) --add_to_plt --src deps/*/src

dialyzer:

	$(DIALYZER) -Wunderspecs -Werror_handling -Wrace_conditions --src -I deps $(DIALYZER_SRC)

$(DEPS):
	@$(REBAR) get-deps

eunit: compile
	@$(REBAR) -v skip_deps=true eunit

eunit_app: compile_app
	@$(REBAR) -v skip_deps=true eunit

test: eunit

tags:
	@find src deps -name "*.[he]rl" -print | etags -

distclean: relclean
	@rm -rf deps
	@$(REBAR) clean
