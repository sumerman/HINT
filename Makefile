#REBAR=`which rebar || ./rebar`
REBAR=./rebar
.PHONY: all compile clean eunit test eqc doc check dialyzer deps cleandeps tags

DIRS=src 

#all: compile eunit doc
all: compile 

deps:
	$(REBAR) get-deps

compile: deps
	$(REBAR) compile

clean:
	$(REBAR) clean
	-rm log/*

rel: compile
	-rm -r rel/clamorous
	$(REBAR) generate

cleandeps:
	$(REBAR) delete-deps

eunit:
	$(REBAR) skip_deps=true eunit

test: eunit

doc:
	$(REBAR) doc

tags:
	erl -s tags subdir "./" -s init stop -noshell

dialyzer: compile
	dialyzer --fullpath apps/*/ebin -Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs
