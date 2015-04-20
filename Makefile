.PHONY: all run tests

all:
	rebar skip_deps=true compile

run:
	erl -env ERL_LIBS `pwd` +pc unicode

tests:
	rebar skip_deps=true eunit
