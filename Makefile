REBAR_PATH = $(shell which rebar3)

ifeq ($(REBAR_PATH),)
REBAR = $(shell pwd)/rebar3
else
REBAR = rebar3
endif


all: compile

compile:
	$(REBAR) compile

update:
	$(REBAR) update

test: compile
	$(REBAR) eunit

run:
	$(REBAR) shell

publish:
	$(REBAR) as pkg upgrade
	$(REBAR) as pkg hex publish
	$(REBAR) upgrade

