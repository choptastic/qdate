all: compile

# Check if rebar3.mk exists, and if not, download it
ifeq ("$(wildcard rebar3.mk)","")
$(shell curl -O https://raw.githubusercontent.com/choptastic/rebar3.mk/master/rebar3.mk)
endif

# rebar3.mk adds a new rebar3 rule to your Makefile
# (see https://github.com/choptastic/rebar3.mk) for full info
include rebar3.mk

compile: rebar3
	$(REBAR) compile

update: rebar3
	$(REBAR) update

test: compile
	$(REBAR) eunit

dialyzer: compile
	$(REBAR) dialyzer

travis: test dialyzer

dev:
	mkdir -p _checkouts
	cd _checkouts; git clone https://github.com/choptastic/qdate_localtime


run: rebar3
	$(REBAR) shell

push_tags:
	git push --tag

pull_tags:
	git pull --tag

publish: rebar3 pull_tags
	$(REBAR) hex publish

