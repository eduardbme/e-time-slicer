APP = e_time_slicer
REBAR ?= rebar
# verify rebar version
# rebar2 does not have -v option, so the return value would be 1
REBAR_VERSION := $(shell $(REBAR) -v 1> /dev/null 2> /dev/null; echo $$?)
ifeq ($(REBAR_VERSION),1)
    REBAR_ = $(REBAR) --config rebar2.config
else
    REBAR_ = $(REBAR)
endif
DIALYZER = dialyzer

DIALYZER_WARNINGS = -Wunmatched_returns -Werror_handling \
                    -Wrace_conditions -Wunderspecs

.PHONY: all compile test qc clean get-deps build-plt dialyze

all: deps compile

deps: get-deps compile

compile:
	@$(REBAR_) compile

test: compile
	@$(REBAR_) eunit

clean:
	@$(REBAR_) clean

run:
	$(REBAR_) compile
	erl -pa ./ebin -eval "application:ensure_all_started($(APP))"

get-deps:
	@$(REBAR_) get-deps

.dialyzer_plt:
	@$(DIALYZER) --build_plt --output_plt .dialyzer_plt \
	    --apps kernel stdlib

build-plt: .dialyzer_plt

dialyze: build-plt
	@$(DIALYZER) --src src --plt .dialyzer_plt $(DIALYZER_WARNINGS)
