BUILD_DIR := $(CURDIR)/_build

REBAR := rebar3

CT_READABLE ?= false

compile:
	$(REBAR) do compile, dialyzer, xref

.PHONY: all
all: compile test

.PHONY: clean
clean: distclean

.PHONY: distclean
distclean:
	@rm -rf _build erl_crash.dump rebar3.crashdump rebar.lock

.PHONY: xref
xref:
	$(REBAR) xref

.PHONY: eunit
eunit: compile
	$(REBAR) eunit verbose=true

.PHONY: test
test: smoke-test concuerror_test

.PHONY: smoke-test
smoke-test:
	$(REBAR) do eunit, ct -v --readable=$(CT_READABLE)

cover: | test
	$(REBAR) cover

.PHONY: coveralls
coveralls:
	@rebar3 as test coveralls send

.PHONY: dialyzer
dialyzer:
	$(REBAR) dialyzer

##########################################################################################
# Concuerror
##########################################################################################

CONCUERROR := $(BUILD_DIR)/Concuerror/bin/concuerror
CONCUERROR_RUN := $(CONCUERROR) \
	--treat_as_normal shutdown --treat_as_normal normal --treat_as_normal intentional \
	--treat_as_normal optvar_set --treat_as_normal optvar_stopped --treat_as_normal optvar_retry --treat_as_normal killed \
	-x code -x code_server -x error_handler \
	-pa $(BUILD_DIR)/concuerror+test/lib/optvar/ebin

concuerror = $(CONCUERROR_RUN) -f $(BUILD_DIR)/concuerror+test/lib/optvar/test/concuerror_tests.beam -t $(1) || \
	{ cat concuerror_report.txt; exit 1; }

.PHONY: concuerror_test
concuerror_test: $(CONCUERROR)
	rebar3 as concuerror eunit -m concuerror_tests
	$(call concuerror,optvar_read_test)
	$(call concuerror,optvar_unset_test)
	$(call concuerror,optvar_double_wait_test)
	$(call concuerror,optvar_waiter_killed_test)
	$(call concuerror,optvar_wait_multiple_test)
	$(call concuerror,optvar_wait_multiple_timeout_test)
	$(call concuerror,optvar_list_test)
	$(call concuerror,optvar_set_unset_test)
	$(call concuerror,optvar_set_unset_race_test)
	$(call concuerror,optvar_zombie_test)
	$(call concuerror,optvar_zombie_race_test)

$(CONCUERROR):
	mkdir -p _build/
	cd _build && git clone https://github.com/parapluu/Concuerror.git
	$(MAKE) -C _build/Concuerror/

##########################################################################################
# Docs
##########################################################################################
DOC_DIR=doc
DOC_SRC_DIR=$(DOC_DIR)/src

UMLS=$(wildcard $(DOC_SRC_DIR)/*.uml)
PICS=$(UMLS:$(DOC_SRC_DIR)/%.uml=$(DOC_DIR)/%.png)

.PHONY: doc
doc: $(PICS)

$(DOC_DIR)/%.png: $(DOC_SRC_DIR)/%.uml
	cat $< | plantuml -pipe > $@
