PROJECT = clone_tools
ROOT = .

EBINS = $(shell find $(ROOT)/lib -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS = +debug_info +warn_export_all -I$(ROOT)/lib $(PA)
     # +bin_opt_info

DIRS =  . \
    $(ROOT)/lib/ejson-0.1.0

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app
	@./escriptize -o db_clone -d "ebin lib/ejson-0.1.0/ebin lib/ejson-0.1.0/priv"
	@chmod +x db_clone
	@echo "\n\tdb_clone successfuly created\n"

ebin/$(PROJECT).app: src/*.erl
	@mkdir -p ebin/
	erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl
	@mkdir -p test/
	erlc -v $(ERLC_OPTS)  -o test/ -pa test/  $?

clean:
	rm -f ebin/*
	rm -f test/*.beam test/$(PROJECT).app
	rm -f erl_crash.dump

test: clean compile-test eunit

eunit: compile-test
	erl -noshell -pa test -eval "eunit:test([$(MODULES)], [verbose])" -s init stop
