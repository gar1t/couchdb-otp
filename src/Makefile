# Set to top level dir of CouchDB source package
COUCHDB_DIR := ../../couchdb

# Libraries we depend on, defaults based on CouchDB source package
COUCHDB_EBIN := $(COUCHDB_DIR)/src/couchdb
MOCHIWEB_EBIN := $(COUCHDB_DIR)/src/mochiweb

# When using shell target, specify CONFIG file name in ../priv dir
CONFIG := empty

# For shell target, use CONFIG_PATH to point to config file (without ".config"
# suffix)
CONFIG_PATH := ../priv/$(CONFIG)

# To remove tests from modules, override ERLC_OPTS.
#
ERLC_OPTS := -DTEST

# Generally, the rest of these don't need to be overridden.

APP_DIR := ..
EBIN_DIR := $(APP_DIR)/ebin
INCLUDE_DIR := $(APP_DIR)/include

ERL := erl
ERLC := erlc
ERLC_FLAGS := $(ERLC_OPTS)

ifndef no_debug_info
  ERLC_FLAGS += +debug_info
endif

ifdef debug
  ERLC_FLAGS += -Ddebug
endif

ERL_SOURCES := $(wildcard *.erl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.beam)

PA_OPTS := -pa $(MOCHIWEB_EBIN) -pa $(COUCHDB_EBIN) -pa $(EBIN_DIR)
ERL_OPTS := $(PA_OPTS) -config $(CONFIG_PATH)

# TODO - To makefile gurus, I want to add a dependency on _test.hrl files so
# that we can used the "include test" pattern and have the erl files recompile
# when their tests change. How can we add a dependency on tests but still
# compile modules that don't have _tests.hrl files?

$(EBIN_DIR)/%.beam: %.erl
	$(ERLC) -I $(INCLUDE_DIR) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

app: $(ERL_OBJECTS)

release: app

test: $(ERL_OBJECTS)
	$(ERL) $(TEST_OPTS) $(ERL_OPTS) -noshell -s $(TESTS) test -s init stop

shell: $(ERL_OBJECTS)
	$(ERL) $(SHELL_OPTS) $(ERL_OPTS) $(SHELL_OPTS) -s reloader -s couchdb

clean:
	rm -rf $(EBIN_DIR)/*.beam
