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

# For shell target, use ERL_OPTS to override default options (see below)
ERL_OPTS := -config $(CONFIG_PATH)

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

PA_OPTS := -pa $(EBIN_DIR) -pa $(COUCHDB_EBIN) -pa $(MOCHIWEB_EBIN)

$(EBIN_DIR)/%.beam: %.erl
	$(ERLC) -I $(INCLUDE_DIR) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

app:  $(ERL_OBJECTS)

release: app

test: $(ERL_OBJECTS)
	$(ERL) $(ERL_OPTS) $(PA_OPTS) -noshell -s $(TESTS) test -s init stop

shell: $(ERL_OBJECTS)
	$(ERL) $(ERL_OPTS) $(PA_OPTS) $(SHELL_OPTS) -s reloader -s couchdb

clean:
	rm -rf $(EBIN_DIR)/*.beam
