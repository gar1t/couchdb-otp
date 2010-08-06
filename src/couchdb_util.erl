-module(couchdb_util).

-export([start_icu_driver/0]).

%%---------------------------------------------------------------------------
%% @doc Starts the native drive used by CouchDB. The lib dir can be specified
%% in the CouchDB configuration (ini files) under couchdb/util_driver_dir. By
%% default looks in the CouchDB priv for a "lib" dir.
%% ---------------------------------------------------------------------------

start_icu_driver() ->
    LibDir = case couch_config:get("couchdb", "util_driver_dir") of
                 undefined -> couch_lib_dir();
                 Dir -> Dir
             end,
    couch_util:start_driver(LibDir).

%%---------------------------------------------------------------------------
%% @doc Looks for the Couch lib dir. Error if it can't be found. First
%% looks in the canonical app structure relative to the couch beam files
%% (i.e. EBIN_DIR/../priv/lib) and then looks in the location the CouchDB build
%% builds the shared library (i.e. COUCH_SRC/priv/.libs) (works on Linux, not
%% sure about other platforms).
%% ---------------------------------------------------------------------------
couch_lib_dir() ->
    {file, Mod} = code:is_loaded(couch_config),
    Ebin = filename:dirname(Mod),
    Installed = filename:join(Ebin, "../priv/lib"),
    case filelib:is_dir(Installed) of
        true -> Installed;
        false ->
            Dev = filename:join(Ebin, "priv/.libs"),
            case filelib:is_dir(Dev) of
                true -> Dev;
                false -> erlang:error(no_couch_lib)
            end
    end.
