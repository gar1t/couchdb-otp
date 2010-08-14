-module(couchdb_sup).

-behaviour(supervisor).

-export([start_link/0]).

% Non-standard start functions.
-export([start_config_wrapper/1]).
-export([start_httpd_wrapper/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%---------------------------------------------------------------------------
%% @doc Standard supervisor init/1.
%%
%% Any non-standard init is moved into module start_xxx_wrapper methods (see
%% below).
%%
%% The restart and shutdown specs are taken from CouchDB.
%%
%% TODO: shouldn't we use a one_for_one policy here?
%%---------------------------------------------------------------------------
init([]) ->
    Ini = couch_ini(),
    {ok, {{one_for_all, 10, 3600},
          [{couch_config, {?MODULE, start_config_wrapper, [Ini]},
            permanent, brutal_kill, worker, [couch_config]},
           {couch_db_update_event, {gen_event, start_link,
                                    [{local, couch_db_update}]},
            permanent, brutal_kill, worker, dynamic},
           {couch_server, {couch_server, sup_start_link, []},
            permanent, 1000, worker, [couch_server]}]}}.

%%---------------------------------------------------------------------------
%% @doc Reads the list of ini files from config.
%%---------------------------------------------------------------------------
couch_ini() ->
    case application:get_env(ini) of
        {ok, Val} -> Val;
        _ -> []
    end.

%%---------------------------------------------------------------------------
%% @doc Sets any missing config values to sensible defaults.
%%
%% This is a work around for any values that CouchDB assumes to be in config.
%% --------------------------------------------------------------------------
start_config_wrapper(Ini) ->
    {ok, Pid} = couch_config:start_link(Ini),
    % No default value for max_dbs_open - use value from default.ini.
    set_missing_config("couchdb", "max_dbs_open", "100"),
    % No default value for view_index_dir, use same val as database_dir.
    set_missing_config("couchdb", "view_index_dir",
                       couch_config:get("couchdb", "database_dir", ".")),
    % We need to start the ICU driver somewhere and this is the soonest
    % possible point - right after the config proc is started. This would
    % typically be loaded by the supervised process that used it, but couch
    % loads it outside a supervisor tree.
    couchdb_util:start_icu_driver(),
    {ok, Pid}.

%%---------------------------------------------------------------------------
%% @doc Sets missing required httpd config values.
%% --------------------------------------------------------------------------
start_httpd_wrapper() ->

    % Default auth handler is required to run anything over http.
    set_missing_config("httpd", "authentication_handlers",
                       "{couch_httpd_auth, "
                       "default_authentication_handler}"),
    % Handler for root is required to avoid unhandled error.
    set_missing_config("httpd_global_handlers", "/",
                       "{couch_httpd_misc_handlers, handle_welcome_req, "
                       "<<\"Welcome\">>}"),
    % Futon requires an auth db value, though it can be blank.
    set_missing_config("couch_httpd_auth", "authentication_db", ""),
    couch_httpd:start_link().

%%---------------------------------------------------------------------------
%% Private functions
%%---------------------------------------------------------------------------

set_missing_config(S, K, Val) ->
    case couch_config:get(S, K) of
        undefined -> couch_config:set(S, K, Val);
        _ -> ok
    end.
