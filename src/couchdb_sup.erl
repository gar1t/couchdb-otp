-module(couchdb_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_httpd/0,
         stop_httpd/0]).

% Non-standard start functions.
-export([start_config_wrapper/1]).

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
%% @doc Starts the httpd service.
%%
%% This is exernalized to make httpd optional at runtime.
%%
%% TODO - this should be optionally confiugrable to auto-start with the
%% other supervised processes
%% --------------------------------------------------------------------------
start_httpd() ->
    case supervisor:start_child(couchdb_sup, httpd_spec()) of
        {ok, Pid} ->
            set_missing_config("httpd", "authentication_handlers",
                               "{couch_httpd_auth, "
                               "default_authentication_handler}"),
            {ok, Pid};
        {error, already_present} ->
            supervisor:restart_child(couchdb_sup, couch_httpd);
        Other -> Other
    end.

httpd_spec() ->
    {couch_httpd, {couch_httpd, start_link, []},
     permanent, 1000, worker, [couch_httpd]}.

%%---------------------------------------------------------------------------
%% @doc Stops the httpd service.
%% --------------------------------------------------------------------------
stop_httpd() ->
    supervisor:terminate_child(couchdb_sup, couch_httpd).

%%---------------------------------------------------------------------------
%% @doc Sets any missing config values to sensible defaults.
%%
%% This is a work around for any values that CouchDB assumes to be in config.
%% --------------------------------------------------------------------------
start_config_wrapper(Ini) ->
    {ok, Pid} = couch_config:start_link(Ini),
    set_missing_config("couchdb", "max_dbs_open", "100"),
    {ok, Pid}.

set_missing_config(S, K, Val) ->
    case couch_config:get(S, K) of
        undefined -> couch_config:set(S, K, Val);
        _ -> ok
    end.
