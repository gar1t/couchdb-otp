-module(couchdb_sup).

-behaviour(supervisor).

-export([start_link/0]).

% Non-standard start functions.
-export([start_config/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%---------------------------------------------------------------------------
%% @doc Standard supervisor init/1.
%%
%% Any non-standard init is moved into module start_xxx methods (see below).
%%
%% The restart and shutdown specs are taken from CouchDB.
%%
%% TODO: shouldn't we use a one_for_one policy here?
%%---------------------------------------------------------------------------
init([]) ->
    Ini = couch_ini(),
    {ok, {{one_for_all, 10, 3600},
          [{couch_config, {?MODULE, start_config, [Ini]},
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
start_config(Ini) ->
    {ok, Sup} = couch_config:start_link(Ini),
    set_missing_config("couchdb", "max_dbs_open", "100"),
    {ok, Sup}.

set_missing_config(S, K, Val) ->
    case couch_config:get(S, K) of
        undefined -> couch_config:set(S, K, Val);
        _ -> ok
    end.
