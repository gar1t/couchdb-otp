-module(couchdb_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_httpd/0,
         stop_httpd/0,
         start_log/0,
         stop_log/0]).

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
    % Default auth handler is required to run anything over http.
    set_missing_config("httpd", "authentication_handlers",
                       "{couch_httpd_auth, "
                       "default_authentication_handler}"),
    % Handler for root is required to avoid unhandled error.
    set_missing_config("httpd_global_handlers", "/",
                       "{couch_httpd_misc_handlers, handle_welcome_req, "
                       "<<\"Welcome\">>}"),
    start_optional_service(httpd_spec()).

httpd_spec() ->
    {couch_httpd, {couch_httpd, start_link, []},
     permanent, 1000, worker, [couch_httpd]}.

%%---------------------------------------------------------------------------
%% @doc Stops the httpd service.
%% --------------------------------------------------------------------------
stop_httpd() ->
    stop_optional_service(httpd_spec()).

%%---------------------------------------------------------------------------
%% @doc Starts the couch log service.
%%
%% TODO - support auto-start
%% --------------------------------------------------------------------------
start_log() ->
    start_optional_service(log_spec()).

log_spec() ->
    {couch_log, {couch_log, start_link, []},
     permanent, brutal_kill, worker, [couch_log]}.

%%---------------------------------------------------------------------------
%% @doc Stops the log service.
%% --------------------------------------------------------------------------
stop_log() ->
    stop_optional_service(log_spec()).

%%---------------------------------------------------------------------------
%% @doc Sets any missing config values to sensible defaults.
%%
%% This is a work around for any values that CouchDB assumes to be in config.
%% --------------------------------------------------------------------------
start_config_wrapper(Ini) ->
    {ok, Pid} = couch_config:start_link(Ini),
    set_missing_config("couchdb", "max_dbs_open", "100"),
    {ok, Pid}.

%%---------------------------------------------------------------------------
%% Private functions
%%---------------------------------------------------------------------------

set_missing_config(S, K, Val) ->
    case couch_config:get(S, K) of
        undefined -> couch_config:set(S, K, Val);
        _ -> ok
    end.

start_optional_service(Spec) ->
    {Id, _,_,_,_,_} = Spec,
    case supervisor:start_child(?SERVER, Spec) of
        {ok, Pid} ->
            {ok, Pid};
        {error, already_present} ->
            supervisor:restart_child(?SERVER, Id);
        Other -> Other
    end.

stop_optional_service({Id, _,_,_,_,_}) ->
    supervisor:terminate_child(?SERVER, Id).
