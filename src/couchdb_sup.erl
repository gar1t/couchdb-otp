-module(couchdb_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_optional/0,
         start_optional/1,
         stop_optional/0,
         stop_optional/1]).

% Non-standard start functions.
-export([start_config_wrapper/1]).
-export([start_httpd_wrapper/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(VIEW_SUPPORT, [couch_view, couch_task_status, couch_query_servers]).
-define(HTTPD_SUPPORT, [couch_httpd, couch_uuids|?VIEW_SUPPORT]).
-define(ALL_OPTIONAL, [couch_log|?HTTPD_SUPPORT]).

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
%% @doc Starts all of the optional CouchDB services. To start a specific list
%% of services, use start_optional/1.
%% ---------------------------------------------------------------------------

start_optional() ->
    start_optional(?ALL_OPTIONAL).

%%---------------------------------------------------------------------------
%% @doc Starts a specific list of optional CouchDB services.
%% ---------------------------------------------------------------------------

start_optional(view_support) ->
    start_optional(?VIEW_SUPPORT);
start_optional(httpd_support) ->
    start_optional(?HTTPD_SUPPORT);
start_optional([]) -> ok;
start_optional([couch_httpd|T]) -> 
    start_optional_service({couch_httpd, {?MODULE, start_httpd_wrapper, []},
                            permanent, 1000, worker, [couch_httpd]}),
    start_optional(T);
start_optional([couch_uuids|T]) -> 
    % couch_uuids:start/0 is a link.
    start_optional_service({couch_uuids, {couch_uuids, start, []},
                            permanent, brutal_kill, worker, [couch_uuids]}),
    start_optional(T);
start_optional([Module|T]) ->
    start_optional_service({Module, {Module, start_link, []},
                            permanent, brutal_kill, worker, [Module]}),
    start_optional(T).


%% ---------------------------------------------------------------------------
%% @doc Stops the optional CouchDB services.
%% ---------------------------------------------------------------------------

stop_optional() ->
    stop_optional(lists:reverse(?ALL_OPTIONAL)).

%% ---------------------------------------------------------------------------
%% @doc Stops a specific list of optional CouchDB services.
%% ---------------------------------------------------------------------------

stop_optional(view_support) ->
    stop_optional(lists:reverse(?VIEW_SUPPORT));
stop_optional(httpd_support) ->
    stop_optional(lists:reverse(?HTTPD_SUPPORT));
stop_optional([]) -> ok;
stop_optional([Module|T]) ->
    stop_optional_service(Module),
    stop_optional(T).
 
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

start_optional_service(Spec) ->
    {Id, _,_,_,_,_} = Spec,
    case supervisor:start_child(?SERVER, Spec) of
        {ok, Pid} ->
            {ok, Pid};
        {error, already_present} ->
            supervisor:restart_child(?SERVER, Id);
        {error, {already_started, Pid}} ->
            {error, {already_started, Pid}};
        Err -> 
            exit(Err)
    end.

stop_optional_service(Id) ->
    supervisor:terminate_child(?SERVER, Id).
