-module(couchdb_optional).

-export([start/0, start/1, stop/0, stop/1]).

-define(SUP, couchdb_sup).

-define(VIEW_SUPPORT, [couch_view, couch_task_status, couch_query_servers]).
-define(HTTPD_SUPPORT, [couch_httpd, couch_uuids|?VIEW_SUPPORT]).
-define(ALL_OPTIONAL, [couch_log|?HTTPD_SUPPORT]).

start() ->
    start(?ALL_OPTIONAL).

start(view_support) ->
    start(?VIEW_SUPPORT);
start(httpd_support) ->
    start(?HTTPD_SUPPORT);
start([]) -> ok;
start([couch_httpd|T]) -> 
    start_service({couch_httpd, {?SUP, start_httpd_wrapper, []},
                   permanent, 1000, worker, [couch_httpd]}),
    start(T);
start([couch_uuids|T]) -> 
    % couch_uuids:start/0 is a link.
    start_service({couch_uuids, {couch_uuids, start, []},
                   permanent, brutal_kill, worker, [couch_uuids]}),
    start(T);
start([Module|T]) ->
    % We use a 1 second allowance for clean shutdown by default - some of the
    % couch procs need this.
    start_service({Module, {Module, start_link, []},
                   permanent, 1000, worker, [Module]}),
    start(T).

stop() ->
    stop(lists:reverse(?ALL_OPTIONAL)).

stop(view_support) ->
    stop(lists:reverse(?VIEW_SUPPORT));
stop(httpd_support) ->
    stop(lists:reverse(?HTTPD_SUPPORT));
stop([]) -> ok;
stop([Module|T]) ->
    stop_service(Module),
    stop(T).

start_service(Spec) ->
    {Id, _,_,_,_,_} = Spec,
    case supervisor:start_child(?SUP, Spec) of
        {ok, Pid} ->
            {ok, Pid};
        {error, already_present} ->
            supervisor:restart_child(?SUP, Id);
        {error, {already_started, Pid}} ->
            {error, {already_started, Pid}};
        Err -> 
            exit(Err)
    end.

stop_service(Id) ->
    supervisor:terminate_child(?SUP, Id).
