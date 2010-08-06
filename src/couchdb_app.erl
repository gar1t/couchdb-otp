-module(couchdb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case couchdb_sup:start_link() of
        {ok, Pid} -> 
            couchdb_util:start_icu_driver(),
            {ok, Pid};
        Other -> Other
    end.

stop(_State) ->
    ok.
