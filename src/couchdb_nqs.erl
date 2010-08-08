-module(couchdb_nqs).

-behaviour(gen_server).

-export([start_link/0, prompt/2, set_timeout/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {funs=[], timeout=5000}).

-define(RESET,     <<"reset">>).
-define(ADD_FUN,   <<"add_fun">>).
-define(MAP_DOC,   <<"map_doc">>).

-define(ERROR(Msg), [<<"error">>, <<"native_query_server">>, 
                     list_to_binary(Msg)]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

prompt(Nqs, Data) ->
    gen_server:call(Nqs, {prompt, Data}).

set_timeout(Nqs, Timeout) ->
    gen_server:call(Nqs, {timeout, Timeout}).

handle_call({prompt, [?RESET, _]}, _From, State) ->
    {reply, true, State#state{funs=[]}};

handle_call({prompt, [?ADD_FUN, Bin]}, _From, #state{funs=Funs}=State) ->
    {reply, true, State#state{funs=[to_fun(Bin)|Funs]}};

handle_call({prompt, [?MAP_DOC, {Doc}]}, _From, #state{funs=Funs}=State) ->
    {reply, [apply_map(F, Doc) || F <- Funs], State};

handle_call({prompt, Data}, _From, State) ->
    error_logger:error_report({unhandled_prompt, Data}),
    {noreply, State};

handle_call({timeout, Timeout}, _From, State) ->
    {reply, ok, State#state{timeout=Timeout}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% @doc Converts a fun representation into an Erlang function. Funs can be
%% encoded as binary Erlang terms (term_to_binary) or as string source code.
%% ---------------------------------------------------------------------------

to_fun(B) when is_binary(B) ->
    try binary_to_term(B) of
        Fun when is_function(Fun) -> Fun
    catch
        error:badarg -> to_fun(binary_to_list(B))
    end;

to_fun(S) when is_list(S) ->           
    {ok, Tokens, _} = erl_scan:string(S),
    case erl_parse:parse_exprs(Tokens) of
        {ok, [Parsed]} ->
            try erl_eval:expr(Parsed, []) of
                {value, Fun, _} -> Fun
            catch
                error:Err -> throw(Err)
            end;
        {error, {Line, _Mod, [Msg, Params]}} ->
            throw(lists:concat([Msg, Params, " on line ", Line]))
    end.

%% ---------------------------------------------------------------------------
%% @doc Applies a fun to a document map operation.
%% ---------------------------------------------------------------------------

apply_map(Fun, Doc) ->
    to_couch_map(Fun(Doc, []), []).

%% ---------------------------------------------------------------------------
%% @doc Converts {Key, Val} tuples into [Key, Val] and single key values into
%% [Key, null].
%% ---------------------------------------------------------------------------

to_couch_map([], Acc) -> Acc;
to_couch_map([{K, V}|T], Acc) ->
    to_couch_map(T, [[K, V]|Acc]);
to_couch_map([K|T], Acc) ->
    to_couch_map(T, [[K, null]|Acc]).
