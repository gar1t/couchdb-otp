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
    {reply, apply_maps(Funs, Doc, []), State};

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
        Term -> term_to_fun(Term)
    catch
        error:badarg -> to_fun(binary_to_list(B))
    end;

to_fun(S) when is_list(S) ->
    case erl_scan:string(S) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, [Parsed]} ->
                    try erl_eval:expr(Parsed, []) of
                        {value, Term, _} -> term_to_fun(Term)
                    catch
                        error:Err -> throw(Err)
                    end;
                {error, {Line, _Mod, [Msg, Params]}} ->
                    throw(lists:concat([Msg, Params, " on line ", Line]))
            end;
        {error, {Line, erl_scan, {string, _Char, Str}}, _Loc} ->
            throw(lists:concat(["Bad char on line ", Line, " at: ", Str]))
    end.

%% ---------------------------------------------------------------------------
%% @doc Returns a support fun type or throws {invalid_fun_spec, Term}.
%% ---------------------------------------------------------------------------

term_to_fun(F) when is_function(F) -> F;
term_to_fun({M, F}=T) when is_atom(M) andalso is_atom(F) -> T;
term_to_fun({M, F, A}=T) when is_atom(M) andalso 
                              is_atom(F) andalso 
                              is_list(A) -> T;
term_to_fun(Term) -> throw({invalid_fun_spec, Term}). 

%% ---------------------------------------------------------------------------
%% @doc Applies a list of maps to a doc. This reverses the list of mapped
%% results by design.
%% ---------------------------------------------------------------------------

apply_maps([], _, Acc) -> Acc;
apply_maps([F|T], Doc, Acc) ->
    apply_maps(T, Doc, [apply_map(F, Doc)|Acc]).


%% ---------------------------------------------------------------------------
%% @doc Applies a fun to a document map operation.
%% ---------------------------------------------------------------------------

apply_map(Fun, Doc) ->
    AccIn = [],
    try apply_map(Fun, Doc, AccIn) of
        AccOut -> to_couch_map(AccOut, [])
    catch
        _:Err -> throw({map_error, {Err, erlang:get_stacktrace()}})
    end.

apply_map({M, F}, Doc, AccIn) ->
    apply(M, F, [Doc, AccIn]);
apply_map({M, F, A}, Doc, AccIn) when is_list(A) -> 
    apply(M, F, [Doc, AccIn] ++ A);
apply_map(F, Doc, AccIn) when is_function(F) ->
    F(Doc, AccIn).

%% ---------------------------------------------------------------------------
%% @doc Converts {Key, Val} tuples into [Key, Val] and single key values into
%% [Key, null].
%% ---------------------------------------------------------------------------

to_couch_map([], Acc) -> Acc;
to_couch_map([{K, V}|T], Acc) ->
    to_couch_map(T, [[K, V]|Acc]);
to_couch_map([K|T], Acc) ->
    to_couch_map(T, [[K, null]|Acc]).
