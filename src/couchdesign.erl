-module(couchdesign).

-export([new/2]).

% TODO - Do we want to hard code this? Could it be an app config'ed value?
% Should it be overridable in new?
-define(LANGUAGE, <<"otp">>).

%% ----------------------------------------------------------------------------
%% @spec new(doc_name(), [options()]) -> {ok, doc()}
%%
%%   doc_name()      = string()
%%   option()        = {view, view()}
%%   view()          = {view_name(), function()}
%%   view_name()     = string()
%%
%% @doc Creates a new design document.
%% ----------------------------------------------------------------------------

new(DocName, Options) ->
    couchdoc:new("_design/" ++ DocName,
                 [{<<"language">>, ?LANGUAGE},
                  {<<"views">>, views(Options, [])}]).

views([], Acc) -> Acc;
views([{view, {Name, MapFun}}|T], Acc) ->
    View = {to_bin(Name), [{<<"map">>, encode_map_fun(MapFun, Name)}]},
    views(T, [View|Acc]);
views([Other|_], _) -> erlang:exit({bad_option, Other}).

encode_map_fun(F, Name) when is_function(F) ->
    validate_map_arity(F, Name),
    term_to_binary(F);
encode_map_fun({M, F}, _) when is_atom(M) andalso is_atom(F) ->
    list_to_binary(io_lib:format("~p.", [{M, F}]));
encode_map_fun(Other, _) ->
    erlang:exit({bad_map_fun, Other}).

validate_map_arity(F, Name) ->
    case erlang:fun_info(F, arity) of
        {arity, 2} -> ok;
        _ -> erlang:error({bad_map_arity, Name})
    end.

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L);
to_bin(Other) -> erlang:error({badarg, Other}).
