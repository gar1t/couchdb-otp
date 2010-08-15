-module(couchterm).

-include("couch_db.hrl").

% TODO - is this module a good idea? It allows direct use of Erlang terms like
% records and what not. Restricting to proplists isn't awful, but I could see
% applications that would want to store terms directly.

-export([new/1,
         new/2,
         id/1,
         rev/1,
         term/1,
         deleted/1]).

new(Term) -> new(couch_uuids:random(), Term).

new(Id, Term) ->
    #doc{id=to_bin(Id), body={[Term]}}.

id(#doc{id=Id}) -> Id.

rev(#doc{revs={_, []}}) -> undefined;
rev(#doc{revs={Pos, [RevId|_]}}) ->
    couch_doc:rev_to_str({Pos, RevId}).

term(#doc{body={[Term]}}) -> Term.

deleted(#doc{deleted=D}) -> D.

% TODO - to_bin is showing up a lot, do we want to include it in an hrl?

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L).
