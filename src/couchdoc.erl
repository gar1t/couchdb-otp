-module(couchdoc).

-include("couch_db.hrl").

-export([new/0,
         new/1,
         new/2,
         get_id/1,
         get_rev/1,
         get_attr_names/1,
         set_attr/3,
         get_attr/2,
         get_attr/3]).

new() -> #doc{id=couch_uuids:random()}.

new(Attrs) -> #doc{id=couch_uuids:random(), body={Attrs}}.

new(Id, Body) -> #doc{id=Id, body=Body}.

get_id(#doc{id=Id}) -> Id.

get_rev(#doc{revs={_, []}}) -> undefined;
get_rev(#doc{revs={Pos, [RevId|_]}}) -> couch_doc:rev_to_str({Pos, RevId}).

get_attr_names(#doc{body={Attrs}}) -> proplists:get_keys(Attrs).

set_attr(Name, Value, #doc{body={Attrs}}=Doc) ->
    Doc#doc{body={[{Name, Value}|proplists:delete(Name, Attrs)]}}.

get_attr(Name, #doc{body={Attrs}}) ->
    proplists:get_value(Name, Attrs).

get_attr(Name, #doc{body={Attrs}}, Default) ->
    proplists:get_value(Name, Attrs, Default).
