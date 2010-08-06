-module(couchdoc).

-include("couch_db.hrl").

-export([new/0,
         new/1,
         new/2,
         get_id/1,
         get_rev/1,
         get_attr_names/1,
         get_attrs/1,
         set_attr/3,
         get_attr/2,
         get_attr/3,
         del_attr/2,
         is_deleted/1]).

%%---------------------------------------------------------------------------
%% @doc Creates a new empty document. Uses a random UUID for the ID.
%%
%% TODO: could use utc_random as well, make configurable (app cfg)
%%---------------------------------------------------------------------------

new() -> #doc{id=couch_uuids:random()}.

%%---------------------------------------------------------------------------
%% @doc Creates a document with the specified Attrs. Uses a UUID for its ID.
%% ---------------------------------------------------------------------------

new(Attrs) -> #doc{id=couch_uuids:random(), body={Attrs}}.

%%---------------------------------------------------------------------------
%% @doc Creates a document with the specified Id and Attrs. Id must be a binary
%% or a string. String Ids are converted to binary.
%% ---------------------------------------------------------------------------

new(Id, Attrs) -> #doc{id=to_bin(Id), body={Attrs}}.

%%---------------------------------------------------------------------------
%% @doc Returns the document's Id.
%%---------------------------------------------------------------------------

get_id(#doc{id=Id}) -> Id.

%%---------------------------------------------------------------------------
%% @doc Returns the document's current revision or undefined if the document
%% hasn't been added to the database.
%% ---------------------------------------------------------------------------

get_rev(#doc{revs={_, []}}) -> undefined;
get_rev(#doc{revs={Pos, [RevId|_]}}) -> couch_doc:rev_to_str({Pos, RevId}).

%%---------------------------------------------------------------------------
%% @doc Returns the document's attributes as a property list.
%%---------------------------------------------------------------------------

get_attr_names(#doc{body={Attrs}}) -> proplists:get_keys(Attrs).

%%---------------------------------------------------------------------------
%% @doc Returns all of the document's attributes as a property list.
%% ---------------------------------------------------------------------------

get_attrs(#doc{body={Attrs}}) -> Attrs.

%%---------------------------------------------------------------------------
%% @doc Adds or updates a single attribute for Doc.
%%---------------------------------------------------------------------------

set_attr(Name, Value, #doc{body={Attrs}}=Doc) ->
    Doc#doc{body={[{Name, Value}|proplists:delete(Name, Attrs)]}}.

%%---------------------------------------------------------------------------
%% @doc Returns a single attribute value or undefined if the attribute doesn't
%% exist.
%% ---------------------------------------------------------------------------

get_attr(Name, #doc{body={Attrs}}) ->
    proplists:get_value(Name, Attrs).

%%---------------------------------------------------------------------------
%% @doc Returns a single attribute value or Default of the attribute doesn't
%% exist.
%% ---------------------------------------------------------------------------

get_attr(Name, #doc{body={Attrs}}, Default) ->
    proplists:get_value(Name, Attrs, Default).

%%---------------------------------------------------------------------------
%% @doc Deletes a single document attribute.
%%---------------------------------------------------------------------------

del_attr(Name, #doc{body={Attrs}}=Doc) ->
    Doc#doc{body={proplists:delete(Name, Attrs)}}.

%%---------------------------------------------------------------------------
%% @doc Returns a boolean indicating whether the document is deleted.
%% ---------------------------------------------------------------------------

is_deleted(#doc{deleted=D}) -> D.

%%---------------------------------------------------------------------------
%% Private functions
%%---------------------------------------------------------------------------

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L).
