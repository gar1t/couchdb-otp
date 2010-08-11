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
         set_attrs/2,
         replace_attrs/2,
         get_attr/2,
         get_attr/3,
         del_attr/2,
         is_deleted/1]).

%%---------------------------------------------------------------------------
%% @doc Creates a new empty document. Uses a random UUID for the ID.
%%
%% TODO: could use utc_random as well, make configurable (app cfg)
%%---------------------------------------------------------------------------

new() -> new(couch_uuids:random(), []).

%%---------------------------------------------------------------------------
%% @doc Creates a document with the specified Attrs. Uses a UUID for its ID.
%% ---------------------------------------------------------------------------

new(Attrs) -> new(couch_uuids:random(), Attrs).

%%---------------------------------------------------------------------------
%% @doc Creates a document with the specified Id and Attrs. Id must be a binary
%% or a string. String Ids are converted to binary.
%% ---------------------------------------------------------------------------

new(Id, Attrs) -> 
    #doc{id=to_bin(Id), 
         body=wrap_couch_obj(Attrs)}.

%%---------------------------------------------------------------------------
%% @doc Returns the document's Id.
%%---------------------------------------------------------------------------

get_id(#doc{id=Id}) -> Id.

%%---------------------------------------------------------------------------
%% @doc Returns the document's current revision or undefined if the document
%% hasn't been added to the database.
%% ---------------------------------------------------------------------------

get_rev(#doc{revs={_, []}}) -> undefined;
get_rev(#doc{revs={Pos, [RevId|_]}}) ->
    couch_doc:rev_to_str({Pos, RevId}).

%%---------------------------------------------------------------------------
%% @doc Returns the document's attributes as a property list.
%%---------------------------------------------------------------------------

get_attr_names(#doc{body={Attrs}}) ->
    proplists:get_keys(Attrs).

%%---------------------------------------------------------------------------
%% @doc Returns all of the document's attributes as a property list.
%% ---------------------------------------------------------------------------

get_attrs(#doc{body={Attrs}}) ->
    strip_couch_obj(Attrs).

%%---------------------------------------------------------------------------
%% @doc Adds or updates a single attribute for Doc.
%%---------------------------------------------------------------------------

set_attr(Name, Value, #doc{body={Attrs}}=Doc) ->
    Doc#doc{body={[{wrap_couch_obj(Name), wrap_couch_obj(Value)}
                   |proplists:delete(Name, Attrs)]}}.


%%---------------------------------------------------------------------------
%% @doc Sets the specified list of attributes leaving any other attributes
%% umodified. This is equivalent to calling set_attr/3 for each modified
%% attribute.
%% ---------------------------------------------------------------------------

set_attrs([], Doc) -> Doc;
set_attrs([{Name, Value}|T], Doc) ->
    set_attrs(T, set_attr(Name, Value, Doc)).

%%---------------------------------------------------------------------------
%% @doc Replaces all of the document's attributes.
%%---------------------------------------------------------------------------

replace_attrs(Attrs, Doc) ->
    Doc#doc{body=wrap_couch_obj(Attrs)}.

%%---------------------------------------------------------------------------
%% @doc Returns a single attribute value or undefined if the attribute doesn't
%% exist.
%% ---------------------------------------------------------------------------

get_attr(Name, #doc{body={Attrs}}) ->
    strip_couch_obj(proplists:get_value(Name, Attrs)).

%%---------------------------------------------------------------------------
%% @doc Returns a single attribute value or Default of the attribute doesn't
%% exist.
%% ---------------------------------------------------------------------------

get_attr(Name, #doc{body={Attrs}}, Default) ->
    case proplists:get_value(Name, Attrs, '_undefined') of
        '_undefined' -> Default;
        Val -> strip_couch_obj(Val)
    end.

%%---------------------------------------------------------------------------
%% @doc Deletes a single document attribute.
%%---------------------------------------------------------------------------

del_attr(Name, #doc{body={Attrs}}=Doc) ->
    Doc#doc{body={proplists:delete(Name, Attrs)}}.

%%---------------------------------------------------------------------------
%% @doc Returns a boolean indicating whether the document is deleted.
%% ---------------------------------------------------------------------------

is_deleted(#doc{deleted=D}) -> D.

%% ---------------------------------------------------------------------------
%% @doc Removes the tuple wrapper from CouchDB objects. E.g. {[{foo, bar}]}
%% becomes [{foo, bar}].
%% ---------------------------------------------------------------------------

strip_couch_obj({Obj}) ->
    strip_couch_obj(Obj);
strip_couch_obj({N, V}) -> 
    {strip_couch_obj(N), strip_couch_obj(V)};
strip_couch_obj([H|T]) when not is_integer(H) -> 
    [strip_couch_obj(H)|strip_couch_obj(T)];
strip_couch_obj(Other) -> Other.

%% ---------------------------------------------------------------------------
%% @doc Adds wrapping tuple to "objects" - i.e. property lists. E.g. [{foo,
%% bar}] becomes {[{foo, bar}]}.
%% ---------------------------------------------------------------------------

wrap_couch_obj([]) -> {[]};
wrap_couch_obj([{_, _}|_]=Obj) ->
    {[wrap_couch_obj(P) || P <- Obj]};
wrap_couch_obj({N, V}) ->
    {wrap_couch_obj(N), wrap_couch_obj(V)};
wrap_couch_obj(Obj) -> Obj.

%% ===========================================================================
%% Private functions
%% ===========================================================================

%% ---------------------------------------------------------------------------
%% @doc Converts lists to bin and passes bin through.
%% ---------------------------------------------------------------------------

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L).

-ifdef(TEST).
-include("couchdoc_tests.hrl").
-endif.
