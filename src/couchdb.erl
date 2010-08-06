-module(couchdb).

-include("couch_db.hrl").

-export([start/0]).

-export([new/1,
         new/2,
         open/1,
         open/2,
         close/1,
         delete/1,
         delete/2,
         db_info/1,
         all_dbs/0,
         put/2,
         put/3,
         put_many/2,
         get/2,
         get/3,
         remove/2]).

%%---------------------------------------------------------------------------
%% @doc Convenience function to start couchdb app and dependent applications.
%%
%% In production deployments, use Erlang releases, which are better suited at
%% handling startup dependencies.
%%---------------------------------------------------------------------------
start() ->
    application:start(sasl),
    application:start(crypto),
    application:start(couchdb).

%%---------------------------------------------------------------------------
%% @doc Creates a new database.
%%
%% On success, returns {ok, Db} otherwise returns an applicable error.
%%
%% TODO: document options
%%---------------------------------------------------------------------------

new(Name) ->
    new(Name, []).

new(Name, Options) ->
    couch_server:create(to_bin(Name), Options).

%%---------------------------------------------------------------------------
%% @doc Open a database.
%%
%% On success, returns {ok, Db} otherwise returns an applicable error.
%%
%% TODO: document options
%%---------------------------------------------------------------------------

open(Name) ->
    open(Name, []).

open(Name, Options) ->
    couch_db:open(to_bin(Name), Options).

%%---------------------------------------------------------------------------
%% @doc Closes a database.
%%---------------------------------------------------------------------------

close(Db) ->
    couch_db:close(Db).

%%---------------------------------------------------------------------------
%% @doc Deletes a database.
%%
%% Returns ok if deleted successfully otherwise returns an applicable error.
%%
%% TODO: document options
%%
%% TODO: This collides a bit with 'remove' and should probably be 'delete_db'
%% or something more obvious (esp given its severity), though _db breaks
%% symmetry with the rest of the API :(
%% ---------------------------------------------------------------------------

delete(Name) ->
    delete(Name, []).

delete(Name, Options) ->
    couch_server:delete(to_bin(Name), Options).

%%---------------------------------------------------------------------------
%% @doc Returns info about the specified database.
%%
%% Error if Db is invalid.
%%---------------------------------------------------------------------------

db_info(Db) ->
    {ok, Info} = couch_db:get_db_info(Db),
    [db_info_item(I) || I <- Info].

db_info_item({db_name, B}) ->
    {db_name, binary_to_list(B)};
db_info_item({instance_start_time, B}) -> 
    {instance_start_time, list_to_integer(binary_to_list(B))};
db_info_item({_, _}=I) -> I.

%%---------------------------------------------------------------------------
%% @doc Returns a list of all db names.
%%---------------------------------------------------------------------------

all_dbs() ->
    {ok, Dbs} = couch_server:all_databases(),
    lists:map(fun binary_to_list/1, Dbs).

%%---------------------------------------------------------------------------
%% @doc Stores a document in a db.
%%
%% Use couchdoc to create a document.
%%
%% Returns ???
%%
%% TODO: lots more here
%% TODO: missing UpdateType arg variant -- how used?
%%---------------------------------------------------------------------------

put(Db, Doc) ->
    put(Db, Doc, []).

put(Db, Doc, Options) ->
    case couch_db:update_doc(Db, Doc, Options) of
        {ok, {Start, RevId}} -> 
            {ok, Doc#doc{revs={Start, [RevId]}}};
        Err -> Err
    end.

%%---------------------------------------------------------------------------
%% @doc Stores documents in bulk.
%%---------------------------------------------------------------------------

put_many(Db, Docs) ->
    {ok, Result} = couch_db:update_docs(Db, Docs),
    lists:zipwith(fun({ok, {Start, RevId}}, Doc) -> 
                          {ok, Doc#doc{revs={Start, [RevId]}}};
                     (Err, Doc) ->
                          {Err, Doc}
                  end, Result, Docs).

%%---------------------------------------------------------------------------
%% @doc Retrieves a document.
%%
%% TODO: more here
%%
%% TODO: I'm not clear on how to properly handle this. Hovercraft requires the
%% db *name* here (hovercraft:open_doc/3) and not the db ref (I'm not a fan of
%% that API here because it breaks symmetry with the other functions). It opens
%% a new db, which can be used to read any documents that have been added since
%% the db was opened. I'm basically hacking this to re-open the db so it can
%% see new items. Is there a better/cleaner way to do this?
%% ---------------------------------------------------------------------------

get(Db, Id) ->
    get(Db, Id, []).

get(#db{name=DbName}, Id, Options) ->
    {ok, Db} = couch_db:open(DbName, []),
    case couch_db:open_doc(Db, to_bin(Id), Options) of
        {ok, Doc} -> {ok, Doc};
        Err -> Err
    end.

%%---------------------------------------------------------------------------
%% @doc Removes Doc from Db.
%%---------------------------------------------------------------------------

remove(Db, #doc{}=Doc) ->
    % NOTE: Not using couch_db:delete_doc/3 - bug as of r980985.
    {ok, [Result]} = couch_db:update_docs(Db, [Doc#doc{deleted=true}], []),
    Result;
remove(Db, Id) ->
    case get(Db, Id) of
        {ok, Doc} -> remove(Db, Doc);
        Other -> Other
    end.

%%---------------------------------------------------------------------------
%% Private functions
%%---------------------------------------------------------------------------

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L).
