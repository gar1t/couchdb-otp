-module(couchdb_tests).

-include_lib("eunit/include/eunit.hrl").

-export([test/0]).

-import(proplists, [get_value/2]).

test() ->
    Tests = [fun basic_db_test/0,
             fun basic_doc_test/0,
             fun batch_insert_test/0],
             %fun basic_view_test/0],
    eunit:test({setup, fun setup/0, Tests}).

setup() ->
    couchdb:start().

basic_db_test() ->

    % Create a new db with a unique name.
    Name = random_dbname(),
    {ok, Db} = couchdb:open(Name),

    % Use info/1 to get info about the db.
    Info = couchdb:info(Db),
    ?assertEqual(Name, get_value(db_name, Info)),

    % We can close the db using close/1 (TODO - what does this do?).
    ?assertEqual(ok, couchdb:close(Db)),

    % Delete using the db name.
    couchdb:delete_db(Name).

basic_doc_test() ->
    
    DbName = random_dbname(),
    {ok, Db} = couchdb:open(DbName),

    % Docs are created using couchdoc:new.
    Doc = couchdoc:new(),

    % This is an empty doc, but it's been assigned an ID.
    Id = couchdoc:get_id(Doc),
    ?assert(is_binary(Id)),
    ?assert(size(Id) > 0),

    % It has no revisions.
    ?assertEqual(undefined, couchdoc:get_rev(Doc)),

    % Store the document using put/2. We get back a revised document.
    {ok, DocR} = couchdb:put(Db, Doc),
    ?assert(couchdoc:get_rev(DocR) =/= undefined),

    % The document current has no attributes.
    ?assertEqual([], couchdoc:get_attr_names(DocR)),

    % We can modify the revised document.
    Doc2 = couchdoc:set_attr(tags, [red, green, blue], DocR),
    {ok, Doc2R} = couchdb:put(Db, Doc2),
    ?assertEqual([tags], couchdoc:get_attr_names(Doc2R)),
    ?assertEqual([red, green, blue], couchdoc:get_attr(tags, Doc2R)),

    % TODO: finish basic doc ops: get_attrs, del_attr, delete.
    
    couchdb:delete_db(DbName).

batch_insert_test() ->
    
    DbName = random_dbname(),
    {ok, Db} = couchdb:open(DbName),

    % Use put_many to insert a batch of documents. This can have a 10x increase
    % in throughput at the expense of durability.

    Docs = [couchdoc:new() || _ <- lists:seq(1, 10)],
    DocsR = couchdb:put_many(Db, Docs),
    ?assertEqual(10, length(DocsR)),
    ?assertMatch([{ok, _}|_], DocsR),

    couchdb:delete_db(DbName).

basic_view_test() ->

    DbName = random_dbname(),
    {ok, Db} = couchdb:open(DbName),

    Map = "function(doc) { emit(doc._id, 1) }",
    DDoc = couchdoc:new("_design/test", [{views, [{basic, [{map, Map}]}]}]),
    couchdb:put(Db, DDoc).

random_dbname() ->
    random:seed(erlang:now()),
    lists:concat(["testdb_", random:uniform(1000000)]).
