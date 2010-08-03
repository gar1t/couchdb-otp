-module(couchdb_tests).

-include_lib("eunit/include/eunit.hrl").

-export([test/0]).

-import(proplists, [get_value/2]).

test() ->
    Tests = [%fun basic_db_test/0,
             fun basic_doc_test/0],
    eunit:test({setup, fun setup/0, Tests}).

setup() ->
    couchdb:start().

basic_db_test() ->

    % Create a new db with a unique name.
    Name = random_dbname(),
    {ok, Db} = couchdb:new(Name),

    % We can't open a db that doesn't already exist.
    ?assertEqual({not_found, no_db_file}, couchdb:open(random_dbname())),

    % Use db_info/1 to get info about the db.
    Info = couchdb:db_info(Db),
    ?assertEqual(Name, get_value(db_name, Info)),

    % We can't create a db that exists.
    ?assertEqual(file_exists, couchdb:new(Name)),

    % We can close the db using close/1 (TODO - what does this do?).
    ?assertEqual(ok, couchdb:close(Db)),

    % Delete using the db name.
    ?assertEqual(ok, couchdb:delete(Name)).

basic_doc_test() ->
    
    % We'll create a new database for our doc tests.
    DbName = random_dbname(),
    {ok, Db} = couchdb:new(DbName),

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

    % TODO: finish basic doc ops.
    
    % Delete the db.
    ok = couchdb:delete(DbName).

random_dbname() ->
    random:seed(erlang:now()),
    lists:concat(["testdb_", random:uniform(1000000)]).