-module(couchdb_tests).

-include_lib("eunit/include/eunit.hrl").

-export([test/0]).

-import(proplists, [get_value/2]).

test() ->
    Tests = [%fun basic_db_test/0,
             %fun basic_doc_test/0,
             %fun batch_insert_test/0],
             fun select_all_test/0],
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

select_all_test() ->

    DbName = random_dbname(),
    {ok, Db} = couchdb:open(DbName),

    % To select from all docs using ID ranges, use couchdb:select/4. Let's add
    % some docs to select.

    D1 = couchdoc:new("1", [{name, "doc-1"}]),
    D2 = couchdoc:new("2", [{name, "doc-2"}]),
    D3 = couchdoc:new("3", [{name, "doc-3"}]),
    D4 = couchdoc:new("4", [{name, "doc-4"}]),
    couchdb:put_many(Db, [D1, D2, D3, D4]),

    % We can select every doc in the database ordered by ID as follows:

    {TotalRowCount, Offset, Rows1} = couchdb:select(Db, []),
    ?assertEqual(TotalRowCount, 4),
    ?assertEqual(0, Offset),

    % Rows is a proplist containing id, key, value and optionally doc
    % properties. Let's view the ordered ids for the result.

    ?assertEqual([<<"1">>, <<"2">>, <<"3">>, <<"4">>], 
                 [get_value(id, Row) || Row <- Rows1]),

    % Note that the IDs, which were specified as strings, are now
    % binaries. CouchDB stores keys strictly as binaries - the string IDs were
    % implicitly converted when the documents were added.

    % In the case when we haven't specified a view (i.e. we're selecting
    % documents from the database directly), the keys are the document ids.

    ?assertEqual([<<"1">>, <<"2">>, <<"3">>, <<"4">>], 
                 [get_value(key, Row) || Row <- Rows1]),

    % Result values are the document's revisions.
    
    ?assertMatch([[{rev, _}], [{rev, _}], [{rev, _}], [{rev, _}]],
                 [get_value(value, Row) || Row <- Rows1]),

    % By default, the documents themselves aren't retured in the results.

    ?assertEqual([undefined, undefined, undefined, undefined],
                 [get_value(doc, Row) || Row <- Rows1]),

    % We can include the documents by specifying the include_docs property.

    {4, 0, Rows2} = couchdb:select(Db, [include_docs]),
    ?assertEqual(["doc-1", "doc-2", "doc-3", "doc-4"],
                 [get_value(name, get_value(doc, Row)) || Row <- Rows2]),

    % To reverse the result (i.e. sort by ID descending), use the reverse
    % property.

    {4, 0, Rows3} = couchdb:select(Db, [reverse]),
    ?assertEqual([<<"4">>, <<"3">>, <<"2">>, <<"1">>],
                 [get_value(id, Row) || Row <- Rows3]),

    % The result can be limited using limit.

    {4, 0, Rows4} = couchdb:select(Db, [{limit, 2}]),
    ?assertEqual([<<"1">>, <<"2">>], [get_value(id, Row) || Row <- Rows4]),

    % Docs can be skipped from the start of the match using skip. The offset in
    % the document list will reflect the skipped docs.

    {4, 2, Rows5} = couchdb:select(Db, [{skip, 2}]),
    ?assertEqual([<<"3">>, <<"4">>], [get_value(id, Row) || Row <- Rows5]),

    % Document can be selected by range using a start and end ID. Either value
    % may be undefined, indicating that the range is unbounded for that
    % side. Let's first find documents that include "2" and "3".

    {4, 1, Rows6} = couchdb:select(Db, "2", "3", []),
    ?assertEqual([<<"2">>, <<"3">>], [get_value(id, Row) || Row <- Rows6]),

    % Note again that strings are implicitly converted to binaries. We could
    % have used binaries as the start and end IDs as well.

    {4, 1, Rows6} = couchdb:select(Db, <<"2">>, <<"3">>, []),
    ?assertEqual([<<"2">>, <<"3">>], [get_value(id, Row) || Row <- Rows6]),

    % By default, ID selection is inclusive of the end ID. We can exlucde the
    % end using exclude_end.

    {4, 1, Rows7} = couchdb:select(Db, "2", "3", [exclude_end]),
    ?assertEqual([<<"2">>], [get_value(id, Row) || Row <- Rows7]),

    % Here are some unbounded selects.

    {4, 2, Rows8} = couchdb:select(Db, "3", undefined, []),
    ?assertEqual([<<"3">>, <<"4">>], [get_value(id, Row) || Row <- Rows8]),

    {4, 0, Rows9} = couchdb:select(Db, undefined, "2", []),
    ?assertEqual([<<"1">>, <<"2">>], [get_value(id, Row) || Row <- Rows9]),

    % Take care when specifying reverse - the start and end IDs must be
    % reversed as well.

    {4, 2, Rows10} = couchdb:select(Db, "2", "1", [reverse]),
    ?assertEqual([<<"2">>, <<"1">>], [get_value(id, Row) || Row <- Rows10]),

    {4, 2, Rows11} = couchdb:select(Db, "2", undefined, [reverse]),
    ?assertEqual([<<"2">>, <<"1">>], [get_value(id, Row) || Row <- Rows11]),

    couchdb:delete_db(DbName).

random_dbname() ->
    random:seed(erlang:now()),
    lists:concat(["testdb_", random:uniform(1000000)]).
