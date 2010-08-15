-module(couchdb_tests).

-include_lib("eunit/include/eunit.hrl").

-export([test/0,
         map_date_title/2,
         map_date_title/3]).

-import(proplists, [get_value/2]).

test() ->
    Tests = [fun basic_db_test/0,
             fun basic_doc_test/0,
             fun batch_insert_test/0,
             fun select_docs_test/0,
             fun basic_view_test/0,
             fun view_support_test/0,
             fun term_store_test/0,
             fun composite_id_pattern_test/0,
             fun composite_key_test/0],
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

select_docs_test() ->

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

    % TODO - test stale

    couchdb:delete_db(DbName).

basic_view_test() ->

    % These tests require view support.
    couchdb_optional:start(view_support),
    
    DbName = random_dbname(),
    {ok, Db} = couchdb:open(DbName),

    % Views are lazily created indexes on databases. Let's create some
    % documents to index.

    D1 = couchdoc:new(
           "biking", 
           [{"_rev", "AE19EBC7654"},
            {"title", "Biking"},
            {"body", "My biggest hobby is mountainbiking. The other day..."},
            {"date", "2009/01/30 18:04:11"}]),
    D2 = couchdoc:new(
           "bought-a-cat",
           [{"_rev", "4A3BBEE711"},
            {"title", "Bought a Cat"},
            {"body", "I went to the the pet store earlier and brought home "
             "a little kitty..."},
            {"date", "2009/02/17 21:13:39"}]),
    D3 = couchdoc:new(
           "hello-world",
           [{"_rev", "43FBA4E7AB"},
            {"title", "Hello World"},
            {"body", "Well hello and welcome to my new blog..."},
            {"date", "2009/01/15 15:52:20"}]),

    couchdb:put_many(Db, [D1, D2, D3]),

    % At a minimum, a view requires a map function that maps documents to zero
    % or more key value pairs. The key/value pairs are indexed by key and can
    % be used to retrieve and sort values quickly.
    %
    % Map functions use "folding" to append key/value pairs to an accumulator.
    %
    % Here's a map function that provides date/title pairs, which will create a
    % view that let's use select and sort documents by date.
    %
    % couchdb_nqs supports map functions in the following forms:
    %
    % - function source code (same as default CouchDB definition)
    % - Binary encoded Erlang fun of arity 2
    % - string {M, F} of a 2-arity function (Doc, Acc)
    % - string {M, F, A} of a 3-arity function (Doc, Args, Acc)
    %
    % Let's start with a function defined as a string. This follows the pattern
    % used by CouchDB in which map functions are stored as JavaScript.

    MapStr = 
        <<"fun(Doc, Acc) -> "
          "  [{proplists:get_value(\"date\", Doc),"
          "    proplists:get_value(\"title\", Doc)}|Acc]"
          "end.">>,

    % We add a view as a property of a special "design" document. CouchDB uses
    % the "_design/" ID prefix to designate a document as one of these special
    % documents.
    %
    % In addition to the views, the design document must specify a language. In
    % this case, we use "otp", which must be registered to use couchdb_nqs
    % (native query server).
    %
    % To be used as views, the binary strings below are required (as opposed to
    % lists).

    DDoc1 = couchdoc:new("_design/otp_str",
                        [{<<"language">>, <<"otp">>},
                         {<<"views">>, [{<<"by_date">>,
                                         [{<<"map">>, MapStr}]}]}]),
    couchdb:put(Db, DDoc1),

    % We can now select using this view.

    R1 = couchdb:select({Db, "otp_str", "by_date"}, []),

    % Here's what we expect from this map:

    Expected = {3,0, [[{id, <<"hello-world">>},
                       {key, "2009/01/15 15:52:20"},
                       {value, "Hello World"}],
                      [{id, <<"biking">>},
                       {key, "2009/01/30 18:04:11"},
                       {value, "Biking"}],
                      [{id, <<"bought-a-cat">>},
                       {key, "2009/02/17 21:13:39"},
                       {value, "Bought a Cat"}]]},
    ?assertEqual(Expected, R1),

    % Note that the document IDs are returned as binary strings even though
    % they were originally inserts as lists. This is how CouchDB stores IDs.
    %
    % In the result, we see that items are returned with the document ID, and
    % the key and value provided by the map function.
    %
    % The rows are also ordered by their key, which is a date in this case.
    %
    % Let's using the same function, but encoded as an Erlang term.

    MapFun = fun(Doc, Acc) ->
                     [{get_value("date", Doc), get_value("title", Doc)}|Acc]
             end,
    MapFunBin = term_to_binary(MapFun),
    DDoc2 = couchdoc:new("_design/otp_fun",
                        [{<<"language">>, <<"otp">>},
                         {<<"views">>, [{<<"by_date">>,
                                         [{<<"map">>, MapFunBin}]}]}]),
    couchdb:put(Db, DDoc2),

    % Note that the fun uses the imported function get_value/2 from the
    % proplists module. One of the benefits of using an encoded fun is that it
    % provides a persistent closure that is used for indexing.

    % Let's use the view.
    
    R2 = couchdb:select({Db, "otp_fun", "by_date"}, []),
    ?assertEqual(Expected, R2),

    % Let's now provide a two-tuple of module and function for our map. Note
    % that the term must be terminated with a period.
    
    MF = <<"{couchdb_tests, map_date_title}.">>,
    DDoc3 = couchdoc:new("_design/otp_mf",
                        [{<<"language">>, <<"otp">>},
                         {<<"views">>, [{<<"by_date">>,
                                         [{<<"map">>, MF}]}]}]),
    couchdb:put(Db, DDoc3),

    % Our exported function map_date_title/2 provides the same mapping as the
    % previous two functions.

    R3 = couchdb:select({Db, "otp_mf", "by_date"}, []),
    ?assertEqual(Expected, R3),

    % Let's round out the examples with the fourth variant: a {M, F, A} tuple
    % representation where A is a list of arguments that are appended to the
    % [Doc, Acc] list when calling the function.

    MFA = <<"{couchdb_tests, map_date_title, [myarg]}.">>,
    DDoc4 = couchdoc:new("_design/otp_mfa",
                        [{<<"language">>, <<"otp">>},
                         {<<"views">>, [{<<"by_date">>,
                                         [{<<"map">>, MFA}]}]}]),
    couchdb:put(Db, DDoc4),

    % In this view, we'll be using map_data_title/3 below.

    R4 = couchdb:select({Db, "otp_mfa", "by_date"}, []),
    ?assertEqual(Expected, R4),

    couchdb:delete_db(DbName).

view_support_test() ->

    couchdb_optional:start(view_support),    
    DbName = random_dbname(),
    {ok, Db} = couchdb:open(DbName),

    % While you can create views as fields in design documents, it is easier to
    % use couchdb's view support.
    %
    % E.g. you can define the Map function from basic_view_test/0 (see above)
    % this way.
    Map = fun(Doc, Acc) ->
                  [{get_value("date", Doc), get_value("title", Doc)}|Acc]
          end,
    DDoc1 = couchdesign:new("otp_fun", [{view, {"by_date", Map}}]),
    couchdb:put(Db, DDoc1),

    % This is syntactic sugar for the manual process of creatign a design
    % document.
    %
    % Let's add a couple documents to map.

    couchdb:put(Db, couchdoc:new("biking", [{"title", "Biking"},
                                            {"date", "2009/01/30"}])),
    couchdb:put(Db, couchdoc:new("hello", [{"title", "Hello World"},
                                           {"date", "2009/01/15"}])),

    % These are simplified versions of the docs used in basic_view_test. We
    % should get this for our view results.

    Expected = {2, 0, [[{id, <<"hello">>},
                        {key, "2009/01/15"},
                        {value, "Hello World"}],
                       [{id, <<"biking">>},
                        {key, "2009/01/30"},
                        {value, "Biking"}]]},

    R1 = couchdb:select({Db, "otp_fun", "by_date"}, []),
    ?assertEqual(Expected, R1),

    % Using functions directly in a view works well for embedded use (in
    % particular, it provides a persisted closure), but it has a major
    % drawback: it's not previewable in Futon.
    %
    % We can alternatively specify a {M, F} tuple.

    DDoc2 = couchdesign:new("otp_mf", [{view, {"by_date", {couchdb_tests, 
                                                           map_date_title}}}]),
    couchdb:put(Db, DDoc2),
    R2 = couchdb:select({Db, "otp_mf", "by_date"}, []),
    ?assertEqual(Expected, R2),

    % TODO - multiple views, API for adding, removing, and replacing views

    couchdb:delete_db(DbName).

term_store_test() ->
    couchdb_optional:start(view_support),    
    DbName = random_dbname(),
    {ok, Db} = couchdb:open(DbName),

    % One of the advantages of using CouchDB as an emebedded Erlang database is
    % that we can store Erlang terms directly without converting back and forth
    % from JavaScript.
    %
    % In this test, we'll illustrate storing terms and using them in views.
    %
    % Let's create a couple documents that have non-standard document bodies
    % (i.e. are not strictly name/value fields).

    couchdb:put(Db, couchterm:new("jane", {user, "jane", admin})),
    couchdb:put(Db, couchterm:new("joe", {user, "joe", user})),

    % While these documents can't be viewed in Futon, we can read them.

    {ok, Jane} = couchdb:get(Db, "jane"),
    ?assertEqual({user, "jane", admin}, couchterm:term(Jane)),

    {ok, Joe} = couchdb:get(Db, "joe"),
    ?assertEqual({user, "joe", user}, couchterm:term(Joe)),

    % Let's create a couple views for these documents: one that indexes by name
    % and another by role.
    
    ByName = fun([_, _, {user, Name, _}], Acc) -> [Name|Acc] end,
    ByRole = fun([_, _, {user, _, Role}], Acc) -> [atom_to_list(Role)|Acc] end,

    DDoc = couchdesign:new("funs", [{view, {"by_name", ByName}},
                                    {view, {"by_role", ByRole}}]),
    couchdb:put(Db, DDoc),

    % We can use these to lookup a sorted list of users by either name or role.

    ByNameR = couchdb:select({Db, "funs", "by_name"}, []),
    ?assertEqual({2, 0, [[{id, <<"jane">>}, {key, "jane"}, {value, null}],
                         [{id, <<"joe">>}, {key, "joe"}, {value, null}]]},
                 ByNameR),
    
    ByRoleR = couchdb:select({Db, "funs", "by_role"}, []),
    ?assertEqual({2, 0, [[{id, <<"jane">>}, {key, "admin"}, {value, null}],
                         [{id, <<"joe">>}, {key, "user"}, {value, null}]]},
                 ByRoleR),

    couchdb:delete_db(DbName).

composite_id_pattern_test() ->
    DbName = random_dbname(),
    {ok, Db} = couchdb:open(DbName),

    % CouchDB indexes database docs using a b+tree index on the doc IDs. To
    % group related data using IDs, we can use a "composite ID" pattern.
    
    % Here's a simple blog post.

    P = couchdoc:new("blog-1/post-1", 
                     [{time, "12:34"},
                      {by, "Frank"},
                      {text, "CouchDB is fun!"}]),
    couchdb:put(Db, P),

    % Here are some comments.

    C1 = couchdoc:new("blog-1/post-1/comment-1",
                      [{time, "12:35"},
                       {by, "Mary"},
                       {text, "And bouncy!"}]),
    couchdb:put(Db, C1),

    C2 = couchdoc:new("blog-1/post-1/comment-2",
                      [{time, "12:45"},
                       {by, "Bob"},
                       {text, "Bouncy?"}]),
    couchdb:put(Db, C2),

    % We can retrieve all of the documents associated with a blog post using
    % the "blog-1/" prefix.

    {_, _, Rows} = couchdb:select(Db, "blog-1/", []),
    ?assertEqual([<<"blog-1/post-1">>,
                  <<"blog-1/post-1/comment-1">>,
                  <<"blog-1/post-1/comment-2">>],
                 [get_value(id, Row) || Row <- Rows]),

    % Document IDs are limited to using list/binary keys. For more flexibility
    % in defining keys from document attributes, use a view (see
    % composite_key_test below).

    couchdb:delete_db(DbName).

composite_key_test() ->
    couchdb_optional:start(view_support),    
    DbName = random_dbname(),
    {ok, Db} = couchdb:open(DbName),

    % Maps can use any Erlang term to index document info. We can use this to
    % create composite keys that group related data together. This is similar
    % to the "composite id pattern" illustrated above but is more flexible.

    % Let's create some blog documents that use randomly assigned IDs and
    % attribute references to related documents. We'll also use a 'type'
    % attribute to distinguish one document type from another.

    B = couchdoc:new("899",
                     [{type, blog},
                      {name, "My Blog"},
                      {by, "Frank"}]),
    P = couchdoc:new("341",
                     [{type, post},
                      {blog, "899"},
                      {time, "12:34"},
                      {by, "Frank"},
                      {text, "CouchDB is fun!"}]),
    C1 = couchdoc:new("154",
                      [{type, comment},
                       {blog, "899"},
                       {post, "341"},
                       {time, "12:35"},
                       {by, "Mary"},
                       {text, "And bouncy!"}]),
    C2 = couchdoc:new("022",
                      [{type, comment},
                       {blog, "899"},
                       {post, "341"},
                       {time, "12:45"},
                       {by, "Bob"},
                       {text, "Bouncy?"}]),

    couchdb:put_many(Db, [B, P, C1, C2]),

    % Let's also add some other blogs.

    couchdb:put(Db, couchdoc:new("102", [{type, blog}])),
    couchdb:put(Db, couchdoc:new("898", [{type, blog}])),
    couchdb:put(Db, couchdoc:new("900", [{type, blog}])),

    % Getting all of the documents...

    {7, 0, All} = couchdb:select(Db, []),
    ?assertEqual(7, length(All)),

    % Let's create a view that will let us access the hierarchy of blogs,
    % posts, and comments.

    % TODO - Accessing doc IDs using binaries here is fuggly. Would it make
    % more sense to provide them as opaque #doc records and let users access
    % the data using couchdoc/couchterm (which could be imported for
    % compactness of code).
    
    % TODO - Not preserving key types (string vs binary) is annoying. Do we
    % have to convert to binary when storing?

    Map = fun(Doc, Acc) ->
                  case get_value(type, Doc) of
                      blog ->
                          [[binary_to_list(get_value(<<"_id">>, Doc))]
                           |Acc];
                      post ->
                          [[get_value(blog, Doc), 
                            binary_to_list(get_value(<<"_id">>, Doc))]
                           |Acc];
                      comment ->
                          [[get_value(blog, Doc),
                            get_value(post, Doc),
                            binary_to_list(get_value(<<"_id">>, Doc))]
                           |Acc];
                      _ -> Acc
                  end
          end,
    couchdb:put(Db, couchdesign:new("blog", [{view, {"list", Map}}])),

    % We can use to select documents at different levels. To get all of the
    % documents associated with blog ID "899", including posts and comments,
    % we'd use a select like this.

    % TODO - We should be able to use a single key and the "starts with"
    % behavior should just work. Alas, not so.

    {_, _, [R1]} = couchdb:select({Db, "blog", "list"}, ["899"], []),
    ["899"] = get_value(key, R1),

    % TODO - It looks like there's a bug here. The result includes {key,
    % ["900"]}, which is an exact match of the end key, despite the exclude_end
    % option.

    {_, _, R2} = couchdb:select({Db, "blog", "list"}, ["899"], ["900"],
                                [exclude_end]),
    ["900"] = get_value(key, lists:last(R2)),
    
    % TOTO (cont) - To work around this, we need to tack on a 255 char to the
    % end of the blog ID. We can drop the exclude_end.

    {_, _, BlogDocs} = couchdb:select({Db, "blog", "list"},
                                      ["899"], ["899"++[255]], []),

    % TODO - This is a glaring and annoying inconsistency with the Doc
    % structure being passed into the map funs. Here we're accessing by the
    % atom id - with the map fun, it's <<"_id">>. Granted, they are inherently
    % different structures, but it's still annoying.

    ?assertEqual([<<"899">>, <<"341">>, <<"022">>, <<"154">>],
                 [get_value(id, R) || R <- BlogDocs]),

    % Let's now access the documents associated with a post.
    
    % TODO - Again, same workaround for range matching described above.

    {_, _, PostDocs} = couchdb:select({Db, "blog", "list"},
                                      ["899", "341"], 
                                      ["899", "341"++[255]], []),

    ?assertEqual([<<"341">>, <<"022">>, <<"154">>],
                 [get_value(id, R) || R <- PostDocs]),

    couchdb:delete_db(DbName).

map_date_title(Doc, Acc) ->
    [{get_value("date", Doc), get_value("title", Doc)}|Acc].

map_date_title(Doc, Acc, myarg) ->
    map_date_title(Doc, Acc).

random_dbname() ->
    random:seed(erlang:now()),
    lists:concat(["testdb_", random:uniform(1000000)]).
