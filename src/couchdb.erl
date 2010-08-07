-module(couchdb).

-include("couch_db.hrl").

-export([start/0]).

-export([open/1,
         open/2,
         close/1,
         delete_db/1,
         delete_db/2,
         info/1,
         all_dbs/0,
         put/2,
         put/3,
         put_many/2,
         get/2,
         get/3,
         delete/2,
         select/2,
         select/4]).

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
%% @doc Open a database. If the database doesn't exist, it's created.
%%
%% On success, returns {ok, Db} otherwise returns an applicable error.
%%
%% TODO: document options
%% TODO: confirm that options make sense for create as well as open, else
%%       we need two functions
%%---------------------------------------------------------------------------

open(Name) ->
    open(Name, []).

open(Name, Options) ->
    case couch_db:open(to_bin(Name), Options) of
        {not_found,no_db_file} ->
            couch_server:create(to_bin(Name), Options);
        Other -> Other
    end.

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
%% ---------------------------------------------------------------------------

delete_db(Name) ->
    delete_db(Name, []).

delete_db(Name, Options) ->
    couch_server:delete(to_bin(Name), Options).

%%---------------------------------------------------------------------------
%% @doc Returns info about the specified database.
%%
%% Error if Db is invalid.
%%---------------------------------------------------------------------------

info(Db) ->
    {ok, Info} = couch_db:get_db_info(Db),
    [info_item(I) || I <- Info].

info_item({db_name, B}) ->
    {db_name, binary_to_list(B)};
info_item({instance_start_time, B}) -> 
    {instance_start_time, list_to_integer(binary_to_list(B))};
info_item({_, _}=I) -> I.

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

delete(Db, #doc{}=Doc) ->
    % NOTE: Not using couch_db:delete_doc/3 - bug as of r980985.
    {ok, [Result]} = couch_db:update_docs(Db, [Doc#doc{deleted=true}], []),
    Result;
delete(Db, Id) ->
    case get(Db, Id) of
        {ok, Doc} -> delete(Db, Doc);
        Other -> Other
    end.

%%---------------------------------------------------------------------------
%% @doc Selects from all documents.
%%
%%  Options    = options()
%%  options()  = [option()]
%%  option()   = {limit, int()}
%%             | {skip, int()}
%%             | reverse
%%             | include_docs
%%---------------------------------------------------------------------------

select(Db, Options) ->
    select(Db, undefined, undefined, Options).

%%---------------------------------------------------------------------------
%% @doc Selects a range of documents using start and end keys.
%%
%%  StartId    = binary() | string() | undefined
%%  EndId      = binary() | string() | undefined
%%  Options    = options()
%%  options()  = [option()]
%%  option()   = {limit, int()}
%%             | {skip, int()}
%%             | reverse
%%             | exclude_end
%%             | include_docs
%%
%% TODO: stale not supported (easy to add) - what else can we pass in?
%%---------------------------------------------------------------------------

select(#db{name=DbName}, StartId0, EndId0, Options) ->
    {ok, Db} = couch_db:open(DbName, []),
    Dir = case proplists:get_bool(reverse, Options) of
              true -> rev;
              false -> fwd
          end,
    StartId = select_start_id(StartId0, Dir),
    EndId = select_end_id(EndId0, Dir),
    Limit = proplists:get_value(limit, Options, 10000000000),
    Skip = proplists:get_value(skip, Options, 0),
    InclEnd = not proplists:get_bool(exclude_end, Options),
    InclDocs = proplists:get_bool(include_docs, Options),
    Args = #view_query_args{start_docid=StartId,
                            end_docid=EndId,
                            limit=Limit,
                            skip=Skip,
                            direction=Dir,
                            inclusive_end=InclEnd,
                            include_docs=InclDocs},
    {ok, Info} = couch_db:get_db_info(Db),
    DocCount = couch_util:get_value(doc_count, Info),
    FoldFun = view_fold_fun(Db, Args, couch_db:get_update_seq(Db), DocCount),
    InAcc = {Limit, Skip, undefined, []},
    EnumOptions = [{start_key, StartId},
                   {dir, Dir}, 
                   {case InclEnd of 
                        true -> end_key; 
                        false -> end_key_gt
                    end, EndId}],
    {ok, _, {_, _, _, Result}} = 
        couch_btree:fold(Db#db.fulldocinfo_by_id_btree,
                         enum_db_acc_fun(FoldFun), InAcc, EnumOptions),
    case Result of
        {Offset, Rows} -> {DocCount, Offset, lists:reverse(Rows)};
        [] -> {DocCount, DocCount, []}
    end.

select_start_id(undefined, fwd) -> <<"">>;
select_start_id(undefined, rev) -> <<255>>;
select_start_id(Id, _) -> to_bin(Id).

select_end_id(undefined, fwd) -> <<255>>;
select_end_id(undefined, rev) -> <<"">>;
select_end_id(Id, _) -> to_bin(Id).    

view_fold_fun(Db, QueryArgs, UpdateSeq, DocCount) ->
    Helpers = #view_fold_helper_funs{
      reduce_count=fun couch_db:enum_docs_reduce_to_count/1,
      start_response=fun view_fold_start_response/6,
      send_row=fun view_fold_send_row/5},
    couch_httpd_view:make_view_fold_fun(
      nil, QueryArgs, <<"">>, Db, UpdateSeq, DocCount, Helpers).

enum_db_acc_fun(Fold) ->
    fun(#full_doc_info{id=Id}=FDI, Offset, Acc) ->
            case couch_doc:to_doc_info(FDI) of
                #doc_info{revs=[#rev_info{deleted=false, rev=Rev}|_]} ->
                    Fold({{Id, Id}, {[{rev, couch_doc:rev_to_str(Rev)}]}},
                         Offset, Acc);
                #doc_info{revs=[#rev_info{deleted=true}|_]} ->
                    {ok, Acc}
            end
    end.

view_fold_start_response(_Req, _Etag, _RowCount, Offset, _Acc, _UpdateSeq) ->
    {ok, nil, {Offset, []}}.

view_fold_send_row(_Resp, Db, Doc, IncludeDocs, {Offset, Acc}) ->
    {Row} = couch_httpd_view:view_row_obj(Db, Doc, IncludeDocs),
    {ok, {Offset, [lists:map(fun format_row_item/1, Row)|Acc]}}.

format_row_item({Name, {Val}}) -> {Name, Val};
format_row_item({Name, Val}) -> {Name, Val}. 

%%---------------------------------------------------------------------------
%% Private functions
%%---------------------------------------------------------------------------

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L).
