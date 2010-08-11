-module(couchdb).

-include("couch_db.hrl").

-export([start/0]).

-export([open/1, open/2,
         close/1,
         delete_db/1, delete_db/2,
         info/1,
         all_dbs/0,
         put/2, put/3,
         put_many/2,
         get/2, get/3,
         delete/2,
         select/2, select/3, select/4]).

%% ---------------------------------------------------------------------------
%% @doc Convenience function to start couchdb app and dependent applications.
%%
%% In production deployments, use Erlang releases, which are better suited at
%% handling startup dependencies.
%% ---------------------------------------------------------------------------
start() ->
    application:start(sasl),
    application:start(crypto),
    application:start(couchdb).

%% ---------------------------------------------------------------------------
%% @doc Open a database. If the database doesn't exist, it's created.
%%
%% On success, returns {ok, Db} otherwise returns an applicable error.
%%
%% TODO: document options
%% TODO: confirm that options make sense for create as well as open, else
%%       we need two functions
%% ---------------------------------------------------------------------------

open(Name) ->
    open(Name, []).

open(Name, Options0) ->
    Options = maybe_add_admin_role(Options0),
    case couch_db:open(to_bin(Name), Options) of
        {not_found, no_db_file} ->
            couch_server:create(to_bin(Name), Options);
        Other -> Other
    end.

%% ---------------------------------------------------------------------------
%% @doc Closes a database.
%% ---------------------------------------------------------------------------

close(Db) ->
    couch_db:close(Db).

%% ---------------------------------------------------------------------------
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

%% ---------------------------------------------------------------------------
%% @doc Returns info about the specified database.
%%
%% Error if Db is invalid.
%% ---------------------------------------------------------------------------

info(Db) ->
    {ok, Info} = couch_db:get_db_info(Db),
    [info_item(I) || I <- Info].

info_item({db_name, B}) ->
    {db_name, binary_to_list(B)};
info_item({instance_start_time, B}) -> 
    {instance_start_time, list_to_integer(binary_to_list(B))};
info_item({_, _}=I) -> I.

%% ---------------------------------------------------------------------------
%% @doc Returns a list of all db names.
%% ---------------------------------------------------------------------------

all_dbs() ->
    {ok, Dbs} = couch_server:all_databases(),
    lists:map(fun binary_to_list/1, Dbs).

%% ---------------------------------------------------------------------------
%% @doc Stores a document in a db.
%%
%% Use couchdoc to create a document.
%%
%% Returns ???
%%
%% TODO: lots more here
%% TODO: missing UpdateType arg variant -- how used?
%% ---------------------------------------------------------------------------

put(Db, Doc) ->
    put(Db, Doc, []).

put(#db{}=Db0, Doc, Options) ->
    Db = reopen(Db0),
    case couch_db:update_doc(Db, Doc, Options) of
        {ok, {Start, RevId}} -> 
            {ok, Doc#doc{revs={Start, [RevId]}}};
        Err -> Err
    end.

%% ---------------------------------------------------------------------------
%% @doc Stores documents in bulk.
%% ---------------------------------------------------------------------------

put_many(#db{}=Db0, Docs) ->
    Db = reopen(Db0),
    {ok, Result} = couch_db:update_docs(Db, Docs),
    lists:zipwith(fun({ok, {Start, RevId}}, Doc) -> 
                          {ok, Doc#doc{revs={Start, [RevId]}}};
                     (Err, Doc) ->
                          {Err, Doc}
                  end, Result, Docs).

%% ---------------------------------------------------------------------------
%% @doc Retrieves a document.
%%
%% TODO: more here
%% ---------------------------------------------------------------------------

get(Db, Id) ->
    get(Db, Id, []).

get(#db{}=Db0, Id, Options) ->
    Db = reopen(Db0),
    case couch_db:open_doc(Db, to_bin(Id), Options) of
        {ok, Doc} -> {ok, Doc};
        Err -> Err
    end.

%% ---------------------------------------------------------------------------
%% @doc Removes Doc from Db.
%% ---------------------------------------------------------------------------

delete(#db{}=Db0, #doc{}=Doc) ->
    Db = reopen(Db0),
    delete_doc(Db, Doc);
delete(#db{}=Db0, Id) ->
    Db = reopen(Db0),
    case couch_db:open_doc(Db, to_bin(Id), []) of
        {ok, Doc} ->
            delete_doc(Db, Doc);
        Err -> Err
    end.

delete_doc(Db, #doc{revs={Start, [Rev|_]}}=Doc) ->
    DelDoc = Doc#doc{revs={Start, [Rev]}, deleted=true},
    {ok, [Result]} = couch_db:update_docs(Db, [DelDoc], []),
    Result.

%% ---------------------------------------------------------------------------
%% @doc Selects all documents or rows. See select/4 for more information.
%% ---------------------------------------------------------------------------

select(Source, Options) ->
    select(Source, undefined, undefined, Options).

%% ---------------------------------------------------------------------------
%% @doc Selects all documents or row matching a specified key of ID. See
%% select/4 for more information.
%%
%% exlucde_end is ignored if specified in Options.
%% ---------------------------------------------------------------------------

select(Source, KeyOrId, Options) ->
    select(Source, KeyOrId, KeyOrId, proplists:delete(exclude_end, Options)).

%% ---------------------------------------------------------------------------
%% @doc Selects a range of documents using start and end keys.
%%
%%   select(Source, Start, End, Options) -> {TotalRowCount, Offset, Rows}
%%
%%   Source          = Db | {Db, Design, View}
%%   Db              = db()
%%   Design          = string()
%%   View            = string()
%%   Start           = binary() | string() | undefined
%%   End             = binary() | string() | undefined
%%   Options         = options()
%%   options()       = [option()]
%%   option()        = {limit, int()}
%%                   | {skip, int()}
%%                   | reverse
%%                   | exclude_end
%%                   | include_docs
%%   TotalRowCount   = int()
%%   Offset          = int()
%%   Rows            = [row()]
%%   row()           = [{id, binary()}, {key, binary()}, {value, term()},
%%                      {doc, proplist()}]
%%
%% TODO: how do you denote an optional property in a proplist (doc above)
%% ---------------------------------------------------------------------------

select(#db{}=Db0, StartId0, EndId0, Options) ->
    Db = reopen(Db0),
    BaseArgs = view_query_base_args(Options),
    #view_query_args{direction=Dir,
                     limit=Limit,
                     skip=Skip} = BaseArgs,
    StartId = view_start(StartId0, Dir),
    EndId = view_end(EndId0, Dir),
    Args = BaseArgs#view_query_args{start_docid=StartId, end_docid=EndId},
    {ok, Info} = couch_db:get_db_info(Db),
    DocCount = couch_util:get_value(doc_count, Info),
    {ok, _, {_, _, _, Result}} = 
        couch_btree:fold(Db#db.fulldocinfo_by_id_btree,
                         enum_db_acc_fun(Db, DocCount, Args),
                         {Limit, Skip, undefined, []},
                         enum_db_options(Args)),
    case Result of
        {Offset, Rows} -> {DocCount, Offset, lists:reverse(Rows)};
        [] -> {DocCount, DocCount, []}
    end;

select({#db{}=Db0, Design, View}, StartKey, EndKey, Options) ->
    Db = reopen(Db0),
    BaseArgs = view_query_base_args(Options),
    #view_query_args{stale=Stale,
                     limit=Limit,
                     skip=Skip} = BaseArgs,
    Args = BaseArgs#view_query_args{start_key=StartKey, end_key=EndKey},
    try couch_view:get_map_view(Db, design_id(Design), to_bin(View), Stale) of
        {ok, Map, _} ->
            {ok, RowCount} = couch_view:get_row_count(Map),
            {ok, _, {_, _, _, {Offset, Rows}}} = 
                couch_view:fold(Map, fold_view_acc_fun(Db, RowCount, Args),
                                {Limit, Skip, undefined, []}, 
                                fold_view_options(Args)),
            {RowCount, Offset, lists:reverse(Rows)};
        {not_found, missing_named_view} -> {error, view_not_found}
    catch
        _:{not_found, missing} -> {error, design_not_found}
    end.

%% ===========================================================================
%% Private functions
%% ===========================================================================

%% ---------------------------------------------------------------------------
%% @doc Reopens the database. This is needed for most db operations. We use the
%% #doc record as our opaque db representation for convenience to the user.
%%
%% This has the upside of preserving user context when the db was open.
%% ---------------------------------------------------------------------------

reopen(#db{name=Name, user_ctx=Ctx}) ->
    {ok, Db} = open(Name, []),
    Db#db{user_ctx=Ctx}.

%% ---------------------------------------------------------------------------
%% @doc Returns a binary for a list or a binary. Used to ensure that various
%% couch values are binaries.
%% ---------------------------------------------------------------------------

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L).

%% ---------------------------------------------------------------------------
%% @doc Converts a proplist of options to a base view_query_args record. Used
%% by select functions.
%% ---------------------------------------------------------------------------

view_query_base_args(Options) ->
    Dir = case proplists:get_bool(reverse, Options) of
              true -> rev;
              false -> fwd
          end,
    Limit = proplists:get_value(limit, Options, 10000000000),
    Skip = proplists:get_value(skip, Options, 0),
    InclEnd = not proplists:get_bool(exclude_end, Options),
    InclDocs = proplists:get_bool(include_docs, Options),
    Stale = proplists:get_bool(stale, Options),
    #view_query_args{limit=Limit,
                     skip=Skip,
                     direction=Dir,
                     inclusive_end=InclEnd,
                     include_docs=InclDocs,
                     stale=Stale}.

%% ---------------------------------------------------------------------------
%% @doc Returns a design doc "id" for the give name.
%% ---------------------------------------------------------------------------

design_id(Name) ->
    NameBin = to_bin(Name),
    <<"_design/", NameBin/binary>>.

%% ---------------------------------------------------------------------------
%% @doc Returns a binary value for a given Start and Dir. If Start is
%% undefined, returns a value that is outside the range for Dir.
%% ---------------------------------------------------------------------------

view_start(undefined, Dir) -> unbound_start(Dir);
view_start(Id, _) -> to_bin(Id).

%% ---------------------------------------------------------------------------
%% @doc Returns a binary value that is outside the range for Dir.
%% ---------------------------------------------------------------------------

unbound_start(fwd) -> <<"">>;
unbound_start(rev) -> <<255>>. 

%% ---------------------------------------------------------------------------
%% @doc Same as view_start/2, but for the end value.
%% ---------------------------------------------------------------------------

view_end(undefined, Dir) -> unbound_end(Dir);
view_end(Id, _) -> to_bin(Id).

%% ---------------------------------------------------------------------------
%% @doc Same for unbound_start/1, but for the end value.
%% ---------------------------------------------------------------------------

unbound_end(fwd) -> <<255>>;    
unbound_end(rev) -> <<"">>. 

%% ---------------------------------------------------------------------------
%% @doc Returns a fun for use in couch_db:enum_docs. This can also be used
%% directly in a call to couch_btree:fold/4.
%%
%% This is a fairly complex function - the logic was largely taken from
%% hovercraft.
%% ---------------------------------------------------------------------------

enum_db_acc_fun(Db, DocCount, #view_query_args{}=Args) ->
    UpdateSeq = couch_db:get_update_seq(Db),
    Helpers = #view_fold_helper_funs{
      reduce_count=fun couch_db:enum_docs_reduce_to_count/1,
      start_response=fun doc_fold_start_response/6,
      send_row=fun doc_fold_send_row/5},
    FoldFun = couch_httpd_view:make_view_fold_fun(
                nil, Args, <<"">>, Db, UpdateSeq, DocCount, Helpers),
    fun(#full_doc_info{id=Id}=FDI, Offset, Acc) ->
            case couch_doc:to_doc_info(FDI) of
                #doc_info{revs=[#rev_info{deleted=false, rev=Rev}|_]} ->
                    FoldFun({{Id, Id}, {[{rev, couch_doc:rev_to_str(Rev)}]}},
                            Offset, Acc);
                #doc_info{revs=[#rev_info{deleted=true}|_]} ->
                    {ok, Acc}
            end
    end.

%% ---------------------------------------------------------------------------
%% @doc Returns a fun that can be used as the start_response field of a
%% view_fold_helper_funs record.
%%
%% Preserves the initial offset and provides an empty list for acc.
%% ---------------------------------------------------------------------------

doc_fold_start_response(_Req, _Etag, _RowCount, Offset, _Acc, _UpdateSeq) ->
    {ok, nil, {Offset, []}}.

%% ---------------------------------------------------------------------------
%% @doc Returns a fun that can be used as the send_row field of a
%% view_fold_helper_funs record.
%%
%% Preserves the offset provided and converts the doc into a view row object.
%%
%% Removes the outer tuple for "objects" to present them as tuple lists.
%% ---------------------------------------------------------------------------

doc_fold_send_row(_Resp, Db, Doc, IncludeDocs, {Offset, Acc}) ->
    {Row} = couch_httpd_view:view_row_obj(Db, Doc, IncludeDocs),
    {ok, {Offset, [lists:map(fun format_row_item/1, Row)|Acc]}}.

%% ---------------------------------------------------------------------------
%% @doc Strips an outer tuple, e.g. {[]} -> [], which is used by couch to
%% represent objects. This is a more natural representation in Erlang, though
%% admittedly tuple lists can be confused with strings.
%% ---------------------------------------------------------------------------

format_row_item({Name, {Val}}) -> {Name, Val};
format_row_item({Name, Val}) -> {Name, Val}.

%% ---------------------------------------------------------------------------
%% @doc Returns a list of options that can be used when enumerating documents.
%% ---------------------------------------------------------------------------

enum_db_options(#view_query_args{start_docid=StartId,
                                 direction=Dir,
                                 inclusive_end=InclEnd,
                                 end_docid=EndId}) ->
    [{start_key, StartId}, {dir, Dir}, 
     {case InclEnd of true -> end_key; false -> end_key_gt end, EndId}].

%% ---------------------------------------------------------------------------
%% @doc Returns a fun that can be used for folding in calls to
%% couch_view:fold/4.
%%
%% It borrows from hovercraft.
%% ---------------------------------------------------------------------------

fold_view_acc_fun(Db, RowCount, Args) ->
    UpdateSeq = couch_db:get_update_seq(Db),
    Helpers = #view_fold_helper_funs{
      reduce_count=fun couch_view:reduce_to_count/1,
      start_response=fun doc_fold_start_response/6,
      send_row=fun doc_fold_send_row/5},
    couch_httpd_view:make_view_fold_fun(
      nil, Args, <<"">>, Db, UpdateSeq, RowCount, Helpers).

%% ---------------------------------------------------------------------------
%% @doc Returns a list of options that can be used with couch_view:fold/4.
%% ---------------------------------------------------------------------------

fold_view_options(#view_query_args{start_key=StartKey, direction=Dir}) ->
    %% TODO - how are doc IDs used in API? - would use here - for now, using
    %% the unbounded val
    Start = {StartKey, unbound_start(Dir)},
    [{dir, Dir}, {start_key, Start}].

%% ---------------------------------------------------------------------------
%% @doc Adds an _admin role user context if user_ctx isn't specified in
%% Options.
%% ---------------------------------------------------------------------------

maybe_add_admin_role(Options) ->
    case proplists:get_value(user_ctx, Options) of
        undefined ->
            [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}|Options];
        _ ->
            Options
    end.
