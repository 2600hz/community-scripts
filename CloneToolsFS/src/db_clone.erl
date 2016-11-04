%% Sponsored by CloudPBX Inc. (http://cloudpbx.ca)
-module(db_clone).

%% {'ok', Dbs} = couch_mgr:db_info(), [begin case Db of <<"test-", _/binary>> -> couch_mgr:db_delete(wh_util:uri_encode(Db)); _ -> ok end end || Db <- Dbs].
-include("clone_tools.hrl").
-include("wh_types.hrl").

-export([run/0
         ,run/1
         ,main/1
        ]).

-define(VIEWS, [{<<"filtered_ids">>, wh_json:from_list([{<<"map">>, update_view(<<"function(doc) { if (doc.pvt_deleted || (doc.pvt_type == 'prviate_media'  && doc.pvt_created < {{date_media}}) || (doc.pvt_type == 'cdr'  && doc.pvt_created < {{date_cdr}}) || doc.pvt_type == 'vmbox' || doc.pvt_type == 'debit' || doc.pvt_type == 'credit' || doc.pvt_type == 'acdc_stat') return; emit(doc._id, null); }">>)}])}
                ,{<<"vmbox_ids">>, wh_json:from_list([{<<"map">>, <<"function(doc) { if (doc.pvt_deleted || doc.pvt_type != 'vmbox') return; emit(doc._id, null); }">>}])}
                ,{<<"has_attachments">>, wh_json:from_list([{<<"map">>, <<"function(doc) { if (doc.pvt_deleted || !doc._attachments) return; for(var attachment in doc._attachments) { emit(doc._id, doc._attachments[attachment].length); } }">>}
                                                            ,{<<"reduce">>, <<"_sum">>}
                                                           ])}
               ]).
-define(VIEW, [{<<"_id">>, <<"_design/clone">>}
               ,{<<"language">>, <<"javascript">>}
               ,{<<"views">>, wh_json:from_list(?VIEWS)}
              ]).
-define(REMOVE_KEYS, [<<"_rev">>, <<"_attachments">>]).
-define(MAX_BULK, 100).
-define(MAX_WORKERS, 1).

-define(FILTER_DESIGN, <<"_all_docs?startkey=\"_design/\"&endkey=\"_design0\"">>).
-define(FILTER_ALL_DOCS, <<"_all_docs">>).
-define(FILTER_FILTERED_IDS, <<"_design/clone/_view/filtered_ids">>).
-define(FILTER_HAS_ATTACHMENTS, <<"_design/clone/_view/has_attachments?reduce=true&group=true">>).
-define(FILTER_VMBOX, <<"_design/clone/_view/vmbox_ids">>).

-define(FAKE_VIEW_DIR, <<"__views">>).
-define(FAKE_VIEW_VMBOX, <<"vmbox">>).
-define(FAKE_VIEW_HAS_ATTACHMENTS, <<"has_attachments">>).

-define(CONTENT_TYPE_BYTES, 255).

run() ->
    inets:start(),
    process_flag('trap_exit', 'true'),
    ?LOG("cloning ~s to ~s~n", [?SOURCE, ?TARGET]),
    case get_source_dbs() of
        {'ok', Dbs} ->
            filter_and_clone_dbs(Dbs);
        'error' ->
            ?LOG_RED("Failed to load source dbs", [])
    end,
    ?WHITE,
    halt().

get_source_dbs() ->
    case is_file(?SOURCE) of
        'true' -> get_source_dbs_fs();
        'false' -> get_source_dbs_couch()
    end.

get_source_dbs_couch() ->
    case httpc:request(source_request([<<"_all_dbs">>])) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            {'ok', wh_json:decode(Body)};
        _Else ->
            ?LOG_RED("unable to get dbs: ~p~n", [_Else]),
            'error'
    end.

get_source_dbs_fs() ->
    case file:list_dir(database_fs_path("", ?SOURCE)) of
        {'ok', Filenames} ->
            {'ok', [filename_decode(Filename) || Filename <- Filenames]};
        {'error', Reason} ->
            ?LOG_RED("could not read dbs in source folder with error ~p", [Reason]),
            'error'
    end.

run(["-s", Source | Rest]) ->
    os:putenv("SOURCE", Source),
    run(Rest);
run(["-t", Target | Rest]) ->
    os:putenv("TARGET", Target),
    run(Rest);
run(["-e", "modb" | Rest]) ->
    os:putenv("EXCLUDE", "^account.*-\\d{6}$"),
    run(Rest);
run(["-e", Exclude | Rest]) ->
    os:putenv("EXCLUDE", Exclude),
    run(Rest);
run(["-dead_accounts", Accounts | Rest]) ->
    os:putenv("DEAD_ACCOUNTS", list_to_binary(Accounts)),
    run(Rest);
run(["-max_cdr_age", MaxCdrAge | Rest]) ->
    os:putenv("MAX_CDR_AGE", MaxCdrAge),
    run(Rest);
run(["-max_vm_age", MaxVmAge | Rest]) ->
    os:putenv("MAX_VM_AGE", MaxVmAge),
    run(Rest);
run([_|_]=Dbs) ->
    ?LOG_GREEN("cloning ~s to ~s~n", [?SOURCE, ?TARGET]),
    inets:start(),
    _ = filter_and_clone_dbs(Dbs),
    ?WHITE,
    halt();
run([]) ->
    run().

main(Opts) ->
    run(Opts).

update_view(Binary) ->
    Binary1 = update_binary(Binary, <<"date_media">>, ?MAX_VM_AGE),
    update_binary(Binary1, <<"date_cdr">>, ?MAX_CR_AGE).

update_binary(Binary, Key, MaxAge) ->
    Replacement = list_to_binary(integer_to_list(get_time(MaxAge))),
    binary:replace(Binary, <<"{{", Key/binary, "}}">>, Replacement, ['global']).

get_time('none') ->
    {CurrDate, _} = calendar:local_time(),
    NowInSec = calendar:datetime_to_gregorian_seconds({CurrDate, {0, 0, 0}}),
    MaxDate = {calendar:gregorian_days_to_date(100), {0, 0, 0}},
    NowInSec + calendar:datetime_to_gregorian_seconds(MaxDate);
get_time(MaxAge) ->
    {CurrDate, _} = calendar:local_time(),
    NowInSec = calendar:datetime_to_gregorian_seconds({CurrDate, {0, 0, 0}}),
    MaxDate = {calendar:gregorian_days_to_date(MaxAge), {0, 0, 0}},
    NowInSec - calendar:datetime_to_gregorian_seconds(MaxDate).

filter_and_clone_dbs(Dbs) ->
    case os:getenv("EXCLUDE") of
        'false' -> clone_dbs(Dbs);
        Exclude ->
            case re:compile(Exclude) of
                {'error', Error} ->
                    ?LOG_RED("wrong exclude regexp: ~p~n", [Error]),
                    halt(127);
                {'ok', MP} ->
            clone_dbs([Db || Db <- Dbs, case re:run(Db, MP) of 'nomatch' -> 'true'; _ -> 'false' end])
            end
    end.

clone_dbs([]) -> 'ok';
clone_dbs([Db|Dbs]) when not is_binary(Db) ->
    clone_dbs([wh_types:to_binary(Db)|Dbs]);
clone_dbs([<<"ts_usage", _/binary>>|Dbs]) ->
    clone_dbs(Dbs);
clone_dbs([<<"ts_cdr", _/binary>>|Dbs]) ->
    clone_dbs(Dbs);
clone_dbs([<<"ts_rates">>|Dbs]) ->
    clone_dbs(Dbs);
clone_dbs([<<"anonymous_cdrs">>|Dbs]) ->
    clone_dbs(Dbs);
clone_dbs([<<"media_files">>|Dbs]) ->
    clone_dbs(Dbs);
clone_dbs([<<"token_auth">>|Dbs]) ->
    clone_dbs(Dbs);
clone_dbs([<<"ts">>|Dbs]) ->
    clone_dbs(Dbs);
clone_dbs([Db|Dbs]) ->
    _ = case wh_account:is_account_db(Db) of
        'true' ->
            ?LOG_WHITE("cloning account db ~s, ~p databases remaining~n"
                       ,[Db, length(Dbs)]),
            clone_account_db(wh_account:format_id(Db, 'encoded'));
        'false' ->
            ?LOG_WHITE("cloning db ~s, ~p databases remaining~n"
                       ,[Db, length(Dbs)]),
            clone_db(wh_types:to_binary(uri_encode(Db)))
        end,
    clone_dbs(Dbs).

clone_db(Db) ->
    case create_db(Db) of
        'false' -> 'ok';
        'true' ->
            put('errors', []),
            _ = clone_design_docs(Db),
            _ = clone_all_docs(Db),
            _ = clone_attachments(Db),
            [?LOG_RED("~s", [Error])
             || Error <- get('errors')
            ]
    end.

clone_account_db(Db) ->
    case create_db(Db) of
        'false' -> 'ok';
        'true' ->
            put('errors', []),
            _ = clone_design_docs(Db),
            _ = clone_filtered_docs(Db),
            _ = clone_attachments(Db),
            _ = clone_voicemail_boxes(Db),
            %_ = roll_up_credit(Db),
            [?LOG_RED("~s", [Error])
             || Error <- get('errors')
            ]
    end.

clone_all_docs(Db) ->
    case find_missing_ids(Db) of
        [] -> ?LOG_GREEN("  documents are in sync~n", []);
        Ids ->
            ?LOG_CYAN("  found ~p missing documents~n"
                      ,[length(Ids)]),
            clone_docs(Ids, Db, 'true')
    end.

find_missing_ids(Db) ->
    SourceIds = sets:from_list(get_ids(Db, ?FILTER_ALL_DOCS, ?SOURCE)),
    TargetIds = sets:from_list(get_ids(Db, ?FILTER_ALL_DOCS, ?TARGET)),
    sets:to_list(sets:subtract(SourceIds, TargetIds)).

clone_design_docs(Db) ->
    _ = maybe_put_clone_view(Db),
    case find_missing_design_ids(Db) of
        [] -> ?LOG_GREEN("  design documents are in sync~n", []);
        Ids ->
            ?LOG_CYAN("  found ~p missing design documents~n"
                      ,[length(Ids)]),
            clone_docs(Ids, Db, 'true')
    end.

find_missing_design_ids(Db) ->
    SourceIds = sets:from_list(get_ids(Db, ?FILTER_DESIGN, ?SOURCE)),
    TargetIds = sets:from_list(get_ids(Db, ?FILTER_DESIGN, ?TARGET)),
    sets:to_list(sets:subtract(SourceIds, TargetIds)).

clone_filtered_docs(Db) ->
    case find_missing_filtered_ids(Db) of
        [] -> ?LOG_GREEN("  filtered documents are in sync~n", []);
        Ids ->
            ?LOG_CYAN("  found ~p missing filtered documents~n"
                      ,[length(Ids)]),
            clone_docs(Ids, Db, 'false')
    end.

find_missing_filtered_ids(Db) ->
    SourceIds = sets:from_list(get_ids(Db, ?FILTER_FILTERED_IDS, ?SOURCE)),
    TargetIds = sets:from_list(get_ids(Db, ?FILTER_FILTERED_IDS, ?TARGET)),
    sets:to_list(sets:subtract(SourceIds, TargetIds)).

clone_attachments(Db) ->
    case find_missing_attachments(Db) of
        [] -> ?LOG_GREEN("  attachments are in sync~n", []);
        Ids ->
            ?LOG_CYAN("  found ~p missing attachments~n"
                      ,[length(Ids)]),
            maybe_copy_attachments(Ids, Db)
    end.

find_missing_attachments(Db) ->
    TargetIds = get_ids(Db, ?FILTER_HAS_ATTACHMENTS, ?TARGET),
    SourceIds = get_ids(Db, ?FILTER_HAS_ATTACHMENTS, ?SOURCE),
    lists:foldl(fun({Id, Length}, Ids) ->
                        case props:get_value(Id, TargetIds) =/= Length of
                            'true' -> [Id|Ids];
                            'false' -> Ids
                        end
                end, [], SourceIds).

clone_voicemail_boxes(Db) ->
    case find_missing_voicemail_boxes(Db) of
        [] -> ?LOG_GREEN("  no voicemail boxes found~n", []);
        Ids -> ?LOG_CYAN("  found ~p voicemail boxes~n"
                         ,[length(Ids)]),
               clone_docs(Ids, Db, 'false')
    end.

find_missing_voicemail_boxes(Db) ->
    get_ids(Db, ?FILTER_VMBOX, ?SOURCE).

roll_up_credit(Db) ->
    SourceCredit = get_source_credit(Db),
    TargetCredit = get_target_credit(Db),
    case SourceCredit =/= 'error' andalso TargetCredit =/= 'error' of
        'false' -> 'ok';
        'true' -> roll_up_credit(SourceCredit - TargetCredit, Db)
    end.

clone_docs([], _, _) -> 'ok';
clone_docs(Ids, Db, 'false') ->
    case length(Ids) > ?MAX_BULK of
        'false' -> individual_doc_clone(Ids, Db);
        'true' ->
            {Bulk, Remaining} = lists:split(?MAX_BULK, Ids),
            _ = individual_doc_clone(Bulk, Db),
            clone_docs(Remaining, Db, 'false')
    end;
clone_docs(Ids, Db, 'true') ->
    case length(Ids) > ?MAX_BULK of
        'false' -> bulk_doc_clone(Ids, Db);
        'true' ->
            {Bulk, Remaining} = lists:split(?MAX_BULK, Ids),
            _ = bulk_doc_clone(Bulk, Db),
            clone_docs(Remaining, Db, 'true')
    end.

individual_doc_clone(Ids, Db) ->
    ?LOG_WHITE("  ", []),
    _ = case get_docs(Ids, Db) of
            {'ok', JObjs} ->
                [put_doc(JObj, Db) || JObj <- JObjs];
            _Else ->
                [?LOG_RED("!", []) || _ <- Ids]
        end,
    ?LOG_WHITE("~n", []).

bulk_doc_clone(Ids, Db) ->
    case get_docs(Ids, Db) of
        {'ok', JObjs} ->
            put_docs(JObjs, Db);
        _Else ->
            ?LOG_RED("  failed to fetch ~p documents~n"
                     ,[length(Ids)])
    end.

get_docs(Ids, Db) ->
    case is_file(?SOURCE) of
        'true' -> get_docs_file(Ids, Db);
        'false' -> get_docs_url(Ids, Db)
    end.

get_docs_file(Ids, Db) ->
    Docs = [get_doc_file(Id, Db, ?SOURCE) || Id <- Ids],
    {'ok', [Doc || Doc <- Docs, Doc =/= 'error', should_clone(wh_json:set_value(<<"doc">>, Doc, wh_json:new()))]}.

get_doc_file(Id, Db, Location) ->
    File = document_fs_path(Db, Id, Location),
    case file:read_file(File) of
        {'ok', Contents} ->
            JObj = wh_json:decode(Contents),
            wh_json:delete_keys(?REMOVE_KEYS, JObj);
        {'error', Reason} ->
            put('errors'
                ,[io_lib:format("  failed to read file ~p with reason ~p~n"
		,[File, Reason]) | get('errors')]),
            'error'
    end.

get_docs_url(Ids, Db) ->
    JObj = wh_json:from_list([{<<"keys">>, Ids}]),
    Source = source_request([Db
                             ,<<"_all_docs?include_docs=true">>
                            ]),
    Request = {Source
               ,[]
               ,"application/json"
               ,wh_json:encode(JObj)
              },
    HTTPOptions = [],
    ClientOptions = [{'body_format', 'binary'}],
    case httpc:request('post', Request, HTTPOptions, ClientOptions) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            {'ok', [wh_json:delete_keys(?REMOVE_KEYS, wh_json:get_value(<<"doc">>, J))
                    || J <- wh_json:get_value(<<"rows">>, wh_json:decode(Body))
                           ,should_clone(J)
                   ]};
        {'ok', {{_, Code, Reason}, _, Body}} ->
            put('errors'
                ,[io_lib:format("  failed to get bulk docs from ~s: ~p ~s ~s"
                                ,[Source, Code, Reason, Body])
                  | get('errors')
                 ]);
        {'error', _R} ->
            put('errors'
                ,[io_lib:format("  failed to get bulk docs from ~s: ~p~n"
                                ,[Source, _R])
                  | get('errors')
                 ])
    end.

put_doc(JObj, Db) ->
    case is_file(?TARGET) of
        'true' ->
            put_doc_file(JObj, Db);
        'false' ->
            put_doc_url(JObj, Db)
    end.

put_doc_file(JObj, Db) ->
    Id = wh_json:get_value(<<"_id">>, JObj),
    Path = document_fs_path(Db, Id, ?TARGET),
    Binary = wh_json:encode(JObj),
    case file:write_file(Path, Binary) of
        'ok' ->
            add_to_fake_views(Id, Db, JObj),
            ?LOG_WHITE(".", []);
        {'error', Reason} ->
            put('errors'
                ,[io_lib:format("  failed to store file ~p: ~p~n"
                                ,[Path, Reason])
                  | get('errors')
                 ]),
            ?LOG_RED("!", [])
    end.

add_to_fake_views(Id, Db, JObj) ->
    lists:foreach(fun(F) -> F(Id, Db, JObj) end,
        [fun maybe_add_to_fake_view_vmbox/3
     ]).

maybe_add_to_fake_view_vmbox(Id, Db, JObj) ->
    case wh_json:get_binary_value(<<"pvt_type">>, JObj) =:= <<"vmbox">> of
        'true' ->
            add_to_fake_view(Id, Db, ?FAKE_VIEW_VMBOX);
        'false' ->
            'noview'
    end.

load_from_fake_view(Db, ?FAKE_VIEW_VMBOX, Location) ->
    load_lines_from_fake_view(Db, ?FAKE_VIEW_VMBOX, Location);

load_from_fake_view(Db, ?FAKE_VIEW_HAS_ATTACHMENTS, Location) ->
    Attachments = load_lines_from_fake_view(Db, ?FAKE_VIEW_HAS_ATTACHMENTS, Location),
    lists:foldl(fun(X, Accum) ->
        [Key, Value] = binary:split(X, <<":">>),
        [wh_json:set_values([{<<"key">>, Key}, {<<"value">>, Value}], wh_json:new()) | Accum]
    end, [], Attachments).

load_lines_from_fake_view(Db, FakeView, Location) ->
    Path = fake_view_fs_path(Db, Location, FakeView),
    case get_all_lines(Path) of
        Lines=[_|_] ->
            [wh_types:to_binary(string:strip(Line, 'both', $\n)) || Line <- Lines];
        _ ->
            []
    end.

get_all_lines(Path) ->
    case file:open(Path, 'read') of
        {'ok', IODevice} ->
            get_all_lines(IODevice, []),
            file:close(IODevice);
        {'error', Reason} -> {'error', Reason}
    end.

%% From https://erlangcentral.org/wiki/index.php?title=Read_File_to_List
get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), lists:reverse(Accum);
        Line -> get_all_lines(Device, [Line|Accum])
    end.

add_to_fake_view(Id, Db, FakeView) ->
    add_to_fake_view(Id, Db, FakeView, Id).

add_to_fake_view(Id, Db, FakeView, Content) ->
    Path = fake_view_fs_path(Db, ?TARGET, FakeView),
    case file:open(Path, ['append']) of
        {'ok', IODevice} ->
            io:fwrite(IODevice, "~s~n", [wh_types:to_list(Id)]),
            file:close(IODevice);
        {'error', Reason} ->
            ?LOG_RED("Failed to open fake view file ~p for writing with error ~p~n", [Path, Reason])
    end.

put_doc_url(JObj0, Db) ->
    JObj = maybe_fix_doc(JObj0, wh_json:get_value(<<"pvt_type">>, JObj0)),
    Id = uri_encode(wh_json:get_value(<<"_id">>, JObj)),
    Target = target_request([Db
                             ,Id
                            ]),
    Request = {Target
               ,[]
               ,"application/json"
               ,wh_json:encode(JObj)
              },
    HTTPOptions = [],
    ClientOptions = [{'body_format', 'binary'}],
    case httpc:request('put', Request, HTTPOptions, ClientOptions) of
        {'ok', {{_, 202, "Accepted"}, _, _}} -> ?LOG_WHITE(".", []);
        {'ok', {{_, 201, "Created"}, _, _}} -> ?LOG_WHITE(".", []);
        {'ok', {{_, 200, "OK"}, _, _}} ->  ?LOG_WHITE(".", []);
        {'ok', {{_, 409, "Conflict"}, _, _}} ->
            case maybe_fix_confict(Id, Db, JObj) of
                <<>> -> ?LOG_YELLOW("-", []);
                Rev ->
                    ?LOG_WHITE("+", []),
                    put_doc(wh_json:set_value(<<"_rev">>, Rev, JObj), Db)
            end;
        {'ok', {{_, Code, Reason}, _, Body}} ->
            put('errors'
                ,[io_lib:format("  failed to store ~s: ~p ~s ~s"
                                ,[Target, Code, Reason, Body])
                  | get('errors')
                 ]),
            ?LOG_RED("!", []);
        {'error', _R} ->
            put('errors'
                ,[io_lib:format("  failed to store ~s: ~p~n"
                                ,[Target, _R])
                  | get('errors')
                 ]),
            ?LOG_RED("!", [])
    end.

put_docs(JObjs, Db) ->
    case is_file(?TARGET) of
        'true' ->
            lists:foreach(fun(JObj) -> put_doc_file(JObj, Db) end, JObjs);
        'false' ->
            put_docs_url(JObjs, Db)
    end.

put_docs_url(JObjs, Db) ->
    Target = target_request([Db
                             ,<<"_bulk_docs?batch=ok">>
                            ]),
    Request = {Target
               ,[]
               ,"application/json"
               ,wh_json:encode(wh_json:from_list([{<<"docs">>, JObjs}]))
              },
    HTTPOptions = [],
    ClientOptions = [{'body_format', 'binary'}],
    case httpc:request('post', Request, HTTPOptions, ClientOptions) of
        {'ok', {{_, 202, "Accepted"}, _, _}} ->
            ?LOG_WHITE("  ~p documents accepted by source~n", [length(JObjs)]);
        {'ok', {{_, 201, "Created"}, _, _}} ->
            ?LOG_WHITE("  ~p documents created on source~n", [length(JObjs)]);
        {'ok', {{_, 200, "OK"}, _, _}} ->
            ?LOG_WHITE("  ~p documents created on source~n", [length(JObjs)]);
        {'ok', {{_, Code, Reason}, _, Body}} ->
            ?LOG_RED("  failed to store ~p documents at ~s: ~p ~s ~s"
                     ,[length(JObjs), Target, Code, Reason, Body]);
        {'error', _R} ->
            ?LOG_RED("  failed to store ~p documents at ~s: ~p~n"
                     ,[length(JObjs), Target, _R])
    end.

should_clone(JObj) ->
    case wh_json:is_true([<<"doc">>, <<"pvt_deleted">>], JObj) of
        'false' -> 'true';
        'true' ->
            ?LOG_YELLOW("*", []),
            'false'
    end.

maybe_copy_attachments(Ids, Db) ->
    ?LOG_WHITE("  ", []),
    maybe_copy_attachments(Ids, Db, ?MAX_BULK).

maybe_copy_attachments([], _, _) -> ?LOG_WHITE("~n", []);
maybe_copy_attachments(Ids, Db, Column) ->
    case length(Ids) > ?MAX_WORKERS of
        'false' ->
            Workers = maybe_start_copy_worker(Ids, Db, 0),
            wait_for_copy_workers(Workers, Column),
            ?LOG_WHITE("~n", []);
        'true' ->
            {Bulk, Remaining} = lists:split(?MAX_WORKERS, Ids),
            Workers = maybe_start_copy_worker(Bulk, Db, 0),
            C = wait_for_copy_workers(Workers, Column),
            maybe_copy_attachments(Remaining, Db, C)
    end.

maybe_start_copy_worker([], _, Workers) -> Workers;
maybe_start_copy_worker([Id|Ids], Db, Workers) ->
   case maybe_start_copy_worker(get_attachments(Db, Id, ?SOURCE)
                                ,get_attachments(Db, Id, ?TARGET)
                                ,Id
                                ,Db)
   of
       'ok' -> maybe_start_copy_worker(Ids, Db, Workers);
       _Pid -> maybe_start_copy_worker(Ids, Db, Workers + 1)
   end.

wait_for_copy_workers(0, Column) -> Column;
wait_for_copy_workers(Workers, Column) ->
    receive
        {'copy_results', _, Results} ->
            C = print_copy_worker_results(Results, Column),
            wait_for_copy_workers(Workers - 1, C)
    end.

print_copy_worker_results([], Column) -> Column;
print_copy_worker_results(Results, 0) ->
    ?LOG_WHITE("~n  ", []),
    print_copy_worker_results(Results, ?MAX_BULK);
print_copy_worker_results(['ok'|Results], Column) ->
    ?LOG_WHITE(".", []),
    print_copy_worker_results(Results, Column - 1);
print_copy_worker_results(['conflict'|Results], Column) ->
    ?LOG_YELLOW("+", []),
    print_copy_worker_results(Results, Column - 1);
print_copy_worker_results([{'error', Error}|Results], Column) ->
    put('errors', [Error | get('errors')]),
    print_copy_worker_results(Results, Column - 1);
print_copy_worker_results([_|Results], Column) ->
    ?LOG_RED("!", []),
    print_copy_worker_results(Results, Column - 1).

maybe_start_copy_worker('error', _, _, _) -> 'ok';
maybe_start_copy_worker(_, 'error', _, _) -> 'ok';
maybe_start_copy_worker(Source, Target, Id, Db) ->
    case [Name
          || Name <- wh_json:get_keys(Source)
                 ,should_copy_attachment(Name, Source, Target)
         ]
    of
        [] -> 'ok';
        Names ->
            Parent = self(),
            spawn(fun() ->
                          Results = copy_worker(Names, Id, Db, []),
                          Parent ! {'copy_results', self(), Results}
                  end)
    end.

copy_worker([], _, _, Results) -> Results;
copy_worker([Name|Names], Id, Db, Results) ->
    case get_attachment(Name, Id, Db) of
        {Attachment, ContentType} ->
            Result = put_attachment(Attachment, ContentType, Name, Id, Db),
            copy_worker(Names, Id, Db, [Result|Results]);
        Else ->
            copy_worker(Names, Id, Db, [Else|Results])
    end.

should_copy_attachment(Name, Source, Target) ->
    wh_json:get_value(Name, Target) =:= 'undefined'
        orelse (wh_json:get_value([Name, <<"content_type">>], Source)
                =/= wh_json:get_value([Name, <<"content_type">>], Target))
        orelse (wh_json:get_value([Name, <<"length">>], Source)
                =/= wh_json:get_value([Name, <<"length">>], Target)).

get_attachments(Db, Id, Location) ->
    case is_file(Location) of
        'true' -> get_attachments_file(Db, Id, Location);
        'false' -> get_attachments_url([Db, Id], Location)
    end.

get_attachments_file(Db, Id, Location) ->
    Path = document_attachments_path(Db, Id, Location),
    case file:list_dir(document_attachments_path(Db, Id, Location)) of
        {'ok', Filenames} ->
            lists:foldl(fun(Filename, JObj) ->
                Name = wh_types:to_binary(filename_decode(Filename)),
                wh_json:set_value(Name, wh_json:new(), JObj)
            end, wh_json:new(), Filenames);
        _ ->
            []
    end.

get_attachments_url(Req, Location) ->
    Source = ?SOURCE,
    Target = ?TARGET,
    case Location of
        Source ->
            get_attachments_url(source_request(Req));
        Target ->
            get_attachments_url(target_request(Req))
    end.

get_attachments_url(URL) ->
    case httpc:request(URL) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            JObj = wh_json:decode(Body),
            wh_json:get_value(<<"_attachments">>, JObj);
        {'ok', {{_, Code, Reason}, _, Body}} ->
            put('errors'
                ,[io_lib:format("  failed to get attachments ~s: ~p ~s ~s"
                                ,[URL, Code, Reason, Body])
                  | get('errors')
                 ]),
            'error';
        {'error', _R} ->
            put('errors'
                ,[io_lib:format("  failed to get attachments ~s: ~p~n"
                                ,[URL, _R])
                  | get('errors')
                 ]),
            'error'
    end.

get_attachment(Name, Id, Db) ->
    case is_file(?SOURCE) of
        'true' -> get_attachment_file(Name, Id, Db, ?SOURCE);
        'false' -> get_attachment_url(Name, Id, Db)
    end.

get_attachment_file(Name, Id, Db, Location) ->
    Path = attachment_fs_path(Db, Id, Name, Location),
    case file:read_file(Path) of
        {'ok', <<ContentTypePart:?CONTENT_TYPE_BYTES/binary, Content/binary>>} ->
            ContentType = string:strip(wh_types:to_list(ContentTypePart), 'both', $\0),
            {Content, ContentType};
        {'error', Reason} ->
            {'error',
             io_lib:format("  failed to get attachment from location ~p with reason ~p"
                           ,[Path, Reason])}
    end.

get_attachment_url(Name, Id, Db) ->
    Source = source_request([Db
                             ,Id
                             ,Name
                            ]),
    case httpc:request(Source) of
        {'ok', {{_, 200, "OK"}, Headers, Attachment}} ->
            {Attachment, props:get_value("content-type", Headers)};
        {'ok', {{_, Code, Reason}, _, Body}} ->
            {'error',
             io_lib:format("  failed to get attachment ~s: ~p ~s ~s"
                           ,[Source, Code, Reason, Body])};
        {'error', _R} ->
            {'error',
             io_lib:format("  failed to get attachment ~s: ~p~n"
                           ,[Source, _R])}
    end.

put_attachment(Attachment, ContentType, Name, Id, Db) ->
    case is_file(?TARGET) of
        'true' -> put_attachment_file(Attachment, ContentType, Name, Id, Db, ?TARGET);
        'false' -> put_attachment_url(Attachment, ContentType, Name, Id, Db)
    end.

maybe_create_doc_attachment_dir(Id, Db, Location) ->
    Path = document_attachments_path(Db, Id, Location),
    case file:make_dir(Path) of
        'ok' -> 'ok';
        {'error', 'eexist'} -> 'ok';
        {'error', Reason} -> ?LOG_RED("Failed to create dir ~p with reason ~p~n", [Path, Reason])
    end.

put_attachment_file(Attachment, ContentType, Name, Id, Db, Location) ->
    {Content, Type} = {wh_types:to_binary(Attachment), wh_types:to_binary(ContentType)},
    maybe_create_doc_attachment_dir(Id, Db, Location),
    Path = attachment_fs_path(Db, Id, Name, Location),
    Binary = <<Type/binary, $\0:(8*(?CONTENT_TYPE_BYTES-size(Type))), Content/binary>>,
    case file:write_file(Path, Binary) of
        'ok' ->
            'ok';
        {'error', Reason} ->
            {'error',
             io_lib:format("  failed to save attachment to file ~p with reason ~p~n"
                           ,[Path, Reason])}
    end.

put_attachment_url(Attachment, ContentType, Name, Id, Db) ->
    Rev = get_rev(Id, Db),
    Target = target_request([Db
                             ,Id
                             ,<<Name/binary
                                ,"?rev="
                                ,Rev/binary>>
                            ]),
    Request = {Target
               ,[]
               ,ContentType
               ,Attachment
              },
    HTTPOptions = [],
    ClientOptions = [{'body_format', 'binary'}],
    case httpc:request('put', Request, HTTPOptions, ClientOptions) of
        {'ok', {{_, 202, "Accepted"}, _, _}} -> 'ok';
        {'ok', {{_, 201, "Created"}, _, _}} -> 'ok';
        {'ok', {{_, 200, "OK"}, _, _}} -> 'ok';
        {'ok', {{_, 409, "Conflict"}, _, _}} -> 'conflict';
        {'ok', {{_, Code, Reason}, _, Body}} ->
            {'error',
             io_lib:format("  failed to put attachment ~s: ~p ~s ~s~n"
                           ,[Target, Code, Reason, Body])};
        {'error', _R} ->
            {'error',
             io_lib:format("  failed to put attachment ~s: ~p~n"
                           ,[Target, _R])}
    end.

maybe_fix_confict(Id, Db, SJObj) ->
    Target = target_request([Db
                             ,Id
                            ]),
    case httpc:request(Target) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            TJObj = wh_json:decode(Body),
            case wh_json:get_value(<<"pvt_modified">>, TJObj) >
                wh_json:get_value(<<"pvt_modified">>, SJObj)
            of
                'true' -> <<>>;
                'false' ->
                    wh_json:get_value(<<"_rev">>, TJObj, <<>>)
            end;
        _Else -> <<>>
    end.

get_rev(Id, Db) ->
    Target = target_request([Db
                             ,Id
                            ]),
    case httpc:request(Target) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            JObj = wh_json:decode(Body),
            wh_json:get_value(<<"_rev">>, JObj, <<>>);
        _Else -> <<>>
    end.

is_file(Url) when is_list(Url) ->
    is_file(wh_types:to_binary(Url));
is_file(<<"file://", _/binary>>) ->
    'true';
is_file(_) ->
    'false'.

filename_encode(Name) ->
    wh_types:to_list(uri_encode(Name)).

filename_decode(Name) ->
    wh_types:to_list(uri_decode(Name)).

database_fs_path(Db, Location) ->
    <<"file://", FolderName/binary>> = wh_types:to_binary(Location),
    %% No need to encode Db name, should already be done
    filename:join([FolderName, Db]).

document_fs_path(Db, DocId, Location) ->
    filename:join([database_fs_path(Db, Location), filename_encode(DocId)]).

document_attachments_path(Db, DocId, Location) ->
    filename:join([database_fs_path(Db, Location), ["__" | filename_encode(DocId)]]).

attachment_fs_path(Db, DocId, Name, Location) ->
    filename:join([document_attachments_path(Db, DocId, Location), filename_encode(Name)]).

fake_view_fs_path(Db, Location, FakeView) ->
    Path = filename:join([database_fs_path(Db, Location), ?FAKE_VIEW_DIR, FakeView]).

create_db(Db) ->
    case is_file(?TARGET) of
        'true' -> create_db(Db, 'file');
        'false' -> create_db(Db, 'url')
    end.

create_db(Db, 'file') ->
    Path = database_fs_path(Db, ?TARGET),
    case file:make_dir(Path) of
        'ok' ->
            'ok' = file:make_dir(filename:join(Path, ?FAKE_VIEW_DIR)),
            'true';
        {'error', 'eexist'} ->
            'true';
        {'error', Reason} ->
            ?LOG_RED("  failed to create folder ~p with reason ~p~n", [Path, Reason]),
            'false'
    end;

create_db(Db, 'url') ->
    Target = target_request([Db]),
    Request = {Target, [], "application/json", <<"{}">>},
    HTTPOptions = [],
    ClientOptions = [{'body_format', 'binary'}],
    case httpc:request('put', Request, HTTPOptions, ClientOptions) of
        {'ok', {{_, 201, "Created"}, _, _}} ->
            ?LOG_GREEN("  created database ~s~n", [Target]),
            'true';
        {'ok', {{_, 412, "Precondition Failed"}, _, _}} ->
            ?LOG_GREEN("  database ~s already exists on target~n", [Target]),
            'true';
        {'error', 'socket_closed_remotely'} ->
            timer:sleep(500),
            ?LOG_YELLOW("  retrying create database ~s~n", [Target]),
            create_db(Db);
        {'ok', {{_, Code, Reason}, _, Body}} ->
            ?LOG_RED("  failed to create db ~s: ~p ~s ~s"
                     ,[Db, Code, Reason, Body]),
            'false';
        {'error', _R}->
            ?LOG_RED("  failed to create db ~s: ~p"
                     ,[Db, _R]),
            'false'
    end.

get_ids(Db, Filter, Location) ->
    case is_file(Location) of
        'true' -> get_ids_file(Db, Filter, Location);
        'false' -> get_ids_url([Db, Filter], Location)
    end.

get_ids_file(Db, ?FILTER_ALL_DOCS, Location) ->
    get_ids_file(Db, Location);

get_ids_file(Db, ?FILTER_FILTERED_IDS, Location) ->
    %% Currently filters only work when cloning from CouchDB
    get_ids_file(Db, Location);

get_ids_file(Db, ?FILTER_VMBOX, Location) ->
    load_from_fake_view(Db, ?FAKE_VIEW_VMBOX, Location);

get_ids_file(Db, ?FILTER_HAS_ATTACHMENTS, Location) ->
    {'ok', Filenames} = file:list_dir(database_fs_path(Db, Location)),
    %TODO: Get file sizes as well
    [{attachments_dir_to_document_name(Filename), 0} || Filename <- Filenames, is_attachments_dir(Filename)];

get_ids_file(Db, ?FILTER_DESIGN, Location) ->
    Filter = fun(Id) ->
        case wh_types:to_binary(Id) of
            <<"_", _/binary>> -> 'true';
            _ -> 'false'
        end
    end,
    [Id || Id <- get_ids_file(Db, Location), Filter(Id)].

get_ids_file(Db, Location) ->
    {'ok', Filenames} = file:list_dir(database_fs_path(Db, Location)),
    [filename_decode(Filename) || Filename <- Filenames, is_special_dir(Filename) =:= 'false'].

attachments_dir_to_document_name(Filename) ->
    <<"__", Name/binary>> = wh_types:to_binary(Filename),
    filename_decode(Name).

%% Special dirs are anything starting with __, including the fake views dir and attachments dir
is_special_dir(Filename) ->
    case wh_types:to_binary(Filename) of
        <<"__", _/binary>> -> 'true';
        _ -> 'false'
    end.

is_attachments_dir(Filename) ->
    case wh_types:to_binary(Filename) of
        ?FAKE_VIEW_DIR -> 'false';
        <<"__", _/binary>> -> 'true';
        _ -> 'false'
    end.

get_ids_url(Req, Location) ->
    Source = ?SOURCE,
    Target = ?TARGET,
    case Location of
        Source ->
            get_ids_url(source_request(Req));
        Target ->
            get_ids_url(target_request(Req))
    end.

get_ids_url(URL) ->
    case httpc:request(URL) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            [handle_has_attachments(JObj)
             || JObj <- wh_json:get_value(<<"rows">>, wh_json:decode(Body), [])
            ];
        {'ok', {{_, Code, Reason}, _, Body}} ->
            ?LOG_RED("  failed to get doc ids ~s: ~p ~s ~s"
                     ,[URL, Code, Reason, Body]),
            [];
        {'error', _R} ->
            ?LOG_RED("  failed to get doc ids ~s: ~p~n"
                     ,[URL, _R]),
            []
    end.

handle_has_attachments(JObj) ->
    case wh_json:get_value(<<"id">>, JObj) of
        'undefined' ->
            {wh_json:get_value(<<"key">>, JObj)
             ,wh_json:get_value(<<"value">>, JObj)};
        Id -> Id
    end.

maybe_put_clone_view(Db) ->
    case is_file(?SOURCE) of
        'true' -> 'true';
        'false' -> put_clone_view(Db)
    end.

put_clone_view(Db) ->
    Source = source_request([Db
                             ,<<"_design/clone">>
                            ]),
    JObj = clone_view(Source),
    Request = {Source, [], "application/json", wh_json:encode(JObj)},
    HTTPOptions = [],
    ClientOptions = [{'body_format', 'binary'}],
    case httpc:request('put', Request, HTTPOptions, ClientOptions) of
        {'ok', {{_, 201, "Created"}, _, _}} -> 'true';
        {'ok', {{_, 200, "OK"}, _, _}} -> 'true';
        {'ok', {{_, Code, Reason}, _, Body}} ->
            ?LOG_RED("  failed to update ~s: ~p ~s ~s"
                     ,[Source, Code, Reason, Body]),
            'error';
        {'error', _R} ->
            ?LOG_RED("  failed to update ~s: ~p~n"
                     ,[Source, _R]),
            'error'
    end.

clone_view(URL) ->
    case httpc:request(URL) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            JObj = wh_json:decode(Body),
            Rev = wh_json:get_value(<<"_rev">>, JObj),
            wh_json:from_list([{<<"_rev">>, Rev}|?VIEW]);
        _Else -> wh_json:from_list(?VIEW)
    end.

roll_up_credit(0, _) ->
    ?LOG_GREEN("  credit is in sync~n", []);
roll_up_credit(Diff, Db) when Diff > 0 ->
    ?LOG_CYAN("  credit ~p units~n", [Diff]),
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    JObj = wh_json:from_list([{<<"description">>, <<"roll up">>}
                              ,{<<"pvt_reason">>, <<"admin_discretion">>}
                              ,{<<"pvt_code">>, 3005}
                              ,{<<"pvt_amount">>, abs(Diff)}
                              ,{<<"pvt_type">>, <<"credit">>}
                              ,{<<"pvt_created">>, Timestamp}
                              ,{<<"pvt_modified_">>, Timestamp}
                              ,{<<"pvt_account_db">>, Db}
                              ,{<<"pvt_account_id">>, wh_account:format_id(Db, 'raw')}
                              ,{<<"pvt_vsn">>, 2}
                             ]),
    update_transactions(JObj, Db);
roll_up_credit(Diff, Db) when Diff < 0 ->
    ?LOG_CYAN("  debit ~p units~n", [Diff]),
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    JObj = wh_json:from_list([{<<"description">>, <<"roll up">>}
                              ,{<<"pvt_reason">>, <<"admin_discretion">>}
                              ,{<<"pvt_code">>, 3005}
                              ,{<<"pvt_amount">>, abs(Diff)}
                              ,{<<"pvt_type">>, <<"debit">>}
                              ,{<<"pvt_created">>, Timestamp}
                              ,{<<"pvt_modified_">>, Timestamp}
                              ,{<<"pvt_account_db">>, Db}
                              ,{<<"pvt_account_id">>, wh_account:format_id(Db, 'raw')}
                              ,{<<"pvt_vsn">>, 2}
                             ]),
    update_transactions(JObj, Db).

get_source_credit(Db) ->
    Source = source_request([Db
                             ,<<"_design/transactions/_view/credit_remaining?reduce=true&group=true">>
                            ]),
    get_credit(Source).

get_target_credit(Db) ->
    Target = target_request([Db
                             ,<<"_design/transactions/_view/credit_remaining?reduce=true&group=true">>
                            ]),
    get_credit(Target).

get_credit(URL) ->
    case httpc:request(URL) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            case wh_json:get_value(<<"rows">>, wh_json:decode(Body), []) of
                [] -> 0;
                [JObj] ->
                    wh_json:get_integer_value(<<"value">>, JObj, 0);
                _Else -> 'error'
            end;
        {'ok', {{_, Code, Reason}, _, Body}} ->
            ?LOG_RED("  failed to get credit ~s: ~p ~s ~s"
                     ,[URL, Code, Reason, Body]),
            'error';
        {'error', _R} ->
            ?LOG_RED("  failed to get credit ~s: ~p~n"
                     ,[URL, _R]),
            'error'
    end.

update_transactions(JObj, Db) ->
    Target = target_request([Db]),
    Request = {Target, [], "application/json", wh_json:encode(JObj)},
    HTTPOptions = [],
    ClientOptions = [{'body_format', 'binary'}],
    case httpc:request('post', Request, HTTPOptions, ClientOptions) of
        {'ok', {{_, 201, "Created"}, _, _}} -> 'true';
        {'ok', {{_, 200, "OK"}, _, _}} -> 'true';
        {'ok', {{_, Code, Reason}, _, Body}} ->
            ?LOG_RED("  failed to update transactions ~s: ~p ~s ~s"
                     ,[Target, Code, Reason, Body]),
            'error';
        {'error', _R} ->
            ?LOG_RED("  failed to update transactions ~s: ~p~n"
                     ,[Target, _R]),
            'error'
    end.

target_request(Path) -> ?TARGET_PATH(Path).

source_request(Path) -> ?SOURCE_PATH(Path).

uri_encode(Binary) when is_binary(Binary) ->
    wh_types:to_binary(http_uri:encode(wh_types:to_list(Binary)));
uri_encode(String) when is_list(String) ->
    http_uri:encode(String);
uri_encode(Atom) when is_atom(Atom) ->
    wh_types:to_atom(http_uri:encode(wh_types:to_list(Atom)), 'true').

uri_decode(Binary) when is_binary(Binary) ->
    wh_types:to_binary(http_uri:decode(wh_types:to_list(Binary)));
uri_decode(String) when is_list(String) ->
    http_uri:decode(String);
uri_decode(Atom) when is_atom(Atom) ->
    wh_types:to_atom(http_uri:decode(wh_types:to_list(Atom)), 'true').

-spec maybe_fix_doc(wh_json:object(), api_binary()) -> wh_json:object().
maybe_fix_doc(JObj, <<"account">>) ->
    wh_account:cleanup_dead_accounts(JObj);
maybe_fix_doc(JObj, <<"cdr">>) ->
    maybe_fix_cdr(JObj);
maybe_fix_doc(JObj, _PvtType) ->
    JObj.

maybe_fix_cdr(JObj) ->
    Fs = [fun maybe_fix_cdr_date/1],
    lists:foldl(fun(F, Acc) -> F(Acc) end, JObj, Fs).

maybe_fix_cdr_date(JObj) ->
    case wh_json:get_value(<<"_id">>, JObj) of
        <<Y:4/binary, M:2/binary, "-", CallId/binary>> ->
            Year = binary_to_list(list_to_integer(Y)),
            Month = binary_to_list(list_to_integer(M)),

            maybe_fix_cdr_date(JObj, Year, Month, CallId);
        _Id ->
            JObj
    end.

maybe_fix_cdr_date(JObj, Year, Month, CallId) ->
    Created = wh_json:get_integer(<<"pvt_created">>, JObj),
    case calendar:gregorian_seconds_to_datetime(Created) of
        {{Year, Month, _}, _} -> JObj;
        {{RealYear, RealMonth, _}, _} ->
            NewId = <<(integer_to_list(list_to_binary(RealYear)))/binary
                      ,(integer_to_list(list_to_binary(RealMonth)))/binary
                      ,"-", CallId/binary
                    >>,
            ?LOG_WHITE(" fixing cdr ID from ~s to ~s", [wh_json:get_value(<<"_id">>, JObj)
                                                        ,NewId
                                                       ]
                      ),
            wh_json:set_value(<<"_id">>, NewId, JObj)
    end.
