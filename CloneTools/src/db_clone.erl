-module(db_clone).

%% {'ok', Dbs} = couch_mgr:db_info(), [begin case Db of <<"test-", _/binary>> -> couch_mgr:db_delete(wh_util:uri_encode(Db)); _ -> ok end end || Db <- Dbs].
-include("clone_tools.hrl").
-include("wh_types.hrl").

-export([run/0
         ,run/1
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

run() ->
    inets:start(),
    process_flag('trap_exit', 'true'),
    case httpc:request(source_request([<<"_all_dbs">>])) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            clone_dbs(wh_json:decode(Body));
        _Else -> ?LOG("unable to get dbs: ~p~n", [_Else])
    end,
    ?WHITE,
    halt().

run([_|_]=Dbs) ->
    inets:start(),
    _ = clone_dbs(Dbs),
    ?WHITE,
    halt().

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
            _ = clone_all_docs(Db),
            %% _ = clone_attachments(Db),
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
            _ = roll_up_credit(Db),
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
    Source = source_request([Db
                             ,<<"_all_docs">>
                            ]),
    Target = target_request([Db
                             ,<<"_all_docs">>
                            ]),
    SourceIds = sets:from_list(get_ids(Source)),
    TargetIds = sets:from_list(get_ids(Target)),
    sets:to_list(sets:subtract(SourceIds, TargetIds)).

clone_design_docs(Db) ->
    _ = put_clone_view(Db),
    case find_missing_design_ids(Db) of
        [] -> ?LOG_GREEN("  design documents are in sync~n", []);
        Ids ->
            ?LOG_CYAN("  found ~p missing design documents~n"
                      ,[length(Ids)]),
            clone_docs(Ids, Db, 'true')
    end.

find_missing_design_ids(Db) ->
    Source = source_request([Db
                             ,<<"_all_docs?startkey=\"_design/\"&endkey=\"_design0\"">>
                            ]),
    Target = target_request([Db
                             ,<<"_all_docs?startkey=\"_design/\"&endkey=\"_design0\"">>
                            ]),
    SourceIds = sets:from_list(get_ids(Source)),
    TargetIds = sets:from_list(get_ids(Target)),
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
    Source = source_request([Db
                             ,<<"_design/clone/_view/filtered_ids">>
                            ]),
    Target = target_request([Db
                             ,<<"_design/clone/_view/filtered_ids">>
                            ]),
    SourceIds = sets:from_list(get_ids(Source)),
    TargetIds = sets:from_list(get_ids(Target)),
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
    Source = source_request([Db
                             ,<<"_design/clone/_view/has_attachments?reduce=true&group=true">>
                            ]),
    Target = target_request([Db
                             ,<<"_design/clone/_view/has_attachments?reduce=true&group=true">>
                            ]),
    TargetIds = get_ids(Target),
    lists:foldl(fun({Id, Length}, Ids) ->
                        case props:get_value(Id, TargetIds) =/= Length of
                            'true' -> [Id|Ids];
                            'false' -> Ids
                        end
                end, [], get_ids(Source)).

clone_voicemail_boxes(Db) ->
    case find_missing_voicemail_boxes(Db) of
        [] -> ?LOG_GREEN("  no voicemail boxes found~n", []);
        Ids -> ?LOG_CYAN("  found ~p voicemail boxes~n"
                         ,[length(Ids)]),
               clone_docs(Ids, Db, 'false')
    end.

find_missing_voicemail_boxes(Db) ->
    Source = source_request([Db
                             ,<<"_design/clone/_view/vmbox_ids">>
                            ]),
    get_ids(Source).

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

put_doc(JObj0, Db) ->
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
    Source = source_request([Db
                             ,Id
                            ]),
    Target = target_request([Db
                             ,Id
                            ]),
   case maybe_start_copy_worker(get_attachments(Source)
                                ,get_attachments(Target)
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
    ?LOG_RED("!", []),
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

get_attachments(URL) ->
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
             io_lib:format("  failed to put attachment ~s: ~p ~s ~s"
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

create_db(Db) ->
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

get_ids(URL) ->
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

target_request(Path) ->
    %% Path = [<<"test-", Db/binary>>] ++ Rest,
    wh_types:to_list(<<?TARGET, (wh_binary:join(Path, <<"/">>))/binary>>).

source_request(Path) ->
    wh_types:to_list(<<?SOURCE, (wh_binary:join(Path, <<"/">>))/binary>>).

uri_encode(Binary) when is_binary(Binary) ->
    wh_types:to_binary(http_uri:encode(wh_types:to_list(Binary)));
uri_encode(String) when is_list(String) ->
    http_uri:encode(String);
uri_encode(Atom) when is_atom(Atom) ->
    wh_types:to_atom(http_uri:encode(wh_types:to_list(Atom)), 'true').

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
