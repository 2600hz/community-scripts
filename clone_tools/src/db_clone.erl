-module(db_clone).

%% {'ok', Dbs} = couch_mgr:db_info(), [begin case Db of <<"test-", _/binary>> -> couch_mgr:db_delete(wh_util:uri_encode(Db)); _ -> ok end end || Db <- Dbs].
-include("clone_tools.hrl").

-export([run/0
         ,run/1
        ]).

-define(VIEWS, [{<<"filtered_ids">>, wh_json:from_list([{<<"map">>, <<"function(doc) { if (doc.pvt_deleted || (doc.pvt_type == 'cdr' && doc.pvt_created < 63547804800) || doc.pvt_type == 'vmbox' || doc.pvt_type == 'debit' || doc.pvt_type == 'credit' || doc.pvt_type == 'acdc_stat') return; emit(doc._id, null); }">>}])}
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

run() ->
    inets:start(),
    process_flag('trap_exit', 'true'),
    case httpc:request(source_request([<<"_all_dbs">>])) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            run(wh_json:decode(Body));
        _Else -> ?LOG("unable to get dbs: ~p~n", [_Else])
    end,
    ?WHITE,
    halt().

run([]) -> 'ok';    
run([Db|Dbs]) -> 
    ?LOG_WHITE("cloning ~s, ~p databases remaining~n"
               ,[Db, length(Dbs)]),
    case  wh_account:is_account_db(Db) of
        'true' -> clone_account_db(wh_account:format_id(Db, 'encoded'));
        'false' -> clone_db(wh_types:to_binary(uri_encode(Db)))
    end.

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
            ?LOG_CYAN("  found ~p missing documents~n  "
                      ,[length(Ids)]),
            clone_docs(Ids, Db, 0)
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
            ?LOG_CYAN("  found ~p missing design documents~n  "
                      ,[length(Ids)]),
            clone_docs(Ids, Db, 0)
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
            ?LOG_CYAN("  found ~p missing filtered documents~n  "
                      ,[length(Ids)]),
            clone_docs(Ids, Db, 0)
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
            ?LOG_CYAN("  found ~p missing attachments~n  "
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
        Ids -> ?LOG_CYAN("  found ~p voicemail boxes~n  "
                         ,[length(Ids)]),
               clone_docs(Ids, Db, 0)
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
    
clone_docs([], _, _) ->
    ?LOG("~n", []);
clone_docs(Ids, Db, Count) when Count > 100 ->
    ?LOG("~n  ", []),
    clone_docs(Ids, Db, 0);
clone_docs([Id|Ids], Db, Count) ->
    _ = case get_doc(uri_encode(Id), Db) of
            {'ok', JObj} ->
                case should_clone(JObj) of
                    'false' -> ?LOG("*", []);
                    'true' -> put_doc(JObj, Db)
                end;
            _Else -> 'ok'
        end,
    clone_docs(Ids, Db, Count + 1).

should_clone(JObj) ->
    (not wh_json:is_true(<<"pvt_deleted">>, JObj)).

get_doc(Id, Db) ->
    Source = source_request([Db
                             ,Id
                            ]),
    case httpc:request(Source) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            JObj = wh_json:decode(Body),
            {'ok', wh_json:delete_keys(?REMOVE_KEYS, JObj)};
        {'ok', {{_, Code, Reason}, _, Body}} ->
            put('errors'
                ,[io_lib:format("  failed to get ~s: ~p ~s ~s"
                                ,[Source, Code, Reason, Body])
                  | get('errors')
                 ]),
            ?RED,
            ?LOG("!", []);
        {'error', _R} ->
            put('errors'
                ,[io_lib:format("  failed to get ~s: ~p~n"
                                ,[Source, _R])
                  | get('errors')
                 ]),
            ?RED,
            ?LOG("!", [])
    end.

put_doc(JObj, Db) ->
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
        {'ok', {{_, 201, "Created"}, _, _}} -> 
            ?WHITE,
            ?LOG(".", []);
        {'ok', {{_, 200, "OK"}, _, _}} -> 
            ?WHITE,
            ?LOG(".", []);
        {'ok', {{_, 409, "Conflict"}, _, _}} ->
            case get_rev(Id, Db) of
                <<>> -> 
                    ?RED,
                    ?LOG("!", []);
                Rev ->
                    ?WHITE,
                    ?LOG("+", []),
                    put_doc(wh_json:set_value(<<"_rev">>, Rev, JObj), Db)
            end;
        {'ok', {{_, Code, Reason}, _, Body}} ->
            put('errors'
                ,[io_lib:format("  failed to store ~s: ~p ~s ~s"
                                ,[Target, Code, Reason, Body])
                  | get('errors')
                 ]),
            ?RED,
            ?LOG("!", []);
        {'error', _R} ->
            put('errors'
                ,[io_lib:format("  failed to store ~s: ~p~n"
                                ,[Target, _R])
                  | get('errors')
                 ]),
            ?RED,
            ?LOG("!", [])
    end.

maybe_copy_attachments([], _) -> 
    ?LOG("~n", []);
maybe_copy_attachments([Id|Ids], Db) -> 
    Source = source_request([Db
                             ,Id
                            ]),
    Target = target_request([Db
                             ,Id
                            ]),
    _ = copy_missing_attachments(get_attachments(Source)
                                 ,get_attachments(Target)
                                 ,Id
                                 ,Db),
    maybe_copy_attachments(Ids, Db).

copy_missing_attachments('error', _, _, _) -> 'ok';
copy_missing_attachments(_, 'error', _, _) -> 'ok';
copy_missing_attachments(Source, Target, Id, Db) ->
    copy_missing_attachments(wh_json:get_keys(Source), Source, Target, Id, Db).

copy_missing_attachments([], _, _, _, _) -> 'ok';
copy_missing_attachments([Name|Names], Source, Target, Id, Db) -> 
    case wh_json:get_value(Name, Target) =:= 'undefined'
        orelse (wh_json:get_value([Name, <<"content_type">>], Source) 
                =/= wh_json:get_value([Name, <<"content_type">>], Target))
        orelse (wh_json:get_value([Name, <<"length">>], Source) 
                =/= wh_json:get_value([Name, <<"length">>], Target))
    of
        'false' -> copy_missing_attachments(Names, Source, Target, Id, Db);
        'true' ->
            _ = case get_attachment(Name, Id, Db) of
                    {Attachment, ContentType} ->
                        put_attachment(Attachment, ContentType, Name, Id, Db);
                    _Else -> 'ok'
                end,
            copy_missing_attachments(Names, Source, Target, Id, Db)
    end.

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
            put('errors'
                ,[io_lib:format("  failed to get attachment ~s: ~p ~s ~s"
                                ,[Source, Code, Reason, Body])
                  | get('errors')
                 ]),
            ?LOG_RED("!", []);
        {'error', _R} ->
            put('errors'
                ,[io_lib:format("  failed to get attachment ~s: ~p~n"
                                ,[Source, _R])
                  | get('errors')
                 ]),
            ?LOG_RED("!", [])
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
        {'ok', {{_, 201, "Created"}, _, _}} -> 
            ?LOG_WHITE(".", []);
        {'ok', {{_, 200, "OK"}, _, _}} -> 
            ?LOG_WHITE(".", []);
        {'ok', {{_, 409, "Conflict"}, _, _}} -> 
            ?LOG_WHITE("+", []);
        {'ok', {{_, Code, Reason}, _, Body}} ->
            put('errors'
                ,[io_lib:format("  failed to put attachment ~s: ~p ~s ~s"
                                ,[Target, Code, Reason, Body])
                  | get('errors')
                 ]),
            ?LOG_RED("!", []);
        {'error', _R} ->
            put('errors'
                ,[io_lib:format("  failed to put attachment ~s: ~p~n"
                                ,[Target, _R])
                  | get('errors')
                 ]),
            ?LOG_RED("!", [])
    end.

get_rev(Id, Db) ->
    Target = target_request([Db
                             ,Id
                            ]),
    case httpc:request(Target) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            JObj = wh_json:decode(Body),
            wh_json:get_value(<<"_rev">>, JObj);
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
%%    Path = [<<"test-", Db/binary>>] ++ Rest,
    wh_types:to_list(<<?TARGET, (wh_binary:join(Path, <<"/">>))/binary>>).
        
source_request(Path) ->
    wh_types:to_list(<<?SOURCE, (wh_binary:join(Path, <<"/">>))/binary>>).

uri_encode(Binary) when is_binary(Binary) ->
    wh_types:to_binary(http_uri:encode(wh_types:to_list(Binary)));
uri_encode(String) when is_list(String) ->
    http_uri:encode(String);
uri_encode(Atom) when is_atom(Atom) ->
    wh_types:to_atom(http_uri:encode(wh_types:to_list(Atom)), 'true').

