-module(hunt_account_id).

-include("clone_tools.hrl").

-export([run/1]).

run([Account]) -> run(Account);
run(Account) when not is_binary(Account) ->
    run(wh_types:to_binary(Account));
run(Account) ->
    inets:start(),
    process_flag('trap_exit', 'true'),
    AccountId = wh_account:format_id(Account, 'raw'),
    ViewOptions = <<"startkey=", (wh_json:encode([AccountId]))/binary
                    ,"&endkey=", (wh_json:encode([AccountId, wh_json:new()]))/binary>>,
    Target = target_request([<<"accounts">>
                             ,<<"_design">>
                             ,<<"accounts">>
                             ,<<"_view">>
                             ,<<"listing_by_descendants?"
                             ,ViewOptions/binary>>
                            ]),
    case httpc:request(Target) of
        {'ok', {{_, 200, "OK"}, _, Body}} -> 
            update_accounts([wh_json:get_value([<<"value">>, <<"id">>], JObj)
                             || JObj <- wh_json:get_value(<<"rows">>, wh_json:decode(Body))
                            ], AccountId);
        _Else -> ?LOG("unable to get account descendants (~s): ~p~n", [Target, _Else])
    end,
    ?WHITE.
%%    halt().

update_accounts([], _) -> 'ok';
update_accounts([AccountId|AccountIds], ParentId) -> 
    ?LOG_WHITE("updating no_match callflow for ~s~n", [AccountId]),
    _ = update_account(AccountId, ParentId),
    update_accounts(AccountIds, ParentId).

update_account(AccountId, ParentId) ->
    Db = wh_account:format_id(AccountId, 'encoded'),
    case no_match_callflow(Db) of
        'undefined' -> 'ok';
        CallflowId ->
            update_no_match_callflow(Db, CallflowId, ParentId)
    end.

update_no_match_callflow(Db, CallflowId, ParentId) -> 
    case get_callflow(Db, CallflowId) of
        'undefined' -> 'ok';
        JObj ->
            J = wh_json:set_value([<<"flow">>
                                   ,<<"data">>
                                   ,<<"hunt_account_id">>
                                  ], ParentId, JObj),
            put_doc(J, Db)
    end.

no_match_callflow(Db) ->    
    Target = target_request([Db
                             ,<<"_design">>
                             ,<<"callflow">>
                             ,<<"_view">>
                             ,<<"listing_by_number">>
                            ]),
    case httpc:request(Target) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            find_no_match(wh_json:get_value(<<"rows">>, wh_json:decode(Body)));
        {'ok', {{_, Code, Reason}, _, Body}} ->
            ?LOG_RED("  failed to find no_match callflow ~s: ~p ~s ~s~n"
                     ,[Target, Code, Reason, Body]),
            'undefined';
        {'error', _R} ->
            ?LOG_RED("  failed to find no match callflow ~s: ~p~n"
                     ,[Target, _R]),
            'undefined'
    end.

find_no_match([]) ->
    ?LOG_RED("  failed to find no match callflow in callflows~n", []),
    'undefined';
find_no_match([JObj|JObjs]) ->
    case wh_json:get_value(<<"key">>, JObj) =:= <<"no_match">> of
        'true' -> wh_json:get_value(<<"id">>, JObj);
        'false' -> find_no_match(JObjs)
    end.

get_callflow(Db, CallflowId) ->
    Target = target_request([Db
                             ,CallflowId
                            ]),
    case httpc:request(Target) of
        {'ok', {{_, 200, "OK"}, _, Body}} -> wh_json:decode(Body);
        {'ok', {{_, Code, Reason}, _, Body}} ->
            ?LOG_RED("  failed to get no_match callflow ~s: ~p ~s ~s~n"
                     ,[Target, Code, Reason, Body]),
            'undefined';
        {'error', _R} ->
            ?LOG_RED("  failed to get no match callflow ~s: ~p~n"
                     ,[Target, _R]),
            'undefined'
    end.

put_doc(JObj, Db) ->    
    Id = wh_json:get_value(<<"_id">>, JObj),
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
        {'ok', {{_, 202, "Accepted"}, _, _}} ->
            ?LOG_WHITE("  updated no_match callflow~n", []);
        {'ok', {{_, 201, "Created"}, _, _}} -> 
            ?LOG_WHITE("  updated no_match callflow~n", []);
        {'ok', {{_, 200, "OK"}, _, _}} -> 
            ?LOG_WHITE("  updated no_match callflow~n", []);
        {'ok', {{_, Code, Reason}, _, Body}} ->
            ?LOG_RED("  failed to store no_match callflow ~s: ~p ~s ~s"
                                ,[Target, Code, Reason, Body]);
        {'error', _R} ->
            ?LOG_RED("  failed to store ~s: ~p~n"
                     ,[Target, _R])
    end.

target_request(Path) ->
    %% Path = [<<"test-", Db/binary>>] ++ Rest,
    wh_types:to_list(<<?TARGET, (wh_binary:join(Path, <<"/">>))/binary>>).
