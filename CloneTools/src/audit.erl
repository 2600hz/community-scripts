-module(audit).

-include("clone_tools.hrl").

-export([run/0]).
-export([single/1]).

run() ->
    inets:start(),
    case httpc:request(target_request([<<"_all_dbs">>])) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            AccountDbs = [wh_account:format_id(Db, 'encoded')
                          || Db <- wh_json:decode(Body)
                                 ,wh_account:is_account_db(Db)
                         ],
            AccountIds = [wh_account:format_id(AccountDb, 'raw')
                          || AccountDb <- AccountDbs
                         ],
            audit_accounts(AccountDbs, AccountIds);
        _Else ->
            ?LOG("unable to get dbs: ~p~n", [_Else])
    end.

single(AccountDb) ->
    inets:start(),  
    audit_account(AccountDb, 'undefined').

audit_accounts([], _) -> 'ok';
audit_accounts([AccountDb|AccountDbs], AccountIds) ->
    ?LOG("audit account ~s, ~p remaining~n", [AccountDb, length(AccountDbs)]),
    _ = audit_account(AccountDb, AccountIds),
    audit_accounts(AccountDbs, AccountIds).

audit_account(AccountDb, AccountIds) ->
    put('errors', []),
    AccountId = wh_account:format_id(AccountDb, 'raw'),
    Account = check_account_definition(AccountDb, AccountId),
    _ = check_accounts_definition(AccountId),
    PhoneNumbers = get_all_phone_numbers(AccountDb),
    _ = check_phone_numbers(AccountDb, PhoneNumbers),
    ?LOG("  checking number assignments~n  ", []),
    _ = check_number_assignments(AccountId, PhoneNumbers),
    ?LOG("~n", []),
    _ = [?LOG("~s", [Error]) || Error <- get('errors')],
    check_pvt_tree(Account, AccountIds).

check_account_definition(AccountDb, AccountId) ->
    URL = target_request([AccountDb
                          ,AccountId
                         ]),
    case httpc:request(URL) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            ?LOG("  found account definition~n", []),
            wh_json:decode(Body);
        _Else ->
            ?LOG("  unable to get account definition~n", []),
            wh_json:new()
    end.

check_accounts_definition(AccountId) ->
    URL = target_request([<<"accounts">>
                          ,AccountId
                         ]),
    case httpc:request('head', {URL, []}, [], []) of
        {'ok', {{_, 200, "OK"}, _, _}} -> 
            ?LOG("  found aggregate account definition~n", []),
            'true';
        _Else ->
            ?LOG("  aggregate account definition is missing~n", [])
    end.

check_phone_numbers(AccountDb, PhoneNumbers) ->
    URL = target_request([AccountDb
                          ,<<"phone_numbers">>
                         ]),
    case httpc:request(URL) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            JObj = wh_json:decode(Body),
            test_phone_number_doc(PhoneNumbers, JObj);
        _Else ->
            ?LOG("  unable to get phone_numbers~n", [])
    end.

test_phone_number_doc([], _) -> 'ok';
test_phone_number_doc([PhoneNumber|PhoneNumbers], JObj) ->
    case wh_json:get_value(PhoneNumber, JObj) of
        'undefined' ->
            ?LOG("  phone_numbers is missing ~s~n", [PhoneNumber]),
            test_phone_number_doc(PhoneNumbers, JObj);
        _Else ->
            test_phone_number_doc(PhoneNumbers, JObj)
    end.     

check_number_assignments(_, []) -> 'ok';
check_number_assignments(AccountId, [PhoneNumber|PhoneNumbers]) ->
    NumberDb = number_to_db_name(PhoneNumber),
    URL = target_request([NumberDb
                          ,uri_encode(PhoneNumber)
                         ]),
    case httpc:request(URL) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            JObj = wh_json:decode(Body),
            _ = test_number_assignment(AccountId, PhoneNumber, JObj),
            _ = test_number_state(PhoneNumber, JObj),
            check_number_assignments(AccountId, PhoneNumbers);
        _Else ->
            ?LOG("  unable to get phone number ~s~n", [PhoneNumber])
    end.

test_number_assignment(AccountId, PhoneNumber, JObj) ->
    case wh_json:get_value(<<"pvt_assigned_to">>, JObj) of
        AccountId -> ?LOG(".", []);
        _Else ->
            put('errors'
                ,[io_lib:format("  phone number ~s is assigned to ~s~n"
                                ,[PhoneNumber, _Else])
                  | get('errors')
                 ]),
            ?LOG("!", [])
    end.

test_number_state(PhoneNumber, JObj) ->
    case wh_json:get_value(<<"pvt_number_state">>, JObj) of
        <<"in_service">> -> ?LOG(".", []);
        <<"port_in">> -> ?LOG(".", []);
        _Else ->
            put('errors'
                ,[io_lib:format("  phone number ~s is in state ~s~n"
                                ,[PhoneNumber, _Else])
                  | get('errors')
                 ]),
            ?LOG("!", [])
    end.    

check_pvt_tree([], _) -> 'ok';
check_pvt_tree(_, 'undefined') -> 'ok';
check_pvt_tree([ParentId|ParentIds], AccountIds) ->
    case lists:member(ParentId, AccountIds) of
        'true' -> check_pvt_tree(ParentIds, AccountIds);
        'false' ->
            ?LOG("  parent account ~s does not exist~n", [ParentId]),
            check_pvt_tree(ParentIds, AccountIds)
    end;
check_pvt_tree(Account, AccountIds) ->
    check_pvt_tree(wh_json:get_value(<<"pvt_tree">>, Account, []), AccountIds).

get_all_phone_numbers(AccountDb) ->
    get_all_trunkstore_phone_numbers(AccountDb)
        ++ get_all_callflow_phone_numbers(AccountDb).

get_all_trunkstore_phone_numbers(AccountDb) ->
    URL = target_request([AccountDb
                          ,<<"_design">>
                          ,<<"trunkstore">>
                          ,<<"_view">>
                          ,<<"LookUpDID">>
                         ]),
    case httpc:request(URL) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            JObjs = wh_json:get_value(<<"rows">>, wh_json:decode(Body)),
            [wh_json:get_value(<<"key">>, JObj)
             || JObj <- JObjs
            ];
        _Else ->
            ?LOG("  unable to lookup trunkstore numbers~n", []),
            []
    end.

get_all_callflow_phone_numbers(AccountDb) ->
    URL = target_request([AccountDb
                          ,<<"_design">>
                          ,<<"callflow">>
                          ,<<"_view">>
                          ,<<"listing_by_number">>
                         ]),
    case httpc:request(URL) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            JObjs = wh_json:get_value(<<"rows">>, wh_json:decode(Body)),
            [Number
             || JObj <- JObjs
                    ,re:run((Number = wh_json:get_value(<<"key">>, JObj))
                            ,<<"^\\+?[0-9]{7,}$">>) =/= 'nomatch'
            ];
        _Else ->
            ?LOG("  unable to lookup callflow numbers~n", []),
            []
    end.

target_request(Path) ->
    wh_types:to_list(<<?TARGET, (wh_binary:join(Path, <<"/">>))/binary>>).

number_to_db_name(<<NumPrefix:5/binary, _/binary>>) ->
    wh_types:to_binary(
      http_uri:encode(
        wh_types:to_list(
          list_to_binary([?WNM_DB_PREFIX, NumPrefix])
         )
       )
     );
number_to_db_name(_) ->
    'undefined'.

uri_encode(Binary) ->
    wh_types:to_binary(http_uri:encode(wh_types:to_list(Binary))).

