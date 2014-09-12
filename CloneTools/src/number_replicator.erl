-module(number_replicator).

-include("clone_tools.hrl").

-export([run/0
         ,run/1
        ]).
-export([single/1]).

run() ->
    inets:start(),
    AccountIds = get_account_ids(),
    import_accounts_phone_numbers(AccountIds).

run(AccountId) ->
    inets:start(),
    import_accounts_phone_numbers([AccountId]).

single(PhoneNumber) ->
    inets:start(),
    maybe_import_phone_numbers([PhoneNumber]).

import_accounts_phone_numbers([]) -> 'ok';
import_accounts_phone_numbers([AccountId|AccountIds]) -> 
    ?LOG("importing ~s, ~p remaining~n", [AccountId, length(AccountIds)]),
    _ = import_account_phone_numbers(AccountId),
    import_accounts_phone_numbers(AccountIds).

import_account_phone_numbers(AccountId) ->
    AccountDb = wh_account:format_id(AccountId, 'encoded'),
    case httpc:request(source_request([AccountDb
                                       ,<<"phone_numbers">>
                                      ]))
    of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            JObj = wh_json:decode(Body),
            PhoneNumbers = get_phone_numbers(wh_json:get_keys(JObj)),
            ?LOG("  found ~p phone numbers~n", [length(PhoneNumbers)]),
            maybe_import_phone_numbers(PhoneNumbers);
        _Else -> ?LOG("unable to get phone_numbers for ~s:~n ~p~n", [AccountDb, _Else])
    end.
    
maybe_import_phone_numbers([]) -> 'ok';
maybe_import_phone_numbers([PhoneNumber|PhoneNumbers]) ->
    _ = maybe_import_phone_number(PhoneNumber),
    maybe_import_phone_numbers(PhoneNumbers).

maybe_import_phone_number(PhoneNumber) ->
    NumberDb = number_to_db_name(PhoneNumber),
    case does_phone_exist(NumberDb, PhoneNumber) of
        'true' -> ?LOG("  phone number ~s already exists on target~n", [PhoneNumber]);
        'false' -> import_phone_number(NumberDb, PhoneNumber)
    end.

import_phone_number(NumberDb, PhoneNumber) ->
    Source = source_request([NumberDb
                             ,uri_encode(PhoneNumber)
                            ]),
    case httpc:request(Source) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            JObj = wh_json:delete_keys([<<"_rev">>
                                        ,<<"_attachments">>
                                       ], wh_json:decode(Body)),
            save_phone_number(NumberDb, PhoneNumber, JObj);
        _Else -> ?LOG("failed to fetch ~s:~n ~p~n", [PhoneNumber, _Else])
    end.    

save_phone_number(NumberDb, PhoneNumber, JObj) ->
    ?LOG("  importing phone number ~s~n", [PhoneNumber]),
    Target = target_request([NumberDb
                             ,uri_encode(PhoneNumber)
                            ]),
    Request = {Target, [], "application/json", wh_json:encode(JObj)},
    HTTPOptions = [],
    ClientOptions = [{'body_format', 'binary'}],
    case httpc:request('put', Request, HTTPOptions, ClientOptions) of
        {'ok', {{_, 404, "Object Not Found"}, _,  _}} ->
            case create_phone_number_db(NumberDb) of
                'true' -> save_phone_number(NumberDb, PhoneNumber, JObj);
                'false' -> 'ok'
            end;
        {'ok', {{_, 201, "Created"}, _, _}} -> 'ok';
        {'ok', {{_, 200, "OK"}, _, _}} -> 'ok';
        _Else -> ?LOG("failed to save phone number ~s:~n ~p~n", [PhoneNumber, _Else])
    end.

create_phone_number_db(NumberDb) ->
    Target = target_request([NumberDb]),
    Request = {Target, [], "application/json", <<>>},
    HTTPOptions = [],
    ClientOptions = [{'body_format', 'binary'}],
    case httpc:request('put', Request, HTTPOptions, ClientOptions) of
        {'ok', {{_, 201, "Created"}, _, _}} -> 'true';
        {'error', 'socket_closed_remotely'} -> 'true';
        _Else -> 
            ?LOG("failed to create number db ~s:~n ~p~n", [NumberDb, _Else]),
            'false'
    end.

does_phone_exist(NumberDb, PhoneNumber) ->
    URL = target_request([NumberDb
                          ,uri_encode(PhoneNumber)
                         ]),
    case httpc:request('head', {URL, []}, [], []) of
        {'ok', {{_, 200, "OK"}, _, _}} -> 'true';
        _Else -> 'false'
    end.
            
get_phone_numbers(Keys) ->
    get_phone_numbers(Keys, []).

get_phone_numbers([], Numbers) -> Numbers;
get_phone_numbers([<<"_", _/binary>>|Keys], Numbers) ->
    get_phone_numbers(Keys, Numbers);
get_phone_numbers([<<"pvt_", _/binary>>|Keys], Numbers) -> 
    get_phone_numbers(Keys, Numbers);
get_phone_numbers([Key|Keys], Numbers) ->
    get_phone_numbers(Keys, [Key|Numbers]).

get_account_ids() ->
    Path = "/opt/clone_tools/priv/account_ids.txt",
    case file:open(Path, ['read', 'read_ahead', 'binary']) of
        {'ok', FD} -> get_account_ids(FD);
        {'error', _R} ->
            ?LOG("unable to open ~s: ~p~n", [Path, _R]),
            []
    end.

get_account_ids(FD) ->
    get_account_ids(FD, []).

get_account_ids(FD, AccountIds) ->
    case file:read_line(FD) of
        'eof' -> AccountIds;
        {'error', _R} ->
            ?LOG("unable to read line: ~p~n", [_R]),
            AccountIds;
        {'ok', AccountId} ->
            get_account_ids(FD, [wh_binary:strip_right(AccountId, $\n)|AccountIds])
    end.

target_request(Path) ->
    wh_types:to_list(<<?TARGET, (wh_binary:join(Path, <<"/">>))/binary>>).
        
source_request(Path) ->
    wh_types:to_list(<<?SOURCE, (wh_binary:join(Path, <<"/">>))/binary>>).

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
