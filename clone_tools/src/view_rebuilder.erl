-module(view_rebuilder).

-include("clone_tools.hrl").

-export([run/0]).
-export([stream_receiver/3]).

run() ->
    inets:start(),
    process_flag('trap_exit', 'true'),
    case httpc:request(target_request([<<"_all_dbs">>])) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            AccountDbs = [wh_account:format_id(Db, 'encoded')
                          || Db <- wh_json:decode(Body)
                                 ,wh_account:is_account_db(Db)
                         ],
            rebuild_accounts_views(AccountDbs);
        _Else -> ?LOG("unable to get dbs: ~p~n", [_Else])
    end.


rebuild_accounts_views([]) -> 'ok';
rebuild_accounts_views([AccountDb|AccountDbs]) ->
    ?LOG("rebuilding ~s, ~p remaining~n", [AccountDb, length(AccountDbs)]),
    _ = request_views(AccountDb),
    rebuild_accounts_views(AccountDbs).

request_views(AccountDb) ->
    URL = target_request([AccountDb
                          ,<<"_all_docs?startkey=\"_design/\"&endkey=\"_design0\"&include_docs=true">>
                         ]),
    case httpc:request(URL) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
            Views = [{wh_json:get_value(<<"id">>, JObj)
                      ,wh_json:get_keys([<<"doc">>, <<"views">>], JObj)
                     }
                     || JObj <- wh_json:get_value(<<"rows">>, wh_json:decode(Body))
                    ],
            request_views(Views, AccountDb);
        _Else -> ?LOG("  unable to get design docs: ~p~n", [_Else])
    end.

request_views([], _) -> 'ok';
request_views([{Design, [View|_]}|Views], AccountDb) ->
    timer:sleep(500),
    URL = target_request([AccountDb
                          ,Design
                          ,"_view"
                          ,<<View/binary, "?stale=update_after">>
                         ]),
    ?LOG("  requesting ~s~n", [URL]),
    case httpc:request(URL) of
        {'ok', {{_, 200, "OK"}, _, _}} ->
            ?LOG("  waiting for updater", []),
            _ = wait_for_view_update(Design, AccountDb),
            ?LOG("\n", []),
            request_views(Views, AccountDb);
        _Else -> 
            ?LOG("  unable to query design: ~p~n", [_Else]),
            request_views(Views, AccountDb)
    end.

wait_for_view_update(Design, AccountDb) ->
    ?LOG(" . ", []),
    URL = target_request([AccountDb
                          ,Design
                          ,"_info"
                         ]),            
    case httpc:request(URL) of
        {'ok', {{_, 200, "OK"}, _, Body}} ->
             case wh_json:is_true(<<"updater_running">>, wh_json:decode(Body)) of
                 'false' -> 'ok';
                 'true' ->
                     timer:sleep(500),
                     wait_for_view_update(Design, AccountDb)
             end;
        _Else ->
            ?LOG("  unable to query design info: ~p~n", [_Else]),                    
            timer:sleep(500),
            wait_for_view_update(Design, AccountDb)
    end.

target_request(Path) ->
    wh_types:to_list(<<?TARGET, (wh_binary:join(Path, <<"/">>))/binary>>).

stream_receiver({_, {'error', _R}}, Views, AccountDb) ->
    io:format("  unable to get view: ~p~n", [_R]),
    request_views(Views, AccountDb);
stream_receiver({_, _}, Views, AccountDb) ->
    io:format("moving on~n", []),
    request_views(Views, AccountDb);
stream_receiver(_, _, _) -> 'ok'.
     
