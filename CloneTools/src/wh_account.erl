%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600Hz INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_account).

-export([format_id/1
         ,format_id/2
         ,format_id/3
        ]).
-export([is_in_hierarchy/2
         ,is_in_hierarchy/3
        ]).
-export([is_system_admin/1]).
-export([get_realm/1
         ,get_realm/2
        ]).
-export([is_enabled/1]).
-export([is_account_mod/1]).
-export([is_account_db/1]).

-export([cleanup_dead_accounts/1]).

-include("wh_types.hrl").
-include("clone_tools.hrl").

-type account_format() :: 'unencoded' | 'encoded' | 'raw'.
-spec format_id(ne_binaries() | ne_binary() | wh_json:object()) -> ne_binary().
format_id(Doc) -> format_id(Doc, 'unencoded').

-spec format_id(ne_binaries() | ne_binary() | wh_json:object(), account_format()) -> ne_binary().
format_id(DbName, Timestamp) when is_integer(Timestamp) andalso Timestamp > 0 ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    format_id(DbName, Year, Month);
format_id(<<"accounts">>, _) -> <<"accounts">>;
%% unencode the account db name
format_id(<<"account/", _/binary>> = DbName, 'unencoded') ->
    DbName;
format_id(<<"account%2F", _/binary>> = DbName, 'unencoded') ->
    binary:replace(DbName, <<"%2F">>, <<"/">>, ['global']);

%% encode the account db name
format_id(<<"account%2F", _/binary>>=DbName, 'encoded') ->
    DbName;
format_id(<<"account/", _/binary>>=DbName, 'encoded') ->
    binary:replace(DbName, <<"/">>, <<"%2F">>, ['global']);

%% get just the account ID from the account db name
format_id(<<"account%2F", AccountId/binary>>, 'raw') ->
    binary:replace(AccountId, <<"%2F">>, <<>>, ['global']);
format_id(<<"account/", AccountId/binary>>, 'raw') ->
    binary:replace(AccountId, <<"/">>, <<>>, ['global']);

format_id([AccountId], Encoding) when is_binary(AccountId) ->
    format_id(AccountId, Encoding);
format_id(Account, Encoding) when not is_binary(Account) ->
    case wh_json:is_json_object(Account) of
        'true' -> format_id([wh_json:get_value([<<"_id">>], Account)], Encoding);
        'false' -> format_id(wh_types:to_binary(Account), Encoding)
    end;

format_id(AccountId, 'unencoded') ->
    [Id1, Id2, Id3, Id4 | IdRest] = wh_types:to_list(AccountId),
    wh_types:to_binary(["account/", Id1, Id2, $/, Id3, Id4, $/, IdRest]);
format_id(AccountId, 'encoded') when is_binary(AccountId) ->
    [Id1, Id2, Id3, Id4 | IdRest] = wh_types:to_list(AccountId),
    wh_types:to_binary(["account%2F", Id1, Id2, "%2F", Id3, Id4, "%2F", IdRest]);
format_id(AccountId, 'raw') -> AccountId.

-spec format_id(ne_binaries(), pos_integer(), pos_integer()) -> ne_binary().
format_id(AccountId, Year, Month) when is_integer(Year), is_integer(Month) ->
    <<(format_id(AccountId, 'encoded'))/binary
      ,"-"
      ,(wh_types:to_binary(Year))/binary
      ,(wh_time:pad_month(Month))/binary>>.

-spec is_in_hierarchy(api_binary(), api_binary()) -> boolean().
is_in_hierarchy(CheckFor, InAccount) ->
    is_in_hierarchy(CheckFor, InAccount, 'false').

-spec is_in_hierarchy(api_binary(), api_binary(), boolean()) -> boolean().
is_in_hierarchy('undefined', _, _) -> 'false';
is_in_hierarchy(_, 'undefined', _) -> 'false';
is_in_hierarchy(CheckFor, InAccount, IncludeSelf) ->
    CheckId = format_id(CheckFor, 'raw'),
    AccountId = format_id(InAccount, 'raw'),
    AccountDb = format_id(InAccount, 'encoded'),
    case (IncludeSelf andalso AccountId =:= CheckId) orelse couch_mgr:open_cache_doc(AccountDb, AccountId) of
        'true' ->
            'true';
        {'ok', JObj} ->
            Tree = wh_json:get_value(<<"pvt_tree">>, JObj, []),
            case lists:member(CheckId, Tree) of
                'true' ->
                    'true';
                'false' ->
                    'false'
            end;
        {'error', _R} ->
            'false'
    end.

-spec is_system_admin(api_binary()) -> boolean().
is_system_admin('undefined') -> 'false';
is_system_admin(Account) ->
    AccountId = format_id(Account, 'raw'),
    AccountDb = format_id(Account, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} -> wh_json:is_true(<<"pvt_superduper_admin">>, JObj);
        {'error', _R} ->
            'false'
    end.

-spec is_enabled(api_binary()) -> 'true'. %boolean().
is_enabled('undefined') -> 'true';
is_enabled(_AccountId) ->
    %% See WHISTLE-1201
    'true'.
%%    case wh_cache:peek({?MODULE, is_enabled, AccountId}) of
%%        {ok, Enabled} ->
%%            lager:debug("account ~s enabled flag is ~s", [AccountId, Enabled]),
%%            Enabled;
%%        {error, not_found} ->
%%            case couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId) of
%%                {ok, JObj} ->
%%                    PvtEnabled = wh_json:is_false(<<"pvt_enabled">>, JObj) =/= 'true',
%%                    lager:debug("account ~s enabled flag is ~s", [AccountId, PvtEnabled]),
%%                    wh_cache:store({?MODULE, is_enabled, AccountId}, PvtEnabled, 300),
%%                    PvtEnabled;
%%                {error, R} ->
%%                    lager:debug("unable to find enabled status of account ~s: ~p", [AccountId, R]),
%%                    wh_cache:store({?MODULE, is_enabled, AccountId}, 'true', 300),
%%                    'true'
%%            end
%%    end.

-spec get_realm(api_binary()) -> api_binary().
get_realm(AccountId) ->
    get_realm(
      format_id(AccountId, 'encoded')
      ,format_id(AccountId, 'raw')
     ).

-spec get_realm(api_binary(), ne_binary()) -> api_binary().
get_realm('undefined', _) -> 'undefined';
get_realm(Db, AccountId) ->
    case couch_mgr:open_cache_doc(Db, AccountId) of
        {'ok', JObj} -> wh_json:get_ne_value(<<"realm">>, JObj);
        {'error', _R} -> 'undefined'
    end.

-spec is_account_mod(ne_binary()) -> boolean().
is_account_mod(<<"account/", _AccountId:34/binary, "-", _Date:6/binary>>) -> 'true';
is_account_mod(<<"account%2F", _AccountId:38/binary, "-", _Date:6/binary>>) -> 'true';
is_account_mod(_) -> 'false'.

-spec is_account_db(ne_binary()) -> boolean().
is_account_db(<<"account/", _AccountId:34/binary, "-", _Date:6/binary>>) -> 'false';
is_account_db(<<"account%2F", _AccountId:38/binary, "-", _Date:6/binary>>) -> 'false';
is_account_db(<<"account/", _/binary>>) -> 'true';
is_account_db(<<"account%2f", _/binary>>) -> 'true';
is_account_db(<<"account%2F", _/binary>>) -> 'true';
is_account_db(_) -> 'false'.

-spec cleanup_dead_accounts(wh_json:object()) -> wh_json:object().
-spec cleanup_dead_accounts(wh_json:object(), ne_binaries()) -> wh_json:object().
cleanup_dead_accounts(AccountDoc) ->
    cleanup_dead_accounts(AccountDoc, ?DEAD_ACCOUNT_IDS).
cleanup_dead_accounts(AccountDoc, []) ->
    AccountDoc;
cleanup_dead_accounts(AccountDoc, DeadAccountIDs) ->
    PvtTree = wh_json:get_value(<<"pvt_tree">>, AccountDoc, []),
    wh_json:set_value(<<"pvt_tree">>
                      ,lists:foldl(fun lists:delete/2, PvtTree, DeadAccountIDs)
                      ,AccountDoc
                     ).
