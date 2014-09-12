%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_types).

-export([to_integer/1
         ,to_integer/2
        ]).
-export([to_float/1
         ,to_float/2
        ]).
-export([to_number/1]).
-export([to_hex/1]).
-export([to_list/1]).
-export([to_binary/1]).
-export([to_atom/1
         ,to_atom/2
        ]).
-export([to_boolean/1]).

-export([is_boolean/1]).
-export([is_true/1]).
-export([is_false/1]).
-export([is_empty/1]).
-export([is_proplist/1]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("wh_types.hrl").

-spec to_integer(string() | binary() | integer() | float()) -> integer().
-spec to_integer(string() | binary() | integer() | float(), 'strict' | 'notstrict') -> integer().
to_integer(X) -> to_integer(X, notstrict).

to_integer(X, 'strict') when is_float(X) -> erlang:error('badarg');
to_integer(X, 'notstrict') when is_float(X) -> round(X);
to_integer(X, S) when is_binary(X) -> to_integer(binary_to_list(X), S);
to_integer(X, S) when is_list(X) ->
    try list_to_integer(X) of
        I -> I
    catch
        'error':'badarg' when S =:= 'notstrict' ->
            round(list_to_float(X))
    end;
to_integer(X, _) when is_integer(X) ->
    X.

-spec to_float(string() | binary() | integer() | float()) -> float().
-spec to_float(string() | binary() | integer() | float(), 'strict' | 'notstrict') -> float().
to_float(X) -> to_float(X, 'notstrict').

to_float(X, S) when is_binary(X) -> to_float(binary_to_list(X), S);
to_float(X, S) when is_list(X) ->
    try list_to_float(X) of
        F -> F
    catch
        'error':'badarg' when S =:= 'notstrict' -> list_to_integer(X)*1.0 %% "500" -> 500.0
    end;
to_float(X, 'strict') when is_integer(X) -> erlang:error('badarg');
to_float(X, 'notstrict') when is_integer(X) -> X * 1.0;
to_float(X, _) when is_float(X) -> X.

-spec to_number(binary() | string() | number()) -> number().
to_number(X) when is_number(X) -> X;
to_number(X) when is_binary(X) -> to_number(to_list(X));
to_number(X) when is_list(X) ->
    try list_to_integer(X) of
        Int -> Int
    catch
        'error':'badarg' -> list_to_float(X)
    end.

-spec to_list(atom() | list() | binary() | integer() | float()) -> list().
to_list(X) when is_float(X) -> mochinum:digits(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_list(X) -> X.

%% Known limitations:
%%   Converting [256 | _], lists with integers > 255
-spec to_binary(atom() | string() | binary() | integer() | float()) -> binary().
to_binary(X) when is_float(X) -> to_binary(mochinum:digits(X));
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) -> iolist_to_binary(X);
to_binary(X) when is_binary(X) -> X.

%% the safer version, won't let you leak atoms
-spec to_atom(atom() | list() | binary() | integer() | float()) -> atom().
to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_list(X) -> list_to_existing_atom(X);
to_atom(X) -> to_atom(to_list(X)).

%% only if you're really sure you want this
%% to protect yourself a bit from overrunning the atom table,
%% pass a list of safe values for X
%% so if X is a binary, the SafeList would be [ne_binary(),...]
%% if X is a list, the SafeList would be [nonempty_string(),...]
%% etc. So to_atom will not coerce the type of X to match the types in SafeList
%% when doing the lists:member/2
-spec to_atom(atom() | list() | binary() | integer() | float(), 'true' | list()) -> atom().
to_atom(X, _) when is_atom(X) -> X;
to_atom(X, 'true') when is_list(X) -> list_to_atom(X);
to_atom(X, 'true') -> to_atom(to_list(X), 'true');
to_atom(X, 'false') -> to_atom(X);
to_atom(X, SafeList) when is_list(SafeList) ->
    to_atom(to_list(X), lists:member(X, SafeList)).

-spec to_boolean(binary() | string() | atom()) -> boolean().
to_boolean(<<"true">>) -> 'true';
to_boolean("true") -> 'true';
to_boolean('true') -> 'true';
to_boolean(<<"false">>) -> 'false';
to_boolean("false") -> 'false';
to_boolean('false') -> 'false'.

-spec is_true(binary() | string() | atom()) -> boolean().
is_true(<<"true">>) -> 'true';
is_true("true") -> 'true';
is_true('true') -> 'true';
is_true(_) -> 'false'.

-spec is_false(binary() | string() | atom()) -> boolean().
is_false(<<"false">>) -> 'true';
is_false("false") -> 'true';
is_false('false') -> 'true';
is_false(_) -> 'false'.

-spec is_boolean(binary() | string() | atom()) -> boolean().
is_boolean(<<"true">>) -> 'true';
is_boolean("true") -> 'true';
is_boolean('true') -> 'true';
is_boolean(<<"false">>) -> 'true';
is_boolean("false") -> 'true';
is_boolean('false') -> 'true';
is_boolean(_) -> 'false'.

-spec is_empty(term()) -> boolean().
is_empty(0) -> 'true';
is_empty([]) -> 'true';
is_empty("0") -> 'true';
is_empty("false") -> 'true';
is_empty("NULL") -> 'true';
is_empty("undefined") -> 'true';
is_empty(<<>>) -> 'true';
is_empty(<<"0">>) -> 'true';
is_empty(<<"false">>) -> 'true';
is_empty(<<"NULL">>) -> 'true';
is_empty(<<"undefined">>) -> 'true';
is_empty('null') -> 'true';
is_empty('false') -> 'true';
is_empty('undefined') -> 'true';
is_empty(Float) when is_float(Float), Float == 0.0 -> 'true';
is_empty(MaybeJObj) ->
    case wh_json:is_json_object(MaybeJObj) of
        'false' -> 'false'; %% if not a json object, its not empty
        'true' -> wh_json:is_empty(MaybeJObj)
    end.

-spec is_proplist(any()) -> boolean().
is_proplist(Term) when is_list(Term) ->
    lists:all(fun({_,_}) -> 'true'; (A) -> is_atom(A) end, Term);
is_proplist(_) -> 'false'.

%% must be a term that can be changed to a list
-spec to_hex(binary() | string()) -> string().
to_hex(S) ->
    string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [H]) || H <- to_list(S)])).

-ifdef(TEST).

%% PROPER TESTING
prop_to_integer() ->
    ?FORALL({F, I}, {float(), integer()},
            begin
                Is = [ [Fun(N), N] || Fun <- [ fun to_list/1, fun to_binary/1], N <- [F, I] ],
                lists:all(fun([FN, N]) -> erlang:is_integer(to_integer(N)) andalso erlang:is_integer(to_integer(FN)) end, Is)
            end).

prop_to_number() ->
    ?FORALL({F, I}, {float(), integer()},
            begin
                Is = [ [Fun(N), N] || Fun <- [ fun to_list/1, fun to_binary/1], N <- [F, I] ],
                lists:all(fun([FN, N]) -> erlang:is_number(to_number(N)) andalso erlang:is_number(to_number(FN)) end, Is)
            end).

prop_to_float() ->
    ?FORALL({F, I}, {float(), integer()},
            begin
                Fs = [ [Fun(N), N] || Fun <- [ fun to_list/1, fun to_binary/1], N <- [F, I] ],
                lists:all(fun([FN, N]) -> erlang:is_float(to_float(N)) andalso erlang:is_float(to_float(FN)) end, Fs)
            end).

prop_to_list() ->
    ?FORALL({A, L, B, I, F}, {atom(), list(), binary(), integer(), float()},
            lists:all(fun(X) -> is_list(to_list(X)) end, [A, L, B, I, F])).

                                                %-type iolist() :: maybe_improper_list(char() | binary() | iolist(), binary() | []).
prop_to_binary() ->
    ?FORALL({A, L, B, I, F, IO}, {atom(), list(range(0,255)), binary(), integer(), float(), iolist()},
            lists:all(fun(X) -> is_binary(to_binary(X)) end, [A, L, B, I, F, IO])).

prop_iolist_t() ->
    ?FORALL(IO, iolist(), is_binary(to_binary(IO))).

proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing",
     {timeout, 15000,
      [
       ?_assertEqual([], proper:module(?MODULE, [{max_shrinks, 0}]))
      ]}}.

to_boolean_test() ->
    All = [<<"true">>, "true", 'true', <<"false">>, "false", 'false'],
    NotAll = [0, 123, 1.23, "123", "abc", abc, <<"abc">>, <<"123">>, {what, is, this, doing, here}],
    ?assertEqual('true', lists:all(fun(X) ->
                                         try to_boolean(X) of
                                             _ -> 'true'
                                         catch _:_ -> 'false'
                                         end
                                 end, All)),
    ?assertEqual('true', lists:all(fun(X) ->
                                         try to_boolean(X) of
                                             _ -> 'false'
                                         catch _:_ -> 'true'
                                         end
                                 end, NotAll)).
-endif.
