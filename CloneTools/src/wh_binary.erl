%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_binary).

-export([to_lower/1]).
-export([to_upper/1]).
-export([ucfirst/1]).
-export([lcfirst/1]).
-export([strip/1
         ,strip/2
        ]).
-export([strip_left/2]).
-export([strip_right/2]).
-export([md5/1]).
-export([pad/3]).
-export([join/1
         ,join/2
        ]).
-export([hex/1]).
-export([rand_hex/1]).

-include_lib("kernel/include/inet.hrl").
-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("wh_types.hrl").

-spec pad(binary(), non_neg_integer(), binary()) -> binary().
pad(Bin, Size, Value) when size(Bin) < Size ->
    pad(<<Bin/binary, Value/binary>>, Size, Value);
pad(Bin, _, _) -> Bin.

-spec join([text() | atom(),...]) -> binary().
join(Bins) -> join(Bins, <<", ">>, []).
join(Bins, Sep) -> join(Bins, Sep, []).

-spec join([text() | atom(),...], binary()) -> binary().
join([], _, Acc) -> iolist_wh_types:to_binary(lists:reverse(Acc));
join([Bin], _, Acc) ->
    iolist_to_binary(lists:reverse([wh_types:to_binary(Bin) | Acc]));
join([Bin|Bins], Sep, Acc) ->
    join(Bins, Sep, [Sep, wh_types:to_binary(Bin) |Acc]).

-spec to_lower(term()) -> api_binary().
to_lower('undefined') -> 'undefined';
to_lower(Bin) when is_binary(Bin) -> << <<(wh_util:to_lower_char(B))>> || <<B>> <= Bin>>;
to_lower(Else) -> to_lower(wh_types:to_binary(Else)).

-spec ucfirst(ne_binary()) -> ne_binary().
ucfirst(<<F:8, Bin/binary>>) -> <<(wh_util:to_upper_char(F)):8, Bin/binary>>.

-spec lcfirst(ne_binary()) -> ne_binary().
lcfirst(<<F:8, Bin/binary>>) -> <<(wh_util:to_lower_char(F)):8, Bin/binary>>.

-spec to_upper(term()) -> api_binary().
to_upper('undefined') -> 'undefined';
to_upper(Bin) when is_binary(Bin) -> << <<(wh_util:to_upper_char(B))>> || <<B>> <= Bin>>;
to_upper(Else) -> to_upper(wh_types:to_binary(Else)).

-spec strip(binary()) -> binary().
-spec strip(binary(), 'both' | 'left' | 'right') -> binary().
-spec strip_left(binary(), char()) -> binary().
-spec strip_right(binary(), char()) -> binary().
strip(B) -> strip(B, 'both').

strip(B, 'left') -> strip_left(B, $\s);
strip(B, 'right') -> strip_right(B, $\s);
strip(B, 'both') -> strip_right(strip_left(B, $\s), $\s);
strip(B, C) when is_integer(C) -> strip_right(strip_left(B, C), C).

strip_left(<<C, B/binary>>, C) -> strip_left(B, C);
strip_left(B, _) -> B.

strip_right(C, C) -> <<>>;
strip_right(<<C, B/binary>>, C) ->
    case strip_right(B, C) of
        <<>> -> <<>>;
        T -> <<C, T/binary>>
    end;
strip_right(<<A, B/binary>>, C) -> <<A, (strip_right(B, C))/binary>>;
strip_right(<<>>, _) -> <<>>.

-spec md5(text()) -> ne_binary().
md5(Text) -> hex(erlang:md5(wh_types:to_binary(Text))).

-spec hex(binary() | string()) -> binary().
hex(S) ->
    Bin = wh_types:to_binary(S),
    << <<(binary_to_hex_char(B div 16)), (binary_to_hex_char(B rem 16))>> || <<B>> <= Bin>>.

-spec rand_hex(pos_integer()) -> ne_binary().
rand_hex(Size) when is_integer(Size) andalso Size > 0 ->
    hex(crypto:rand_bytes(Size)).

binary_to_hex_char(N) when N < 10 -> $0 + N;
binary_to_hex_char(N) when N < 16 -> $a - 10 + N.

-ifdef(TEST).

%% PROPER TESTING
proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing",
     {timeout, 15000,
      [
       ?_assertEqual([], proper:module(?MODULE, [{max_shrinks, 0}]))
      ]}}.

pad_binary_test() ->
    ?assertEqual(<<"1234500000">>, pad(<<"12345">>, 10, <<"0">>)).

join_binary_test() ->
    ?assertEqual(<<"foo">>, join([<<"foo">>], <<", ">>)),
    ?assertEqual(<<"foo, bar">>, join([<<"foo">>, <<"bar">>], <<", ">>)),
    ?assertEqual(<<"foo, bar, baz">>, join([<<"foo">>, <<"bar">>, <<"baz">>], <<", ">>)).

ucfirst_binary_test() ->
    ?assertEqual(<<"Foo">>, ucfirst(<<"foo">>)),
    ?assertEqual(<<"Foo">>, ucfirst(<<"Foo">>)),
    ?assertEqual(<<"FOO">>, ucfirst(<<"FOO">>)),
    ?assertEqual(<<"1oo">>, ucfirst(<<"1oo">>)),
    ?assertEqual(<<"100">>, ucfirst(<<"100">>)),
    ?assertEqual(<<"1FF">>, ucfirst(<<"1FF">>)).

lcfirst_binary_test() ->
    ?assertEqual(<<"foo">>, lcfirst(<<"foo">>)),
    ?assertEqual(<<"foo">>, lcfirst(<<"Foo">>)),
    ?assertEqual(<<"fOO">>, lcfirst(<<"FOO">>)),
    ?assertEqual(<<"1oo">>, lcfirst(<<"1oo">>)),
    ?assertEqual(<<"100">>, lcfirst(<<"100">>)),
    ?assertEqual(<<"1FF">>, lcfirst(<<"1FF">>)).

to_lower_binary_test() ->
    ?assertEqual(<<"foo">>, to_lower(<<"foo">>)),
    ?assertEqual(<<"foo">>, to_lower(<<"Foo">>)),
    ?assertEqual(<<"foo">>, to_lower(<<"FoO">>)),
    ?assertEqual(<<"f00">>, to_lower(<<"f00">>)),
    ?assertEqual(<<"f00">>, to_lower(<<"F00">>)).

to_upper_binary_test() ->
    ?assertEqual(<<"FOO">>, to_upper(<<"foo">>)),
    ?assertEqual(<<"FOO">>, to_upper(<<"Foo">>)),
    ?assertEqual(<<"FOO">>, to_upper(<<"FoO">>)),
    ?assertEqual(<<"F00">>, to_upper(<<"f00">>)),
    ?assertEqual(<<"F00">>, to_upper(<<"F00">>)).

strip_binary_test() ->
    ?assertEqual(<<"foo">>, strip(<<"foo">>)),
    ?assertEqual(<<"foo">>, strip(<<"foo ">>)),
    ?assertEqual(<<"foo">>, strip(<<" foo ">>)),
    ?assertEqual(<<"foo">>, strip(<<"  foo  ">>)),
    ?assertEqual(<<"foo">>, strip(<<"     foo">>)),

    ?assertEqual(<<"foo">>, strip_left(<<"foo">>, $\s)),
    ?assertEqual(<<"foo">>, strip_left(<<" foo">>, $\s)),
    ?assertEqual(<<"foo ">>, strip_left(<<" foo ">>, $\s)),
    ?assertEqual(<<"foo ">>, strip_left(<<"foo ">>, $\s)),

    ?assertEqual(<<"foo">>, strip_right(<<"foo">>, $\s)),
    ?assertEqual(<<" foo">>, strip_right(<<" foo">>, $\s)),
    ?assertEqual(<<" foo">>, strip_right(<<" foo ">>, $\s)),
    ?assertEqual(<<"foo">>, strip_right(<<"foo ">>, $\s)).

strip_test() ->
    ?assertEqual(strip(<<"...Hello.....">>, $.), <<"Hello">>).

-endif.
