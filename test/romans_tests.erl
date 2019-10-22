%%%-------------------------------------------------------------------
%%% @author Álvaro Pagliari
%%% @copyright (C) 2017,
%%% @doc EUnit test file for romans module
%%% @end
%%% Created : 10. dez 2017 17:55
%%%-------------------------------------------------------------------
-module(romans_tests).
-author("alvaro").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

-spec to_int_test() -> ok.
to_int_test() ->
  ?assertEqual(1, romans:to_int("I")),
  ?assertEqual(2, romans:to_int("II")),
  ?assertEqual(3, romans:to_int("III")),
  ?assertEqual(5, romans:to_int("V")),
  ?assertEqual(10, romans:to_int("X")),
  ?assertEqual(30, romans:to_int("XXX")),
  ?assertEqual(50, romans:to_int("L")),
  ?assertEqual(100, romans:to_int("C")),
  ?assertEqual(200, romans:to_int("CC")),
  ?assertEqual(500, romans:to_int("D")),
  ?assertEqual(1000, romans:to_int("M")),
  ?assertEqual(4, romans:to_int("IV")),
  ?assertEqual(9, romans:to_int("IX")),
  ?assertEqual(14, romans:to_int("XIV")),
  ?assertEqual(19, romans:to_int("XIX")),
  ?assertEqual(949, romans:to_int("CMXLIX")),
  ?assertEqual(1878, romans:to_int("MDCCCLXXVIII")),
  ?assertEqual(3876, romans:to_int("MMMDCCCLXXVI")),
  ?assertEqual(fail, romans:to_int("IIII")),
  ?assertEqual(fail, romans:to_int("VV")),
  ?assertEqual(fail, romans:to_int("IL")),
  ?assertEqual(fail, romans:to_int("CXXXVIIII")),
  ?assertEqual(fail, romans:to_int("CXXXIIIV")),
  ?assertEqual(fail, romans:to_int("XM")),
  ok.