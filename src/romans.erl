%%%-------------------------------------------------------------------
%%% @author Álvaro Pagliari
%%% @copyright (C) 2017,
%%% @doc Erlang module to convert Romans numbers to integers
%%% @end
%%% Created : 10. dez 2017 17:55
%%%-------------------------------------------------------------------

-module(romans).

-compile([{nowarn_unused_function, [{ validate_input, 2}, {is_permitted, 2}]}]).

%% API exports
-export([]).

%%====================================================================
%% API functions
%%====================================================================
-export([
  to_int/1
]).

%% @doc Accepts a Roman number and returns a integer or the 'fail' atom
-type roman() :: string().
-spec to_int(roman()) -> pos_integer() | fail.
to_int(Value) ->
  case validate_input(Value) of
    true -> roman_to_int(Value);
    false -> fail
  end.


%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% @doc Entry point function to Convert Roman List to Integer
-spec roman_to_int(list()) -> fail | pos_integer().
roman_to_int([]) ->
  0;
roman_to_int([$V, $V | _]) ->
  fail;
roman_to_int([$L, $L | _]) ->
  fail;
roman_to_int([$D, $D | _]) ->
  fail;
roman_to_int(List) ->
  RArr = roman_to_array(List),
  RArrSignaled = resolve_roman_array_signals(RArr),
  sum_roman_array(RArrSignaled).

%% @private
%% @doc Convert each char to integer
-spec r_to_i(char()) -> pos_integer().
r_to_i($I) ->
  1;
r_to_i($V) ->
  5;
r_to_i($X) ->
  10;
r_to_i($L) ->
  50;
r_to_i($C) ->
  100;
r_to_i($D) ->
  500;
r_to_i($M) ->
  1000.


-spec roman_to_array(roman()) -> list().
roman_to_array(RomanInput) ->
  do_roman_to_array(RomanInput, []).

%% @private
%% @doc Convert the list of roman characters to integer
%% @returns Return a list of integer in inverse order from the input (this is on purpose)
-spec do_roman_to_array(roman(), list()) -> list().
do_roman_to_array([], Acc) ->
  Acc;
do_roman_to_array([Chr|Rest], Acc) ->
  ChrInt = r_to_i(Chr),
  do_roman_to_array(Rest, [{Chr, ChrInt} | Acc]).


-spec resolve_roman_array_signals(list()) -> list().
resolve_roman_array_signals(Arr) ->
  resolve_roman_array_signals(Arr, undefined, []).

%% @private
%% @doc This is the main function, it determines the signal to be used by the
%% sum_roman_array/1 function. It uses a Max visited value and compares it to the current Value
%% to decide if it should add or subtract the current value from the total
%% @returns This function returns a list with tuples {Char, Value, Signal)
-spec resolve_roman_array_signals(list(), pos_integer(), list()) -> list().
resolve_roman_array_signals([], _Max, Acc) ->
  lists:reverse(Acc);
%If no Max value is defined, set the current value as Max
resolve_roman_array_signals([{_Chr, Value} | _] = Arr, undefined, Acc) ->
  resolve_roman_array_signals(Arr, Value, Acc);
%If current Value and the Max value are the same, the signal must be a PLUS
resolve_roman_array_signals([{Chr, Value} | Rest], Value, Acc) ->
  resolve_roman_array_signals(Rest, Value, [{Chr, Value, '+'} | Acc]);
%If the Max value is bigger than the current value, the signal must be a MINUS (ex.: IX, Value = I, Max = X -> 10 - 1 -> 9)
resolve_roman_array_signals([{Chr, Value} | Rest], Max, Acc) when  Max > Value->
  resolve_roman_array_signals(Rest, Max, [{Chr, Value, '-'} | Acc]);
%Else the signal must be a PLUS
resolve_roman_array_signals([{Chr, Value} | Rest], Max, Acc) when  Max =< Value->
  resolve_roman_array_signals(Rest, Value, [{Chr, Value, '+'} | Acc]).

%% @private
%% @doc This function sums all the values collected using the singal defined by resolve_roman_array_signals/3
-spec sum_roman_array(list()) -> pos_integer().
sum_roman_array(RArrSignaled) ->
  F = fun({_Chr, Value, '+'}, Acc) ->
           Acc + Value;
         ({_Chr, Value, '-'}, Acc) ->
           Acc - Value
      end,
  lists:foldl(F, 0, RArrSignaled).

%% @private
%% @doc Validates the input list. Two validations are done, first forbidden sequence
-spec validate_input(list()) -> boolean().
validate_input([]) ->
  true;
validate_input(Input) ->
  Validations = validate_input_repeated(Input, []),

  F = fun(Elem) ->
          Elem
      end,
  lists:all(F, Validations).


%% @private
%% @deprecated
%% @doc Verify some forbidden combinations. Was replaced by {@link  validate_input_repeated}.
-spec validate_input(list(), list()) -> boolean().
validate_input([], Acc) ->
  Acc;
validate_input([_], Acc) ->
  [true | Acc];
validate_input([Chr1, Chr2 | Rest], Acc) ->
  IsPermitted = is_permitted(Chr1, Chr2),
  validate_input([Chr2 | Rest], [IsPermitted | Acc]).

%% @private
%% @deprecated
%% @doc  Used to do basic validation on the input list
-spec is_permitted(char(), char()) -> boolean().
is_permitted($I, $L)   -> false;
is_permitted($V, $V)   -> false;
is_permitted($V, $X)   -> false;
is_permitted($X, $M)   -> false;
is_permitted($L, $L)   -> false;
is_permitted($L, $M)   -> false;
is_permitted($D, $D)   -> false;
is_permitted($D, $M)   -> false;
is_permitted(_, _)   -> true.

%% @private
%% @doc Validate the input list, uses a more flexible pattern matching to search for forbidden combinations of letters
-spec validate_input_repeated(list(), list()) -> boolean().
validate_input_repeated([], Acc) ->
  Acc;
validate_input_repeated([$I, $L| _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([$V, $V| _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([$V, $X| _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([$X, $M| _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([$L, $L| _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([$L, $M| _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([$D, $D| _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([$D, $M| _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([$I, $I, $V | _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([$I, $I, $X | _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([$X, $X, $C | _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([$C, $C, $D | _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([Chr, Chr, Chr, Chr | _Resto], Acc) ->
  [false | Acc];
validate_input_repeated([_ | Resto], Acc) ->
  validate_input_repeated(Resto, [true | Acc]).


