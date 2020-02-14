%%-----------------------------------------------------------------------------
%% @copyright (C) 2019, Matthew Pope
%% @author Matthew Pope
%% @doc Interface for the xor8 filter.
%%
%% Shorthand for the `exor_filter' module.  For indepth documentation, see
%% that module.
%%
%% Example usage:
%% ```
%% Filter = xor8:new(["cat", "dog", "mouse"]),
%% true   = xor8:contain(Filter, "cat"),
%% false  = xor8:contain(Filter, "goose"),
%% ok     = xor8:free(Filter).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(xor8).

-export([
    new/1,
    new/2,
    new_buffered/1,
    new_buffered/2,
    contain/2,
    contain/3,
    to_bin/1,
    from_bin/1,
    from_bin/2
]).

%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter, and runs the default hash function on
%% each of the elements in the list.  This should be fine for the general case.
%% @end
%%-----------------------------------------------------------------------------
-spec new(list()) -> {reference(), atom()} | {error, atom()}.

new(List) ->
    exor_filter:xor8(List).


%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter, and runs the specified hash on each of 
%% the elements in the list.  
%%
%% The option `default_hash' uses the `erlang:phash2/1' function.
%% The option `none' is for prehashed data.
%% A fun can be passed that will be applied to each element.
%% @end
%%-----------------------------------------------------------------------------
-spec new(list(), atom() | fun()) -> 
   {reference(), atom() | fun()} | {error, atom()}.

new(List, HashFunction) ->
    exor_filter:xor8(List, HashFunction).


%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter, and runs the default hash function on
%% each of the elements in the list.  This is the buffered version, meant for
%% large filters.
%% @end
%%-----------------------------------------------------------------------------
-spec new_buffered(list()) -> {reference(), atom()} | {error, atom()}.

new_buffered(List) ->
    exor_filter:xor8_buffered(List).


%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter, and runs the default hash function on
%% each of the elements in the list.  This is the buffered version, meant for
%% large filters.  See the `xor8:new/2' or `exor_filter:xor8_new/2' funtions
%% for more indepth documentaiton.
%% @end
%%-----------------------------------------------------------------------------
-spec new_buffered(list(), atom() | fun()) 
   -> {reference(), atom() | fun()} | {error, atom()}.

new_buffered(List, HashFunction) ->
    exor_filter:xor8_buffered(List, HashFunction).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%% @end
%%-----------------------------------------------------------------------------
-spec contain({reference(), atom() | fun()}, term()) -> true | false.

contain(Filter, Key) ->
    exor_filter:xor8_contain(Filter, Key).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%%
%% Will return the third argument if the element doesn't exist in the filter.
%% @end
%%-----------------------------------------------------------------------------
-spec contain({reference(), atom() | fun()}, term(), any()) -> true | any().

contain(Filter, Key, ReturnValue) ->
    exor_filter:xor8_contain(Filter, Key, ReturnValue).


%%-----------------------------------------------------------------------------
%% @doc Serializes the filter to a binary that can be later be deserialized with
%% `from_bin/1' or `from_bin/2'
%%
%% Returns `binary()'.
%% @end
%%-----------------------------------------------------------------------------
-spec to_bin(reference()) -> binary().

to_bin(Filter) ->
    exor_filter:xor8_to_bin(Filter).


%%-----------------------------------------------------------------------------
%% @doc Deserializes a filter from a binary previously serialized with `to_bin/1'
%% that used the default hash function.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec from_bin(binary()) -> {reference(), atom()}.

from_bin(Filter) ->
    exor_filter:xor8_from_bin(Filter).


%%-----------------------------------------------------------------------------
%% @doc Deserializes a filter from a binary previously serialized with `to_bin/1'
%% that used a custom hash function.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec from_bin(binary(), atom() | fun()) -> {reference(), atom() | fun()}.

from_bin(Filter, Hash) ->
    exor_filter:xor8_from_bin(Filter, Hash).
