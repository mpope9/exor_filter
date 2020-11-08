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
%% false  = xor8:contain(Filter, "goose").
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(xor8).

-export([
    new/1,
    new/2,
    new_buffered/1,
    new_buffered/2,
    new_empty/0,
    new_empty/1,
    add/2,
    finalize/1,
    contain/2,
    contain/3,
    to_bin/1,
    from_bin/1
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
-spec new(list(), exor_filter:hash_function()) -> 
   {reference(), exor_filter:hash_function()} | {error, atom()}.

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
-spec new_buffered(list(), exor_filter:hash_function()) 
   -> {reference(), exor_filter:hash_function()} | {error, atom()}.

new_buffered(List, HashFunction) ->
    exor_filter:xor8_buffered(List, HashFunction).


%%-----------------------------------------------------------------------------
%% @doc Initializes an empty filter.  Can be filled incrementally, and is
%% more memory efficient than storing entire data set in the Erlang VM.
%% Initializes the filter to 64 elements, but will be dynamically expanded
%% if more elements are added.
%% @end
%%-----------------------------------------------------------------------------
-spec new_empty() -> {builder, reference()}.

new_empty() ->
   new_empty(64).


%%-----------------------------------------------------------------------------
%% @doc Initializes an empty filter to the size passed.  Sizing of the filter 
%% is flexible, but it is more efficient to pre-allocate the estimated size.
%% @end
%%-----------------------------------------------------------------------------
-spec new_empty(integer()) -> {builder, reference()}.

new_empty(InitialSize) ->
   exor_filter:exor_empty(InitialSize).


%%-----------------------------------------------------------------------------
%% @doc Adds elements to filter, and applys the default hashing mechanism.
%% Dynamically re-sizes filter if needed.
%% @end
%%-----------------------------------------------------------------------------
-spec add({builder, reference()}, list()) -> {builder, reference()}.

add(Filter, Elements) ->
   exor_filter:exor_add(Filter, Elements).


%%-----------------------------------------------------------------------------
%% @doc Initializes filter internally, and frees data buffer.  Equivalent to
%% calling `xor8:new'.
%% Deduplication is not done, `finalize' will fail if duplicates are inserted.
%% @end
%%-----------------------------------------------------------------------------
-spec finalize({builder, reference()}) -> reference().

finalize(Filter) ->
   exor_filter:xor8_finalize(Filter).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%%
%% A filter previously serialized by `to_bin' is allowed
%% @end
%%-----------------------------------------------------------------------------
-spec contain({reference() | binary(), exor_filter:hash_function()}, term()) -> true | false.

contain(Filter, Key) ->
    exor_filter:xor8_contain(Filter, Key).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%%
%% A filter previously serialized by `to_bin' is allowed
%%
%% Will return the third argument if the element doesn't exist in the filter.
%% @end
%%-----------------------------------------------------------------------------
-spec contain({reference() | binary(), exor_filter:hash_function()}, term(), any()) -> true | any().

contain(Filter, Key, ReturnValue) ->
    exor_filter:xor8_contain(Filter, Key, ReturnValue).


%%-----------------------------------------------------------------------------
%% @doc Serializes the filter to a binary that can be later be deserialized with
%% `from_bin/1'.
%%
%% Returns `{binary(), exor_filter:hash_function()}'.
%% @end
%%-----------------------------------------------------------------------------
-spec to_bin({reference(), exor_filter:hash_function()})
    -> {binary(), exor_filter:hash_function()}.

to_bin(Filter) ->
    exor_filter:xor8_to_bin(Filter).


%%-----------------------------------------------------------------------------
%% @doc Deserializes a filter previously serialized with `to_bin/1'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec from_bin({binary(), exor_filter:hash_function()})
    -> {reference(), exor_filter:hash_function()}.

from_bin({Filter, Hash}) ->
    exor_filter:xor8_from_bin({Filter, Hash}).
