%%-----------------------------------------------------------------------------
%% @copyright (C) 2019, Matthew Pope
%% @author Matthew Pope
%% @doc Interface for the xor16 filter.
%%
%% Shorthand for the `exor_filter' module.  For indepth documentation, see
%% that module.
%%
%% Example usage:
%% ```
%% Filter = xor16:new(["cat", "dog", "mouse"]),
%% true   = xor16:contain(Filter, "cat"),
%% false  = xor16:contain(Filter, "goose"),
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(xor16).

-export([
    new/1,
    new/2,
    new_buffered/1,
    new_buffered/2,
    new_empty/0,
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
    exor_filter:xor16(List).


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
    exor_filter:xor16(List, HashFunction).


%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter, and runs the default hash function on
%% each of the elements in the list.  This is the buffered version, meant for
%% large filters.
%% @end
%%-----------------------------------------------------------------------------
-spec new_buffered(list()) -> {reference(), atom()} | {error, atom()}.

new_buffered(List) ->
    exor_filter:xor16_buffered(List).


%%-----------------------------------------------------------------------------
%% @doc Initializes an empty filter.  Can be filled incrementally, and is
%% more memory efficient than storing entire data set in the Erlang VM.
%% Initializes the filter to 64 elements, but will be dynamically expanded
%% if more elements are added.
%% @end
%%-----------------------------------------------------------------------------
-spec new_empty() -> {builder, reference()}.

new_empty() ->
   exor_filter:exor_empty().


%%-----------------------------------------------------------------------------
%% @doc Adds elements to filter, and applys the default hashing mechanism.
%% Dynamically re-sizes filter if needed.
%% @end
%%-----------------------------------------------------------------------------
-spec add({builder, reference()}, list()) -> {builder, reference()}.

add(Filter, Elements) ->

   SortedElements = lists:sort(Elements),
   exor_filter:exor_add(Filter, SortedElements).


%%-----------------------------------------------------------------------------
%% @doc Initializes filter internally, and frees data buffer.  Equivalent to
%% calling `xor16:new'.
%% Deduplication is not done, `finalize' will fail if duplicates are inserted.
%% @end
%%-----------------------------------------------------------------------------
-spec finalize({builder, reference()}) -> reference().

finalize(Filter) ->
   exor_filter:xor16_finalize(Filter).


%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter, and runs the default hash function on
%% each of the elements in the list.  This is the buffered version, meant for
%% large filters.  See the `xor16:new/2' or `exor_filter:xor16_new/2' funtions
%% for more indepth documentaiton.
%% @end
%%-----------------------------------------------------------------------------
-spec new_buffered(list(), exor_filter:hash_function()) 
   -> {reference(), exor_filter:hash_function()} | {error, atom()}.

new_buffered(List, HashFunction) ->
    exor_filter:xor16_buffered(List, HashFunction).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%%
%% A filter previously serialized by `to_bin' is allowed
%% @end
%%-----------------------------------------------------------------------------
-spec contain({reference() | binary(), exor_filter:hash_function()}, term()) -> true | false.

contain(Filter, Key) ->
    exor_filter:xor16_contain(Filter, Key).


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
    exor_filter:xor16_contain(Filter, Key, ReturnValue).


%%-----------------------------------------------------------------------------
%% @doc Serializes the filter to a binary that can be later be deserialized with
%% `from_bin/1'.
%%
%% Returns `{binary(), hash_function()}'.
%% @end
%%-----------------------------------------------------------------------------
-spec to_bin({reference(), exor_filter:hash_function()}) -> {binary(), exor_filter:hash_function()}.

to_bin(Filter) ->
    exor_filter:xor16_to_bin(Filter).


%%-----------------------------------------------------------------------------
%% @doc Deserializes a filter previously serialized with `to_bin'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec from_bin({binary(), exor_filter:hash_function()})
    -> {reference(), exor_filter:hash_function()}.

from_bin({Filter, Hash}) ->
    exor_filter:xor16_from_bin({Filter, Hash}).
