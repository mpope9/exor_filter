%%-----------------------------------------------------------------------------
%% @copyright (C) 2019, Matthew Pope
%% @doc Nif wrapper for the xor_filter: 
%% https://github.com/FastFilter/xor_singleheader
%%
%% They're 'Faster and Smaller Than Bloom and Cuckoo Filters'.
%%
%% Be wary of memory usage when using this module.
%%
%% Example Usage:
%%
%% ```
%% Filter = exor_filter:xor8_initialize([1, 2, 3]).
%% true   = exor_filter:xor8_contain(Filter, 2).
%% false  = exor_filter:xor8_contain(Filter, 6).
%% ok     = exor_filter:xor8_free(Filter).
%% '''
%%
%% Filters are initialized independently:
%% ```
%% Filter1 = exor_filter:xor8_initialize([1, 2, 3]).
%% Filter2 = exor_filter:xor8_initialize([4, 5, 6]).
%%
%% false   = exor_filter:xor8_contain(Filter1, 6).
%% true    = exor_filter:xor8_contain(Filter1, 2).
%% false   = exor_filter:xor8_contain(Filter2, 2).
%% true    = exor_filter:xor8_contain(Filter2, 5).
%% '''
%%
%% Example usage from Elixir:
%% ```
%% ...
%% Alias :exor_filter, as: XorFilter
%% ...
%% true =
%%    [1, 2, 3, 4]
%%    |> XorFilter.xor8_initialize()
%%    |> XorFilter.xor8_contain(1)
%%
%% '''
%%
%% The usage of the xor16 is the same.  That structure is larger, but
%% has a smaller false positive rate.
%% 
%% The buffered versions of initialize are provided for larger data sets.
%% This can be faster.  See xor8_buffered_initialize/2 for more information.
%%
%% Versions of the initialization function that can be passed a hash 
%% function are provided, so that data can be passed without being 
%% hashed first.
%% See `xor8_initialize/2' for more details.
%% @author Matthew Pope
%% @end
%%-----------------------------------------------------------------------------
-module(exor_filter).

-export([
   xor8_initialize/2,
   xor8_initialize/1,
   xor8_buffered_initialize/2,
   xor8_buffered_initialize/1,
   xor8_contain/3,
   xor8_contain/2,
   xor8_free/1,

   xor16_initialize/2,
   xor16_initialize/1,
   xor16_buffered_initialize/2,
   xor16_buffered_initialize/1,
   xor16_contain/3,
   xor16_contain/2,
   xor16_free/1
]).
-on_load(init/0).

-define(APPNAME, exor_filter).
-define(LIBNAME, exor_filter).


%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter, and runs the specified hash function on
%% each of the elements.  The hash function must output unsigned 64 bit 
%% numbers, or an error will occur.  This could be 'safer' than passing raw 
%% data, because of the minimal checks on the output of the function are done.
%%
%% The function must be of arity /1.  If you need to pass more data, consider
%% using a list of tuples to transform:
%%
%% ```
%% exor_filter:xor8_initialize([{1, 1}, {2, 2}], 
%%    fun({Num1, Num1} -> 
%%       Num1 + Num2 
%%    end)).
%% '''
%%
%% Otherwise, an `{error, "Reason"}' be returned.
%%
%% Returns a `Ref<>' to a filter to be used in `contain' and `free'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_initialize(list(), fun()) -> reference() | {error, string()}.

xor8_initialize(List, HashFunction) ->

   case erlang:fun_info(HashFunction, arity) of

      {arity, 1} ->

         HashedList = lists:map(
            fun(Element) ->
               HashFunction(Element)
            end,
         List),
         xor8_initialize(HashedList);

      _ ->
         {error, wrong_arity_hash_function_error}
   end.


%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter on a passed list.  If the list isn't a list
%% of 64 unsigned numbers, an error will be thrown.
%% 
%% Returns a `Ref<>' to a filter to be used in `contain' and `free'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_initialize(list()) -> reference() | {error, string()}.

xor8_initialize(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Similar to the initialize function, but is a buffered version for lists
%% That are over 100,000,000 keys.  Use for greater speed.
%%
%% See xor8_initialize/2 for example usage.
%% 
%% Returns a `Ref<>' to a filter to be used in `contain' and `free'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_buffered_initialize(list(), fun()) 
   -> reference() | {error, string()}.

xor8_buffered_initialize(List, HashFunction) ->

   case erlang:fun_info(HashFunction, arity) of

      {arity, 1} ->

         HashedList = lists:map(
            fun(Element) ->
               HashFunction(Element)
            end,
         List),
         xor8_buffered_initialize(HashedList);

      _ ->
         {error, wrong_arity_hash_function_error}
   end.


%%-----------------------------------------------------------------------------
%% @doc Similar to the initialize function, but is a buffered version for lists
%% That are over 100,000,000 keys.  Use for greater speed.
%%
%% See xor8_initialize/1 for example usage.
%% 
%% Returns a `Ref<>' to a filter to be used in `contain' and `free'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_buffered_initialize(list()) -> reference() | {error, string()}.

xor8_buffered_initialize(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter, the second must be an unsighed
%% 64 bit number.
%%
%% Returns `true' if the element exists (or there is a false positive).
%% The third argument will be returned instead of `false' if the element is
%% not in the filter.
%% @end
-spec xor8_contain(reference(), integer(), any()) -> true | any().

xor8_contain(Filter, Key, ReturnValue) ->
   
   case xor8_contain(Filter, Key) of
   
      false ->
         ReturnValue;

      Value ->
         Value
   end.


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter, the second must be an unsighed
%% 64 bit number.
%%
%% Returns `true' if the element exists (or there is a false positive).
%% Returns `false' if otherwise.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_contain(reference(), integer()) -> true | false.

xor8_contain(_, _) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Frees the memory of the filter.  These can be large structures, so it
%% is recommended that this is called for cleanup.
%%
%% Returns `ok'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_free(reference()) -> true | false.

xor8_free(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc See the xor8_initialize/2 documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_initialize(list(), fun()) -> reference() | {error, string()}.

xor16_initialize(List, HashFunction) ->

   case erlang:fun_info(HashFunction, arity) of

      {arity, 1} ->

         HashedList = lists:map(
            fun(Element) ->
               HashFunction(Element)
            end,
         List),
         xor16_initialize(HashedList);

      _ ->
         {error, wrong_arity_hash_function_error}
   end.


%%-----------------------------------------------------------------------------
%% @doc See the xor8_initialize/1 documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_initialize(list()) -> reference() | {error, string()}.

xor16_initialize(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc See the xor8_buffered_initialize/2 documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_buffered_initialize(list(), fun()) 
   -> reference() | {error, string()}.

xor16_buffered_initialize(List, HashFunction) ->

   case erlang:fun_info(HashFunction, arity) of

      {arity, 1} ->

         HashedList = lists:map(
            fun(Element) ->
               HashFunction(Element)
            end,
         List),
         xor8_buffered_initialize(HashedList);

      _ ->
         {error, wrong_arity_hash_function_error}
   end.


%%-----------------------------------------------------------------------------
%% @doc See the xor8_buffered_initialize/1 documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_buffered_initialize(list()) -> reference() | {error, string()}.

xor16_buffered_initialize(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc See the xor8_contain/3 documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_contain(reference(), integer(), any()) -> true | any().

xor16_contain(Filter, Key, ReturnValue) ->
   
   case xor16_contain(Filter, Key) of
   
      false ->
         ReturnValue;

      Value ->
         Value
   end.


%%-----------------------------------------------------------------------------
%% @doc See the xor8_contain/2 documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_contain(reference(), integer()) -> true | false.

xor16_contain(_, _) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc See the xor8_free/1 documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_free(reference()) -> true | false.

xor16_free(_) ->
   not_loaded(?LINE).


init() ->
   SoName = case code:priv_dir(?APPNAME) of
      {error, bad_name} ->
         case filelib:is_dir(filename:join(["..", priv])) of
            true ->
               filename:join(["..", priv, ?LIBNAME]);
            _ ->
               filename:join([priv, ?LIBNAME])
         end;
      Dir ->
         filename:join(Dir, ?LIBNAME)
   end,
   erlang:load_nif(SoName, 0).

not_loaded(Line) ->
   exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
