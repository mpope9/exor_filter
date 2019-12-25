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
%% Filter = exor_filter:xor8(["test1", "test2", "test3"]),
%% true   = exor_filter:xor8_contain(Filter, "test1"),
%% false  = exor_filter:xor8_contain(Filter, "test6"),
%% ok     = exor_filter:xor8_free(Filter).
%% '''
%%
%% Filters are initialized independently:
%% ```
%% Filter1 = exor_filter:xor8([1, 2, 3]),
%% Filter2 = exor_filter:xor8([4, 5, 6]),
%% 
%% false   = exor_filter:xor8_contain(Filter1, 6),
%% true    = exor_filter:xor8_contain(Filter1, 2),
%% false   = exor_filter:xor8_contain(Filter2, 2),
%% true    = exor_filter:xor8_contain(Filter2, 5),
%% 
%% ok      = exor_filter:xor8_free(Filter1),
%% ok      = exor_filter:xor8_free(Filter2).
%% '''
%%
%% Example usage from Elixir:
%% ```
%% ...
%% Alias :exor_filter, as: XorFilter
%% ...
%% true =
%%    [1, 2, 3, 4]
%%    |> XorFilter.xor8()
%%    |> XorFilter.xor8_contain(1)
%%
%% '''
%%
%% The usage of the xor16 is the same.  That structure is larger, but
%% has a smaller false positive rate.
%% 
%% The buffered versions of initialize are provided for larger data sets.
%% This can be faster.  See xor8_buffered/1 for more information.
%%
%% @author Matthew Pope
%% @end
%%-----------------------------------------------------------------------------
-module(exor_filter).

-export([
   xor8/1,
   xor8/2,
   xor8_buffered/1,
   xor8_buffered/2,
   xor8_contain/2,
   xor8_contain/3,
   xor8_free/1,

   xor16/1,
   xor16/2,
   xor16_buffered/1,
   xor16_buffered/2,
   xor16_contain/2,
   xor16_contain/3,
   xor16_free/1
]).
-on_load(init/0).

-define(APPNAME, exor_filter).
-define(LIBNAME, exor_filter).


%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter, and runs the default hash function on
%% each of the elements in the list.  This should be fine for the general case.
%%
%% Returns a {`Ref<>', `default_hash'} to be later used, or an error.
%% @end
-spec xor8(list()) -> {reference(), default_hash} | {error, atom()}.

xor8(List) ->
   xor8(List, default_hash).


%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter, and runs the specified pre-defined 
%% hash function on each of the elements.  
%%
%% There are predefined hashing function provided, can can be specified by
%% using `default_hash' or `fast_hash'.  To pass pre-hashed data, use
%% `none'.
%%
%% OR
%%
%% Initializes the xor filter, and runs the passed hash function on
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
%% Returns a {`Ref<>', hash_method} to a filter to be used in `contain' 
%% and `free', if a predefined hash function is specified.
%%
%% Returns a {`Ref<>', hash_function()} if a custom function is passed.
%%
%% Otherwise, an `{error, reason}' be returned.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8(list(), atom() | fun()) -> {reference(), atom() | fun()} | {error, atom()}.

xor8(List, HashFunction) when 
   HashFunction == default_hash;
   HashFunction == fast_hash;
   HashFunction == none ->

   case xor8_initialize_nif(List, HashFunction) of
      
      {error, Reason} ->
         {error, Reason};

      Reference ->
         {Reference, HashFunction}
   end;

xor8(List, HashFunction) when is_function(HashFunction) ->

   case erlang:fun_info(HashFunction, arity) of

      {arity, 1} ->

         HashedList = lists:map(
            fun(Element) ->
               HashFunction(Element)
            end,
         List),
         
         case xor8_initialize_nif(HashedList, passed) of
            
            {error, Reason} ->
               {error, Reason};

            Reference ->
               {Reference, HashFunction}
         end;

      _ ->
         {error, wrong_arity_hash_function_error}
   end;

xor8(_, _) ->
   {error, invalid_hash_method}.


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Initializes the xor filter on a passed list.  
%% If the list isn't a list of 64 unsigned numbers, an error will be thrown.
%% 
%% Returns a `Ref<>' to a filter to be used in `contain' and `free'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_initialize_nif(list(), atom()) -> reference() | {error, atom()}.

xor8_initialize_nif(_, _) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Similar to the initialize function, but is a buffered version for lists
%% that are large.  This version uses the default hash.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_buffered(list()) -> {reference(), atom()} | {error, atom()}.

xor8_buffered(List) ->
   xor8_buffered(List, default_hash).
 

%%-----------------------------------------------------------------------------
%% @doc Similar to the initialize function, but is a buffered version for lists
%% that are over 100,000,000 keys.  Use for greater speed.
%%
%% See xor8/1 for example usage.
%% 
%% Returns a `Ref<>' to a filter to be used in `contain' and `free'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_buffered(list(), atom() | fun()) 
   -> {reference(), atom() | fun()} | {error, atom()}.

xor8_buffered(List, HashFunction) when 
   HashFunction == default_hash;
   HashFunction == fast_hash;
   HashFunction == none ->

   case xor8_buffered_initialize_nif(List, HashFunction) of
      
      {error, Reason} ->
         {error, Reason};

      Reference ->
         {Reference, HashFunction}
   end;

xor8_buffered(List, HashFunction) when is_function(HashFunction) ->

   case erlang:fun_info(HashFunction, arity) of

      {arity, 1} ->

         HashedList = lists:map(
            fun(Element) ->
               HashFunction(Element)
            end,
         List),
         case xor8_buffered_initialize_nif(HashedList, passed) of

            {error, Reason} ->
               {error, Reason};

            Reference ->
               {Reference, HashFunction}
         end;

      _ ->
         {error, wrong_arity_hash_function_error}
   end.


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Similar to the initialize function, but is a buffered 
%% version for lists
%%
%% Returns a `Ref<>' to a filter to be used in `contain' and `free'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_buffered_initialize_nif(list(), atom()) 
   -> reference() | {error, atom()}.

xor8_buffered_initialize_nif(_, _) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%%
%% DO NOT PASS PRE-HASHED VALUES.  The method / fun passed to the 
%% initialization function is saved, and is used to compute the hash.
%%
%% Returns true if the element exists (or if there is a false positive).
%% False if not.
%% @end
-spec xor8_contain({reference(), atom() | fun()}, term()) -> true | false.

xor8_contain({Filter, HashFunction}, Key) when is_function(HashFunction) ->
   xor8_contain_nif(Filter, HashFunction(Key), passed);

xor8_contain({Filter, HashFunction}, Key) ->
   xor8_contain_nif(Filter, Key, HashFunction).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%%
%% Returns `true' if the element exists (or there is a false positive).
%% The third argument will be returned instead of `false' if the element is
%% not in the filter.
%% @end
-spec xor8_contain({reference(), atom() | fun()}, term(), any())
   -> true | any().

xor8_contain({Filter, HashFunction}, Key, ReturnValue) 
   when is_function(HashFunction) ->
   
   HashedKey = HashFunction(Key),
   case xor8_contain_nif(Filter, HashedKey, passed) of
   
      false ->
         ReturnValue;

      Value ->
         Value
   end;

xor8_contain({Filter, HashFunction}, Key, ReturnValue) ->

   case xor8_contain_nif(Filter, Key, HashFunction) of

      false ->
         ReturnValue;

      Value ->
         Value
   end.


%%-----------------------------------------------------------------------------
%% @doc Nif api for the contain function.
%%
%% Returns `true' if the element exists (or there is a false positive).
%% Returns `false' if otherwise.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_contain_nif(reference(), term(), atom()) -> true | false.

xor8_contain_nif(_, _, _) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Frees the memory of the filter.  These can be large structures, so it
%% is recommended that this is called for cleanup.
%%
%% Returns `ok'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_free({reference(), any()}) -> ok.

xor8_free({Filter, _}) ->
   xor8_free_nif(Filter).


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Frees the memory of the filter.  
%%
%% Returns `ok'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_free_nif(reference()) -> ok.

xor8_free_nif(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc See the xor8/2 documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16(list()) -> {reference(), default_hash} | {error, atom()}.

xor16(List) ->
   xor16(List, default_hash).


%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter, and runs the specified pre-defined 
%% hash function on each of the elements.  
%%
%% See the xor8/2 documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16(list(), atom() | fun()) -> {reference(), atom() | fun()} | {error, atom()}.

xor16(List, HashFunction) when 
   HashFunction == default_hash;
   HashFunction == fast_hash;
   HashFunction == none ->

   case xor16_initialize_nif(List, HashFunction) of
      
      {error, Reason} ->
         {error, Reason};

      Reference ->
         {Reference, HashFunction}
   end;

xor16(List, HashFunction) when is_function(HashFunction) ->

   case erlang:fun_info(HashFunction, arity) of

      {arity, 1} ->

         HashedList = lists:map(
            fun(Element) ->
               HashFunction(Element)
            end,
         List),
         
         case xor16_initialize_nif(HashedList, passed) of
            
            {error, Reason} ->
               {error, Reason};

            Reference ->
               {Reference, HashFunction}
         end;

      _ ->
         {error, wrong_arity_hash_function_error}
   end;

xor16(_, _) ->
   {error, invalid_hash_method}.


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Initializes the xor filter on a passed list.  
%% If the list isn't a list of 64 unsigned numbers, an error will be thrown.
%% 
%% Returns a `Ref<>' to a filter to be used in `contain' and `free'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_initialize_nif(list(), atom()) -> reference() | {error, atom()}.

xor16_initialize_nif(_, _) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Similar to the initialize function, but is a buffered version for lists
%% that are large.  This version uses the default hash.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_buffered(list()) -> {reference(), atom()} | {error, atom()}.

xor16_buffered(List) ->
   xor16_buffered(List, default_hash).
 

%%-----------------------------------------------------------------------------
%% @doc Similar to the initialize function, but is a buffered version for lists
%% that are over 100,000,000 keys.  Use for greater speed.
%%
%% See xor16/1 for example usage.
%% 
%% Returns a `Ref<>' to a filter to be used in `contain' and `free'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_buffered(list(), atom() | fun()) 
   -> {reference(), atom() | fun()} | {error, atom()}.

xor16_buffered(List, HashFunction) when 
   HashFunction == default_hash;
   HashFunction == fast_hash;
   HashFunction == none ->

   case xor16_buffered_initialize_nif(List, HashFunction) of
      
      {error, Reason} ->
         {error, Reason};

      Reference ->
         {Reference, HashFunction}
   end;

xor16_buffered(List, HashFunction) when is_function(HashFunction) ->

   case erlang:fun_info(HashFunction, arity) of

      {arity, 1} ->

         HashedList = lists:map(
            fun(Element) ->
               HashFunction(Element)
            end,
         List),
         case xor16_buffered_initialize_nif(HashedList, passed) of

            {error, Reason} ->
               {error, Reason};

            Reference ->
               {Reference, HashFunction}
         end;

      _ ->
         {error, wrong_arity_hash_function_error}
   end.


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Similar to the initialize function, but is a buffered 
%% version for lists
%%
%% Returns a `Ref<>' to a filter to be used in `contain' and `free'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_buffered_initialize_nif(list(), atom()) 
   -> reference() | {error, atom()}.

xor16_buffered_initialize_nif(_, _) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%%
%% DO NOT PASS PRE-HASHED VALUES.  The method / fun passed to the 
%% initialization function is saved, and is used to compute the hash.
%%
%% Returns true if the element exists (or if there is a false positive).
%% False if not.
%% @end
-spec xor16_contain({reference(), atom() | fun()}, term()) -> true | false.

xor16_contain({Filter, HashFunction}, Key) when is_function(HashFunction) ->
   xor16_contain_nif(Filter, HashFunction(Key), passed);

xor16_contain({Filter, HashFunction}, Key) ->
   xor16_contain_nif(Filter, Key, HashFunction).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%%
%% Returns `true' if the element exists (or there is a false positive).
%% The third argument will be returned instead of `false' if the element is
%% not in the filter.
%% @end
-spec xor16_contain({reference(), atom() | fun()}, term(), any())
   -> true | any().

xor16_contain({Filter, HashFunction}, Key, ReturnValue) 
   when is_function(HashFunction) ->
   
   HashedKey = HashFunction(Key),
   case xor16_contain_nif(Filter, HashedKey, passed) of
   
      false ->
         ReturnValue;

      Value ->
         Value
   end;

xor16_contain({Filter, HashFunction}, Key, ReturnValue) ->

   case xor16_contain_nif(Filter, Key, HashFunction) of

      false ->
         ReturnValue;

      Value ->
         Value
   end.


%%-----------------------------------------------------------------------------
%% @doc Nif api for the contain function.
%%
%% Returns `true' if the element exists (or there is a false positive).
%% Returns `false' if otherwise.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_contain_nif(reference(), term(), atom()) -> true | false.

xor16_contain_nif(_, _, _) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Frees the memory of the filter.  These can be large structures, so it
%% is recommended that this is called for cleanup.
%%
%% Returns `ok'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_free({reference(), any()}) -> ok.

xor16_free({Filter, _}) ->
   xor16_free_nif(Filter).


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Frees the memory of the filter.  
%%
%% Returns `ok'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_free_nif(reference()) -> ok.

xor16_free_nif(_) ->
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
