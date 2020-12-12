%%-----------------------------------------------------------------------------
%% @copyright (C) 2019, Matthew Pope
%% @author Matthew Pope
%% @doc Nif wrapper for the xor_filter: 
%% https://github.com/FastFilter/xor_singleheader
%%
%% They're 'Faster and Smaller Than Bloom and Cuckoo Filters'.
%%
%% Example Usage:
%%
%% ```
%% Filter = exor_filter:xor8(["test1", "test2", "test3"]),
%% true   = exor_filter:xor8_contain(Filter, "test1"),
%% false  = exor_filter:xor8_contain(Filter, "test6"),
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
%% Convinience modules `xor8' and `xor16' are provided.
%% @end
%%-----------------------------------------------------------------------------
-module(exor_filter).

-export([

   %% Filter agnostic functions.
   exor_empty/0,
   exor_add/2,

   %% xor8 functions.
   xor8/1,
   xor8/2,
   xor8_buffered/1,
   xor8_buffered/2,
   xor8_finalize/1,
   xor8_contain/2,
   xor8_contain/3,
   xor8_to_bin/1,
   xor8_from_bin/1,

   %% xor16 functions.
   xor16/1,
   xor16/2,
   xor16_buffered/1,
   xor16_buffered/2,
   xor16_finalize/1,
   xor16_contain/2,
   xor16_contain/3,
   xor16_to_bin/1,
   xor16_from_bin/1
]).
-on_load(init/0).

-define(APPNAME, exor_filter).
-define(LIBNAME, exor_filter).

-type hash_function() :: default_hash | none | fun((any()) -> non_neg_integer()).

-export_type([hash_function/0]).


%%-----------------------------------------------------------------------------
%% @doc Initializes empty filter of specified size.  This function will not
%% work with `contain' unless `finalize' is called.
%% Adds a `builder' atom to the data, to protect against usage in `contain'.
%% @end
%%-----------------------------------------------------------------------------
-spec exor_empty() -> {builder, reference()} | {error, atom()}.

exor_empty() ->
   {builder, exor_initialize_empty_filter_nif()}.


%%-----------------------------------------------------------------------------
%% @doc Nif api.
%% @end
%%-----------------------------------------------------------------------------
-spec exor_initialize_empty_filter_nif() -> reference() | {error, atom()}.

exor_initialize_empty_filter_nif() ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Adds values to filter builder.  Applys default hashing to the elements
%% first.  Duplication checks are not done.
%% @end
%%-----------------------------------------------------------------------------
-spec exor_add({builder, reference()}, list()) -> {builder, reference()} | {error, atom()}.

exor_add({builder, Filter}, Elements) ->

   ElementsNew = lists:map(fun erlang:phash2/1, Elements),
   {builder, exor_add_to_filter_nif(Filter, ElementsNew)}.


%%-----------------------------------------------------------------------------
%% @doc Nif api.
%% @end
%%-----------------------------------------------------------------------------
-spec exor_add_to_filter_nif(reference(), list()) -> reference() | {error, atom()}.

exor_add_to_filter_nif(_, _) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter, and runs the default hash function on
%% each of the elements in the list.  This should be fine for the general case.
%%
%% Returns a {`Ref<>', `default_hash'} to be later used, or an error.
%% @end
-spec xor8(list()) -> {reference(), atom()} | {error, atom()}.

xor8(List) ->
   xor8(List, default_hash).


%%-----------------------------------------------------------------------------
%% @doc Initializes the xor filter, and runs the specified pre-defined 
%% hash function on each of the elements.  
%%
%% There is a predefined hashing function provided, can can be specified by
%% using `default_hash' To pass pre-hashed data, use `none'.
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
%% if a predefined hash function is specified.
%%
%% Returns a {`Ref<>', hash_function()} if a custom function is passed.
%%
%% Otherwise, an `{error, reason}' be returned.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8(list(), hash_function()) -> 
   {reference(), hash_function()} | {error, atom()}.

xor8(List, HashFunction) ->
   initialize_filter(List, HashFunction, xor8).


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
%% Returns a `Ref<>' to a filter to be used in `contain'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_buffered(list(), hash_function()) 
   -> {reference(), hash_function()} | {error, atom()}.

xor8_buffered(List, HashFunction) ->
   initialize_filter(List, HashFunction, xor8_buffered).


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
-spec xor16(list(), hash_function()) 
   -> {reference(), hash_function()} | {error, atom()}.

xor16(List, HashFunction) ->
   initialize_filter(List, HashFunction, xor16).
 

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
%% Returns a `Ref<>' to a filter to be used in `contain'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_buffered(list(), hash_function()) 
   -> {reference(), hash_function()} | {error, atom()}.

xor16_buffered(List, HashFunction) ->
   initialize_filter(List, HashFunction, xor16_buffered).
 
%%-----------------------------------------------------------------------------
%% @doc Function that does the actual work.  Does some error checking,
%% as well as checks the passed lists for dups, and does hashing.
%% @end
%%-----------------------------------------------------------------------------
-spec initialize_filter(list(), atom(), atom()) 
   -> {reference(), atom()} | {error, atom()}.

initialize_filter(PassedList, default_hash, FilterType) ->

   DupedList = lists:map(fun erlang:phash2/1, PassedList),
   List = lists:usort(DupedList),
   nif_wrapper(List, default_hash, FilterType);

initialize_filter(List, none, FilterType) ->
   nif_wrapper(List, none, FilterType);

initialize_filter(List, HashFunction, FilterType) 
   when is_function(HashFunction) ->

   case erlang:fun_info(HashFunction, arity) of

      {arity, 1} ->
         HashedList = lists:map(HashFunction, List),
         nif_wrapper(HashedList, HashFunction, FilterType);

      _ ->
         {error, wrong_arity_hash_function_error}
   end;

initialize_filter(_, _, _) ->
   {error, invalid_hash_method}.


%%-----------------------------------------------------------------------------
%% @doc Unsafe API that is a wrapper for nif errors and bypassing duplication
%% checking.  Use with caution.
%% @end
%%-----------------------------------------------------------------------------
-spec nif_wrapper(list(), hash_function(), atom()) 
   -> {error, atom()} | {reference(), hash_function()}.
nif_wrapper(List, HashFunction, FilterType) ->

   case filter_selector(List, FilterType) of
      
      {error, Reason} ->
         {error, Reason};

      Reference ->
         {Reference, HashFunction}
   end.


%%-----------------------------------------------------------------------------
%% @doc Internal function that determines if the nif should be dirty scheduled 
%% or not, if above 10K records.  Also chooses what filter should be init'd.
%% @end
%%-----------------------------------------------------------------------------
-spec filter_selector(list(), atom()) -> reference() | {error, atom()}.

filter_selector(List, xor8) ->

   case over_10_thousand(List) of
      true ->
         xor8_initialize_nif_dirty(List);
      _ ->
         xor8_initialize_nif(List)
   end;

filter_selector(List, xor8_buffered) ->

   case over_10_thousand(List) of
      true ->
         xor8_buffered_initialize_nif_dirty(List);
      _ ->
         xor8_buffered_initialize_nif(List)
   end;

filter_selector(List, xor16) ->

   case over_10_thousand(List) of
      true ->
         xor16_initialize_nif_dirty(List);
      _ ->
         xor16_initialize_nif(List)
   end;

filter_selector(List, xor16_buffered) ->

   case over_10_thousand(List) of
      true ->
         xor16_buffered_initialize_nif_dirty(List);
      _ ->
         xor16_buffered_initialize_nif(List)
   end.


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Initializes the xor filter on a passed list.  
%% If the list isn't a list of 64 unsigned numbers, an error will be thrown.
%% 
%% Returns a `Ref<>' to a filter to be used in `contain'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_initialize_nif(list()) -> reference() | {error, atom()}.

xor8_initialize_nif(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Initializes the xor filter on a passed list, with a dirty
%% scheduler.
%% If the list isn't a list of 64 unsigned numbers, an error will be thrown.
%% 
%% Returns a `Ref<>' to a filter to be used in `contain'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_initialize_nif_dirty(list()) -> reference() | {error, atom()}.

xor8_initialize_nif_dirty(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Similar to the initialize function, but is a buffered 
%% version for lists
%%
%% Returns a `Ref<>' to a filter to be used in `contain'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_buffered_initialize_nif(list()) -> reference() | {error, atom()}.

xor8_buffered_initialize_nif(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Similar to the initialize function, but is a buffered, dirty
%% version for large lists.
%%
%% Returns a `Ref<>' to a filter to be used in `contain'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_buffered_initialize_nif_dirty(list()) -> reference() | {error, atom()}.

xor8_buffered_initialize_nif_dirty(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Fully initializes the filter, and frees the data.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_finalize({builder, reference()}) -> {reference(), hash_function()} | {error, atom()}.

xor8_finalize({builder, Filter}) ->
   {xor8_finalize_nif(Filter), default_hash}.


%%-----------------------------------------------------------------------------
%% @doc Nif api.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_finalize_nif(reference()) -> reference() | {error, atom()}.

xor8_finalize_nif(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%%
%% DO NOT PASS PRE-HASHED VALUES unless you've specified a pre-hashed filter.  
%% The method / fun passed to the initialization function is saved, and 
%% is used to compute the hash.
%%
%% Filters previously serialized with `xor8_to_bin' are allowed.
%%
%% Returns true if the element exists (or if there is a false positive).
%% False if not.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_contain({reference() | binary(), hash_function()} | {builder, reference()}, term()) 
   -> true | false | {error, unfinalized_filter_error}.

xor8_contain({builder, _Filter}, _Key) ->
   {error, unfinalized_filter_error};

xor8_contain({Filter, default_hash}, Key) ->
    xor8_contain_nif(Filter, erlang:phash2(Key));

xor8_contain({Filter, HashFunction}, Key) when is_function(HashFunction) ->
   xor8_contain_nif(Filter, HashFunction(Key));

xor8_contain({Filter, _HashFunction}, Key) ->
   xor8_contain_nif(Filter, Key);

xor8_contain(_BadValue, _Key) ->
   {error, get_key_for_contains_error}.

%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%%
%% Filters previously serialized with `xor8_to_bin' are allowed.
%%
%% Returns `true' if the element exists (or there is a false positive).
%% The third argument will be returned instead of `false' if the element is
%% not in the filter.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_contain({reference() | binary(), hash_function()} | {builder, reference()}, term(), any())
   -> true | any() | {error, unfinalized_filter_error}.

xor8_contain({builder, _Filter}, _Key, _ReturnValue) ->
   {error, unfinalized_filter_error};

xor8_contain({Filter, default_hash}, Key, ReturnValue) ->
    xor8_contain(Filter, erlang:phash2(Key), ReturnValue);

xor8_contain({Filter, HashFunction}, Key, ReturnValue) 
   when is_function(HashFunction) ->
   
   HashedKey = HashFunction(Key),
   xor8_contain(Filter, HashedKey, ReturnValue);

xor8_contain(Filter, Key, ReturnValue) ->

   case xor8_contain_nif(Filter, Key) of

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
-spec xor8_contain_nif(reference() | binary(), term()) -> true | false.

xor8_contain_nif(_, _) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Serialize the filter to a binary
%%
%% Returns `binary()'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_to_bin({reference(), hash_function()}) -> {binary(), hash_function()}.

xor8_to_bin({Filter, Hash}) ->
    {xor8_to_bin_nif(Filter), Hash}.

%% @private
xor8_to_bin_nif(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Deserialize the filter from a previous `xor8_to_bin' call.
%%
%% Returns `{reference(), hash_function()}'
%% @end
%%-----------------------------------------------------------------------------
-spec xor8_from_bin({binary(), hash_function()}) -> {reference(), hash_function()}.

xor8_from_bin({Bin, Hash}) ->
    {xor8_from_bin_nif(Bin), Hash}.

%% @private
xor8_from_bin_nif(_) ->
   not_loaded(?LINE).

%%-----------------------------------------------------------------------------
%% @doc Nif api.  Initializes the xor filter on a passed list.  
%% If the list isn't a list of 64 unsigned numbers, an error will be thrown.
%% 
%% Returns a `Ref<>' to a filter to be used in `contain''.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_initialize_nif(list()) -> reference() | {error, atom()}.

xor16_initialize_nif(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Initializes the xor filter on a passed list, dirty version.
%% If the list isn't a list of 64 unsigned numbers, an error will be thrown.
%% 
%% Returns a `Ref<>' to a filter to be used in `contain'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_initialize_nif_dirty(list()) -> reference() | {error, atom()}.

xor16_initialize_nif_dirty(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Similar to the initialize function, but is a buffered 
%% version for lists
%%
%% Returns a `Ref<>' to a filter to be used in `contain'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_buffered_initialize_nif(list()) -> reference() | {error, atom()}.

xor16_buffered_initialize_nif(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Nif api.  Similar to the initialize function, but is a buffered , dirty
%% version for lists
%%
%% Returns a `Ref<>' to a filter to be used in `contain'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_buffered_initialize_nif_dirty(list()) -> reference() | {error, atom()}.

xor16_buffered_initialize_nif_dirty(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Fully initializes the filter, and frees the data.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_finalize({builder, reference()}) -> {reference(), hash_function()} | {error, atom()}.

xor16_finalize({builder, Filter}) ->
   {xor16_finalize_nif(Filter), default_hash}.


%%-----------------------------------------------------------------------------
%% @doc Nif api.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_finalize_nif(reference()) -> reference() | {error, atom()}.

xor16_finalize_nif(_) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%%
%% DO NOT PASS PRE-HASHED VALUES.  The method / fun passed to the 
%% initialization function is saved, and is used to compute the hash.
%%
%% Filters previously serialized with `xor16_to_bin' are allowed.
%%
%% Returns true if the element exists (or if there is a false positive).
%% False if not.
%% @end
-spec xor16_contain({reference() | binary(), hash_function()} | {builder, reference()}, term()) 
   -> true | false | {error, unfinalized_filter_error}.

xor16_contain({builder, _filter}, _Key) ->
   {error, unfinalized_filter_error};

xor16_contain({Filter, default_hash}, Key) ->
    xor16_contain_nif(Filter, erlang:phash2(Key));

xor16_contain({Filter, HashFunction}, Key) when is_function(HashFunction) ->
   xor16_contain_nif(Filter, HashFunction(Key));

xor16_contain({Filter, _HashFunction}, Key) ->
   xor16_contain_nif(Filter, Key);

xor16_contain(_BadValue, _Key) ->
   {error, get_key_for_contains_error}.


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter.
%%
%% Filters previously serialized with `xor16_to_bin' are allowed.
%%
%% Returns `true' if the element exists (or there is a false positive).
%% The third argument will be returned instead of `false' if the element is
%% not in the filter.
%% @end
-spec xor16_contain({reference() | binary(), hash_function()} | {builder, reference()}, term(), any())
   -> true | any() | {error, unfinalized_filter_error}.

xor16_contain({builder, _Filter}, _Key, _ReturnValue) ->
   {error, unfinalized_filter_error};

xor16_contain({Filter, default_hash}, Key, ReturnValue) ->
    xor16_contain(Filter, erlang:phash2(Key), ReturnValue);

xor16_contain({Filter, HashFunction}, Key, ReturnValue) 
   when is_function(HashFunction) ->
   
   HashedKey = HashFunction(Key),
   xor16_contain(Filter, HashedKey, ReturnValue);

xor16_contain(Filter, Key, ReturnValue) ->

   case xor16_contain_nif(Filter, Key) of

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
-spec xor16_contain_nif(reference() | binary(), term()) -> true | false.

xor16_contain_nif(_, _) ->
   not_loaded(?LINE).


%%-----------------------------------------------------------------------------
%% @doc Serialize the filter to a binary
%%
%% Returns `binary()'.
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_to_bin({reference(), hash_function()}) -> {binary(), hash_function()}.

xor16_to_bin({Filter, Hash}) ->
    {xor16_to_bin_nif(Filter), Hash}.

%% @private
xor16_to_bin_nif(_) ->
   not_loaded(?LINE).

%%-----------------------------------------------------------------------------
%% @doc Deserialize the filter from a previous `xor16_to_bin' call.
%%
%% Returns `{reference(), hash_function()}'
%% @end
%%-----------------------------------------------------------------------------
-spec xor16_from_bin({binary(), hash_function()}) -> {reference(), hash_function()}.

xor16_from_bin({Bin, Hash}) ->
    {xor16_from_bin_nif(Bin), Hash}.

%% @private
xor16_from_bin_nif(_) ->
   not_loaded(?LINE).


over_10_thousand(List) ->
   over_10_thousand(List, 0).

over_10_thousand(_, 10001) ->
   true;

over_10_thousand([], Counter) when Counter =< 10000 ->
   false;

over_10_thousand([_|L], Counter) ->
   over_10_thousand(L, Counter + 1).

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
   erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
