-module(exor_filter_test).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->

   %% Expensive pre-computation.
   Ints = lists:seq(1, 80000),
   MappingFun = fun(X) -> "test" ++ integer_to_list(X) end,
   Strings = lists:map(MappingFun, Ints),

   [
      {
         "xor8 Test Group",
         [?_test(xor8_filter()),
          ?_test(xor8_buffered_filter()),
          ?_test(xor8_non_uint64()),
          ?_test(xor8_non_uint64_buffered()),
          ?_test(xor8_invalid_pre_defined_hash()),
          ?_test(xor8_invalid_pre_defined_hash_buffered()),
          ?_test(xor8_dup_in_hash()),
          ?_test(xor8_dup_in_pre_hash()),
          ?_test(xor8_valid_hash()),
          ?_test(xor8_valid_hash_buffered()),
          ?_test(xor8_wrong_hash_arity()),
          ?_test(xor8_wrong_hash_arity_buffered()),
          ?_test(xor8_hash_does_not_return_uint64()),
          ?_test(xor8_hash_does_not_return_uint64_buffered()),
          ?_test(xor8_custom_contain_return()),
          ?_test(xor8_contain_hash_function_custom_return()),
          ?_test(xor8_contain_key_not_uint64()),
          ?_test(xor8_contain_custom_key_not_uint64()),
          ?_test(xor8_valid_filter_in_contain()),
          ?_test(xor8_large()),
          ?_test(xor8_large_buffered()),
          ?_test(xor8_medium_default(Strings)),
          ?_test(xor8_medium_default_buffered(Strings))]
      },
      {
         "xor16 Test Group",
         [?_test(xor16_filter()),
          ?_test(xor16_buffered_filter()),
          ?_test(xor16_non_uint64()),
          ?_test(xor16_non_uint64_buffered()),
          ?_test(xor16_invalid_pre_defined_hash()),
          ?_test(xor16_invalid_pre_defined_hash_buffered()),
          ?_test(xor16_dup_in_hash()),
          ?_test(xor16_dup_in_pre_hash()),
          ?_test(xor16_valid_hash()),
          ?_test(xor16_valid_hash_buffered()),
          ?_test(xor16_wrong_hash_arity()),
          ?_test(xor16_wrong_hash_arity_buffered()),
          ?_test(xor16_hash_does_not_return_uint64()),
          ?_test(xor16_hash_does_not_return_uint64_buffered()),
          ?_test(xor16_custom_contain_return()),
          ?_test(xor16_contain_hash_function_custom_return()),
          ?_test(xor16_contain_key_not_uint64()),
          ?_test(xor16_contain_custom_key_not_uint64()),
          ?_test(xor16_valid_filter_in_contain()),
          ?_test(xor16_large()),
          ?_test(xor16_large_buffered()),
          ?_test(xor16_medium_default(Strings)),
          ?_test(xor16_medium_default_buffered(Strings))]
      }
   ].


%% Begin xor8 tests.
xor8_filter() ->
   Filter = xor8:new(["test1", "test2", "test3"]),
   ?_assertEqual(true, xor8:contain(Filter, "test1")),
   ?_assertEqual(false, xor8:contain(Filter, "test4")).

xor8_buffered_filter() ->
   Filter = xor8:new_buffered(["test1", "test2", "test3"]),
   ?_assertEqual(true, xor8:contain(Filter, "test2")),
   ?_assertEqual(false, xor8:contain(Filter, "test6")).

xor8_non_uint64() ->
   ?assertEqual({error, convert_to_uint64_t_error}, 
      xor8:new(["test"], none)).

xor8_non_uint64_buffered() ->
   ?assertEqual({error, convert_to_uint64_t_error}, 
      xor8:new_buffered(["test"], none)).

xor8_invalid_pre_defined_hash() ->
   ?_assertEqual({error, invalid_hash_method},
      xor8:new(["test"], fake_hash)).

xor8_invalid_pre_defined_hash_buffered() ->
   ?_assertEqual({error, invalid_hash_method},
      xor8:new(["test"], fake_hash)).

xor8_dup_in_hash() ->
   Fun = fun(_X) -> 1 end,
   ?_assertEqual({error, duplicates_in_hash_error},
      xor8:new([1, 2], Fun)).

xor8_dup_in_pre_hash() ->
   ?_assertEqual({error, duplicates_in_hash_error},
      xor8:new([1, 1], none)).

xor8_valid_hash() ->
   Fun = fun(X) -> X + 1 end,
   Filter = xor8:new([1, 2, 3], Fun),
   ?_assertEqual(true, xor8:contain(Filter, 4)),
   ?_assertEqual(false, xor8:contain(Filter, 1)).

xor8_valid_hash_buffered() ->
   Fun = fun(X) -> X + 1 end,
   Filter = xor8:new_buffered([1, 2, 3], Fun),
   ?_assertEqual(true, xor8:contain(Filter, 4)),
   ?_assertEqual(false, xor8:contain(Filter, 1)).

xor8_wrong_hash_arity() ->
   Fun = fun(X, Y) -> X + Y end,
   ?_assertEqual({error, wrong_arity_hash_function_error},
      xor8:new([1, 2, 3], Fun)).

xor8_wrong_hash_arity_buffered() ->
   Fun = fun(X, Y) -> X + Y end,
   ?_assertEqual({error, wrong_arity_hash_function_error},
      xor8:new_buffered([1, 2, 3], Fun)).

xor8_hash_does_not_return_uint64() ->
   Fun = fun(_X) -> "test" end,
   ?_assertEqual({error, convert_to_uint64_t_error},
      xor8:new([1, 2, 3], Fun)).

xor8_hash_does_not_return_uint64_buffered() ->
   Fun = fun(_X) -> "test" end,
   ?_assertEqual({error, convert_to_uint64_t_error},
      xor8:new_buffered([1, 2, 3], Fun)).

xor8_custom_contain_return() ->
   Filter = xor8:new([1, 2, 3]),
   ?_assertEqual(true, xor8:contain(Filter, 2, asdf)),
   ?_assertEqual(asdf, xor8:contain(Filter, 6, asdf)).

xor8_contain_hash_function_custom_return() ->
   Fun = fun(X) -> X + 1 end,
   Filter = xor8:new([1, 2, 3], Fun),
   ?_assertEqual(true, xor8:contain(Filter, 2)),
   ?_assertEqual({error, reason}, 
      xor8:contain(Filter, 1, {error, reason})).

xor8_contain_key_not_uint64() ->
   Filter = xor8:new([1, 2, 3], none),
   ?_assertEqual({error, get_key_for_contains_error}, 
      xor8:contain(Filter, "test")).

xor8_contain_custom_key_not_uint64() ->
   Filter = xor8:new([1, 2, 3]),
   ?_assertEqual({error, get_key_for_contains_error}, 
      xor8:contain(Filter, "test", asdf)).

xor8_valid_filter_in_contain() ->
   ?_assertEqual({error, get_key_for_contains_error}, 
      xor8:contain(asdf, 1)).

xor8_large() ->
   X = lists:seq(1, 10000000),
   Filter = xor8:new(X, none),
   ?_assertEqual(true, xor8:contain(Filter, 100)).

xor8_large_buffered() ->
   X = lists:seq(1, 10000000),
   Filter = xor8:new_buffered(X, none),
   ?_assertEqual(true, xor8:contain(Filter, 100)).

xor8_medium_default(Strings) ->
   Filter = xor8:new(Strings),
   ?_assertEqual(true, xor8:contain(Filter, "test100")).

xor8_medium_default_buffered(Strings) ->
   Filter = xor8:new_buffered(Strings),
   ?_assertEqual(true, xor8:contain(Filter, "test100")).

%% Begin xor16 tests.
xor16_filter() ->
   Filter = xor16:new(["test1", "test2", "test3"]),
   ?_assertEqual(true, xor16:contain(Filter, "test1")),
   ?_assertEqual(false, xor16:contain(Filter, "test4")).

xor16_buffered_filter() ->
   Filter = xor16:new_buffered(["test1", "test2", "test3"]),
   ?_assertEqual(true, xor16:contain(Filter, "test2")),
   ?_assertEqual(false, xor16:contain(Filter, "test6")).

xor16_non_uint64() ->
   ?assertEqual({error, convert_to_uint64_t_error}, 
      xor16:new(["test"], none)).

xor16_non_uint64_buffered() ->
   ?assertEqual({error, convert_to_uint64_t_error}, 
      xor16:new_buffered(["test"], none)).

xor16_invalid_pre_defined_hash() ->
   ?_assertEqual({error, invalid_hash_method},
      xor16:new(["test"], fake_hash)).

xor16_invalid_pre_defined_hash_buffered() ->
   ?_assertEqual({error, invalid_hash_method},
      xor16:new(["test"], fake_hash)).

xor16_dup_in_hash() ->
   Fun = fun(X) -> 1 end,
   ?_assertEqual({error, duplicates_in_hash_error},
      xor16:new([1, 2], Fun)).

xor16_dup_in_pre_hash() ->
   ?_assertEqual({error, duplicates_in_hash_error},
      xor16:new([1, 1], none)).

xor16_valid_hash() ->
   Fun = fun(X) -> X + 1 end,
   Filter = xor16:new([1, 2, 3], Fun),
   ?_assertEqual(true, xor16:contain(Filter, 4)),
   ?_assertEqual(false, xor16:contain(Filter, 1)).

xor16_valid_hash_buffered() ->
   Fun = fun(X) -> X + 1 end,
   Filter = xor16:new_buffered([1, 2, 3], Fun),
   ?_assertEqual(true, xor16:contain(Filter, 4)),
   ?_assertEqual(false, xor16:contain(Filter, 1)).

xor16_wrong_hash_arity() ->
   Fun = fun(X, Y) -> X + Y end,
   ?_assertEqual({error, wrong_arity_hash_function_error},
      xor16:new([1, 2, 3], Fun)).

xor16_wrong_hash_arity_buffered() ->
   Fun = fun(X, Y) -> X + Y end,
   ?_assertEqual({error, wrong_arity_hash_function_error},
      xor16:new_buffered([1, 2, 3], Fun)).

xor16_hash_does_not_return_uint64() ->
   Fun = fun(_X) -> "test" end,
   ?_assertEqual({error, convert_to_uint64_t_error},
      xor16:new([1, 2, 3], Fun)).

xor16_hash_does_not_return_uint64_buffered() ->
   Fun = fun(_X) -> "test" end,
   ?_assertEqual({error, convert_to_uint64_t_error},
      xor16:new_buffered([1, 2, 3], Fun)).

xor16_custom_contain_return() ->
   Filter = xor16:new([1, 2, 3]),
   ?_assertEqual(true, xor16:contain(Filter, 2, asdf)),
   ?_assertEqual(asdf, xor16:contain(Filter, 6, asdf)).

xor16_contain_hash_function_custom_return() ->
   Fun = fun(X) -> X + 1 end,
   Filter = xor16:new([1, 2, 3], Fun),
   ?_assertEqual(true, xor16:contain(Filter, 2)),
   ?_assertEqual({error, reason}, 
      xor16:contain(Filter, 1, {error, reason})).

xor16_contain_key_not_uint64() ->
   Filter = xor16:new([1, 2, 3], none),
   ?_assertEqual({error, get_key_for_contains_error}, 
      xor16:contain(Filter, "test")).

xor16_contain_custom_key_not_uint64() ->
   Filter = xor16:new([1, 2, 3]),
   ?_assertEqual({error, get_key_for_contains_error}, 
      xor16:contain(Filter, "test", asdf)).

xor16_valid_filter_in_contain() ->
   ?_assertEqual({error, get_key_for_contains_error}, 
      xor16:contain(asdf, 1)).

xor16_large() ->
   X = lists:seq(1, 10000000),
   Filter = xor16:new(X, none),
   ?_assertEqual(true, xor16:contain(Filter, 100)).

xor16_large_buffered() ->
   X = lists:seq(1, 10000000),
   Filter = xor16:new_buffered(X, none),
   ?_assertEqual(true, xor16:contain(Filter, 100)).

xor16_medium_default(Strings) ->
   Filter = xor16:new(Strings),
   ?_assertEqual(true, xor16:contain(Filter, "test100")).

xor16_medium_default_buffered(Strings) ->
   Filter = xor16:new_buffered(Strings),
   ?_assertEqual(true, xor16:contain(Filter, "test100")).

%% EOF
