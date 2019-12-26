-module(exor_filter_test).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->

   %% Expensive pre-computation.
   Ints = lists:seq(1, 20000),
   MappingFun = fun(X) -> "test" ++ integer_to_list(X) end,
   Strings = lists:map(MappingFun, Ints),

   [
      {
         "xor8 Test Group",
         [?_test(xor8_filter()),
          ?_test(xor8_buffered_filter()),
          ?_test(xor8_non_uint64()),
          ?_test(xor8_non_uint64_buffered()),
          ?_test(xor8_fast_hash()),
          ?_test(xor8_fast_buffered_hash()),
          ?_test(xor8_invalid_pre_defined_hash()),
          ?_test(xor8_invalid_pre_defined_hash_buffered()),
          ?_test(xor8_valid_hash()),
          ?_test(xor8_valid_hash_buffered()),
          ?_test(xor8_wrong_hash_arity()),
          ?_test(xor8_wrong_hash_arity_buffered()),
          ?_test(xor8_hash_does_not_return_uint64()),
          ?_test(xor8_hash_does_not_return_uint64_buffered()),
          ?_test(xor8_custom_contain_return()),
          ?_test(xor8_contain_key_not_uint64()),
          ?_test(xor8_contain_custom_key_not_uint64()),
          ?_test(xor8_valid_filter_in_contain()),
          ?_test(xor8_valid_filter_in_free()),
          ?_test(xor8_cannot_free_twice()),
          ?_test(xor8_large()),
          ?_test(xor8_large_buffered()),
          ?_test(xor8_medium_fast(Strings)),
          ?_test(xor8_medium_fast_buffered(Strings)),
          ?_test(xor8_medium_default(Strings)),
          ?_test(xor8_medium_default_buffered(Strings))]
      },
      {
         "xor16 Test Group",
         [?_test(xor16_filter()),
          ?_test(xor16_buffered_filter()),
          ?_test(xor16_non_uint64()),
          ?_test(xor16_non_uint64_buffered()),
          ?_test(xor16_fast_hash()),
          ?_test(xor16_fast_buffered_hash()),
          ?_test(xor16_invalid_pre_defined_hash()),
          ?_test(xor16_invalid_pre_defined_hash_buffered()),
          ?_test(xor16_valid_hash()),
          ?_test(xor16_valid_hash_buffered()),
          ?_test(xor16_wrong_hash_arity()),
          ?_test(xor16_wrong_hash_arity_buffered()),
          ?_test(xor16_hash_does_not_return_uint64()),
          ?_test(xor16_hash_does_not_return_uint64_buffered()),
          ?_test(xor16_custom_contain_return()),
          ?_test(xor16_contain_key_not_uint64()),
          ?_test(xor16_contain_custom_key_not_uint64()),
          ?_test(xor16_valid_filter_in_contain()),
          ?_test(xor16_valid_filter_in_free()),
          ?_test(xor16_cannot_free_twice()),
          ?_test(xor16_large()),
          ?_test(xor16_large_buffered()),
          ?_test(xor16_medium_fast(Strings)),
          ?_test(xor16_medium_fast_buffered(Strings)),
          ?_test(xor16_medium_default(Strings)),
          ?_test(xor16_medium_default_buffered(Strings))]
      }
   ].


%% Begin xor8 tests.
xor8_filter() ->
   Filter = exor_filter:xor8(["test1", "test2", "test3"]),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, "test1")),
   ?_assertMatch(false, exor_filter:xor8_contain(Filter, "test4")),
   exor_filter:xor8_free(Filter).

xor8_buffered_filter() ->
   Filter = exor_filter:xor8_buffered(["test1", "test2", "test3"]),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, "test2")),
   ?_assertMatch(false, exor_filter:xor8_contain(Filter, "test6")),
   exor_filter:xor8_free(Filter).

xor8_non_uint64() ->
   ?assertMatch({error, convert_to_uint64_t_error}, 
      exor_filter:xor8(["test"], none)).

xor8_non_uint64_buffered() ->
   ?assertMatch({error, convert_to_uint64_t_error}, 
      exor_filter:xor8_buffered(["test"], none)).

xor8_fast_hash() ->
   Filter = exor_filter:xor8(["test1", "test2", "test3"], fast_hash),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, "test1")),
   ?_assertMatch(false, exor_filter:xor8_contain(Filter, "test4")),
   exor_filter:xor8_free(Filter).

xor8_fast_buffered_hash() ->
   Filter = exor_filter:xor8(["test1", "test2", "test3"], fast_hash),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, "test1")),
   ?_assertMatch(false, exor_filter:xor8_contain(Filter, "test4")),
   exor_filter:xor8_free(Filter).

xor8_invalid_pre_defined_hash() ->
   ?_assertMatch({error, invalid_hash_method},
   exor_filter:xor8(["test"], fake_hash)).

xor8_invalid_pre_defined_hash_buffered() ->
   ?_assertMatch({error, invalid_hash_method},
   exor_filter:xor8(["test"], fake_hash)).

xor8_valid_hash() ->
   Fun = fun(X) -> X + 1 end,
   Filter = exor_filter:xor8([1, 2, 3], Fun),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, 4)),
   ?_assertMatch(false, exor_filter:xor8_contain(Filter, 1)),
   exor_filter:xor8_free(Filter).

xor8_valid_hash_buffered() ->
   Fun = fun(X) -> X + 1 end,
   Filter = exor_filter:xor8_buffered([1, 2, 3], Fun),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, 4)),
   ?_assertMatch(false, exor_filter:xor8_contain(Filter, 1)),
   exor_filter:xor8_free(Filter).

xor8_wrong_hash_arity() ->
   Fun = fun(X, Y) -> X + Y end,
   ?_assertMatch({error, wrong_arity_hash_function_error},
      exor_filter:xor8([1, 2, 3], Fun)).

xor8_wrong_hash_arity_buffered() ->
   Fun = fun(X, Y) -> X + Y end,
   ?_assertMatch({error, wrong_arity_hash_function_error},
      exor_filter:xor8_buffered([1, 2, 3], Fun)).

xor8_hash_does_not_return_uint64() ->
   Fun = fun(_X) -> "test" end,
   ?_assertMatch({error, convert_to_uint64_t_error},
      exor_filter:xor8([1, 2, 3], Fun)).

xor8_hash_does_not_return_uint64_buffered() ->
   Fun = fun(_X) -> "test" end,
   ?_assertMatch({error, convert_to_uint64_t_error},
      exor_filter:xor8_buffered([1, 2, 3], Fun)).

xor8_custom_contain_return() ->
   Filter = exor_filter:xor8([1, 2, 3]),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, 2, asdf)),
   ?_assertMatch(asdf, exor_filter:xor8_contain(Filter, 6, asdf)),
   exor_filter:xor8_free(Filter).

xor8_contain_key_not_uint64() ->
   Filter = exor_filter:xor8([1, 2, 3], none),
   ?_assertMatch({error, get_key_for_contains_error}, 
      exor_filter:xor8_contain(Filter, "test")),
   exor_filter:xor8_free(Filter).

xor8_contain_custom_key_not_uint64() ->
   Filter = exor_filter:xor8([1, 2, 3]),
   ?_assertMatch({error, get_key_for_contains_error}, 
      exor_filter:xor8_contain(Filter, "test", asdf)),
   exor_filter:xor8_free(Filter).

xor8_valid_filter_in_contain() ->
   ?_assertMatch({error, get_key_for_contains_error}, 
      exor_filter:xor8_contain(asdf, 1)).

xor8_valid_filter_in_free() ->
   ?_assertMatch(ok, exor_filter:xor8_free(asdf)).

xor8_cannot_free_twice() ->
   Filter = exor_filter:xor8([1, 2, 3]),
   ?_assertMatch(ok, exor_filter:xor8_free(Filter)),
   ?_assertMatch(ok, exor_filter:xor8_free(Filter)).

xor8_large() ->
   X = lists:seq(1, 10000000),
   Filter = exor_filter:xor8(X, none),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, 100)),
   exor_filter:xor8_free(Filter).

xor8_large_buffered() ->
   X = lists:seq(1, 10000000),
   Filter = exor_filter:xor8_buffered(X, none),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, 100)),
   exor_filter:xor8_free(Filter).

xor8_medium_fast(Strings) ->
   Filter = exor_filter:xor8(Strings, fast_hash),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, "test100")),
   exor_filter:xor8_free(Filter).

xor8_medium_fast_buffered(Strings) ->
   Filter = exor_filter:xor8_buffered(Strings, fast_hash),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, "test100")),
   exor_filter:xor8_free(Filter).

xor8_medium_default(Strings) ->
   Filter = exor_filter:xor8(Strings),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, "test100")),
   exor_filter:xor8_free(Filter).

xor8_medium_default_buffered(Strings) ->
   Filter = exor_filter:xor8_buffered(Strings),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, "test100")),
   exor_filter:xor8_free(Filter).

%% Begin xor16 tests.
xor16_filter() ->
   Filter = exor_filter:xor16(["test1", "test2", "test3"]),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, "test1")),
   ?_assertMatch(false, exor_filter:xor16_contain(Filter, "test4")),
   exor_filter:xor16_free(Filter).

xor16_buffered_filter() ->
   Filter = exor_filter:xor16_buffered(["test1", "test2", "test3"]),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, "test2")),
   ?_assertMatch(false, exor_filter:xor16_contain(Filter, "test6")),
   exor_filter:xor16_free(Filter).

xor16_non_uint64() ->
   ?assertMatch({error, convert_to_uint64_t_error}, 
      exor_filter:xor16(["test"], none)).

xor16_non_uint64_buffered() ->
   ?assertMatch({error, convert_to_uint64_t_error}, 
      exor_filter:xor16_buffered(["test"], none)).

xor16_fast_hash() ->
   Filter = exor_filter:xor16(["test1", "test2", "test3"], fast_hash),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, "test1")),
   ?_assertMatch(false, exor_filter:xor16_contain(Filter, "test4")),
   exor_filter:xor16_free(Filter).

xor16_fast_buffered_hash() ->
   Filter = exor_filter:xor16(["test1", "test2", "test3"], fast_hash),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, "test1")),
   ?_assertMatch(false, exor_filter:xor16_contain(Filter, "test4")),
   exor_filter:xor16_free(Filter).

xor16_invalid_pre_defined_hash() ->
   ?_assertMatch({error, invalid_hash_method},
   exor_filter:xor16(["test"], fake_hash)).

xor16_invalid_pre_defined_hash_buffered() ->
   ?_assertMatch({error, invalid_hash_method},
   exor_filter:xor16(["test"], fake_hash)).

xor16_valid_hash() ->
   Fun = fun(X) -> X + 1 end,
   Filter = exor_filter:xor16([1, 2, 3], Fun),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, 4)),
   ?_assertMatch(false, exor_filter:xor16_contain(Filter, 1)),
   exor_filter:xor16_free(Filter).

xor16_valid_hash_buffered() ->
   Fun = fun(X) -> X + 1 end,
   Filter = exor_filter:xor16_buffered([1, 2, 3], Fun),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, 4)),
   ?_assertMatch(false, exor_filter:xor16_contain(Filter, 1)),
   exor_filter:xor16_free(Filter).

xor16_wrong_hash_arity() ->
   Fun = fun(X, Y) -> X + Y end,
   ?_assertMatch({error, wrong_arity_hash_function_error},
      exor_filter:xor16([1, 2, 3], Fun)).

xor16_wrong_hash_arity_buffered() ->
   Fun = fun(X, Y) -> X + Y end,
   ?_assertMatch({error, wrong_arity_hash_function_error},
      exor_filter:xor16_buffered([1, 2, 3], Fun)).

xor16_hash_does_not_return_uint64() ->
   Fun = fun(_X) -> "test" end,
   ?_assertMatch({error, convert_to_uint64_t_error},
      exor_filter:xor16([1, 2, 3], Fun)).

xor16_hash_does_not_return_uint64_buffered() ->
   Fun = fun(_X) -> "test" end,
   ?_assertMatch({error, convert_to_uint64_t_error},
      exor_filter:xor16_buffered([1, 2, 3], Fun)).

xor16_custom_contain_return() ->
   Filter = exor_filter:xor16([1, 2, 3]),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, 2, asdf)),
   ?_assertMatch(asdf, exor_filter:xor16_contain(Filter, 6, asdf)),
   exor_filter:xor16_free(Filter).

xor16_contain_key_not_uint64() ->
   Filter = exor_filter:xor16([1, 2, 3], none),
   ?_assertMatch({error, get_key_for_contains_error}, 
      exor_filter:xor16_contain(Filter, "test")),
   exor_filter:xor16_free(Filter).

xor16_contain_custom_key_not_uint64() ->
   Filter = exor_filter:xor16([1, 2, 3]),
   ?_assertMatch({error, get_key_for_contains_error}, 
      exor_filter:xor16_contain(Filter, "test", asdf)),
   exor_filter:xor16_free(Filter).

xor16_valid_filter_in_contain() ->
   ?_assertMatch({error, get_key_for_contains_error}, 
      exor_filter:xor16_contain(asdf, 1)).

xor16_valid_filter_in_free() ->
   ?_assertMatch(ok, exor_filter:xor16_free(asdf)).

xor16_cannot_free_twice() ->
   Filter = exor_filter:xor16([1, 2, 3]),
   ?_assertMatch(ok, exor_filter:xor16_free(Filter)),
   ?_assertMatch(ok, exor_filter:xor16_free(Filter)).

xor16_large() ->
   X = lists:seq(1, 10000000),
   Filter = exor_filter:xor16(X, none),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, 100)),
   exor_filter:xor16_free(Filter).

xor16_large_buffered() ->
   X = lists:seq(1, 10000000),
   Filter = exor_filter:xor16_buffered(X, none),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, 100)),
   exor_filter:xor16_free(Filter).

xor16_medium_fast(Strings) ->
   Filter = exor_filter:xor16(Strings, fast_hash),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, "test100")),
   exor_filter:xor16_free(Filter).

xor16_medium_fast_buffered(Strings) ->
   Filter = exor_filter:xor16_buffered(Strings, fast_hash),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, "test100")),
   exor_filter:xor16_free(Filter).

xor16_medium_default(Strings) ->
   Filter = exor_filter:xor16(Strings),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, "test100")),
   exor_filter:xor16_free(Filter).

xor16_medium_default_buffered(Strings) ->
   Filter = exor_filter:xor16_buffered(Strings),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, "test100")),
   exor_filter:xor16_free(Filter).


%% EOF
