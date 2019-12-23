-module(exor_filter_test).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
   [
      {
         "xor8 Test Group",
         [?_test(xor8_base_test()),
          ?_test(xor8_buffered_initialize_test()),
          ?_test(xor8_non_uint64_test()),
          ?_test(xor8_non_uint64_buffered_test()),
          ?_test(xor8_valid_hash_test()),
          ?_test(xor8_valid_hash_buffered_test()),
          ?_test(xor8_wrong_hash_arity_test()),
          ?_test(xor8_wrong_hash_arity_buffered_test()),
          ?_test(xor8_hash_does_not_return_uint64_test()),
          ?_test(xor8_hash_does_not_return_uint64_buffered_test()),
          ?_test(xor8_custom_contain_return_test()),
          ?_test(xor8_contain_key_not_uint64()),
          ?_test(xor8_contain_custom_key_not_uint64()),
          ?_test(xor8_valid_filter_in_contain_test()),
          ?_test(xor8_valid_filter_in_free_test()),
          ?_test(xor8_cannot_free_twice_test())
         ]
      },
      {
         "xor16 Test Group",
         [?_test(xor16_base_test()),
          ?_test(xor16_buffered_initialize_test()),
          ?_test(xor16_non_uint64_test()),
          ?_test(xor16_non_uint64_buffered_test()),
          ?_test(xor16_valid_hash_test()),
          ?_test(xor16_valid_hash_buffered_test()),
          ?_test(xor16_wrong_hash_arity_test()),
          ?_test(xor16_wrong_hash_arity_buffered_test()),
          ?_test(xor16_hash_does_not_return_uint64_test()),
          ?_test(xor16_hash_does_not_return_uint64_buffered_test()),
          ?_test(xor16_custom_contain_return_test()),
          ?_test(xor16_contain_key_not_uint64()),
          ?_test(xor16_contain_custom_key_not_uint64()),
          ?_test(xor16_valid_filter_in_contain_test()),
          ?_test(xor16_valid_filter_in_free_test()),
          ?_test(xor16_cannot_free_twice_test())]
      }
   ].


%% Begin xor8 tests.
xor8_base_test() ->
   Filter = exor_filter:xor8_initialize([1, 2, 3]),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, 2)),
   ?_assertMatch(false, exor_filter:xor8_contain(Filter, 6)),
   exor_filter:xor8_free(Filter).

xor8_buffered_initialize_test() ->
   Filter = exor_filter:xor8_buffered_initialize([1, 2, 3]),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, 2)),
   ?_assertMatch(false, exor_filter:xor8_contain(Filter, 6)),
   exor_filter:xor8_free(Filter).

xor8_non_uint64_test() ->
   ?assertMatch({error, convert_to_uint64_t_error}, 
      exor_filter:xor8_initialize(["test"])).

xor8_non_uint64_buffered_test() ->
   ?assertMatch({error, convert_to_uint64_t_error}, 
      exor_filter:xor8_buffered_initialize(["test"])).

xor8_valid_hash_test() ->
   Fun = fun(X) -> X + 1 end,
   Filter = exor_filter:xor8_initialize([1, 2, 3], Fun),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, 4)),
   ?_assertMatch(false, exor_filter:xor8_contain(Filter, 1)),
   exor_filter:xor8_free(Filter).

xor8_valid_hash_buffered_test() ->
   Fun = fun(X) -> X + 1 end,
   Filter = exor_filter:xor8_buffered_initialize([1, 2, 3], Fun),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, 4)),
   ?_assertMatch(false, exor_filter:xor8_contain(Filter, 1)),
   exor_filter:xor8_free(Filter).

xor8_wrong_hash_arity_test() ->
   Fun = fun(X, Y) -> X + Y end,
   ?_assertMatch({error, wrong_arity_hash_function_error},
      exor_filter:xor8_initialize([1, 2, 3], Fun)).

xor8_wrong_hash_arity_buffered_test() ->
   Fun = fun(X, Y) -> X + Y end,
   ?_assertMatch({error, wrong_arity_hash_function_error},
      exor_filter:xor8_buffered_initialize([1, 2, 3], Fun)).

xor8_hash_does_not_return_uint64_test() ->
   Fun = fun(_X) -> "test" end,
   ?_assertMatch({error, convert_to_uint64_t_error},
      exor_filter:xor8_initialize([1, 2, 3], Fun)).

xor8_hash_does_not_return_uint64_buffered_test() ->
   Fun = fun(_X) -> "test" end,
   ?_assertMatch({error, convert_to_uint64_t_error},
      exor_filter:xor8_buffered_initialize([1, 2, 3], Fun)).

xor8_custom_contain_return_test() ->
   Filter = exor_filter:xor8_initialize([1, 2, 3]),
   ?_assertMatch(true, exor_filter:xor8_contain(Filter, 2, asdf)),
   ?_assertMatch(asdf, exor_filter:xor8_contain(Filter, 6, asdf)),
   exor_filter:xor8_free(Filter).

xor8_contain_key_not_uint64() ->
   Filter = exor_filter:xor8_initialize([1, 2, 3]),
   ?_assertMatch({error, get_key_for_contains_error}, 
      exor_filter:xor8_contain(Filter, "test")),
   exor_filter:xor8_free(Filter).

xor8_contain_custom_key_not_uint64() ->
   Filter = exor_filter:xor8_initialize([1, 2, 3]),
   ?_assertMatch({error, get_key_for_contains_error}, 
      exor_filter:xor8_contain(Filter, "test", asdf)),
   exor_filter:xor8_free(Filter).

xor8_valid_filter_in_contain_test() ->
   ?_assertMatch({error, get_key_for_contains_error}, 
      exor_filter:xor8_contain(asdf, 1)).

xor8_valid_filter_in_free_test() ->
   ?_assertMatch(ok, exor_filter:xor8_free(asdf)).

xor8_cannot_free_twice_test() ->
   Filter = exor_filter:xor8_initialize([1, 2, 3]),
   ?_assertMatch(ok, exor_filter:xor8_free(Filter)),
   ?_assertMatch(ok, exor_filter:xor8_free(Filter)).

%% Begin xor16 tests.
xor16_base_test() ->
   Filter = exor_filter:xor16_initialize([1, 2, 3]),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, 2)),
   ?_assertMatch(false, exor_filter:xor16_contain(Filter, 6)),
   exor_filter:xor16_free(Filter).

xor16_buffered_initialize_test() ->
   Filter = exor_filter:xor16_buffered_initialize([1, 2, 3]),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, 2)),
   ?_assertMatch(false, exor_filter:xor16_contain(Filter, 6)),
   exor_filter:xor16_free(Filter).

xor16_non_uint64_test() ->
   ?assertMatch({error, convert_to_uint64_t_error}, 
      exor_filter:xor16_initialize(["test"])).

xor16_non_uint64_buffered_test() ->
   ?assertMatch({error, convert_to_uint64_t_error}, 
      exor_filter:xor16_buffered_initialize(["test"])).

xor16_valid_hash_test() ->
   Fun = fun(X) -> X + 1 end,
   Filter = exor_filter:xor16_initialize([1, 2, 3], Fun),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, 4)),
   ?_assertMatch(false, exor_filter:xor16_contain(Filter, 1)),
   exor_filter:xor16_free(Filter).

xor16_valid_hash_buffered_test() ->
   Fun = fun(X) -> X + 1 end,
   Filter = exor_filter:xor16_buffered_initialize([1, 2, 3], Fun),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, 4)),
   ?_assertMatch(false, exor_filter:xor16_contain(Filter, 1)),
   exor_filter:xor16_free(Filter).

xor16_wrong_hash_arity_test() ->
   Fun = fun(X, Y) -> X + Y end,
   ?_assertMatch({error, wrong_arity_hash_function_error},
      exor_filter:xor16_initialize([1, 2, 3], Fun)).

xor16_wrong_hash_arity_buffered_test() ->
   Fun = fun(X, Y) -> X + Y end,
   ?_assertMatch({error, wrong_arity_hash_function_error},
      exor_filter:xor16_buffered_initialize([1, 2, 3], Fun)).

xor16_hash_does_not_return_uint64_test() ->
   Fun = fun(_X) -> "test" end,
   ?_assertMatch({error, convert_to_uint64_t_error},
      exor_filter:xor16_initialize([1, 2, 3], Fun)).

xor16_hash_does_not_return_uint64_buffered_test() ->
   Fun = fun(_X) -> "test" end,
   ?_assertMatch({error, convert_to_uint64_t_error},
      exor_filter:xor16_buffered_initialize([1, 2, 3], Fun)).

xor16_custom_contain_return_test() ->
   Filter = exor_filter:xor16_initialize([1, 2, 3]),
   ?_assertMatch(true, exor_filter:xor16_contain(Filter, 2, asdf)),
   ?_assertMatch(asdf, exor_filter:xor16_contain(Filter, 6, asdf)),
   exor_filter:xor16_free(Filter).

xor16_contain_key_not_uint64() ->
   Filter = exor_filter:xor16_initialize([1, 2, 3]),
   ?_assertMatch({error, get_key_for_contains_error}, 
      exor_filter:xor16_contain(Filter, "test")),
   exor_filter:xor16_free(Filter).

xor16_contain_custom_key_not_uint64() ->
   Filter = exor_filter:xor16_initialize([1, 2, 3]),
   ?_assertMatch({error, get_key_for_contains_error}, 
      exor_filter:xor16_contain(Filter, "test", asdf)),
   exor_filter:xor16_free(Filter).

xor16_valid_filter_in_contain_test() ->
   ?_assertMatch({error, get_key_for_contains_error}, 
      exor_filter:xor16_contain(asdf, 1)).

xor16_valid_filter_in_free_test() ->
   ?_assertMatch(ok, exor_filter:xor16_free(asdf)).

xor16_cannot_free_twice_test() ->
   Filter = exor_filter:xor16_initialize([1, 2, 3]),
   ?_assertMatch(ok, exor_filter:xor16_free(Filter)),
   ?_assertMatch(ok, exor_filter:xor16_free(Filter)).
