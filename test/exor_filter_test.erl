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
          ?_test(xor8_medium_default_buffered(Strings)),
          ?_test(xor8_serialization()),
          ?_test(xor8_incremental_builder()),
          ?_test(xor8_incremental_large_resize()),
          ?_test(xor8_cannot_use_incrament_in_contain()),
          ?_test(xor8_incremental_dups()),
          ?_test(xor8_incremental_dups2()),
          ?_test(xor8_incremental_dups_large())
      ]},
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
          ?_test(xor16_medium_default(Strings)),
          ?_test(xor16_medium_default_buffered(Strings)),
          ?_test(xor16_serialization()),
          ?_test(xor16_incremental_builder()),
          ?_test(xor16_incremental_large_resize()),
          ?_test(xor16_cannot_use_incrament_in_contain()),
          ?_test(xor16_large()),
          ?_test(xor16_large_buffered())
      ]}
   ].


%% Begin xor8 tests.
xor8_filter() ->
   Filter = xor8:new(["test1", "test2", "test3"]),
   ?assertEqual(true, xor8:contain(Filter, "test1")),
   ?assertEqual(false, xor8:contain(Filter, "test4")).

xor8_buffered_filter() ->
   Filter = xor8:new_buffered(["test1", "test2", "test3"]),
   ?assertEqual(true, xor8:contain(Filter, "test2")),
   ?assertEqual(false, xor8:contain(Filter, "test6")).

xor8_non_uint64() ->
   ?assertEqual({error, convert_to_uint64_t_error}, 
      xor8:new(["test"], none)).

xor8_non_uint64_buffered() ->
   ?assertEqual({error, convert_to_uint64_t_error}, 
      xor8:new_buffered(["test"], none)).

xor8_invalid_pre_defined_hash() ->
   ?assertEqual({error, invalid_hash_method},
      xor8:new(["test"], fake_hash)).

xor8_invalid_pre_defined_hash_buffered() ->
   ?assertEqual({error, invalid_hash_method},
      xor8:new(["test"], fake_hash)).

xor8_dup_in_hash() ->
   Fun = fun(_X) -> 1 end,
   ?assertEqual({error, duplicates_in_hash_error},
      xor8:new([1, 2], Fun)).

xor8_dup_in_pre_hash() ->
   ?assertEqual({error, duplicates_in_hash_error},
      xor8:new([1, 1], none)).

xor8_valid_hash() ->
   Fun = fun(X) -> X + 1 end,
   Filter = xor8:new([1, 2, 3], Fun),
   ?assertEqual(true, xor8:contain(Filter, 3)),
   ?assertEqual(false, xor8:contain(Filter, 4)).

xor8_valid_hash_buffered() ->
   Fun = fun(X) -> X + 1 end,
   Filter = xor8:new_buffered([1, 2, 3], Fun),
   ?assertEqual(true, xor8:contain(Filter, 3)),
   ?assertEqual(false, xor8:contain(Filter, 4)).

xor8_wrong_hash_arity() ->
   Fun = fun(X, Y) -> X + Y end,
   ?assertEqual({error, wrong_arity_hash_function_error},
      xor8:new([1, 2, 3], Fun)).

xor8_wrong_hash_arity_buffered() ->
   Fun = fun(X, Y) -> X + Y end,
   ?assertEqual({error, wrong_arity_hash_function_error},
      xor8:new_buffered([1, 2, 3], Fun)).

xor8_hash_does_not_return_uint64() ->
   Fun = fun(_X) -> "test" end,
   ?assertEqual({error, convert_to_uint64_t_error},
      xor8:new([1, 2, 3], Fun)).

xor8_hash_does_not_return_uint64_buffered() ->
   Fun = fun(_X) -> "test" end,
   ?assertEqual({error, convert_to_uint64_t_error},
      xor8:new_buffered([1, 2, 3], Fun)).

xor8_custom_contain_return() ->
   Filter = xor8:new([1, 2, 3]),
   ?assertEqual(true, xor8:contain(Filter, 2, asdf)),
   ?assertEqual(asdf, xor8:contain(Filter, 6, asdf)).

xor8_contain_hash_function_custom_return() ->
   Fun = fun(X) -> X + 1 end,
   Filter = xor8:new([1, 2, 3], Fun),
   ?assertEqual(true, xor8:contain(Filter, 2)),
   ?assertEqual({error, reason}, 
      xor8:contain(Filter, 0, {error, reason})).

xor8_contain_key_not_uint64() ->
   Filter = xor8:new([1, 2, 3], none),
   ?assertEqual({error, get_key_for_contains_error}, 
      xor8:contain(Filter, "test")).

xor8_contain_custom_key_not_uint64() ->
   Filter = xor8:new([1, 2, 3], none),
   ?assertEqual({error, get_key_for_contains_error}, 
      xor8:contain(Filter, "test", asdf)).

xor8_valid_filter_in_contain() ->
   ?assertEqual({error, get_key_for_contains_error}, 
      xor8:contain(asdf, 1)).

xor8_large() ->
   X = lists:seq(1, 10000000),
   Filter = xor8:new(X, none),
   ?assertEqual(true, xor8:contain(Filter, 100)).

xor8_large_buffered() ->
   X = lists:seq(1, 10000000),
   Filter = xor8:new_buffered(X, none),
   ?assertEqual(true, xor8:contain(Filter, 100)).

xor8_medium_default(Strings) ->
   Filter = xor8:new(Strings),
   ?assertEqual(true, xor8:contain(Filter, "test100")).

xor8_medium_default_buffered(Strings) ->
   Filter = xor8:new_buffered(Strings),
   ?assertEqual(true, xor8:contain(Filter, "test100")).

xor8_serialization() ->
   Filter = xor8:new(["test1", "test2", "test3"]),
   ?assertEqual(true, xor8:contain(Filter, "test1")),
   ?assertEqual(false, xor8:contain(Filter, "test4")),
   BinFilter = xor8:to_bin(Filter),
   ?assertEqual(true, xor8:contain(BinFilter, "test1")),
   ?assertEqual(false, xor8:contain(BinFilter, "test4")),
   Filter2 = xor8:from_bin(BinFilter),
   ?assertEqual(true, xor8:contain(Filter2, "test1")),
   ?assertEqual(false, xor8:contain(Filter2, "test4")).

xor8_incremental_builder() ->
   Filter0 = xor8:new_empty(),
   Filter1 = xor8:add(Filter0, [1, 2]),
   Filter2 = xor8:add(Filter1, [3, 4]),
   Filter3 = xor8:finalize(Filter2),
   ?assertEqual(true, xor8:contain(Filter3, 1)),
   ?assertEqual(true, xor8:contain(Filter3, 2)),
   ?assertEqual(true, xor8:contain(Filter3, 3)),
   ?assertEqual(true, xor8:contain(Filter3, 4)),
   ?assertEqual(false, xor8:contain(Filter3, 6)).

xor8_incremental_large_resize() ->
   Filter0 = xor8:new_empty(),
   Filter1 = xor8:add(Filter0, [1, 2]),
   Filter2 = xor8:add(Filter1, [3, 4, 5, 6, 7, 8, 9, 10]),
   Filter3 = xor8:finalize(Filter2),
   ?assertEqual(true, xor8:contain(Filter3, 10)),
   ?assertEqual(true, xor8:contain(Filter3, 9)),
   ?assertEqual(true, xor8:contain(Filter3, 8)),
   ?assertEqual(true, xor8:contain(Filter3, 7)),
   ?assertEqual(true, xor8:contain(Filter3, 6)),
   ?assertEqual(true, xor8:contain(Filter3, 5)),
   ?assertEqual(true, xor8:contain(Filter3, 4)),
   ?assertEqual(true, xor8:contain(Filter3, 3)),
   ?assertEqual(true, xor8:contain(Filter3, 2)),
   ?assertEqual(true, xor8:contain(Filter3, 1)).

xor8_cannot_use_incrament_in_contain() ->
   Filter0 = xor8:new_empty(),
   Filter1 = xor8:add(Filter0, [1, 2]),
   ?assertEqual({error, unfinalized_filter_error}, xor8:contain(Filter1, 1)).

xor8_incremental_dups() ->
   Filter0 = xor8:new_empty(),
   Filter1 = xor8:add(Filter0, [1, 2, 2, 3]),
   Filter2 = xor8:finalize(Filter1),
   ?assertEqual(true, xor8:contain(Filter2, 1)),
   ?assertEqual(true, xor8:contain(Filter2, 2)),
   ?assertEqual(true, xor8:contain(Filter2, 3)).

xor8_incremental_dups2() ->
   Filter0 = xor8:new_empty(),
   Filter1 = xor8:add(Filter0, [1, 2, 3]),
   Filter2 = xor8:add(Filter1, [2, 3]),
   Filter3 = xor8:finalize(Filter2),
   ?assertEqual(true, xor8:contain(Filter3, 1)),
   ?assertEqual(true, xor8:contain(Filter3, 2)),
   ?assertEqual(true, xor8:contain(Filter3, 3)).

xor8_incremental_dups_large() ->
   Filter0 = xor8:new_empty(),
   Filter1 = xor8:add(Filter0, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
   Filter2 = xor8:add(Filter1, [2, 3, 4, 5, 6, 7, 8, 9, 10, 11]),
   Filter3 = xor8:add(Filter2, [5, 6, 7, 8, 9, 10, 11, 13, 14, 15]),
   Filter4 = xor8:add(Filter3, [10, 11, 13, 14, 15, 16, 17, 18, 19, 20]),
   Filter5 = xor8:add(Filter4, [15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]),
   Filter6 = xor8:finalize(Filter5),
   ?assertEqual(true, xor8:contain(Filter6, 1)),
   ?assertEqual(true, xor8:contain(Filter6, 2)),
   ?assertEqual(true, xor8:contain(Filter6, 3)),
   ?assertEqual(true, xor8:contain(Filter6, 4)),
   ?assertEqual(true, xor8:contain(Filter6, 5)),
   ?assertEqual(true, xor8:contain(Filter6, 6)),
   ?assertEqual(true, xor8:contain(Filter6, 7)),
   ?assertEqual(true, xor8:contain(Filter6, 8)),
   ?assertEqual(true, xor8:contain(Filter6, 9)),
   ?assertEqual(true, xor8:contain(Filter6, 10)),
   ?assertEqual(true, xor8:contain(Filter6, 11)),
   ?assertEqual(true, xor8:contain(Filter6, 13)),
   ?assertEqual(true, xor8:contain(Filter6, 14)),
   ?assertEqual(true, xor8:contain(Filter6, 15)),
   ?assertEqual(true, xor8:contain(Filter6, 16)),
   ?assertEqual(true, xor8:contain(Filter6, 17)),
   ?assertEqual(true, xor8:contain(Filter6, 18)),
   ?assertEqual(true, xor8:contain(Filter6, 19)),
   ?assertEqual(true, xor8:contain(Filter6, 20)),
   ?assertEqual(true, xor8:contain(Filter6, 21)),
   ?assertEqual(true, xor8:contain(Filter6, 22)),
   ?assertEqual(true, xor8:contain(Filter6, 23)),
   ?assertEqual(true, xor8:contain(Filter6, 24)),
   ?assertEqual(true, xor8:contain(Filter6, 25)).


%% Begin xor16 tests.
xor16_filter() ->
   Filter = xor16:new(["test1", "test2", "test3"]),
   ?assertEqual(true, xor16:contain(Filter, "test1")),
   ?assertEqual(false, xor16:contain(Filter, "test4")).

xor16_buffered_filter() ->
   Filter = xor16:new_buffered(["test1", "test2", "test3"]),
   ?assertEqual(true, xor16:contain(Filter, "test2")),
   ?assertEqual(false, xor16:contain(Filter, "test6")).

xor16_non_uint64() ->
   ?assertEqual({error, convert_to_uint64_t_error}, 
      xor16:new(["test"], none)).

xor16_non_uint64_buffered() ->
   ?assertEqual({error, convert_to_uint64_t_error}, 
      xor16:new_buffered(["test"], none)).

xor16_invalid_pre_defined_hash() ->
   ?assertEqual({error, invalid_hash_method},
      xor16:new(["test"], fake_hash)).

xor16_invalid_pre_defined_hash_buffered() ->
   ?assertEqual({error, invalid_hash_method},
      xor16:new(["test"], fake_hash)).

xor16_dup_in_hash() ->
   Fun = fun(X) -> 1 end,
   ?assertEqual({error, duplicates_in_hash_error},
      xor16:new([1, 2], Fun)).

xor16_dup_in_pre_hash() ->
   ?assertEqual({error, duplicates_in_hash_error},
      xor16:new([1, 1], none)).

xor16_valid_hash() ->
   Fun = fun(X) -> X + 1 end,
   Filter = xor16:new([1, 2, 3], Fun),
   ?assertEqual(true, xor16:contain(Filter, 3)),
   ?assertEqual(false, xor16:contain(Filter, 4)).

xor16_valid_hash_buffered() ->
   Fun = fun(X) -> X + 1 end,
   Filter = xor16:new_buffered([1, 2, 3], Fun),
   ?assertEqual(true, xor16:contain(Filter, 3)),
   ?assertEqual(false, xor16:contain(Filter, 4)).

xor16_wrong_hash_arity() ->
   Fun = fun(X, Y) -> X + Y end,
   ?assertEqual({error, wrong_arity_hash_function_error},
      xor16:new([1, 2, 3], Fun)).

xor16_wrong_hash_arity_buffered() ->
   Fun = fun(X, Y) -> X + Y end,
   ?assertEqual({error, wrong_arity_hash_function_error},
      xor16:new_buffered([1, 2, 3], Fun)).

xor16_hash_does_not_return_uint64() ->
   Fun = fun(_X) -> "test" end,
   ?assertEqual({error, convert_to_uint64_t_error},
      xor16:new([1, 2, 3], Fun)).

xor16_hash_does_not_return_uint64_buffered() ->
   Fun = fun(_X) -> "test" end,
   ?assertEqual({error, convert_to_uint64_t_error},
      xor16:new_buffered([1, 2, 3], Fun)).

xor16_custom_contain_return() ->
   Filter = xor16:new([1, 2, 3]),
   ?assertEqual(true, xor16:contain(Filter, 2, asdf)),
   ?assertEqual(asdf, xor16:contain(Filter, 6, asdf)).

xor16_contain_hash_function_custom_return() ->
   Fun = fun(X) -> X + 1 end,
   Filter = xor16:new([1, 2, 3], Fun),
   ?assertEqual(true, xor16:contain(Filter, 2)),
   ?assertEqual({error, reason}, 
      xor16:contain(Filter, 0, {error, reason})).

xor16_contain_key_not_uint64() ->
   Filter = xor16:new([1, 2, 3], none),
   ?assertEqual({error, get_key_for_contains_error}, 
      xor16:contain(Filter, "test")).

xor16_contain_custom_key_not_uint64() ->
   Filter = xor16:new([1, 2, 3], none),
   ?assertEqual({error, get_key_for_contains_error}, 
      xor16:contain(Filter, "test", asdf)).

xor16_valid_filter_in_contain() ->
   ?assertEqual({error, get_key_for_contains_error}, 
      xor16:contain(asdf, 1)).

xor16_large() ->
   X = lists:seq(1, 10000000),
   Filter = xor16:new(X, none),
   ?assertEqual(true, xor16:contain(Filter, 100)).

xor16_large_buffered() ->
   X = lists:seq(1, 10000000),
   Filter = xor16:new_buffered(X, none),
   ?assertEqual(true, xor16:contain(Filter, 100)).

xor16_medium_default(Strings) ->
   Filter = xor16:new(Strings),
   ?assertEqual(true, xor16:contain(Filter, "test100")).

xor16_medium_default_buffered(Strings) ->
   Filter = xor16:new_buffered(Strings),
   ?assertEqual(true, xor16:contain(Filter, "test100")).

xor16_serialization() ->
   Filter = xor16:new(["test1", "test2", "test3"]),
   ?assertEqual(true, xor16:contain(Filter, "test1")),
   ?assertEqual(false, xor16:contain(Filter, "test4")),
   BinFilter = xor16:to_bin(Filter),
   ?assertEqual(true, xor16:contain(BinFilter, "test1")),
   ?assertEqual(false, xor16:contain(BinFilter, "test4")),
   Filter2 = xor16:from_bin(BinFilter),
   ?assertEqual(true, xor16:contain(Filter2, "test1")),
   ?assertEqual(false, xor16:contain(Filter2, "test4")).

xor16_incremental_builder() ->
   Filter0 = xor16:new_empty(),
   Filter1 = xor16:add(Filter0, [1, 2]),
   Filter2 = xor16:add(Filter1, [3, 4]),
   Filter3 = xor16:finalize(Filter2),
   ?assertEqual(true, xor16:contain(Filter3, 1)),
   ?assertEqual(true, xor16:contain(Filter3, 2)),
   ?assertEqual(true, xor16:contain(Filter3, 3)),
   ?assertEqual(true, xor16:contain(Filter3, 4)),
   ?assertEqual(false, xor16:contain(Filter3, 6)).

xor16_incremental_large_resize() ->
   Filter0 = xor16:new_empty(),
   Filter1 = xor16:add(Filter0, [1, 2]),
   Filter2 = xor16:add(Filter1, [3, 4, 5, 6, 7, 8, 9, 10]),
   Filter3 = xor16:finalize(Filter2),
   ?assertEqual(true, xor16:contain(Filter3, 10)),
   ?assertEqual(true, xor16:contain(Filter3, 9)),
   ?assertEqual(true, xor16:contain(Filter3, 8)),
   ?assertEqual(true, xor16:contain(Filter3, 7)),
   ?assertEqual(true, xor16:contain(Filter3, 6)),
   ?assertEqual(true, xor16:contain(Filter3, 5)),
   ?assertEqual(true, xor16:contain(Filter3, 4)),
   ?assertEqual(true, xor16:contain(Filter3, 3)),
   ?assertEqual(true, xor16:contain(Filter3, 2)),
   ?assertEqual(true, xor16:contain(Filter3, 1)).

xor16_cannot_use_incrament_in_contain() ->
   Filter0 = xor16:new_empty(),
   Filter1 = xor16:add(Filter0, [1, 2]),
   ?assertEqual({error, unfinalized_filter_error}, xor16:contain(Filter1, 1)).

%% EOF
