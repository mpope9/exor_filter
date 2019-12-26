exor_filter
=====

Nif wrapper for the xor_filter: https://github.com/FastFilter/xor_singleheader

They're 'Faster and Smaller Than Bloom and Cuckoo Filters'.

Be wary of memory usage when using this module.

This library uses dirty nifs for initializing filters over 10K elements!  Make sure your environment is setup correctly.  Filters of 10M elements can be initialized within 3 seconds.

## Example Usage
Basic usage with default hashing is as follows:
```erlang
Filter = exor_filter:xor8(["cat", "dog", "mouse"]),
true   = exor_filter:xor8_contain(Filter, "cat"),
false  = exor_filter:xor8_contain(Filter, "goose"),
ok     = exor_filter:xor8_free(Filter).
```

Filters are initialized independently:
```erlang
Filter1 = exor_filter:xor8([1, 2, 3]),
Filter2 = exor_filter:xor8([4, 5, 6]),

false   = exor_filter:xor8_contain(Filter1, 6),
true    = exor_filter:xor8_contain(Filter1, 2),
false   = exor_filter:xor8_contain(Filter2, 2),
true    = exor_filter:xor8_contain(Filter2, 5),

ok      = exor_filter:xor8_free(Filter1),
ok      = exor_filter:xor8_free(Filter2).
```

Do not modify the return value of the `exor_filter:xor8/1` or `/2` functions.  The other APIs will not function correctly.

## Hashing
The function `exor_filter:xor8/1` uses the default hash algorithm.  To specify the hashing algorithm to use, use the `exor_filter:xor8/2` function.  The filter initialization functions return values contain the context of hashing, so there is no need to specify it in the `exor_filter:xor8_contain/2` function.  **Do not pre-hash the value** being passed to `exor_filter:xor8_contain/2` or `/3`.  **Pass the raw value!**
*  (Unless you're using pre-hashed data.  See below).  
### Example
```erlang
Filter = exor_filter:xor8(["test1", "test2", "test3"], :fast_hash),
true   = exor_filter:xor8_contain(Filter, "test1"),
false  = exor_filter:xor8_contain(Filter, "test6"),
ok     = exor_filter:xor8_free(Filter).
```

### Hashing API
* The default hash function used is [`erlang:phash/1`](http://erlang.org/doc/man/erlang.html#phash2-1)
    * It can be specified with the `:default_hash` as the second argument to `exor_filter:xor8/2`.
    * It uses 60 bits on a 64-bit system and is consistent across nodes.
    * Do not use the default hash if your list exceeds 20K strings, there will be duplicates and the initialization will spin forever.
        * This is a known bug that is being worked on.
    * The default hashing function should be fine for most use cases, but if the filter has over 20K elements, create your own hashing function.
*  An option for a faster hashing function is available, using the option `:fash_hash`.  
    * This uses 64 bits, and is not consistent across nodes.  
    * The consequence is that false positives may be inconsistent across nodes.
    * It isn't recommended to use this method if there are more than 30K items in the filter.

#### Pre-Hashing and Custom Hashing
*  There is an option to pass a hash function during intialization.  
*  It must return a unsigned 64 bit number and have an airty of `/1`.  
*  Due to the Erlang nif api lacking the functionality to pass and call a function in a nif, this method creates a second list of equal length.  Be weary of that.
*  The has function **must** return unique keys, or else initialization will never return.  
    * This is a known implementation bug and will be addressed in the future.  
    * If your set is known to have a large amount of elements, consider pre-hashing and checking for dups before initing.
    * Or consider wrapping initialization in a timed gen-server call.
    * Make your unit testing reflect reality, if possible.  This will catch the issue early.
```erlang
Fun    = fun(X) -> X + 1 end,
Filter = exor_filter:xor8([1, 2, 3], Fun),
true   = exor_filter:xor8_contain(Filter, 4, Fun),
false  = exor_filter:xor8_contain(Filter, 1, Fun),
ok     = exor_filter:xor8_free(Filter).
```

* To pass pre-hashed data, use the hash option `:none`.  The `exor_filter:contain/2` and `/3` functions must be passed pre-hashed data in this case.

## Elixir Example
```elixir
# ...
alias :exor_filter, as: XorFilter
# ...
true =
   [1, 2, 3, 4]
   |> XorFilter.xor8()
   |> XorFilter.xor8_contain(1)

```

## Custom Return Values
`contain/3` can return a custom value instead of `false` if the value isn't present in the filter:
```erlang
Filter1            = exor_filter:xor8([1, 2, 3]),
true               = exor_filter:xor8_contain(Filter1, 2, {error, not_found}),
{error, not_found} = exor_filter:xor8_contain(Filter1, 6, {error, not_found}),
ok                 = exor_filter:xor8_free(Filter1).
```

## xor16 and Other Information
The usage of the xor16 is the same.  That structure is larger, but has a smaller false positive rate.  Just sub `xor8` for `xor16` in all of the examples.

The buffered versions of initialize are provided for larger data sets.  This can be faster.  See `xor8_buffered/2` for more information.

Build
-----

    $ rebar3 compile

Test
-----

    $ rebar3 eunit
    $ rebar3 cover

Docs
-----

    $ rebar3 edoc

Coverage is low due to suggested nif error handling code, but basic functionality is covered.

## Implements of xor filters in other programming languages
* [Go](https://github.com/FastFilter/xor_filter)
* Rust: [1](https://github.com/bnclabs/xorfilter) and [2](https://github.com/codri/xorfilter-rs)
* [C++](https://github.com/FastFilter/fastfilter_cpp)
* [Java](https://github.com/FastFilter/fastfilter_java)
* [C](https://github.com/FastFilter/xor_singleheader)
