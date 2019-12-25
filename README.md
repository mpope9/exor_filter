exor_filter
=====

Nif wrapper for the xor_filter: https://github.com/FastFilter/xor_singleheader

They're 'Faster and Smaller Than Bloom and Cuckoo Filters'.

Be wary of memory usage when using this module.

## Example Usage
Basic usage with default hashing is as follows:
```erlang
Filter = exor_filter:xor8(["test1", "test2", "test3"]),
true   = exor_filter:xor8_contain(Filter, "test1"),
false  = exor_filter:xor8_contain(Filter, "test6"),
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
    * The default hashing function should be fine for most use cases, but if the filter has millions of elements, consider using a different method.
*  An option for a faster hashing function is available, using the option `:fash_hash`.  
    * This uses 64 bits, and is non consistent across nodes.  
    * The consequence is that false positives may be inconsistent across nodes.
* To pass pre-hashed data, use the hash option `:none`.

There is an option to pass a hash function during intialization.  It must return a unsigned 64 bit number and have an airty of `/1`.  Due to the Erlang nif api lacking the functionality to pass and call a function in a nif.  This method creates a second list of equal length.  Be weary of that.
```erlang
Fun    = fun(X) -> X + 1 end,
Filter = exor_filter:xor8_initialize([1, 2, 3], Fun),
true   = exor_filter:xor8_contain(Filter, 4, Fun),
false  = exor_filter:xor8_contain(Filter, 1, Fun),
ok     = exor_filter:xor8_free(Filter).
```

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
Filter1            = exor_filter:xor8_initialize([1, 2, 3]),
true               = exor_filter:xor8_contain(Filter1, 2, {error, not_found}),
{error, not_found} = exor_filter:xor8_contain(Filter1, 6, {error, not_found}),
ok                 = exor_filter:xor8_free(Filter1).
```

## xor16 and Other Information
The usage of the xor16 is the same.  That structure is larger, but has a smaller false positive rate.  Just sub `xor8` for `xor16` in all of the examples.

The buffered versions of initialize are provided for larger data sets.  This can be faster.  See `xor8_buffered_initialize/2` for more information.

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
