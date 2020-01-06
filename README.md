exor_filter
=====

Nif wrapper for the xor_filter: https://github.com/FastFilter/xor_singleheader

They're 'Faster and Smaller Than Bloom and Cuckoo Filters'.

Be wary of memory usage when using this module.

This library uses dirty nifs for initializing filters over 10K elements!  Make sure your environment is setup correctly.  Filters of 10M elements can be initialized within 4 seconds.  Within 2.5 seconds if the library is used unsafely.

## Example Usage
Basic usage with default hashing is as follows:
```erlang
Filter = xor8:new(["cat", "dog", "mouse"]),
true   = xor8:contain(Filter, "cat"),
false  = xor8:contain(Filter, "goose"),
ok     = xor8:free(Filter).
```

Filters are initialized independently:
```erlang
Filter1 = xor8:new([1, 2, 3]),
Filter2 = xor8:new([4, 5, 6]),

false   = xor8:contain(Filter1, 6),
true    = xor8:contain(Filter1, 2),

false   = xor8:contain(Filter2, 2),
true    = xor8:contain(Filter2, 5),

ok      = xor8:free(Filter1),
ok      = xor8:free(Filter2).
```

Do not modify the return value of the `xor8:new/1` or `/2` functions.  The other APIs will not function correctly.

## Hashing
* The function `xor8:new/1` uses the default hash algorithm.
    * See [`erlang:phash2/1`](http://erlang.org/doc/man/erlang.html#phash2-1).
* To specify the hashing algorithm to use, use the `xor8:new/2` function.
* The filter initialization functions return values contain the context of hashing, so there is no need to specify it in the `xor8:contain/2` function.
    * **Do not pre-hash the value** being passed to `xor8:contain/2` or `/3`.  **Pass the raw value!**
    *  (Unless you're using pre-hashed data.  See below).
* The default hashing mechanisms remove duplicate keys.  Pre-hashed data will need to be checked by the user.  An error will be returned if duplicate keys are detected.
### Example
```erlang
Filter = xor8:new([1, 2, 3], none),
true   = xor8:contain(Filter, 1),
false  = xor8:contain(Filter, 6),
ok     = xor8:free(Filter).
```

### Hashing API
* The default hash function used is [`erlang:phash2/1`](http://erlang.org/doc/man/erlang.html#phash2-1)
    * It can be specified with the `default_hash` as the second argument to `xor8:new/2`.
    * It uses 60 bits on a 64-bit system and is consistent across nodes.
    * The default hashing function should be fine for most use cases, but if the filter has over 20K elements, create your own hashing function, as hashing collisions will become more frequent.
        * Errors won't happen if a collision occurs.

#### Pre-Hashing and Custom Hashing
*  There is an option to pass a hash function during intialization.
*  It must return a unsigned 64 bit number and have an airty of `/1`.
*  Due to the Erlang nif api lacking the functionality to pass and call a function in a nif, this method creates a second list of equal length.  Be weary of that.
*  The custom hashing function **must** return unique keys.
    * An error will be returned otherwise.
    * Make your unit testing reflect reality, if possible.  This will catch the issue early.
```erlang
Fun    = fun(X) -> X + 1 end,
Filter = xor8:new([1, 2, 3], Fun),
true   = xor8:contain(Filter, 4),
false  = xor8:contain(Filter, 1),
ok     = xor8:free(Filter).
```

* To pass pre-hashed data, use the hash option `none`.  The `xor8:contain/2` and `/3` functions must be passed pre-hashed data in this case.
    * This too will check for duplicate hashed values, and will return an error if it is detected.

## Elixir Example
```elixir
# ...
alias :xor8, as: Xor8
# ...
true =
   [1, 2, 3, 4]
   |> Xor8.new()
   |> Xor8.contain(1)

```

## Custom Return Values
`contain/3` can return a custom value instead of `false` if the value isn't present in the filter:
```erlang
Filter1            = xor8:new(["Ricky Bobby", "Cal Naughton Jr."]),
true               = xor8:contain(Filter1, "Ricky Bobby", {error, not_found}),
{error, not_found} = xor8:contain(Filter1, "Reese Bobby", {error, not_found}),
ok                 = xor8:free(Filter1).
```

## xor16
The usage of the xor16 is the same.  That structure is larger, but has a smaller false positive rate.  Just sub `xor8` for `xor16` in all of the examples.

## Buffered Initialization
The buffered versions of initialize are provided for larger data sets.  This can be faster.  See `xor8:new_buffered/2` for more information.

## Unsafe Usage
The underlying C library used has an issue where duplicate keys cause an infinite loop on initialization.  The convinience modules `xor8` and `xor16` check for duplicates in the passed list for pre-hashed data.  HOWEVER, they're just wrappers for the raw `exor_filter` module.  If you're confident that the values in the list are unique, and wish to skip the checking step in initialization for greater speed, you can use the following example as a template:
```
Filter = exor_filter:nif_wrapper([1, 2, 3], none, xor8),
true   = xor8:contain(Filter, 1),
ok     = xor8:free(Filter).
```
You didn't hear it from me, though ;)

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

## Implements of xor filters in other programming languages
* [Go](https://github.com/FastFilter/xor_filter)
* Rust: [1](https://github.com/bnclabs/xorfilter) and [2](https://github.com/codri/xorfilter-rs)
* [C++](https://github.com/FastFilter/fastfilter_cpp)
* [Java](https://github.com/FastFilter/fastfilter_java)
* [C](https://github.com/FastFilter/xor_singleheader)
