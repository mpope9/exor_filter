exor_filter
=====

Nif wrapper for the xor_filter: https://github.com/FastFilter/xor_singleheader

They're 'Faster and Smaller Than Bloom and Cuckoo Filters'.

Be wary of memory usage when using this module.

### Example Usage
Basic usage is as follows:
```
Filter = exor_filter:xor8_initialize([1, 2, 3]).
true   = exor_filter:xor8_contain(Filter, 2).
false  = exor_filter:xor8_contain(Filter, 6).
ok     = exor_filter:xor8_free(Filter).
```

Filters are initialized independently:
```
Filter1 = exor_filter:xor8_initialize([1, 2, 3]).
Filter2 = exor_filter:xor8_initialize([4, 5, 6]).

false   = exor_filter:xor8_contain(Filter1, 6).
true    = exor_filter:xor8_contain(Filter1, 2).
false   = exor_filter:xor8_contain(Filter2, 2).
true    = exor_filter:xor8_contain(Filter2, 5).
```

Example usage from Elixir:
```
...
Alias :exor_filter, as: XorFilter
...
true =
   [1, 2, 3, 4]
   |> XorFilter.xor8_initialize()
   |> XorFilter.xor8_contain(1)

```

`contain/3` can return a custom value instead of `false` if the value isn't present in the filter:
```
Filter1 = exor_filter:xor8_initialize([1, 2, 3]).
true = exor_filter:xor8_contain(Filter1, 2, {error, not_found}).
{error, not_found} = exor_filter:xor8_contain(Filter1, 6, {error, not_found}).
```

The usage of the xor16 is the same.  That structure is larger, but
has a smaller false positive rate.

The buffered versions of initialize are provided for larger data sets.
This can be faster.  See `xor8_buffered_initialize/2` for more information.

Versions of the initialization function that can be passed a hash 
function are provided, so that data can be passed without being 
hashed first.
See `xor8_initialize/2` for more details.

Similar functionality exists for the `xor16` functions.

Build
-----

    $ rebar3 compile

Test
-----

    $ rebar3 eunit
    $ rebar3 cover

Coverage is low due to suggested nif error handling code, but basic functionality is covered.
