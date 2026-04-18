# Design principles for the Rarr package

## Guiding principles

- **performant**. We focus on speed & memory efficiency. This results in
  Rarr being among the most performant Zarr implementations. This is
  ensured by the use of an extensive [continuous
  benchmarking](https://lorenzwalthert.github.io/touchstone/index.html)
  suite. Performance critical steps are written in C.
- **maintainable and extensible**. Additional codecs can easily be
  supported as the entire codec codebase is decoupled from the rest of
  the codebase.

## Scope

We aim for full support of the Zarr specification.

There is currently no clear decision process regarding support for [Zarr
extensions](https://github.com/zarr-developers/zarr-extensions). Please
reach out if you have a specific use case that relies on a Zarr
extension.

## Zarr version

Rarr is a “Zarr version 3 first” implementation.

There is full support for both version 2 and version 3 Zarr arrays but:

- the package API is modelled on the version 3 specification
- we backport some backward compatible feature from version 3 to
  version 2. For example, the `dimension_names` field is supported for
  both version 2 and version 3 arrays. This is neither strictly defined
  nor forbidden in the version 2 specification, but it is a feature that
  we have chosen to support in both versions for consistency.
- if we ever had to make a decision on a tradeoff (e.g., performance
  tradeoff) between version 2 and version 3, we would prioritise version
  3.

## Functional programming and API design

- Reading and writing Zarr arrays should be as easy as reading and
  writing `.csv` files. In other words, writing
  `read_zarr_array("my_array.zarr")` should be enough to read a Zarr
  array, and writing `write_zarr_array(my_array, "my_array.zarr")`
  should be enough to write a Zarr array. This has several consequences:
  - it is not necessary to explicitly manipulate custom objects for Zarr
    stores or groups. Passing a file path as a string should work out of
    the box.
  - when writing, we provide sensible defaults for:
    - data type (derived from `storage.mode` of the input array)
    - chunk size (TBD)
    - compression (Zstd with default compression level, for a good
      balance between speed and compression ratio)
    - dimension names (taken from the dimension names of the input
      array, if they exist)
