# Design principles for the Rarr package

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
