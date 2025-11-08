# Supported Zarr features in Rarr

## Zarr version

Rarr currently only works with [Zarr specification version
2](https://zarr-specs.readthedocs.io/en/latest/v2/v2.0.html). Support
for [version
3](https://zarr-specs.readthedocs.io/en/latest/v3/core/index.html) is
actively being worked on.

## Reading and Writing

Reading Zarr arrays is reasonably well supported. Writing is available,
but is more limited. Both aspects are under active development.

### Data Types

Currently there is only support for reading and writing a subset of the
possible datatypes that can be found in a Zarr array. In some instances
there are also limitations on the datatypes natively supported by R,
requiring conversion from the Zarr datatype. The table below summarises
the current status of datatype support. It will be updated as progress
is made.

[TABLE]

### Codecs

#### Compression codecs

[TABLE]

Please open an [issue](https://github.com/Huber-group-EMBL/Rarr/issues)
if support for a required compression codec is missing.

#### Other codecs

[TABLE]
