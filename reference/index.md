# Package index

## Read Zarr data

- [`read_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_array.md)
  : Read a Zarr array
- [`zarr_overview()`](https://huber-group-embl.github.io/Rarr/reference/zarr_overview.md)
  : Print a summary of a Zarr array
- [`read_zarr_attributes()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_attributes.md)
  : Read the attributes associated with a Zarr array or group

## Write Zarr data

- [`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md)
  : Write an R array to Zarr
- [`ZarrRealizationSink`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  [`writeZarrArray`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  [`ZarrRealizationSink-class`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  [`write_block,ZarrRealizationSink-method`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  [`type,ZarrRealizationSink-method`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  [`chunkdim,ZarrRealizationSink-method`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  [`coerce,ZarrRealizationSink,ZarrMatrix-method`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  [`coerce,ZarrRealizationSink,ZarrArray-method`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  [`coerce,ZarrRealizationSink,ZarrArraySeed-method`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  [`coerce,ZarrRealizationSink,DelayedArray-method`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  [`coerce,ANY,ZarrArray-method`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  [`coerce,ANY,ZarrRealizationSink-method`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  : Write arrays to Zarr
- [`write_zarr_attributes()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_attributes.md)
  : Read the .zattrs file associated with a Zarr array or group
- [`update_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/update_zarr_array.md)
  : Update (a subset of) an existing Zarr array
- [`create_empty_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/create_empty_zarr_array.md)
  : Create an (empty) Zarr array

## Custom classes

- [`ZarrArray()`](https://huber-group-embl.github.io/Rarr/reference/ZarrArray-classes.md)
  : ZarrArray constructor

## Compression helpers

- [`use_blosc()`](https://huber-group-embl.github.io/Rarr/reference/compressors.md)
  [`use_zlib()`](https://huber-group-embl.github.io/Rarr/reference/compressors.md)
  [`use_gzip()`](https://huber-group-embl.github.io/Rarr/reference/compressors.md)
  [`use_bz2()`](https://huber-group-embl.github.io/Rarr/reference/compressors.md)
  [`use_lzma()`](https://huber-group-embl.github.io/Rarr/reference/compressors.md)
  [`use_lz4()`](https://huber-group-embl.github.io/Rarr/reference/compressors.md)
  [`use_zstd()`](https://huber-group-embl.github.io/Rarr/reference/compressors.md)
  : Define compression tool and settings
