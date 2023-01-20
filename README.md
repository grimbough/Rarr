# Introduction to Rarr

**Rarr** is intended to be a simple interface to reading (and eventually writing) individual Zarr arrays.  

It is developed in R and C with no reliance on external libraries or APIs for interfacing with the Zarr arrays.
Additional compression libraries (e.g. blosc) are bundled with **Rarr** to provide support for datasets compressed
using these tools.

**Rarr** is not designed to be aware of heirarchical Zarr array stores, but the component arrays can be read individually
by providing the path to them directly.

# Basic usage

## Reading a subset from a local Zarr array

We have some example Zarr arrays in the package.   

```{r}
zarr_example <- system.file("extdata", "zarr_examples", "column-first", "int32.zarr",
                      package = "Rarr")
```

We can get an summary of the array properties, such as its shape and datatype, using the function `zarr_array_overview()`:

```r
zarr_array_overview(zarr_example)
```

```
Path: /home/msmith/Projects/Rarr/inst/extdata/zarr_examples/column-first/int32.zarr 
Shape: 30 x 20 x 10 
Chunk Shape: 10 x 10 x 5 
No. of Chunks: 12 (3 x 2 x 2)
Data Type: int32
Endianness:  little 
Compressor: blosc
```

This is useful, as to read the array with **Rarr** you need to know the shape and size of the array (unless you want to read the entire array).  From the output above we can see our example array has three dimensions of size 30 x 20 x 10.  We can select the subset we want to extract using a `list`.
The list must have the same length as the number of dimensions in our array.

```{r}
index <- list(1:4, 1:2, 1)
```

We then extract the subset using `read_zarr_array()`:

```{r}
read_zarr_array(zarr_example, index = index)
```

```
, , 1

     [,1] [,2]
[1,]    1    2
[2,]    1    0
[3,]    1    0
[4,]    1    0
```


## Read a data selection from an S3 bucket

### Amazon Web Services

If reading from an AWS S3 bucket, **Rarr** currently required you to use the `https` address
and provide it in the path style.  Support for virtual-host style addressing, using `s3://` notation
and other features is planned.

The examples below read Zarr arrays found in public datasets such as  
[Pangeo / ESGF Coupled Model Intercomparison Project 6](https://registry.opendata.aws/cmip6/) and
[NASA Prediction of Worldwide Energy Resources](https://registry.opendata.aws/nasa-power/) 


```r
read_zarr_array("https://s3.us-west-2.amazonaws.com/cmip6-pds/CMIP3/BCCR/bccr_bcm2_0/piControl/r1i1p1f1/Amon/psl/lon")
read_zarr_array("https://s3.us-west-2.amazonaws.com/power-analysis-ready-datastore/power_901_constants.zarr/FRLAKE")
```

### Other S3 Storage

```{r}
path <- 's3://mghp.osn.xsede.org/bir190004-bucket01/TMA11/zarr/10.zarr'
read_zarr_array(path, index = list(1, 1:10, 1:10))
```



# Current Status

## Reading

Reading Zarr arrays is partially supported and under active development.  

### Data Types

Currently there is only support for reading a subset of the possible datatypes
that can be found in a Zarr array.  In some instances there are also limitations on the 
datatypes natively supported by R, requiring conversion from the Zarr datatype.  The table below summarises the current status of
datatype support.  It will be updated as progress is made.

| Zarr Data Type | Status | Notes |
|-----------|--------|-------|
|`int8`  |&#x2714;||
|`uint8` |&#x2714;||
|`int16` |&#x2714;||
|`uint16`|&#x2714;||
|`int32` |&#x2714;||
|`uint32`|&#x2714;|Values outside the range of `int32` are converted to `NA`.  Future plan is to allow conversion to `double` or use the [bit64](https://cran.r-project.org/package=bit64) package.| 
|`int64`|&#x2714;|Values outside the range of `int32` are converted to `NA`. Future plan is to allow conversion to `double` or use the [bit64](https://cran.r-project.org/package=bit64) package.|
|`uint64`|&#x2754;||
|`half` / `float16`|&#x2714;| Converted to `double` in R.  No effort is made to assess loss of precision due to conversion.  |
|`single` / `float32`|&#x2714;| Converted to `double` in R.  No effort is made to assess loss of precision due to conversion. |
|`double` / `float64`|&#x2714;||
|`complex`|&#x274C;||
|`timedelta`|&#x274C;||
|`datetime`|&#x274C;||
|`string`|&#x2714;||
|`Unicode`|&#x274C;||
|`void *`|&#x274C;||
| Structured data types | &#x274C; | |

### Compression Tools

| Data Type | Status | Notes |
|-----------|--------|-------|
|`zlib / gzip`|&#x2714;||
|`bzip2`      |&#x2714;||
|`blosc`      |&#x2714;||
|`LZMA `      |&#x2714;||
|`LZ4`        |&#x274C;||
|`Zstd`       |&#x274C;||

Please open an [issue](https://github.com/grimbough/Rarr/issues) if support for a required compression tool is missing.

### Filters

The is currently no support for additional filters.  Please open an [issue](https://github.com/grimbough/Rarr/issues) if you require filter support.

# Writing

There is currently no support for writing Zarr arrays with **Rarr**.  This will be added once the reading functionality is more mature.
