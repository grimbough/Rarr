Zarr arrays with Rarr
================
Mike L. Smith

- <a href="#introduction-to-rarr"
  id="toc-introduction-to-rarr">Introduction to Rarr</a>
  - <a href="#limitations-with-rarr"
    id="toc-limitations-with-rarr">Limitations with
    <strong>Rarr</strong></a>
- <a href="#quick-start-guide" id="toc-quick-start-guide">Quick start
  guide</a>
  - <a href="#installation-and-setup"
    id="toc-installation-and-setup">Installation and setup</a>
  - <a href="#reading-a-from-a-local-zarr-array"
    id="toc-reading-a-from-a-local-zarr-array">Reading a from a local Zarr
    array</a>
  - <a href="#reading-from-s3-storage"
    id="toc-reading-from-s3-storage">Reading from S3 storage</a>
  - <a href="#writing-to-a-zarr-array"
    id="toc-writing-to-a-zarr-array">Writing to a Zarr array</a>
- <a href="#current-status" id="toc-current-status">Current Status</a>
  - <a href="#reading-and-writing" id="toc-reading-and-writing">Reading and
    Writing</a>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/grimbough/Rarr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/grimbough/Rarr?branch=main)
<!-- badges: end -->

# Introduction to Rarr

The Zarr specification defines a format for chunked, compressed,
N-dimensional arrays. It’s design allows efficient access to subsets of
the stored array, and supports both local and cloud storage systems.
Zarr is experiencing increasing adoption in a number of scientific
fields, where multi-dimensional data are prevalent.

**Rarr** is intended to be a simple interface to reading and writing
individual Zarr arrays. It is developed in R and C with no reliance on
external libraries or APIs for interfacing with the Zarr arrays.
Additional compression libraries (e.g. blosc) are bundled with **Rarr**
to provide support for datasets compressed using these tools.

## Limitations with **Rarr**

If you know about Zarr arrays already, you’ll probably be aware they can
be stored in hierarchical groups, where additional meta data can explain
the relationship between the arrays. Currently, **Rarr** is not designed
to be aware of these hierarchical Zarr array collections. However, the
component arrays can be read individually by providing the path to them
directly.

Currently, there are also limitations on the Zarr datatypes that can be
accessed using **Rarr**. For now most numeric types can be read into R,
although in some instances e.g. 64-bit integers there is potential for
loss of information. Writing is more limited with support only for
datatypes that are supported natively in R and only using the
column-first representation.

# Quick start guide

## Installation and setup

If you want to quickly get started reading an existing Zarr array with
the package, this section should have the essentials covered. First, we
need to install **Rarr**[^1] with the commands below.

``` r
## we need BiocManager to perform the installation
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
## install Rarr
BiocManager::install("Rarr")
```

Once **Rarr** is installed, we have to load it into our R session:

``` r
library(Rarr)
```

**Rarr** can be used to read files either on local disk or on remote S3
storage systems. First lets take a look at reading from a local file.

## Reading a from a local Zarr array

To demonstrate reading a local file, we’ll pick the example file
containing 32-bit integers arranged in the “column first” ordering.

``` r
zarr_example <- system.file("extdata", "zarr_examples", "column-first", "int32.zarr",
  package = "Rarr"
)
```

### Exploring the data

We can get an summary of the array properties, such as its shape and
datatype, using the function `zarr_overview()`[^2].

``` r
zarr_overview(zarr_example)
```

    ## Type: Array
    ## Path: /mnt/data/R-lib/4.3-bioc_3.17/Rarr/extdata/zarr_examples/column-first/int32.zarr
    ## Shape: 30 x 20 x 10
    ## Chunk Shape: 10 x 10 x 5
    ## No. of Chunks: 12 (3 x 2 x 2)
    ## Data Type: int32
    ## Endianness: little
    ## Compressor: blosc

You can use this to check that the location is a valid Zarr array, and
that the shape and datatype of the array content are what you are
expecting. For example, we can see in the output above that the data
type (`int32`) corresponds to what we expect.

### Reading the Zarr array

The summary information retrieved above is required, as to read the
array with **Rarr** you need to know the shape and size of the array
(unless you want to read the entire array). From the previous output we
can see our example array has three dimensions of size 30 x 20 x 10. We
can select the subset we want to extract using a `list`. The list must
have the same length as the number of dimensions in our array, with each
element of the list corresponding to the indices you want to extract in
that dimension.

``` r
index <- list(1:4, 1:2, 1)
```

We then extract the subset using `read_zarr_array()`:

``` r
read_zarr_array(zarr_example, index = index)
```

    ## , , 1
    ## 
    ##      [,1] [,2]
    ## [1,]    1    2
    ## [2,]    1    0
    ## [3,]    1    0
    ## [4,]    1    0

## Reading from S3 storage

Reading files in S3 storage works in a very similar fashion to local
disk. This time the path needs to be a URL to the Zarr array. We can
again use `zarr_overview()` to quickly retrieve the array metadata.

``` r
s3_address <- "https://uk1s3.embassy.ebi.ac.uk/idr/zarr/v0.4/idr0076A/10501752.zarr/0"
zarr_overview(s3_address)
```

    ## Type: Array
    ## Path: https://uk1s3.embassy.ebi.ac.uk/idr/zarr/v0.4/idr0076A/10501752.zarr/0/
    ## Shape: 50 x 494 x 464
    ## Chunk Shape: 1 x 494 x 464
    ## No. of Chunks: 50 (50 x 1 x 1)
    ## Data Type: float64
    ## Endianness: little
    ## Compressor: blosc

The output above indicates that the array is stored in 50 chunks, each
containing a slice of the overall data. In the example below we use the
`index` argument to extract the first and tenth slices from the array.
We then plot these on top of one another using the `image()` function.
Choosing to read only 2 of the 50 slices is much faster than if we opted
to download the entire array before accessing the data.

``` r
## z2 <- read_zarr_array(s3_address, index = list(c(1, 10), NULL, NULL))
par(mar = c(0, 0, 0, 0))
## plot the first slice in blue
image(log2(z2[1, , ]),
  col = hsv(h = 0.6, v = 1, s = 1, alpha = 0:100 / 100),
  asp = dim(z2)[2] / dim(z2)[3], axes = FALSE
)
## overlay the tenth slice in green
image(log2(z2[2, , ]),
  col = hsv(h = 0.3, v = 1, s = 1, alpha = 0:100 / 100),
  asp = dim(z2)[2] / dim(z2)[3], axes = FALSE, add = TRUE
)
```

<img src="README_files/figure-gfm/plot-raster-1.png" width="30%" />

**Note:** if you receive the error message
`"Error in stop(aws_error(request$error)) : bad error message"` it is
likely you have some AWS credentials available in to your R session,
which are being inappropriately used to access this public bucket.
Please see the section @ref(s3-credentials) for details on how to set
credentials for a specific request.

## Writing to a Zarr array

Up until now we’ve only covered reading existing Zarr array into R.
However, **Rarr** can also be used to write R data to disk following the
Zarr specification. To explore this, lets create an example array we
want to save as a Zarr. In this case it’s going to be a three
dimensional array and store the values 1 to 600.

``` r
x <- array(1:600, dim = c(10, 10, 6))
```

``` r
path_to_new_zarr <- file.path(tempdir(), "new.zarr")
write_zarr_array(x = x, zarr_array_path = path_to_new_zarr, chunk_dim = c(10, 5, 1))
```

We can check that the contents of the Zarr array is what we’re
expecting. Since the contents of the whole array will be too large to
display here, we use the `index` argument to extract rows 6 to 10, from
the 10th column and 1st slice. That should be the values 96, 97, 98, 99,
100, but retaining the 3-dimensional array structure of the original
array. The second line below uses `identical()` to confirm that reading
the whole Zarr returns something equivalent to our original input `x`.

``` r
read_zarr_array(zarr_array_path = path_to_new_zarr, index = list(6:10, 10, 1))
```

    ## , , 1
    ## 
    ##      [,1]
    ## [1,]   96
    ## [2,]   97
    ## [3,]   98
    ## [4,]   99
    ## [5,]  100

``` r
identical(read_zarr_array(zarr_array_path = path_to_new_zarr), x)
```

    ## [1] TRUE

# Current Status

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

| Zarr Data Type        | Status<br/>(reading / writing) | Notes                                                                                                                                                                           |
|-----------------------|:------------------------------:|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `int8`                |             ✔ / ❌             |                                                                                                                                                                                 |
| `uint8`               |             ✔ / ❌             |                                                                                                                                                                                 |
| `int16`               |            ❔ / ❌             |                                                                                                                                                                                 |
| `uint16`              |             ✔ / ❌             |                                                                                                                                                                                 |
| `int32`               |             ✔ / ❌             |                                                                                                                                                                                 |
| `uint32`              |             ✔ / ❌             | Values outside the range of `int32` are converted to `NA`. Future plan is to allow conversion to `double` or use the [bit64](https://cran.r-project.org/package=bit64) package. |
| `int64`               |             ✔ / ❌             | Values outside the range of `int32` are converted to `NA`. Future plan is to allow conversion to `double` or use the [bit64](https://cran.r-project.org/package=bit64) package. |
| `uint64`              |             ✔ / ❌             | Values outside the range of `int32` are converted to `NA`. Future plan is to allow conversion to `double` or use the [bit64](https://cran.r-project.org/package=bit64) package. |
| `half` / `float16`    |             ✔ / ❌             | Converted to `double` in R. No effort is made to assess loss of precision due to conversion.                                                                                    |
| `single` / `float32`  |             ✔ / ❌             | Converted to `double` in R. No effort is made to assess loss of precision due to conversion.                                                                                    |
| `double` / `float64`  |             ✔ / ✔              |                                                                                                                                                                                 |
| `complex`             |            ❌ / ❌             |                                                                                                                                                                                 |
| `timedelta`           |            ❌ / ❌             |                                                                                                                                                                                 |
| `datetime`            |            ❌ / ❌             |                                                                                                                                                                                 |
| `string`              |             ✔ / ✔              |                                                                                                                                                                                 |
| `Unicode`             |            ❌ / ❌             |                                                                                                                                                                                 |
| `void *`              |            ❌ / ❌             |                                                                                                                                                                                 |
| Structured data types |            ❌ / ❌             |                                                                                                                                                                                 |

### Compression Tools

| Data Type     | Status<br/>(reading / writing) | Notes                                                                                               |
|---------------|:------------------------------:|-----------------------------------------------------------------------------------------------------|
| `zlib / gzip` |             ✔ / ✔              | Only system default compression level (normally 6) is enabled for writing.                          |
| `bzip2`       |             ✔ / ✔              | Only compression level 9 is enabled for writing.                                                    |
| `blosc`       |             ✔ / ✔              | Only `lz4` compression level 5 is enabled for writing.                                              |
| `LZMA`        |             ✔ / ❔             |                                                                                                     |
| `LZ4`         |             ✔ / ❌             |                                                                                                     |
| `Zstd`        |            ❌ / ❌             | Algorithm is available via blosc for writing, but can’t currently be access through the R interface |

Please open an [issue](https://github.com/grimbough/Rarr/issues) if
support for a required compression tool is missing.

### Filters

The is currently no support for additional filters. Please open an
[issue](https://github.com/grimbough/Rarr/issues) if you require filter
support.

[^1]: you only need to do the installation step once

[^2]: This is essentially reading and formatting the array metadata that
    accompanies any Zarr array.
