# Working with Zarr arrays in R

## Introduction

The Zarr specification defines a format for chunked, compressed,
N-dimensional arrays. It’s design allows efficient access to subsets of
the stored array, and supports both local and cloud storage systems.
Zarr is experiencing increasing adoption in a number of scientific
fields, where multi-dimensional data are prevalent. In particular as a
back-end to the The Open Microscopy Environment’s [OME-NGFF
format](https://ngff.openmicroscopy.org/latest/) for storing bioimaging
data in the cloud.

**Rarr** is intended to be a simple interface to reading and writing
individual Zarr arrays. It is developed in R and C with no reliance on
external libraries or APIs for interfacing with the Zarr arrays.
Additional compression libraries (e.g. blosc) are bundled with **Rarr**
to provide support for datasets compressed using these tools.

### Limitations with **Rarr**

If you know about Zarr arrays already, you’ll probably be aware they can
be stored in hierarchical groups, where additional meta data can explain
the relationship between the arrays. Currently, **Rarr** is not designed
to be aware of these hierarchical Zarr array collections. However, the
component arrays can be read individually by providing the path to them
directly.

Currently, there are also limitations on the Zarr datatypes that can be
accessed using **Rarr**. For now most numeric types can be read into R,
although in some instances e.g. 64-bit integers there is potential for
loss of information.

### Example data

The are some example Zarr arrays included with the package. These were
created using the Zarr Python implementation and are primarily intended
for testing the functionality of **Rarr**. You can use the code below to
list the complete set on your system, however it’s a long list so we
don’t show the output here.

``` r

list.dirs(
  system.file("extdata", "zarr_examples", package = "Rarr"),
  recursive = TRUE
) |>
  grep(pattern = "zarr$", value = TRUE)
```

## Quick start guide

### Installation and setup

If you want to quickly get started reading an existing Zarr array with
the package, this section should have the essentials covered. First, we
need to install **Rarr**[^1] with the commands below.

``` r

## we need BiocManager to perform the installation
if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
## install Rarr
BiocManager::install("Rarr")
```

Once **Rarr** is installed, we have to load it into our R session:

``` r

library(Rarr)
```

**Rarr** can be used to read files either on local disk or on remote S3
storage systems. First lets take a look at reading from a local file.

### Reading a from a local Zarr array

To demonstrate reading a local file, we’ll pick the example file
containing 32-bit integers arranged in the “column first” ordering.

``` r

zarr_example <- system.file(
  "extdata",
  "zarr_examples",
  "column-first",
  "int32.zarr",
  package = "Rarr"
)
```

#### Exploring the data

We can get an summary of the array properties, such as its shape and
datatype, or group properties, using the function
[`zarr_overview()`](https://huber-group-embl.github.io/Rarr/reference/zarr_overview.md)[^2].

``` r

zarr_overview(zarr_example)
```

    ## Type: Array
    ## Path: /home/runner/work/_temp/Library/Rarr/extdata/zarr_examples/column-first/int32.zarr
    ## Shape: 30 x 20 x 10
    ## Chunk Shape: 10 x 10 x 5
    ## No. of Chunks: 12 (3 x 2 x 2)
    ## Data Type: int32
    ## Endianness: little
    ## Compressor: blosc
    ## Attributes: no

You can use this to check that the location is a valid Zarr array, and
that the shape and datatype of the array content are what you are
expecting. For example, we can see in the output above that the data
type (`int32`) corresponds to what we expect.

#### Reading the Zarr array

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

We then extract the subset using
[`read_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_array.md):

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

### Reading from S3 storage

Read the dedicated [“Working with **remote** Zarr arrays in R”
vignette](https://huber-group-embl.github.io/Rarr/articles/S3.html) for
more information on reading Zarr arrays from S3 storage.

### Writing to a Zarr array

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
write_zarr_array(
  x = x,
  zarr_array_path = path_to_new_zarr,
  chunk_dim = c(10, 5, 1)
)
```

We can check that the contents of the Zarr array is what we’re
expecting. Since the contents of the whole array will be too large to
display here, we use the `index` argument to extract rows 6 to 10, from
the 10th column and 1st slice. That should be the values 96, 97, 98, 99,
100, but retaining the 3-dimensional array structure of the original
array. The second line below uses
[`identical()`](https://rdrr.io/r/base/identical.html) to confirm that
reading the whole Zarr returns something equivalent to our original
input `x`.

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

## Additional details

### Working with Zarr metadata

By default the
[`zarr_overview()`](https://huber-group-embl.github.io/Rarr/reference/zarr_overview.md)
function prints a summary of the array to screen, so you can get a quick
idea of the array properties. However, there are times when it might be
useful to compute on that metadata, in which case printing to screen
isn’t very helpful. If his is the case the function also has the
argument `as_data_frame` which toggles whether to print the output to
screen, as seen before above, or to return a `data.frame` containing the
array details.

``` r

zarr_overview(zarr_example, as_data_frame = TRUE)
```

    ##                                                                                 path
    ## 1 /home/runner/work/_temp/Library/Rarr/extdata/zarr_examples/column-first/int32.zarr
    ##   data_type endianness compressor        dim chunk_dim nchunks attributes
    ## 1     int32     little      blosc 30, 20, 10 10, 10, 5 3, 2, 2      FALSE

### Writing subsets of data

One of the key features of the Zarr specification is that the arrays are
chunked, allowing rapid access to the required data without needed to
read or write everything else. If you want to modify a subset of a Zarr
array, it is very inefficient to write all chunks to disk, which is what
[`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md)
does. Instead, **Rarr** provides two functions for reducing the amount
of writing required if the circumstances allow:
[`create_empty_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/create_empty_zarr_array.md)
and
[`update_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/update_zarr_array.md).

#### Creating an “empty” array

Despite the name, you can actually think of
[`create_empty_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/create_empty_zarr_array.md)
as creating an array where all the values are the same. The Zarr
specification allows for “uninitialized” chunks, which are not actually
present on disk. In this case, any reading application assumes the
entirety of the chunk is filled with a single value, which is found in
the array metadata. This allows for very efficient creation of the new
array, since only a small metadata file is actually written. However it
is necessary to provide some additional details, such as the shape of
the array, since there’s no R array to infer these from. Let’s look at
an example:

``` r

path <- tempfile()
create_empty_zarr_array(
  zarr_array_path = path,
  dim = c(50, 20),
  chunk_dim = c(5, 10),
  data_type = "integer",
  fill_value = 7L
)
```

First we have to provide a location for the array to be created using
the `zarr_array_path` argument. Then we provide the dimensions of the
new array, and the shape of the chunks it should be split into. These
two arguments must be compatible with one another i.e. have the same
number of dimensions and no value in `chunk_dim` should exceed the
corresponding value in `dim`. The `data_type` argument defines what type
of values will be stored in the array. Finally we use the `fill_value`
argument to provide our default value for the uninitialized chunks. The
next few lines check what’s actually been created on our file system.
First, we use [`list.files()`](https://rdrr.io/r/base/list.files.html)
to confirm that that only file that’s been created is the `zarr.json`
metadata; there are no chunk files. Then we use
[`table()`](https://rdrr.io/r/base/table.html) to check the contents of
the array, and confirm that when it’s read the resulting array in R is
full of 7s, our fill value.

``` r

list.files(path, all.files = TRUE, no.. = TRUE)
```

    ## [1] "zarr.json"

``` r

table(read_zarr_array(path))
```

    ## 
    ##    7 
    ## 1000

#### Updating a subset of an existing array

Lets assume we want to update the first row of our array to contain the
sequence of integers from 1 to 20. In the code below we create an
example vector containing the new data. We then use
[`update_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/update_zarr_array.md),
passing the location of the Zarr and the new values to be inserted.
Finally, we provide the `index` argument which defines which elements in
the Zarr array should be updated. It’s important that the shape and
number of values in `x` corresponds to the total count of points in the
Zarr array we want to update e.g. in this case we’re updating a single
row of 20 values.

``` r

x <- 1:20
update_zarr_array(
  zarr_array_path = path,
  x = x,
  index = list(1, 1:20)
)
```

As before, we can take a look at what’s happened on disk and confirm the
values are present in the array if we read it into R.

``` r

list.files(path, all.files = TRUE, no.. = TRUE)
```

    ## [1] "c"         "zarr.json"

``` r

read_zarr_array(path, index = list(1:2, 1:5))
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    1    2    3    4    5
    ## [2,]    7    7    7    7    7

``` r

table(read_zarr_array(path))
```

    ## 
    ##   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
    ##   1   1   1   1   1   1 981   1   1   1   1   1   1   1   1   1   1   1   1   1

Here [`list.files()`](https://rdrr.io/r/base/list.files.html) confirms
that there’s now two chunk files that have been created. When we first
created the Zarr we specified that the chunks should be 10 columns wide,
so by modifying 20 columns we’d expect at least two chunks to be
realized on disk. We use
[`read_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_array.md)
to confirm visually that the first row contains our sequence of values,
whilst the second row is still all 7. We use
[`table()`](https://rdrr.io/r/base/table.html) to confirm that the total
contents is as expected.

## Appendix

### Session info

    ## R Under development (unstable) (2026-06-21 r90185)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.4 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] Rarr_2.1.21      BiocStyle_2.40.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] crayon_1.5.3        cli_3.6.6           knitr_1.51         
    ##  [4] rlang_1.3.0         xfun_0.60           otel_0.2.0         
    ##  [7] textshaping_1.0.5   jsonlite_2.0.0      glue_1.8.1         
    ## [10] grumpy_0.1.1        htmltools_0.5.9     ragg_1.5.2         
    ## [13] sass_0.4.10         rappdirs_0.3.4      rmarkdown_2.31     
    ## [16] evaluate_1.0.5      jquerylib_0.1.4     fastmap_1.2.0      
    ## [19] yaml_2.3.12         lifecycle_1.0.5     httr2_1.2.3        
    ## [22] bookdown_0.47       BiocManager_1.30.27 compiler_4.7.0     
    ## [25] fs_2.1.0            Rcpp_1.1.2          R.oo_1.27.1        
    ## [28] R.utils_2.13.0      systemfonts_1.3.2   digest_0.6.39      
    ## [31] R6_2.6.1            curl_7.1.0          paws.common_0.8.10 
    ## [34] paws.storage_0.10.0 magrittr_2.0.5      R.methodsS3_1.8.2  
    ## [37] bslib_0.11.0        tools_4.7.0         pkgdown_2.2.1      
    ## [40] cachem_1.1.0        desc_1.4.3

[^1]: you only need to do the installation step once

[^2]: This is essentially reading and formatting the array metadata that
    accompanies any Zarr array, or the consolidated metadata if present
    in the case of a Zarr group.
