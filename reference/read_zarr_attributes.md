# Read the attributes associated with a Zarr array or group

Read the attributes associated with a Zarr array or group

## Usage

``` r
read_zarr_attributes(
  zarr_path,
  s3_client = NULL,
  missing = c("ignore", "warning", "error")
)
```

## Arguments

- zarr_path:

  A character vector of length 1. This provides the path to a Zarr array
  or group of arrays. This can either be on a local file system or on S3
  storage.

- s3_client:

  A list representing an S3 client. This should be produced by
  [`paws.storage::s3()`](https://paws-r.r-universe.dev/paws.storage/reference/s3.html).

- missing:

  A character vector of length 1. This determines the behaviour when no
  file containing attributes is found. This can be one of:

  - "ignore" (the default): an empty list is returned silently

  - "warning": a warning is issued and an empty list is returned.

  - "error": an error is raised.

## Value

A list containing the attributes. If the file containing attributes
(`.zattrs` for Zarr v2 or `zarr.json` for Zarr v3) exists but no
attributes are provided, an empty list is returned.

## Examples

``` r
read_zarr_attributes(
  "https://uk1s3.embassy.ebi.ac.uk/idr/zarr/v0.4/idr0048A/9846152.zarr"
)
#> $`_creator`
#> $`_creator`$name
#> [1] "omero-zarr"
#> 
#> $`_creator`$version
#> [1] "0.5.2.dev20+g6af4061"
#> 
#> 
#> $multiscales
#> $multiscales[[1]]
#> $multiscales[[1]]$axes
#> $multiscales[[1]]$axes[[1]]
#> $multiscales[[1]]$axes[[1]]$name
#> [1] "c"
#> 
#> $multiscales[[1]]$axes[[1]]$type
#> [1] "channel"
#> 
#> 
#> $multiscales[[1]]$axes[[2]]
#> $multiscales[[1]]$axes[[2]]$name
#> [1] "z"
#> 
#> $multiscales[[1]]$axes[[2]]$type
#> [1] "space"
#> 
#> 
#> $multiscales[[1]]$axes[[3]]
#> $multiscales[[1]]$axes[[3]]$name
#> [1] "y"
#> 
#> $multiscales[[1]]$axes[[3]]$type
#> [1] "space"
#> 
#> 
#> $multiscales[[1]]$axes[[4]]
#> $multiscales[[1]]$axes[[4]]$name
#> [1] "x"
#> 
#> $multiscales[[1]]$axes[[4]]$type
#> [1] "space"
#> 
#> 
#> 
#> $multiscales[[1]]$datasets
#> $multiscales[[1]]$datasets[[1]]
#> $multiscales[[1]]$datasets[[1]]$coordinateTransformations
#> $multiscales[[1]]$datasets[[1]]$coordinateTransformations[[1]]
#> $multiscales[[1]]$datasets[[1]]$coordinateTransformations[[1]]$scale
#> $multiscales[[1]]$datasets[[1]]$coordinateTransformations[[1]]$scale[[1]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[1]]$coordinateTransformations[[1]]$scale[[2]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[1]]$coordinateTransformations[[1]]$scale[[3]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[1]]$coordinateTransformations[[1]]$scale[[4]]
#> [1] 1
#> 
#> 
#> $multiscales[[1]]$datasets[[1]]$coordinateTransformations[[1]]$type
#> [1] "scale"
#> 
#> 
#> 
#> $multiscales[[1]]$datasets[[1]]$path
#> [1] "0"
#> 
#> 
#> $multiscales[[1]]$datasets[[2]]
#> $multiscales[[1]]$datasets[[2]]$coordinateTransformations
#> $multiscales[[1]]$datasets[[2]]$coordinateTransformations[[1]]
#> $multiscales[[1]]$datasets[[2]]$coordinateTransformations[[1]]$scale
#> $multiscales[[1]]$datasets[[2]]$coordinateTransformations[[1]]$scale[[1]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[2]]$coordinateTransformations[[1]]$scale[[2]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[2]]$coordinateTransformations[[1]]$scale[[3]]
#> [1] 2
#> 
#> $multiscales[[1]]$datasets[[2]]$coordinateTransformations[[1]]$scale[[4]]
#> [1] 2
#> 
#> 
#> $multiscales[[1]]$datasets[[2]]$coordinateTransformations[[1]]$type
#> [1] "scale"
#> 
#> 
#> 
#> $multiscales[[1]]$datasets[[2]]$path
#> [1] "1"
#> 
#> 
#> $multiscales[[1]]$datasets[[3]]
#> $multiscales[[1]]$datasets[[3]]$coordinateTransformations
#> $multiscales[[1]]$datasets[[3]]$coordinateTransformations[[1]]
#> $multiscales[[1]]$datasets[[3]]$coordinateTransformations[[1]]$scale
#> $multiscales[[1]]$datasets[[3]]$coordinateTransformations[[1]]$scale[[1]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[3]]$coordinateTransformations[[1]]$scale[[2]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[3]]$coordinateTransformations[[1]]$scale[[3]]
#> [1] 4
#> 
#> $multiscales[[1]]$datasets[[3]]$coordinateTransformations[[1]]$scale[[4]]
#> [1] 4
#> 
#> 
#> $multiscales[[1]]$datasets[[3]]$coordinateTransformations[[1]]$type
#> [1] "scale"
#> 
#> 
#> 
#> $multiscales[[1]]$datasets[[3]]$path
#> [1] "2"
#> 
#> 
#> $multiscales[[1]]$datasets[[4]]
#> $multiscales[[1]]$datasets[[4]]$coordinateTransformations
#> $multiscales[[1]]$datasets[[4]]$coordinateTransformations[[1]]
#> $multiscales[[1]]$datasets[[4]]$coordinateTransformations[[1]]$scale
#> $multiscales[[1]]$datasets[[4]]$coordinateTransformations[[1]]$scale[[1]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[4]]$coordinateTransformations[[1]]$scale[[2]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[4]]$coordinateTransformations[[1]]$scale[[3]]
#> [1] 8
#> 
#> $multiscales[[1]]$datasets[[4]]$coordinateTransformations[[1]]$scale[[4]]
#> [1] 8
#> 
#> 
#> $multiscales[[1]]$datasets[[4]]$coordinateTransformations[[1]]$type
#> [1] "scale"
#> 
#> 
#> 
#> $multiscales[[1]]$datasets[[4]]$path
#> [1] "3"
#> 
#> 
#> $multiscales[[1]]$datasets[[5]]
#> $multiscales[[1]]$datasets[[5]]$coordinateTransformations
#> $multiscales[[1]]$datasets[[5]]$coordinateTransformations[[1]]
#> $multiscales[[1]]$datasets[[5]]$coordinateTransformations[[1]]$scale
#> $multiscales[[1]]$datasets[[5]]$coordinateTransformations[[1]]$scale[[1]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[5]]$coordinateTransformations[[1]]$scale[[2]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[5]]$coordinateTransformations[[1]]$scale[[3]]
#> [1] 16
#> 
#> $multiscales[[1]]$datasets[[5]]$coordinateTransformations[[1]]$scale[[4]]
#> [1] 16
#> 
#> 
#> $multiscales[[1]]$datasets[[5]]$coordinateTransformations[[1]]$type
#> [1] "scale"
#> 
#> 
#> 
#> $multiscales[[1]]$datasets[[5]]$path
#> [1] "4"
#> 
#> 
#> $multiscales[[1]]$datasets[[6]]
#> $multiscales[[1]]$datasets[[6]]$coordinateTransformations
#> $multiscales[[1]]$datasets[[6]]$coordinateTransformations[[1]]
#> $multiscales[[1]]$datasets[[6]]$coordinateTransformations[[1]]$scale
#> $multiscales[[1]]$datasets[[6]]$coordinateTransformations[[1]]$scale[[1]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[6]]$coordinateTransformations[[1]]$scale[[2]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[6]]$coordinateTransformations[[1]]$scale[[3]]
#> [1] 32
#> 
#> $multiscales[[1]]$datasets[[6]]$coordinateTransformations[[1]]$scale[[4]]
#> [1] 32
#> 
#> 
#> $multiscales[[1]]$datasets[[6]]$coordinateTransformations[[1]]$type
#> [1] "scale"
#> 
#> 
#> 
#> $multiscales[[1]]$datasets[[6]]$path
#> [1] "5"
#> 
#> 
#> $multiscales[[1]]$datasets[[7]]
#> $multiscales[[1]]$datasets[[7]]$coordinateTransformations
#> $multiscales[[1]]$datasets[[7]]$coordinateTransformations[[1]]
#> $multiscales[[1]]$datasets[[7]]$coordinateTransformations[[1]]$scale
#> $multiscales[[1]]$datasets[[7]]$coordinateTransformations[[1]]$scale[[1]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[7]]$coordinateTransformations[[1]]$scale[[2]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[7]]$coordinateTransformations[[1]]$scale[[3]]
#> [1] 64
#> 
#> $multiscales[[1]]$datasets[[7]]$coordinateTransformations[[1]]$scale[[4]]
#> [1] 64
#> 
#> 
#> $multiscales[[1]]$datasets[[7]]$coordinateTransformations[[1]]$type
#> [1] "scale"
#> 
#> 
#> 
#> $multiscales[[1]]$datasets[[7]]$path
#> [1] "6"
#> 
#> 
#> $multiscales[[1]]$datasets[[8]]
#> $multiscales[[1]]$datasets[[8]]$coordinateTransformations
#> $multiscales[[1]]$datasets[[8]]$coordinateTransformations[[1]]
#> $multiscales[[1]]$datasets[[8]]$coordinateTransformations[[1]]$scale
#> $multiscales[[1]]$datasets[[8]]$coordinateTransformations[[1]]$scale[[1]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[8]]$coordinateTransformations[[1]]$scale[[2]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[8]]$coordinateTransformations[[1]]$scale[[3]]
#> [1] 128
#> 
#> $multiscales[[1]]$datasets[[8]]$coordinateTransformations[[1]]$scale[[4]]
#> [1] 128
#> 
#> 
#> $multiscales[[1]]$datasets[[8]]$coordinateTransformations[[1]]$type
#> [1] "scale"
#> 
#> 
#> 
#> $multiscales[[1]]$datasets[[8]]$path
#> [1] "7"
#> 
#> 
#> $multiscales[[1]]$datasets[[9]]
#> $multiscales[[1]]$datasets[[9]]$coordinateTransformations
#> $multiscales[[1]]$datasets[[9]]$coordinateTransformations[[1]]
#> $multiscales[[1]]$datasets[[9]]$coordinateTransformations[[1]]$scale
#> $multiscales[[1]]$datasets[[9]]$coordinateTransformations[[1]]$scale[[1]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[9]]$coordinateTransformations[[1]]$scale[[2]]
#> [1] 1
#> 
#> $multiscales[[1]]$datasets[[9]]$coordinateTransformations[[1]]$scale[[3]]
#> [1] 256
#> 
#> $multiscales[[1]]$datasets[[9]]$coordinateTransformations[[1]]$scale[[4]]
#> [1] 256
#> 
#> 
#> $multiscales[[1]]$datasets[[9]]$coordinateTransformations[[1]]$type
#> [1] "scale"
#> 
#> 
#> 
#> $multiscales[[1]]$datasets[[9]]$path
#> [1] "8"
#> 
#> 
#> 
#> $multiscales[[1]]$name
#> [1] "/"
#> 
#> $multiscales[[1]]$version
#> [1] "0.4"
#> 
#> 
#> 
#> $omero
#> $omero$channels
#> $omero$channels[[1]]
#> $omero$channels[[1]]$active
#> [1] TRUE
#> 
#> $omero$channels[[1]]$coefficient
#> [1] 1
#> 
#> $omero$channels[[1]]$color
#> [1] "FF0000"
#> 
#> $omero$channels[[1]]$family
#> [1] "linear"
#> 
#> $omero$channels[[1]]$inverted
#> [1] FALSE
#> 
#> $omero$channels[[1]]$label
#> [1] "0"
#> 
#> $omero$channels[[1]]$window
#> $omero$channels[[1]]$window$end
#> [1] 25000
#> 
#> $omero$channels[[1]]$window$max
#> [1] 65535
#> 
#> $omero$channels[[1]]$window$min
#> [1] 0
#> 
#> $omero$channels[[1]]$window$start
#> [1] 0
#> 
#> 
#> 
#> $omero$channels[[2]]
#> $omero$channels[[2]]$active
#> [1] TRUE
#> 
#> $omero$channels[[2]]$coefficient
#> [1] 1
#> 
#> $omero$channels[[2]]$color
#> [1] "00FF00"
#> 
#> $omero$channels[[2]]$family
#> [1] "linear"
#> 
#> $omero$channels[[2]]$inverted
#> [1] FALSE
#> 
#> $omero$channels[[2]]$label
#> [1] "1"
#> 
#> $omero$channels[[2]]$window
#> $omero$channels[[2]]$window$end
#> [1] 25000
#> 
#> $omero$channels[[2]]$window$max
#> [1] 65535
#> 
#> $omero$channels[[2]]$window$min
#> [1] 0
#> 
#> $omero$channels[[2]]$window$start
#> [1] 0
#> 
#> 
#> 
#> $omero$channels[[3]]
#> $omero$channels[[3]]$active
#> [1] TRUE
#> 
#> $omero$channels[[3]]$coefficient
#> [1] 1
#> 
#> $omero$channels[[3]]$color
#> [1] "0000FF"
#> 
#> $omero$channels[[3]]$family
#> [1] "linear"
#> 
#> $omero$channels[[3]]$inverted
#> [1] FALSE
#> 
#> $omero$channels[[3]]$label
#> [1] "2"
#> 
#> $omero$channels[[3]]$window
#> $omero$channels[[3]]$window$end
#> [1] 25000
#> 
#> $omero$channels[[3]]$window$max
#> [1] 65535
#> 
#> $omero$channels[[3]]$window$min
#> [1] 0
#> 
#> $omero$channels[[3]]$window$start
#> [1] 0
#> 
#> 
#> 
#> 
#> $omero$id
#> [1] 1
#> 
#> $omero$rdefs
#> $omero$rdefs$defaultT
#> [1] 0
#> 
#> $omero$rdefs$defaultZ
#> [1] 45
#> 
#> $omero$rdefs$model
#> [1] "color"
#> 
#> 
#> $omero$version
#> [1] "0.4"
#> 
#> 
```
