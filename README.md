# Rarr

## Reading a subset from a local Zarr array

We have some example Zarr arrays in the package.   

```{r}
zarr_example <- system.file("extdata", "zarr_examples", "column-first", "int32.zarr",
                      package = "Rarr")
```

In this example the array has three dimensions of size 30 x 20 x 10.  We can select the subset we want to extract using a `list`:

``{r}
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

If reading from and AWS S3 bucket, **Rarr** currently required you to use the `https` address
and provide it in the oath style.  Support for virtual-host style addressing, using `s3://` notation
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

Currently there is only support for reading a subset of the possible datatypes
that can be found in a Zarr array.  

## Data Type

There are limitations on the 
datatypes supported by R.  The table below summarised the current status of
datatype support.  It will be updated as progress is made.

## Datatypes

| Data Type | Status | Notes |
|-----------|--------|-------|
|`int8`|&#x2754;||
|`uint8`|&#x2754;||
|`int16`|&#x2754;||
|`uint16`|&#x2714;||
|`int32`|&#x2714;||
|`uint32`|&#x2714;|Values outside the range of `int32` are converted to `NA`| 
|`int64`|&#x274C;|Values outside the range of `int32` are converted to `NA`|
|`uint64`|&#x2754;||
|`float16`|&#x274C;||
|`float32`|&#x274C;||
|`double`|&#x2714;||
|`complex`|&#x274C;||
|`timedelta`|&#x274C;||
|`datetime`|&#x274C;||
|`string`|&#x2714;||
|`Unicode`|&#x274C;||
|`void *`|&#x274C;||
