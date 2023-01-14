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

```{r}
path <- 's3://mghp.osn.xsede.org/bir190004-bucket01/TMA11/zarr/10.zarr'
read_zarr_array(path, index = list(1, 1:10, 1:10))
```


# Current Status

## Datatypes

| Data Type | Status | Notes |
|-----------|--------|-------|
|`int8`|&#x2754||
|`unit8`|❔||
|`int16`|❔||
|`uint16`|✅||
|`int32`|✅||
|`uint32`|✅||Values outs ide the range of `int32` are converted to `NA` 
|`int64`|❌||
|`uint64`|❔||
|`float`|❌||
|`double`|✅||
|`string`|✅||
|`Unicode`|❌||
