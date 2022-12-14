# Rarr


## Read a data selection from an S3 bucket

```{r}
path <- 's3://mghp.osn.xsede.org/bir190004-bucket01/TMA11/zarr/10.zarr'
read_zarr_array(path, index = list(1, 1:10, 1:10))
```
