# read_zarr_attributes errors clearly for invalid mixed arrays

    The path contains both `.zattrs` (v2 Zarr specification) and `zarr.json` (v3 Zarr specification) files, which is not allowed.

# read_zarr_attributes from s3

    Code
      read_zarr_attributes(
        "https://uk1s3.embassy.ebi.ac.uk/idr/zarr/v0.3/9836842.zarr/")
    Output
      $`_creator`
      $`_creator`$name
      [1] "omero-zarr"
      
      $`_creator`$version
      [1] "0.1.dev219+g541c88e"
      
      
      $multiscales
           axes     datasets version
      1 c, y, x c("0", "....     0.3
      
      $omero
      $omero$channels
        active coefficient  color family inverted   label window.end window.max
      1   TRUE           1 FF0000 linear    FALSE Cam2-T1       5090      65535
      2   TRUE           1 00C000 linear    FALSE Cam1-T2       5736      65535
      3   TRUE           1 FFFFFF linear    FALSE       2       5312      65535
      4  FALSE           1 FFFFFF linear    FALSE       3       4128      65535
        window.min window.start
      1          0          198
      2          0          198
      3          0          196
      4          0          194
      
      $omero$id
      [1] 1
      
      $omero$rdefs
      $omero$rdefs$defaultT
      [1] 0
      
      $omero$rdefs$defaultZ
      [1] 0
      
      $omero$rdefs$model
      [1] "color"
      
      
      $omero$version
      [1] "0.3"
      
      

