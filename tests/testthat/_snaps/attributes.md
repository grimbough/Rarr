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
      $multiscales[[1]]
      $multiscales[[1]]$axes
      $multiscales[[1]]$axes[[1]]
      [1] "c"
      
      $multiscales[[1]]$axes[[2]]
      [1] "y"
      
      $multiscales[[1]]$axes[[3]]
      [1] "x"
      
      
      $multiscales[[1]]$datasets
      $multiscales[[1]]$datasets[[1]]
      $multiscales[[1]]$datasets[[1]]$path
      [1] "0"
      
      
      $multiscales[[1]]$datasets[[2]]
      $multiscales[[1]]$datasets[[2]]$path
      [1] "1"
      
      
      $multiscales[[1]]$datasets[[3]]
      $multiscales[[1]]$datasets[[3]]$path
      [1] "2"
      
      
      $multiscales[[1]]$datasets[[4]]
      $multiscales[[1]]$datasets[[4]]$path
      [1] "3"
      
      
      $multiscales[[1]]$datasets[[5]]
      $multiscales[[1]]$datasets[[5]]$path
      [1] "4"
      
      
      $multiscales[[1]]$datasets[[6]]
      $multiscales[[1]]$datasets[[6]]$path
      [1] "5"
      
      
      
      $multiscales[[1]]$version
      [1] "0.3"
      
      
      
      $omero
      $omero$channels
      $omero$channels[[1]]
      $omero$channels[[1]]$active
      [1] TRUE
      
      $omero$channels[[1]]$coefficient
      [1] 1
      
      $omero$channels[[1]]$color
      [1] "FF0000"
      
      $omero$channels[[1]]$family
      [1] "linear"
      
      $omero$channels[[1]]$inverted
      [1] FALSE
      
      $omero$channels[[1]]$label
      [1] "Cam2-T1"
      
      $omero$channels[[1]]$window
      $omero$channels[[1]]$window$end
      [1] 5090
      
      $omero$channels[[1]]$window$max
      [1] 65535
      
      $omero$channels[[1]]$window$min
      [1] 0
      
      $omero$channels[[1]]$window$start
      [1] 198
      
      
      
      $omero$channels[[2]]
      $omero$channels[[2]]$active
      [1] TRUE
      
      $omero$channels[[2]]$coefficient
      [1] 1
      
      $omero$channels[[2]]$color
      [1] "00C000"
      
      $omero$channels[[2]]$family
      [1] "linear"
      
      $omero$channels[[2]]$inverted
      [1] FALSE
      
      $omero$channels[[2]]$label
      [1] "Cam1-T2"
      
      $omero$channels[[2]]$window
      $omero$channels[[2]]$window$end
      [1] 5736
      
      $omero$channels[[2]]$window$max
      [1] 65535
      
      $omero$channels[[2]]$window$min
      [1] 0
      
      $omero$channels[[2]]$window$start
      [1] 198
      
      
      
      $omero$channels[[3]]
      $omero$channels[[3]]$active
      [1] TRUE
      
      $omero$channels[[3]]$coefficient
      [1] 1
      
      $omero$channels[[3]]$color
      [1] "FFFFFF"
      
      $omero$channels[[3]]$family
      [1] "linear"
      
      $omero$channels[[3]]$inverted
      [1] FALSE
      
      $omero$channels[[3]]$label
      [1] "2"
      
      $omero$channels[[3]]$window
      $omero$channels[[3]]$window$end
      [1] 5312
      
      $omero$channels[[3]]$window$max
      [1] 65535
      
      $omero$channels[[3]]$window$min
      [1] 0
      
      $omero$channels[[3]]$window$start
      [1] 196
      
      
      
      $omero$channels[[4]]
      $omero$channels[[4]]$active
      [1] FALSE
      
      $omero$channels[[4]]$coefficient
      [1] 1
      
      $omero$channels[[4]]$color
      [1] "FFFFFF"
      
      $omero$channels[[4]]$family
      [1] "linear"
      
      $omero$channels[[4]]$inverted
      [1] FALSE
      
      $omero$channels[[4]]$label
      [1] "3"
      
      $omero$channels[[4]]$window
      $omero$channels[[4]]$window$end
      [1] 4128
      
      $omero$channels[[4]]$window$max
      [1] 65535
      
      $omero$channels[[4]]$window$min
      [1] 0
      
      $omero$channels[[4]]$window$start
      [1] 194
      
      
      
      
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
      
      

