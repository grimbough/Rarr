# Empty zarr arrays have expected metadata

    Code
      jsonlite::read_json(file.path(empty_zarr, ".zarray"))
    Output
      $shape
      $shape[[1]]
      [1] 10
      
      $shape[[2]]
      [1] 30
      
      
      $chunks
      $chunks[[1]]
      [1] 5
      
      $chunks[[2]]
      [1] 3
      
      
      $dtype
      [1] "<i4"
      
      $fill_value
      [1] 0
      
      $dimension_separator
      [1] "."
      
      $order
      [1] "F"
      
      $zarr_format
      [1] 2
      
      $filters
      NULL
      
      $compressor
      $compressor$id
      [1] "blosc"
      
      $compressor$cname
      [1] "blosclz"
      
      $compressor$clevel
      [1] 9
      
      $compressor$shuffle
      [1] "bitshuffle"
      
      $compressor$typesize
      [1] 4
      
      $compressor$blocksize
      [1] 0
      
      

