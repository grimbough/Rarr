# zarr_consolidate_metadata() on v2

    Code
      zarr_consolidate_metadata(zarr_v2, action = "return")
    Output
      $zarr_consolidated_format
      [1] 1
      
      $metadata
      $metadata$.zgroup
      $metadata$.zgroup$zarr_format
      $metadata$.zgroup$zarr_format[[1]]
      [1] 2
      
      
      
      $metadata$`array1/.zarray`
      $metadata$`array1/.zarray`$shape
      $metadata$`array1/.zarray`$shape[[1]]
      [1] 2
      
      $metadata$`array1/.zarray`$shape[[2]]
      [1] 2
      
      
      $metadata$`array1/.zarray`$chunks
      $metadata$`array1/.zarray`$chunks[[1]]
      [1] 1
      
      $metadata$`array1/.zarray`$chunks[[2]]
      [1] 2
      
      
      $metadata$`array1/.zarray`$dtype
      [1] "<i4"
      
      $metadata$`array1/.zarray`$fill_value
      [1] 0
      
      $metadata$`array1/.zarray`$dimension_separator
      [1] "."
      
      $metadata$`array1/.zarray`$order
      [1] "F"
      
      $metadata$`array1/.zarray`$zarr_format
      [1] 2
      
      $metadata$`array1/.zarray`$filters
      NULL
      
      $metadata$`array1/.zarray`$compressor
      $metadata$`array1/.zarray`$compressor$id
      [1] "zstd"
      
      $metadata$`array1/.zarray`$compressor$level
      [1] 3
      
      
      
      $metadata$`array1/.zattrs`
      $metadata$`array1/.zattrs`$description
      [1] "This is array 1"
      
      
      $metadata$`array2/.zarray`
      $metadata$`array2/.zarray`$shape
      $metadata$`array2/.zarray`$shape[[1]]
      [1] 2
      
      $metadata$`array2/.zarray`$shape[[2]]
      [1] 2
      
      
      $metadata$`array2/.zarray`$chunks
      $metadata$`array2/.zarray`$chunks[[1]]
      [1] 1
      
      $metadata$`array2/.zarray`$chunks[[2]]
      [1] 2
      
      
      $metadata$`array2/.zarray`$dtype
      [1] "<f8"
      
      $metadata$`array2/.zarray`$fill_value
      [1] 0
      
      $metadata$`array2/.zarray`$dimension_separator
      [1] "."
      
      $metadata$`array2/.zarray`$order
      [1] "F"
      
      $metadata$`array2/.zarray`$zarr_format
      [1] 2
      
      $metadata$`array2/.zarray`$filters
      NULL
      
      $metadata$`array2/.zarray`$compressor
      $metadata$`array2/.zarray`$compressor$id
      [1] "zstd"
      
      $metadata$`array2/.zarray`$compressor$level
      [1] 3
      
      
      
      

# zarr_consoliate_metadata() on v3

    Code
      zarr_consolidate_metadata(zarr_v3, action = "return")
    Output
      $zarr_format
      [1] 3
      
      $node_type
      [1] "group"
      
      $attributes
      list()
      
      $consolidated_metadata
      $consolidated_metadata$kind
      [1] "inline"
      
      $consolidated_metadata$must_understand
      [1] FALSE
      
      $consolidated_metadata$metadata
      $consolidated_metadata$metadata$array1
      $consolidated_metadata$metadata$array1$node_type
      [1] "array"
      
      $consolidated_metadata$metadata$array1$zarr_format
      [1] 3
      
      $consolidated_metadata$metadata$array1$shape
      $consolidated_metadata$metadata$array1$shape[[1]]
      [1] 2
      
      $consolidated_metadata$metadata$array1$shape[[2]]
      [1] 2
      
      
      $consolidated_metadata$metadata$array1$chunk_grid
      $consolidated_metadata$metadata$array1$chunk_grid$name
      [1] "regular"
      
      $consolidated_metadata$metadata$array1$chunk_grid$configuration
      $consolidated_metadata$metadata$array1$chunk_grid$configuration$chunk_shape
      $consolidated_metadata$metadata$array1$chunk_grid$configuration$chunk_shape[[1]]
      [1] 1
      
      $consolidated_metadata$metadata$array1$chunk_grid$configuration$chunk_shape[[2]]
      [1] 2
      
      
      
      
      $consolidated_metadata$metadata$array1$chunk_key_encoding
      $consolidated_metadata$metadata$array1$chunk_key_encoding$name
      [1] "default"
      
      $consolidated_metadata$metadata$array1$chunk_key_encoding$configuration
      $consolidated_metadata$metadata$array1$chunk_key_encoding$configuration$separator
      [1] "/"
      
      
      
      $consolidated_metadata$metadata$array1$fill_value
      [1] 0
      
      $consolidated_metadata$metadata$array1$codecs
      $consolidated_metadata$metadata$array1$codecs[[1]]
      $consolidated_metadata$metadata$array1$codecs[[1]]$name
      [1] "transpose"
      
      $consolidated_metadata$metadata$array1$codecs[[1]]$configuration
      $consolidated_metadata$metadata$array1$codecs[[1]]$configuration$order
      $consolidated_metadata$metadata$array1$codecs[[1]]$configuration$order[[1]]
      [1] 1
      
      $consolidated_metadata$metadata$array1$codecs[[1]]$configuration$order[[2]]
      [1] 0
      
      
      
      
      $consolidated_metadata$metadata$array1$codecs[[2]]
      $consolidated_metadata$metadata$array1$codecs[[2]]$name
      [1] "bytes"
      
      $consolidated_metadata$metadata$array1$codecs[[2]]$configuration
      $consolidated_metadata$metadata$array1$codecs[[2]]$configuration$endian
      [1] "little"
      
      
      
      $consolidated_metadata$metadata$array1$codecs[[3]]
      $consolidated_metadata$metadata$array1$codecs[[3]]$name
      [1] "zstd"
      
      $consolidated_metadata$metadata$array1$codecs[[3]]$configuration
      $consolidated_metadata$metadata$array1$codecs[[3]]$configuration$level
      [1] 3
      
      
      
      
      $consolidated_metadata$metadata$array1$data_type
      [1] "int32"
      
      $consolidated_metadata$metadata$array1$attributes
      $consolidated_metadata$metadata$array1$attributes$description
      [1] "This is array 1"
      
      
      
      $consolidated_metadata$metadata$array2
      $consolidated_metadata$metadata$array2$node_type
      [1] "array"
      
      $consolidated_metadata$metadata$array2$zarr_format
      [1] 3
      
      $consolidated_metadata$metadata$array2$shape
      $consolidated_metadata$metadata$array2$shape[[1]]
      [1] 2
      
      $consolidated_metadata$metadata$array2$shape[[2]]
      [1] 2
      
      
      $consolidated_metadata$metadata$array2$chunk_grid
      $consolidated_metadata$metadata$array2$chunk_grid$name
      [1] "regular"
      
      $consolidated_metadata$metadata$array2$chunk_grid$configuration
      $consolidated_metadata$metadata$array2$chunk_grid$configuration$chunk_shape
      $consolidated_metadata$metadata$array2$chunk_grid$configuration$chunk_shape[[1]]
      [1] 1
      
      $consolidated_metadata$metadata$array2$chunk_grid$configuration$chunk_shape[[2]]
      [1] 2
      
      
      
      
      $consolidated_metadata$metadata$array2$chunk_key_encoding
      $consolidated_metadata$metadata$array2$chunk_key_encoding$name
      [1] "default"
      
      $consolidated_metadata$metadata$array2$chunk_key_encoding$configuration
      $consolidated_metadata$metadata$array2$chunk_key_encoding$configuration$separator
      [1] "/"
      
      
      
      $consolidated_metadata$metadata$array2$fill_value
      [1] 0
      
      $consolidated_metadata$metadata$array2$codecs
      $consolidated_metadata$metadata$array2$codecs[[1]]
      $consolidated_metadata$metadata$array2$codecs[[1]]$name
      [1] "transpose"
      
      $consolidated_metadata$metadata$array2$codecs[[1]]$configuration
      $consolidated_metadata$metadata$array2$codecs[[1]]$configuration$order
      $consolidated_metadata$metadata$array2$codecs[[1]]$configuration$order[[1]]
      [1] 1
      
      $consolidated_metadata$metadata$array2$codecs[[1]]$configuration$order[[2]]
      [1] 0
      
      
      
      
      $consolidated_metadata$metadata$array2$codecs[[2]]
      $consolidated_metadata$metadata$array2$codecs[[2]]$name
      [1] "bytes"
      
      $consolidated_metadata$metadata$array2$codecs[[2]]$configuration
      $consolidated_metadata$metadata$array2$codecs[[2]]$configuration$endian
      [1] "little"
      
      
      
      $consolidated_metadata$metadata$array2$codecs[[3]]
      $consolidated_metadata$metadata$array2$codecs[[3]]$name
      [1] "zstd"
      
      $consolidated_metadata$metadata$array2$codecs[[3]]$configuration
      $consolidated_metadata$metadata$array2$codecs[[3]]$configuration$level
      [1] 3
      
      
      
      
      $consolidated_metadata$metadata$array2$data_type
      [1] "float64"
      
      
      $consolidated_metadata$metadata$.
      $consolidated_metadata$metadata$.$zarr_format
      [1] 3
      
      $consolidated_metadata$metadata$.$node_type
      [1] "group"
      
      
      
      

