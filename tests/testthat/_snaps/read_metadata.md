# zarr_overview console output matches snapshot for single array

    Code
      zarr_overview(zarr_c, as_data_frame = FALSE)
    Output
      Type: Array
      Path: <path>
      Shape: 30 x 20 x 10
      Chunk Shape: 10 x 10 x 5
      No. of Chunks: 12 (3 x 2 x 2)
      Data Type: int64
      Endianness: little
      Compressor: blosc

# zarr_overview console output matches snapshot for consolidated store

    Code
      zarr_overview(zarr_store_consolidated, as_data_frame = FALSE)
    Output
      Type: Group of Arrays
      Path: <path>
      Arrays:
      ---
      Path: <path>
        Shape: 1
        Chunk Shape: 1
        No. of Chunks: 1 (1)
        Data Type: float64
        Endianness: little
        Compressor: blosc
      ---
      Path: <path>
        Shape: 2 x 2
        Chunk Shape: 2 x 2
        No. of Chunks: 1 (1 x 1)
        Data Type: float64
        Endianness: little
        Compressor: blosc
      ---
      Path: <path>
        Shape: 3 x 3 x 3
        Chunk Shape: 3 x 3 x 3
        No. of Chunks: 1 (1 x 1 x 1)
        Data Type: float64
        Endianness: little
        Compressor: blosc

# zarr_overview console output matches snapshot for v3 metadata

    Code
      zarr_overview(zarr_v3, as_data_frame = FALSE)
    Output
      Type: Array
      Path: <path>
      Shape: 3 x 4 x 5
      Chunk Shape: 3 x 4 x 5
      No. of Chunks: 1 (1 x 1 x 1)
      Data Type: int64
      Endianness: little
      Compressor: zstd

# zarr_overview throws error for mixed v2/v3 zarr

    The path contains both `.zarray` (Zarr V2 specification) and `zarr.json` (Zarr V3 specification) metadata files.
    An array or group must conform to either the Zarr V2 or V3 specification.

