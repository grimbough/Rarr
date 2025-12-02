# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

# TODO OPTIONAL Add directories you want to be available in this file or during the
# benchmarks.
# touchstone::pin_assets("some/dir")

# installs branches to benchmark
touchstone::branch_install()

# Types ----------

## Read/Write ----------

# bool
touchstone::benchmark_run(
  {
    library(Rarr)
    x_bool <- matrix(
      c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
      nrow = 5,
      ncol = 2
    )
  },
  write_bool = write_zarr_array(
    x = x_bool,
    zarr_array_path = "boolean.zarr",
    chunk_dim = c(2, 2),
    compressor = use_blosc()
  ),
  n = 25
)

# int8
touchstone::benchmark_run(
  {
    library(Rarr)
    x_int8 <- array(1:1e3, dim = c(10, 10, 10))
  },
  write_int8 = write_zarr_array(
    x = x_int8,
    zarr_array_path = "int8.zarr",
    chunk_dim = c(2, 2, 5),
    compressor = NULL
  ),
  n = 25
)

# int16
touchstone::benchmark_run(
  {
    library(Rarr)
    x_int16 <- array(1:1e3, dim = c(10, 10, 10))
  },
  write_int16 = write_zarr_array(
    x = x_int16,
    zarr_array_path = "int16.zarr",
    chunk_dim = c(2, 2, 5),
    compressor = NULL
  ),
  n = 25
)

# int32 (R integer)
touchstone::benchmark_run(
  {
    library(Rarr)
    x_int32 <- array(1:1e3, dim = c(10, 10, 10))
  },
  write_int32 = write_zarr_array(
    x = x_int32,
    zarr_array_path = "int32.zarr",
    chunk_dim = c(2, 2, 5),
    compressor = NULL
  ),
  n = 25
)

# single (float32)
touchstone::benchmark_run(
  {
    library(Rarr)
    x_float32 <- array(runif(1e3), dim = c(10, 10, 10))
  },
  write_float32 = write_zarr_array(
    x = x_float32,
    zarr_array_path = "float32.zarr",
    chunk_dim = c(2, 2, 5),
    compressor = NULL
  ),
  n = 25
)

# double (float64)
touchstone::benchmark_run(
  {
    library(Rarr)
    x_double <- array(runif(1e3), dim = c(10, 10, 10))
  },
  write_double = write_zarr_array(
    x = x_double,
    zarr_array_path = "double.zarr",
    chunk_dim = c(2, 2, 5),
    compressor = NULL
  ),
  n = 25
)

# character
touchstone::benchmark_run(
  {
    library(Rarr)
    x_string <- matrix(
      sprintf("str_%06d", 1:100),
      nrow = 10,
      ncol = 10
    )
  },
  write_string = write_zarr_array(
    x = x_string,
    zarr_array_path = "string.zarr",
    chunk_dim = c(5, 5),
    compressor = NULL
  ),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
    x_int32 <- array(1:1e3, dim = c(10, 10, 10))
    write_int32 = write_zarr_array(
      x = x_int32,
      zarr_array_path = "int32.zarr",
      chunk_dim = c(2, 2, 5),
      compressor = NULL
    )
  },
  read_int32 = read_zarr_array("int32.zarr"),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
    x_double <- array(runif(1e3), dim = c(10, 10, 10))
    write_double = write_zarr_array(
      x = x_double,
      zarr_array_path = "double.zarr",
      chunk_dim = c(2, 2, 5),
      compressor = NULL
    )
  },
  read_double = read_zarr_array("double.zarr"),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
    x_string <- matrix(
      sprintf("str_%06d", 1:100),
      nrow = 10,
      ncol = 10
    )
    write_string = write_zarr_array(
      x = x_string,
      zarr_array_path = "string.zarr",
      chunk_dim = c(5, 5),
      compressor = NULL
    )
  },
  read_unicode = read_zarr_array("string.zarr"),
  n = 25
)

## Read only ----------

touchstone::benchmark_run(
  {
    library(Rarr)
  },
  read_boolean = read_zarr_array(
    system.file(
      "extdata",
      "zarr_examples",
      "column-first",
      "boolean.zarr",
      package = "Rarr"
    )
  ),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
  },
  read_int8 = read_zarr_array(
    system.file(
      "extdata",
      "zarr_examples",
      "column-first",
      "int8.zarr",
      package = "Rarr"
    )
  ),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
  },
  read_int16 = read_zarr_array(
    system.file(
      "extdata",
      "zarr_examples",
      "column-first",
      "int16.zarr",
      package = "Rarr"
    )
  ),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
  },
  read_int64 = read_zarr_array(
    system.file(
      "extdata",
      "zarr_examples",
      "column-first",
      "int64.zarr",
      package = "Rarr"
    )
  ),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
  },
  read_uint32 = read_zarr_array(
    system.file(
      "extdata",
      "zarr_examples",
      "column-first",
      "uint32.zarr",
      package = "Rarr"
    )
  ),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
  },
  read_uint64 = read_zarr_array(
    system.file(
      "extdata",
      "zarr_examples",
      "column-first",
      "uint64.zarr",
      package = "Rarr"
    )
  ),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
  },
  read_float16 = read_zarr_array(
    system.file(
      "extdata",
      "zarr_examples",
      "column-first",
      "float16.zarr",
      package = "Rarr"
    )
  ),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
  },
  read_float32 = read_zarr_array(
    system.file(
      "extdata",
      "zarr_examples",
      "column-first",
      "float32.zarr",
      package = "Rarr"
    )
  ),
  n = 25
)

## Compression ----------

touchstone::benchmark_run(
  {
    library(Rarr)
  },
  write_zstd = write_zarr_array(
    array(1:1e6, dim = c(100, 100, 100)),
    "zstd.zarr",
    chunk_dim = c(10, 10, 10),
    compressor = use_zstd(level = 22)
  ),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
    write_zarr_array(
      array(1:1e6, dim = c(100, 100, 100)),
      "zstd.zarr",
      chunk_dim = c(10, 10, 10),
      compressor = use_zstd(level = 22)
    )
  },
  read_zstd = read_zarr_array("zstd.zarr"),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
  },
  write_blosc_lz4 = write_zarr_array(
    array(1:1e6, dim = c(100, 100, 100)),
    "blosc_lz4.zarr",
    chunk_dim = c(10, 10, 10),
    compressor = use_blosc(cname = "lz4")
  ),
  n = 25
)

touchstone::benchmark_run(
  {
    library(Rarr)
    write_zarr_array(
      array(1:1e6, dim = c(100, 100, 100)),
      "blosc_lz4.zarr",
      chunk_dim = c(10, 10, 10),
      compressor = use_blosc(cname = "lz4")
    )
  },
  read_blosc_lz4 = read_zarr_array("blosc_lz4.zarr"),
  n = 25
)

touchstone::benchmark_analyze()
