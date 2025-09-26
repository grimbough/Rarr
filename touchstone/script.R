# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

# TODO OPTIONAL Add directories you want to be available in this file or during the
# benchmarks.
# touchstone::pin_assets("some/dir")

# installs branches to benchmark
touchstone::branch_install()

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

touchstone::benchmark_analyze()
