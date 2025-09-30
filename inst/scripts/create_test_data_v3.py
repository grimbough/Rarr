import zarr
import numpy as np

x = np.arange(60).reshape(3, 4, 5)
zarr.save("inst/extdata/zarr_examples/metadata/v3.zarr", x)

z1 = zarr.create_array(
    store="inst/extdata/zarr_examples/metadata/v3_attr.zarr",
    shape=(10, 10),
    chunks=(10, 10),
    dtype="int32",
)

z1.attrs["custom"] = "Hello, Zarr!"
