import zarrita
from numpy import array, arange
import numpy as np
import asyncio
import os
import shutil

store = zarrita.LocalStore("/data/v3")

# 1d.contiguous.compressed.i2
data = array([1, 2, 3, 4], dtype="int32")
path = "1d.contiguous.compressed.i2.zarr"
shutil.rmtree(os.path.join("/data/v3/", "1d.contiguous.compressed.i2.zarr"))
a = zarrita.Array.create(
        store / path,
        shape=data.shape,
        dtype=data.dtype,
        chunk_shape=(4,),
        codecs=[
          zarrita.codecs.endian_codec(),
          zarrita.codecs.gzip_codec()
        ],
    )
a[:] = data


data = np.full((30, 20, 10), 0, dtype='int16')
data[0, :, 0] = np.arange(start=1, stop=21)
data[:, 0, 0] = 1
path = "column-first/int16.zarr"
shutil.rmtree(os.path.join("/data/v3/", path), ignore_errors=True)
a = zarrita.Array.create(
        store / path,
        shape=(30, 20, 10),
        dtype='int16',
        chunk_shape=(10, 10, 5),
        codecs=[
          zarrita.codecs.endian_codec(),
          zarrita.codecs.gzip_codec()
        ],
    )
a[:] = data

