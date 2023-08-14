import zarrita
from numpy import array, arange
import numpy as np
import asyncio
import os
import shutil

data_store = "/data/v3"
store = zarrita.LocalStore(data_store)

#####################

shape = (30,20,10)

data = np.full(shape, 0, dtype='int32')
data[0, :, 0] = np.arange(start=1, stop=21)
data[:, 0, 0] = 1
path = "column-first/int32.zarr"
shutil.rmtree(os.path.join(data_store, path), ignore_errors=True)
a = zarrita.Array.create(
        store / path,
        shape=shape,
        dtype='int32',
        chunk_shape=(10, 10, 5),
        codecs=[
          zarrita.codecs.endian_codec(),
          zarrita.codecs.gzip_codec()
        ],
    )
a[:] = data

data = np.full(shape, 0, dtype='int32')
data[0, :, 0] = np.arange(start=1, stop=21)
data[:, 0, 0] = 1
path = "row-first/int32.zarr"
shutil.rmtree(os.path.join(data_store, path), ignore_errors=True)
a = zarrita.Array.create(
        store / path,
        shape=shape,
        dtype='int32',
        chunk_shape=(10, 10, 5),
        codecs=[
          zarrita.codecs.transpose_codec(order='F'),
          zarrita.codecs.endian_codec(),
          zarrita.codecs.gzip_codec()
        ],
    )
a[:] = data

###################

dtype = 'uint32'

path = "column-first/uint32.zarr"
data = np.full(shape, 0, dtype=dtype)
data[0, :, 0] = np.arange(start=1, stop=21)
data[:, 0, 0] = 1
shutil.rmtree(os.path.join(data_store, path), ignore_errors=True)
a = zarrita.Array.create(
        store / path,
        shape=shape,
        dtype=dtype,
        chunk_shape=(10, 10, 5),
        codecs=[
          zarrita.codecs.endian_codec(),
          zarrita.codecs.gzip_codec()
        ],
    )
a[:] = data

path = "row-first/uint32.zarr"
data = np.full(shape, 0, dtype=dtype)
data[0, :, 0] = np.arange(start=1, stop=21)
data[:, 0, 0] = 1
shutil.rmtree(os.path.join(data_store, path), ignore_errors=True)
a = zarrita.Array.create(
        store / path,
        shape=shape,
        dtype=dtype,
        chunk_shape=(10, 10, 5),
        codecs=[
          zarrita.codecs.transpose_codec(order='F'),
          zarrita.codecs.endian_codec(),
          zarrita.codecs.gzip_codec()
        ],
    )
a[:] = data

###################

dtype = 'float32'

path = "column-first/float32.zarr"
data = np.full(shape, 0, dtype=dtype)
data[0, :, 0] = np.arange(start=1, stop=21)
data[:, 0, 0] = 10.52
data[0, 0, 0] = -1
## some denormalised examples
data[1, 1, 0] = 0.00005693
data[1, 2, 0] = -5.97e-8
data[1, 3, 0] = 0.0000039
## special case values
data[2, 1, 0] = np.nan
data[2, 2, 0] = np.inf
data[2, 3, 0] = np.NINF
shutil.rmtree(os.path.join(data_store, path), ignore_errors=True)
a = zarrita.Array.create(
        store / path,
        shape=shape,
        dtype=dtype,
        chunk_shape=(10, 10, 5),
        codecs=[
          zarrita.codecs.endian_codec(),
          zarrita.codecs.gzip_codec()
        ],
    )
a[:] = data


###################

dtype = 'float64'

path = "column-first/float64.zarr"
data = np.full(shape, 0, dtype=dtype)
data[0, :, 0] = np.arange(start=1, stop=21)
data[:, 0, 0] = 10.52
shutil.rmtree(os.path.join(data_store, path), ignore_errors=True)
a = zarrita.Array.create(
        store / path,
        shape=shape,
        dtype=dtype,
        chunk_shape=(10, 10, 5),
        codecs=[
          zarrita.codecs.endian_codec(),
          zarrita.codecs.gzip_codec()
        ],
    )
a[:] = data

####################

shape = (20,10)
dtype = 'int8'

data = np.full(shape, 0, dtype=dtype)
data[0, :] = np.arange(start=1, stop=11)
data[:, 0] = np.arange(start=1, stop=60, step=3)
path = "compression/blosc_lz4.zarr"
shutil.rmtree(os.path.join(data_store, path), ignore_errors=True)
a = zarrita.Array.create(
        store / path,
        shape=shape,
        dtype=dtype,
        chunk_shape=(10, 10),
        codecs=[
          zarrita.codecs.endian_codec(),
          zarrita.codecs.blosc_codec(cname = "lz4", typesize=1)
        ],
    )
a[:] = data

###################

shape = (10,)
dtype = 'int8'

data = np.full(shape, 0, dtype=dtype)
data[:] = np.arange(start=1, stop=11)
path = "sharded/1d-sub-chunks.zarr"
shutil.rmtree(os.path.join(data_store, path), ignore_errors=True)
a = zarrita.Array.create(
        store / path,
        shape=shape,
        dtype=dtype,
        chunk_shape=(10,),
        codecs=[
            zarrita.codecs.sharding_codec(
                chunk_shape=(5,),
                codecs=[
                    zarrita.codecs.endian_codec(),
                    zarrita.codecs.gzip_codec()
                ],
            )
        ],
    )
a[:] = data
