import zarr
import numpy as np
import numcodecs.zarr3
from zarr.core.dtype.npy.structured import Struct

z = zarr.create_array('inst/extdata/zarr_examples/column-first/int32_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i4')
z[:] = 0
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

z = zarr.create_array('inst/extdata/zarr_examples/row-first/int32_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i4', filters = (zarr.codecs.TransposeCodec(order = [2, 1, 0])))
z[:] = 0
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

####################

z3 = zarr.create_array('inst/extdata/zarr_examples/column-first/uint32_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='u4')
z3[:] = 0
z3[0, :, 0] = np.arange(start=1, stop=21)
z3[:, 0, 0] = 1
z3[29,19,9] = pow(2,31)

z4 = zarr.create_array('inst/extdata/zarr_examples/row-first/uint32_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='u4', filters = (zarr.codecs.TransposeCodec(order = [2, 1, 0])))
z4[:] = 0
z4[0, :, 0] = np.arange(start=1, stop=21)
z4[:, 0, 0] = 1
z4[29,19,9] = pow(2,31)

#######################

z = zarr.create_array('inst/extdata/zarr_examples/column-first/int8_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1')
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

z = zarr.create_array('inst/extdata/zarr_examples/row-first/int8_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', filters = (zarr.codecs.TransposeCodec(order = [2, 1, 0])))
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

z = zarr.create_array('inst/extdata/zarr_examples/column-first/uint8_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1')
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = -1

#######################

z = zarr.create_array('inst/extdata/zarr_examples/column-first/int16_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1')
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

z = zarr.create_array('inst/extdata/zarr_examples/row-first/int16_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', filters = (zarr.codecs.TransposeCodec(order = [2, 1, 0])))
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

z = zarr.create_array('inst/extdata/zarr_examples/column-first/uint16_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1')
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = -1

#######################

z = zarr.create_array('inst/extdata/zarr_examples/row-first/uint8_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', filters = (zarr.codecs.TransposeCodec(order = [2, 1, 0])))
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = -1

#######################

z = zarr.create_array('inst/extdata/zarr_examples/column-first/int64_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i8')
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1
z[29,19,9] = pow(2,32)
z[28,19,9] = pow(2,32)
z[29,19,8] = -pow(2,32)


z = zarr.create_array('inst/extdata/zarr_examples/row-first/int64_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i8', filters = (zarr.codecs.TransposeCodec(order = [2, 1, 0])))
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1
z[29,19,9] = pow(2,32)
z[28,19,9] = pow(2,32)
z[29,19,8] = -pow(2,32)

#######################

z = zarr.create_array('inst/extdata/zarr_examples/column-first/uint64_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='u8')
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1
z[29,19,9] = pow(2,32)

#######################

z = zarr.create_array('inst/extdata/zarr_examples/column-first/string_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='|S6', fill_value = "")

z[0, :, 0] = "test"
z[:, 0, 0] = "ready"

z = zarr.create_array('inst/extdata/zarr_examples/row-first/string_v3.zarr', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='|S6', filters = (zarr.codecs.TransposeCodec(order = [2, 1, 0])), fill_value = "")

z[0, :, 0] = "test"
z[:, 0, 0] = "ready"

#######################

z = zarr.create_array('inst/extdata/zarr_examples/column-first/float16_v3.zarr', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f2')
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52
z[0, 0, 0] = -1
## some denormalised examples
z[1, 1, 0] = 0.00005693
z[1, 2, 0] = -5.97e-8
z[1, 3, 0] = 0.0000039
## special case values
z[2, 1, 0] = np.nan
z[2, 2, 0] = np.inf
z[2, 3, 0] = -np.inf

z = zarr.create_array('inst/extdata/zarr_examples/row-first/float16_v3.zarr', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f2', filters = (zarr.codecs.TransposeCodec(order = [2, 1, 0])), fill_value = "0.0")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52
z[0, 0, 0] = -1
## some denormalised examples
z[1, 1, 0] = 0.00005693
z[1, 2, 0] = -5.97e-8
z[1, 3, 0] = 0.0000039
## special case values
z[2, 1, 0] = np.nan
z[2, 2, 0] = np.inf
z[2, 3, 0] = -np.inf

######################

z = zarr.create_array('inst/extdata/zarr_examples/column-first/float32_v3.zarr', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f4')
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52

z = zarr.create_array('inst/extdata/zarr_examples/row-first/float32_v3.zarr', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f4', filters = (zarr.codecs.TransposeCodec(order = [2, 1, 0])), fill_value = "0.0")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52

######################

z = zarr.create_array('inst/extdata/zarr_examples/column-first/float64_v3.zarr', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f8')
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52

z = zarr.create_array('inst/extdata/zarr_examples/row-first/float64_v3.zarr', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f8', filters = (zarr.codecs.TransposeCodec(order = [2, 1, 0])), fill_value = "0.0")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52


z = zarr.create_array('inst/extdata/fill-values/double-inf_v3.zarr', shape=(20, 10),
              chunks=(10, 10), dtype='f8', filters = (zarr.codecs.TransposeCodec(order = [1, 0])), fill_value = "Infinity")
z[0, :] = 1
z = zarr.create_array('inst/extdata/fill-values/double-neginf_v3.zarr', shape=(20, 10),
              chunks=(10, 10), dtype='f8', filters = (zarr.codecs.TransposeCodec(order = [1, 0])), fill_value = "-Infinity")
z[0, :] = 1

#####################################

z = zarr.create_array('inst/extdata/zarr_examples/column-first/boolean_v3.zarr', shape=(20, 10),
               chunks=(10, 10), dtype='b1')
z[0, :] = 1

compressor = numcodecs.zarr3.Zlib(level = 6)
z = zarr.create_array('inst/extdata/zarr_examples/compression/zlib_v3.zarr', shape=(20, 10),
              chunks=(10, 10), dtype='i4', filters = (zarr.codecs.TransposeCodec(order = [1, 0])), compressors=compressor)
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

compressor = numcodecs.zarr3.BZ2(level = 6)
z = zarr.create_array('inst/extdata/zarr_examples/compression/bzip2_v3.zarr', shape=(20, 10),
              chunks=(10, 10), dtype='i4', filters = (zarr.codecs.TransposeCodec(order = [1, 0])), compressors=compressor)
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

compressor = numcodecs.zarr3.LZMA()
z = zarr.create_array('inst/extdata/zarr_examples/compression/lzma_v3.zarr', shape=(20, 10),
              chunks=(10, 10), dtype='i4', filters = (zarr.codecs.TransposeCodec(order = [1, 0])), compressors=compressor)
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

compressor = numcodecs.zarr3.LZ4()
z = zarr.create_array('inst/extdata/zarr_examples/compression/lz4_v3.zarr', shape=(20, 10),
              chunks=(10, 10), dtype='i4', filters = (zarr.codecs.TransposeCodec(order = [1, 0])), compressors=compressor)
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

compressor = numcodecs.zarr3.Zstd()
z = zarr.create_array('inst/extdata/zarr_examples/compression/zstd_v3.zarr', shape=(20, 10),
              chunks=(10, 10), dtype='i4', filters = (zarr.codecs.TransposeCodec(order = [1, 0])), compressors=compressor)
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

#####################################

greetings = ['¡Hola mundo!', 'Hej Världen!', 'Servus Woid!', 'Hei maailma!',
             'Xin chào thế giới', 'Njatjeta Botë!', 'Γεια σου κόσμε!',
             'こんにちは世界', '世界，你好！', 'Helló, világ!', 'Zdravo svete!',
             'เฮลโลเวิลด์']
             
z = zarr.create_array('inst/extdata/zarr_examples/column-first/Unicode_v3.zarr', shape=(12, 12),
              chunks=(6, 6), fill_value = "",
              dtype='U20')
z[:,0] = greetings
z[0,:] = greetings

z = zarr.create_array('inst/extdata/zarr_examples/column-first/vlenUTF8_v3.zarr', shape=(12, 12),
              chunks=(6, 6), fill_value = "",
              dtype=str)
z[:,0] = greetings
z[0,:] = greetings

#####################################

# Store with consolidated metadata
group = zarr.create_group(
    "inst/extdata/zarr_examples/metadata/zarr_examples/consolidated_v3.zarr"
)
group.create_array(shape=(1,), name="a", dtype="float64")
group.create_array(shape=(2, 2), name="b", dtype="float64")
group.create_array(shape=(3, 3, 3), name="c", dtype="float64")
zarr.consolidate_metadata(group.store)

#####################################

# Scalar
z = zarr.create_array('inst/extdata/zarr_examples/scalar/scalar_v3.zarr', shape=(), dtype='i4')
z[()] = 42

#####################################

# Structured datatype
dt = np.dtype([('x', 'i4'), ('y', 'f4')])
z = zarr.create_array('inst/extdata/zarr_examples/structured/structured_v3.zarr', shape=(10,2), chunks=(5,2), dtype=Struct.from_native_dtype(dt))
z['x'] = np.arange(20).reshape(10,2)
z['y'] = np.array([0.34, 0.67, 0.12, 0.89, 0.45, 0.23, 0.78, 0.56, 0.90, 0.11, 0.34, 0.67, 0.12, 0.89, 0.45, 0.23, 0.78, 0.56, 0.90, 0.11]).reshape(10,2)

# Sharded array
z = zarr.create_array(
    'inst/extdata/zarr_examples/sharding/int32_sharded.zarr',
    shape=(30, 20, 10),
    chunks=(2, 4, 5),   # inner chunk shape within each shard
    shards=(10, 20, 5),  # shard shape
    dtype='i4',
    filters=[zarr.codecs.TransposeCodec(order=[2, 1, 0])],
    overwrite=True,
    compressors=zarr.codecs.BloscCodec(),
)
z[:] = 0
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

# Sharded array (index at the start)
z = zarr.create_array(
    'inst/extdata/zarr_examples/sharding/int32_sharded_index_at_start.zarr',
    shape=(30, 20, 10),
    dtype='i4',
    serializer=zarr.codecs.ShardingCodec(chunk_shape = (2, 4, 5), index_location = 'start', codecs = [zarr.codecs.TransposeCodec(order=[2, 1, 0]), zarr.codecs.BytesCodec()]),
    overwrite=True
)
z[:] = 0
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1
