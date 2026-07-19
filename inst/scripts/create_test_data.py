from zarr.abc import numcodec
import zarr
import numcodecs
import numpy as np

z = zarr.open('inst/extdata/zarr_examples/column-first/int32.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i4', order="C", dimension_separator = "/", zarr_format=2)
z[:] = 0
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

z = zarr.open('inst/extdata/zarr_examples/row-first/int32.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i4', order="F", zarr_format=2)
z[:] = 0
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

####################

z3 = zarr.open('inst/extdata/zarr_examples/column-first/uint32.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='u4', order="C", zarr_format=2)
z3[:] = 0
z3[0, :, 0] = np.arange(start=1, stop=21)
z3[:, 0, 0] = 1
z3[29,19,9] = pow(2,31)

z4 = zarr.open('inst/extdata/zarr_examples/row-first/uint32.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='u4', order="F", zarr_format=2)
z4[:] = 0
z4[0, :, 0] = np.arange(start=1, stop=21)
z4[:, 0, 0] = 1
z4[29,19,9] = pow(2,31)

#######################

z = zarr.open('inst/extdata/zarr_examples/column-first/int8.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', order="C", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = -1

z = zarr.open('inst/extdata/zarr_examples/row-first/int8.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', order="F", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = -1

z = zarr.open('inst/extdata/zarr_examples/column-first/uint8.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='u1', order="C", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

#######################

z = zarr.open('inst/extdata/zarr_examples/column-first/int16.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i2', order="C", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = -1

z = zarr.open('inst/extdata/zarr_examples/row-first/int16.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i2', order="F", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = -1

z = zarr.open('inst/extdata/zarr_examples/column-first/uint16.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='u2', order="C", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

#######################

z = zarr.open('inst/extdata/zarr_examples/row-first/uint8.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='u1', order="F", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

#######################

z = zarr.open('inst/extdata/zarr_examples/column-first/int64.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i8', order="C", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1
z[29,19,9] = pow(2,32)
z[28,19,9] = pow(2,32)
z[29,19,8] = -pow(2,32)


z = zarr.open('inst/extdata/zarr_examples/row-first/int64.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i8', order="F", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1
z[29,19,9] = pow(2,32)
z[28,19,9] = pow(2,32)
z[29,19,8] = -pow(2,32)

#######################

z = zarr.open('inst/extdata/zarr_examples/column-first/uint64.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='u8', order="C", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1
z[29,19,9] = pow(2,32)

#######################

z = zarr.open('inst/extdata/zarr_examples/column-first/string.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='|S6', order="C", fill_value = "", 
               dimension_separator = "/", zarr_format=2)

z[0, :, 0] = "test"
z[:, 0, 0] = "ready"

z = zarr.open('inst/extdata/zarr_examples/row-first/string.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='|S6', order="F", fill_value = "", zarr_format=2)

z[0, :, 0] = "test"
z[:, 0, 0] = "ready"

#######################

z = zarr.open('inst/extdata/zarr_examples/column-first/float16.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f2', order="C", zarr_format=2)
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

z = zarr.open('inst/extdata/zarr_examples/row-first/float16.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f2', order="F", fill_value = "0.0", zarr_format=2)
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

z = zarr.open('inst/extdata/zarr_examples/column-first/float32.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f4', order="C", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52

z = zarr.open('inst/extdata/zarr_examples/row-first/float32.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f4', order="F", fill_value = "0.0", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52

######################

z = zarr.open('inst/extdata/zarr_examples/column-first/float64.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f8', order="C", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52

z = zarr.open('inst/extdata/zarr_examples/row-first/float64.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f8', order="F", fill_value = "0.0", zarr_format=2)
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52


z = zarr.open('inst/extdata/zarr_examples/fill-values/double-inf.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='f8', order="F", fill_value = "Infinity", zarr_format=2)
z[0, :] = 1
z = zarr.open('inst/extdata/zarr_examples/fill-values/double-neginf.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='f8', order="F", fill_value = "-Infinity", zarr_format=2)
z[0, :] = 1

#####################################

z = zarr.open('inst/extdata/zarr_examples/column-first/boolean.zarr', mode='w', shape=(20, 10),
               chunks=(10, 10), dtype='b1', order="C", zarr_format=2)
z[0, :] = 1

z = zarr.open('inst/extdata/zarr_examples/compression/zlib.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='i4', order="F", compressor=numcodecs.Zlib(level=6), zarr_format=2)
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

z = zarr.open('inst/extdata/zarr_examples/compression/bzip2.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='i4', order="F", compressor=numcodecs.BZ2(level=6), zarr_format=2)
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

z = zarr.open('inst/extdata/zarr_examples/compression/lzma.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='i4', order="F", compressor=numcodecs.LZMA(), zarr_format=2)
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

z = zarr.open('inst/extdata/zarr_examples/compression/lz4.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='i4', order="F", compressor=numcodecs.LZ4(), zarr_format=2)
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

z = zarr.open('inst/extdata/zarr_examples/compression/zstd.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='i4', order="F", compressor=numcodecs.Zstd(), zarr_format=2)
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

#####################################

greetings = ['¡Hola mundo!', 'Hej Världen!', 'Servus Woid!', 'Hei maailma!',
             'Xin chào thế giới', 'Njatjeta Botë!', 'Γεια σου κόσμε!',
             'こんにちは世界', '世界，你好！', 'Helló, világ!', 'Zdravo svete!',
             'เฮลโลเวิลด์']
             
z = zarr.open('inst/extdata/zarr_examples/column-first/Unicode.zarr', mode='w', shape=(12, 12),
              chunks=(6, 6), order="C", fill_value = "",
              dtype='U20', zarr_format=2)
z[:,0] = greetings
z[0,:] = greetings

z = zarr.open('inst/extdata/zarr_examples/column-first/vlenUTF8.zarr', mode='w', shape=(12, 12),
              chunks=(6, 6), order="C", fill_value = "",
              dtype=str, zarr_format=2)
z[:,0] = greetings
z[0,:] = greetings

#####################################

# Store with consolidated metadata
group = zarr.create_group(
    "inst/extdata/zarr_examples/metadata/consolidated.zarr",
    zarr_format=2,
    overwrite=True
)
group.create_array(shape=(1,), name="a", dtype="float64")
group.create_array(shape=(2, 2), name="b", dtype="float64")
group.create_array(shape=(3, 3, 3), name="c", dtype="float64")
zarr.consolidate_metadata(group.store)
