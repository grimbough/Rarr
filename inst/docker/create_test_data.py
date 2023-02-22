import zarr
import numpy as np

z = zarr.open('/data/column-first/int32.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i4', order="C", dimension_separator = "/")
z[:] = 0
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

z = zarr.open('/data/row-first/int32.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i4', order="F")
z[:] = 0
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

####################

z3 = zarr.open('/data/column-first/uint32.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='u4', order="C")
z3[:] = 0
z3[0, :, 0] = np.arange(start=1, stop=21)
z3[:, 0, 0] = 1
z3[29,19,9] = pow(2,31)

z4 = zarr.open('/data/row-first/uint32.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='u4', order="F")
z4[:] = 0
z4[0, :, 0] = np.arange(start=1, stop=21)
z4[:, 0, 0] = 1
z4[29,19,9] = pow(2,31)

#######################

z = zarr.open('/data/column-first/int8.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', order="C")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

z = zarr.open('/data/row-first/int8.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', order="F")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

z = zarr.open('/data/column-first/uint8.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', order="C")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = -1

#######################

z = zarr.open('/data/column-first/int16.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', order="C")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

z = zarr.open('/data/row-first/int16.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', order="F")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1

z = zarr.open('/data/column-first/uint16.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', order="C")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = -1

#######################

z = zarr.open('/data/row-first/uint8.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', order="F")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = -1

z = zarr.open('/data/column-first/int64.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i8', order="C")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1
z[29,19,9] = pow(2,32)

z = zarr.open('/data/row-first/int64.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i8', order="F")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 1
z[29,19,9] = pow(2,32)

#######################

z = zarr.open('/data/column-first/string.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='|S6', order="C", fill_value = "", dimension_separator = "/")

z[0, :, 0] = "test"
z[:, 0, 0] = "ready"

z = zarr.open('/data/row-first/string.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='|S6', order="F", fill_value = "")

z[0, :, 0] = "test"
z[:, 0, 0] = "ready"

#######################

z = zarr.open('/data/column-first/float16.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f2', order="C")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52
z[0, 0, 0] = -1
## some denormalised examples
z[1, 1, 0] = 0.00005693
z[1, 2, 0] = -5.97e-8
z[1, 3, 0] = 0.0000039

z = zarr.open('/data/row-first/float16.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f2', order="F", fill_value = "0.0")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52
z[0, 0, 0] = -1
## some denormalised examples
z[1, 1, 0] = 0.00005693
z[1, 2, 0] = -5.97e-8
z[1, 3, 0] = 0.0000039

######################

z = zarr.open('/data/column-first/float32.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f4', order="C")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52

z = zarr.open('/data/row-first/float32.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f4', order="F", fill_value = "0.0")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52

######################

z = zarr.open('/data/column-first/float64.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f8', order="C")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52

z = zarr.open('/data/row-first/float64.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f8', order="F", fill_value = "0.0")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52


z = zarr.open('/data/fill-values/double-inf.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='f8', order="F", fill_value = "Infinity")
z[0, :] = 1
z = zarr.open('/data/fill-values/double-neginf.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='f8', order="F", fill_value = "-Infinity")
z[0, :] = 1

#####################################

z = zarr.open('/data/column-first/boolean.zarr', mode='w', shape=(20, 10),
               chunks=(10, 10), dtype='b1', order="C")
z[0, :] = 1

# 
# 
# 
z = zarr.open('/data/compression/zlib.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='i4', order="F", compressor=zarr.Zlib(level=6))
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

z = zarr.open('/data/compression/bzip2.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='i4', order="F", compressor=zarr.BZ2(level=6))
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

z = zarr.open('/data/compression/lzma.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='i4', order="F", compressor=zarr.LZMA())
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)

z = zarr.open('/data/compression/lz4.zarr', mode='w', shape=(20, 10),
              chunks=(10, 10), dtype='i4', order="F", compressor=zarr.LZ4())
z[0, :] = np.arange(start=1, stop=11)
z[:, 0] = np.arange(start=1, stop=60, step=3)



