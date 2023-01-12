import zarr
import numpy as np

z = zarr.open('/data/column-first/int32.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i4', order="C")
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

z5 = zarr.open('/data/column-first/int8.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', order="C")
z5[:] = 0
z5[0, :, 0] = np.arange(start=1, stop=21)
z5[:, 0, 0] = 1

z6 = zarr.open('/data/row-first/int8.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i1', order="F")
z6[:] = 0
z6[0, :, 0] = np.arange(start=1, stop=21)
z6[:, 0, 0] = 1


z3 = zarr.open('/data/column-first/int64.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i8', order="C")
z3[:] = 0
z3[0, :, 0] = np.arange(start=1, stop=21)
z3[:, 0, 0] = 1
z3[29,19,9] = pow(2,32)

z4 = zarr.open('/data/row-first/int64.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='i8', order="F")
z4[:] = 0
z4[0, :, 0] = np.arange(start=1, stop=21)
z4[:, 0, 0] = 1
z4[29,19,9] = pow(2,32)

#######################

z = zarr.open('/data/column-first/string.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='|S6', order="C", fill_value = "")

z[0, :, 0] = "test"
z[:, 0, 0] = "ready"

z = zarr.open('/data/row-first/string.zarr', mode='w', shape=(30, 20, 10),
               chunks=(10, 10, 5), dtype='|S6', order="F", fill_value = "")

z[0, :, 0] = "test"
z[:, 0, 0] = "ready"

#######################


# z3 = zarr.open('/data/2bit-int.zarr', mode='w', shape=(100, 250),
#                chunks=(10, 25), dtype='u2', order="F")
# z3[:] = 0
# z3[0, :] = np.arange(start=0, stop=250)
# z3[:, 0] = np.arange(start=0, stop=200, step=2)
# 
# 
# 
z = zarr.open('/data/column-first/double.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f8', order="C")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52

z = zarr.open('/data/row-first/double.zarr', mode='w', shape=(30, 20, 10),
                chunks=(10, 10, 5), dtype='f8', order="C", fill_value = "NaN")
z[0, :, 0] = np.arange(start=1, stop=21)
z[:, 0, 0] = 10.52

# 
# 
# z5 = zarr.open('/data/boolean.zarr', mode='w', shape=(100, 250),
#                chunks=(10, 25), dtype='b1', order="F")
# z5[:] = 0
# z5[0, :] = 1
# z5[:, 0] = 1
# 
# 
# 
# z6 = zarr.open('/data/zlib.zarr', mode='w', shape=(1000, 1000),
#                chunks=(100, 100), dtype='i4', order="F", compressor=zarr.Zlib(level=1))
# z6[:] = 42
# z6[0, :] = np.arange(start=1, stop=1001)
# z6[:, 0] = np.arange(start=1, stop=3000, step=3)
# 
# 
# z7 = zarr.open('/data/8bit-int.zarr', mode='w', shape=(100, 250),
#                chunks=(10, 25), dtype='i8', order="F")
# z7[:] = 0
# z7[0, :] = pow(2, 32)
# z7[:, 0] = -10
# 

