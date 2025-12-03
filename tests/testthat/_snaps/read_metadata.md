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

# zarr_overview works with consolidated metadata store

    Code
      metadata_df
    Output
                                                        path  data_type endianness
      1                           /conditions/geometry/angle unicode224     little
      2                            /conditions/geometry/band  unicode96     little
      3                        /conditions/geometry/detector      int64     little
      4                 /conditions/geometry/mean_sun_angles    float64     little
      5   /conditions/geometry/mean_viewing_incidence_angles    float64     little
      6                      /conditions/geometry/sun_angles    float64     little
      7        /conditions/geometry/viewing_incidence_angles    float64     little
      8                               /conditions/geometry/x      int64     little
      9                               /conditions/geometry/y      int64     little
      10        /conditions/mask/detector_footprint/r10m/b02      uint8       <NA>
      11        /conditions/mask/detector_footprint/r10m/b03      uint8       <NA>
      12        /conditions/mask/detector_footprint/r10m/b04      uint8       <NA>
      13        /conditions/mask/detector_footprint/r10m/b08      uint8       <NA>
      14          /conditions/mask/detector_footprint/r10m/x      int64     little
      15          /conditions/mask/detector_footprint/r10m/y      int64     little
      16        /conditions/mask/detector_footprint/r20m/b05      uint8       <NA>
      17        /conditions/mask/detector_footprint/r20m/b06      uint8       <NA>
      18        /conditions/mask/detector_footprint/r20m/b07      uint8       <NA>
      19        /conditions/mask/detector_footprint/r20m/b11      uint8       <NA>
      20        /conditions/mask/detector_footprint/r20m/b12      uint8       <NA>
      21        /conditions/mask/detector_footprint/r20m/b8a      uint8       <NA>
      22          /conditions/mask/detector_footprint/r20m/x      int64     little
      23          /conditions/mask/detector_footprint/r20m/y      int64     little
      24        /conditions/mask/detector_footprint/r60m/b01      uint8       <NA>
      25        /conditions/mask/detector_footprint/r60m/b09      uint8       <NA>
      26        /conditions/mask/detector_footprint/r60m/b10      uint8       <NA>
      27          /conditions/mask/detector_footprint/r60m/x      int64     little
      28          /conditions/mask/detector_footprint/r60m/y      int64     little
      29        /conditions/mask/l1c_classification/r60m/b00      uint8       <NA>
      30          /conditions/mask/l1c_classification/r60m/x      int64     little
      31          /conditions/mask/l1c_classification/r60m/y      int64     little
      32        /conditions/mask/l2a_classification/r20m/scl      uint8       <NA>
      33          /conditions/mask/l2a_classification/r20m/x      int64     little
      34          /conditions/mask/l2a_classification/r20m/y      int64     little
      35        /conditions/mask/l2a_classification/r60m/scl      uint8       <NA>
      36          /conditions/mask/l2a_classification/r60m/x      int64     little
      37          /conditions/mask/l2a_classification/r60m/y      int64     little
      38                /conditions/meteorology/cams/aod1240    float32     little
      39                 /conditions/meteorology/cams/aod469    float32     little
      40                 /conditions/meteorology/cams/aod550    float32     little
      41                 /conditions/meteorology/cams/aod670    float32     little
      42                 /conditions/meteorology/cams/aod865    float32     little
      43               /conditions/meteorology/cams/bcaod550    float32     little
      44               /conditions/meteorology/cams/duaod550    float32     little
      45          /conditions/meteorology/cams/isobaricInhPa    float64     little
      46               /conditions/meteorology/cams/latitude    float64     little
      47              /conditions/meteorology/cams/longitude    float64     little
      48                 /conditions/meteorology/cams/number      int64     little
      49               /conditions/meteorology/cams/omaod550    float32     little
      50               /conditions/meteorology/cams/ssaod550    float32     little
      51                   /conditions/meteorology/cams/step      int64     little
      52               /conditions/meteorology/cams/suaod550    float32     little
      53                /conditions/meteorology/cams/surface    float64     little
      54                   /conditions/meteorology/cams/time      int64     little
      55             /conditions/meteorology/cams/valid_time      int64     little
      56                      /conditions/meteorology/cams/z    float32     little
      57         /conditions/meteorology/ecmwf/isobaricInhPa    float64     little
      58              /conditions/meteorology/ecmwf/latitude    float64     little
      59             /conditions/meteorology/ecmwf/longitude    float64     little
      60                   /conditions/meteorology/ecmwf/msl    float32     little
      61                /conditions/meteorology/ecmwf/number      int64     little
      62                     /conditions/meteorology/ecmwf/r    float32     little
      63                  /conditions/meteorology/ecmwf/step      int64     little
      64               /conditions/meteorology/ecmwf/surface    float64     little
      65                  /conditions/meteorology/ecmwf/tco3    float32     little
      66                  /conditions/meteorology/ecmwf/tcwv    float32     little
      67                  /conditions/meteorology/ecmwf/time      int64     little
      68                   /conditions/meteorology/ecmwf/u10    float32     little
      69                   /conditions/meteorology/ecmwf/v10    float32     little
      70            /conditions/meteorology/ecmwf/valid_time      int64     little
      71                  /measurements/reflectance/r10m/b02     uint16     little
      72                  /measurements/reflectance/r10m/b03     uint16     little
      73                  /measurements/reflectance/r10m/b04     uint16     little
      74                  /measurements/reflectance/r10m/b08     uint16     little
      75                    /measurements/reflectance/r10m/x      int64     little
      76                    /measurements/reflectance/r10m/y      int64     little
      77                  /measurements/reflectance/r20m/b01     uint16     little
      78                  /measurements/reflectance/r20m/b02     uint16     little
      79                  /measurements/reflectance/r20m/b03     uint16     little
      80                  /measurements/reflectance/r20m/b04     uint16     little
      81                  /measurements/reflectance/r20m/b05     uint16     little
      82                  /measurements/reflectance/r20m/b06     uint16     little
      83                  /measurements/reflectance/r20m/b07     uint16     little
      84                  /measurements/reflectance/r20m/b11     uint16     little
      85                  /measurements/reflectance/r20m/b12     uint16     little
      86                  /measurements/reflectance/r20m/b8a     uint16     little
      87                    /measurements/reflectance/r20m/x      int64     little
      88                    /measurements/reflectance/r20m/y      int64     little
      89                  /measurements/reflectance/r60m/b01     uint16     little
      90                  /measurements/reflectance/r60m/b02     uint16     little
      91                  /measurements/reflectance/r60m/b03     uint16     little
      92                  /measurements/reflectance/r60m/b04     uint16     little
      93                  /measurements/reflectance/r60m/b05     uint16     little
      94                  /measurements/reflectance/r60m/b06     uint16     little
      95                  /measurements/reflectance/r60m/b07     uint16     little
      96                  /measurements/reflectance/r60m/b09     uint16     little
      97                  /measurements/reflectance/r60m/b11     uint16     little
      98                  /measurements/reflectance/r60m/b12     uint16     little
      99                  /measurements/reflectance/r60m/b8a     uint16     little
      100                   /measurements/reflectance/r60m/x      int64     little
      101                   /measurements/reflectance/r60m/y      int64     little
      102                       /quality/atmosphere/r10m/aot     uint16     little
      103                       /quality/atmosphere/r10m/wvp     uint16     little
      104                         /quality/atmosphere/r10m/x      int64     little
      105                         /quality/atmosphere/r10m/y      int64     little
      106                       /quality/atmosphere/r20m/aot     uint16     little
      107                       /quality/atmosphere/r20m/wvp     uint16     little
      108                         /quality/atmosphere/r20m/x      int64     little
      109                         /quality/atmosphere/r20m/y      int64     little
      110                       /quality/atmosphere/r60m/aot     uint16     little
      111                       /quality/atmosphere/r60m/wvp     uint16     little
      112                         /quality/atmosphere/r60m/x      int64     little
      113                         /quality/atmosphere/r60m/y      int64     little
      114                   /quality/l2a_quicklook/r10m/band      int64     little
      115                    /quality/l2a_quicklook/r10m/tci      uint8       <NA>
      116                      /quality/l2a_quicklook/r10m/x      int64     little
      117                      /quality/l2a_quicklook/r10m/y      int64     little
      118                   /quality/l2a_quicklook/r20m/band      int64     little
      119                    /quality/l2a_quicklook/r20m/tci      uint8       <NA>
      120                      /quality/l2a_quicklook/r20m/x      int64     little
      121                      /quality/l2a_quicklook/r20m/y      int64     little
      122                   /quality/l2a_quicklook/r60m/band      int64     little
      123                    /quality/l2a_quicklook/r60m/tci      uint8       <NA>
      124                      /quality/l2a_quicklook/r60m/x      int64     little
      125                      /quality/l2a_quicklook/r60m/y      int64     little
      126                             /quality/mask/r10m/b02      uint8       <NA>
      127                             /quality/mask/r10m/b03      uint8       <NA>
      128                             /quality/mask/r10m/b04      uint8       <NA>
      129                             /quality/mask/r10m/b08      uint8       <NA>
      130                               /quality/mask/r10m/x      int64     little
      131                               /quality/mask/r10m/y      int64     little
      132                             /quality/mask/r20m/b05      uint8       <NA>
      133                             /quality/mask/r20m/b06      uint8       <NA>
      134                             /quality/mask/r20m/b07      uint8       <NA>
      135                             /quality/mask/r20m/b11      uint8       <NA>
      136                             /quality/mask/r20m/b12      uint8       <NA>
      137                             /quality/mask/r20m/b8a      uint8       <NA>
      138                               /quality/mask/r20m/x      int64     little
      139                               /quality/mask/r20m/y      int64     little
      140                             /quality/mask/r60m/b01      uint8       <NA>
      141                             /quality/mask/r60m/b09      uint8       <NA>
      142                             /quality/mask/r60m/b10      uint8       <NA>
      143                               /quality/mask/r60m/x      int64     little
      144                               /quality/mask/r60m/y      int64     little
      145                     /quality/probability/r20m/band      int64     little
      146                      /quality/probability/r20m/cld      uint8       <NA>
      147                      /quality/probability/r20m/snw      uint8       <NA>
      148                        /quality/probability/r20m/x      int64     little
      149                        /quality/probability/r20m/y      int64     little
          compressor          dim    chunk_dim      nchunks
      1        blosc            2            2            1
      2        blosc           13           13            1
      3        blosc            6            6            1
      4        blosc            2            2            1
      5        blosc        13, 2        13, 2         1, 1
      6        blosc    2, 23, 23    2, 23, 23      1, 1, 1
      7        blosc 13, 6, 2.... 7, 3, 2,.... 2, 2, 1,....
      8        blosc           23           23            1
      9        blosc           23           23            1
      10       blosc 10980, 10980   1830, 1830         6, 6
      11       blosc 10980, 10980   1830, 1830         6, 6
      12       blosc 10980, 10980   1830, 1830         6, 6
      13       blosc 10980, 10980   1830, 1830         6, 6
      14       blosc        10980        10980            1
      15       blosc        10980        10980            1
      16       blosc   5490, 5490     915, 915         6, 6
      17       blosc   5490, 5490     915, 915         6, 6
      18       blosc   5490, 5490     915, 915         6, 6
      19       blosc   5490, 5490     915, 915         6, 6
      20       blosc   5490, 5490     915, 915         6, 6
      21       blosc   5490, 5490     915, 915         6, 6
      22       blosc         5490         5490            1
      23       blosc         5490         5490            1
      24       blosc   1830, 1830     305, 305         6, 6
      25       blosc   1830, 1830     305, 305         6, 6
      26       blosc   1830, 1830     305, 305         6, 6
      27       blosc         1830         1830            1
      28       blosc         1830         1830            1
      29       blosc   1830, 1830     305, 305         6, 6
      30       blosc         1830         1830            1
      31       blosc         1830         1830            1
      32       blosc   5490, 5490     915, 915         6, 6
      33       blosc         5490         5490            1
      34       blosc         5490         5490            1
      35       blosc   1830, 1830     305, 305         6, 6
      36       blosc         1830         1830            1
      37       blosc         1830         1830            1
      38       blosc         9, 9         9, 9         1, 1
      39       blosc         9, 9         9, 9         1, 1
      40       blosc         9, 9         9, 9         1, 1
      41       blosc         9, 9         9, 9         1, 1
      42       blosc         9, 9         9, 9         1, 1
      43       blosc         9, 9         9, 9         1, 1
      44       blosc         9, 9         9, 9         1, 1
      45        <NA>            1            1            1
      46       blosc            9            9            1
      47       blosc            9            9            1
      48        <NA>            1            1            1
      49       blosc         9, 9         9, 9         1, 1
      50       blosc         9, 9         9, 9         1, 1
      51        <NA>            1            1            1
      52       blosc         9, 9         9, 9         1, 1
      53        <NA>            1            1            1
      54        <NA>            1            1            1
      55        <NA>            1            1            1
      56       blosc         9, 9         9, 9         1, 1
      57        <NA>            1            1            1
      58       blosc            9            9            1
      59       blosc            9            9            1
      60       blosc         9, 9         9, 9         1, 1
      61        <NA>            1            1            1
      62       blosc         9, 9         9, 9         1, 1
      63        <NA>            1            1            1
      64        <NA>            1            1            1
      65       blosc         9, 9         9, 9         1, 1
      66       blosc         9, 9         9, 9         1, 1
      67        <NA>            1            1            1
      68       blosc         9, 9         9, 9         1, 1
      69       blosc         9, 9         9, 9         1, 1
      70        <NA>            1            1            1
      71       blosc 10980, 10980   1830, 1830         6, 6
      72       blosc 10980, 10980   1830, 1830         6, 6
      73       blosc 10980, 10980   1830, 1830         6, 6
      74       blosc 10980, 10980   1830, 1830         6, 6
      75       blosc        10980        10980            1
      76       blosc        10980        10980            1
      77       blosc   5490, 5490     915, 915         6, 6
      78       blosc   5490, 5490     915, 915         6, 6
      79       blosc   5490, 5490     915, 915         6, 6
      80       blosc   5490, 5490     915, 915         6, 6
      81       blosc   5490, 5490     915, 915         6, 6
      82       blosc   5490, 5490     915, 915         6, 6
      83       blosc   5490, 5490     915, 915         6, 6
      84       blosc   5490, 5490     915, 915         6, 6
      85       blosc   5490, 5490     915, 915         6, 6
      86       blosc   5490, 5490     915, 915         6, 6
      87       blosc         5490         5490            1
      88       blosc         5490         5490            1
      89       blosc   1830, 1830     305, 305         6, 6
      90       blosc   1830, 1830     305, 305         6, 6
      91       blosc   1830, 1830     305, 305         6, 6
      92       blosc   1830, 1830     305, 305         6, 6
      93       blosc   1830, 1830     305, 305         6, 6
      94       blosc   1830, 1830     305, 305         6, 6
      95       blosc   1830, 1830     305, 305         6, 6
      96       blosc   1830, 1830     305, 305         6, 6
      97       blosc   1830, 1830     305, 305         6, 6
      98       blosc   1830, 1830     305, 305         6, 6
      99       blosc   1830, 1830     305, 305         6, 6
      100      blosc         1830         1830            1
      101      blosc         1830         1830            1
      102      blosc 10980, 10980   1830, 1830         6, 6
      103      blosc 10980, 10980   1830, 1830         6, 6
      104      blosc        10980        10980            1
      105      blosc        10980        10980            1
      106      blosc   5490, 5490     915, 915         6, 6
      107      blosc   5490, 5490     915, 915         6, 6
      108      blosc         5490         5490            1
      109      blosc         5490         5490            1
      110      blosc   1830, 1830     305, 305         6, 6
      111      blosc   1830, 1830     305, 305         6, 6
      112      blosc         1830         1830            1
      113      blosc         1830         1830            1
      114      blosc            3            3            1
      115      blosc 3, 10980.... 1, 1830,....      3, 6, 6
      116      blosc        10980        10980            1
      117      blosc        10980        10980            1
      118      blosc            3            3            1
      119      blosc 3, 5490,....  1, 915, 915      3, 6, 6
      120      blosc         5490         5490            1
      121      blosc         5490         5490            1
      122      blosc            3            3            1
      123      blosc 3, 1830,....  1, 305, 305      3, 6, 6
      124      blosc         1830         1830            1
      125      blosc         1830         1830            1
      126      blosc 10980, 10980   1830, 1830         6, 6
      127      blosc 10980, 10980   1830, 1830         6, 6
      128      blosc 10980, 10980   1830, 1830         6, 6
      129      blosc 10980, 10980   1830, 1830         6, 6
      130      blosc        10980        10980            1
      131      blosc        10980        10980            1
      132      blosc   5490, 5490     915, 915         6, 6
      133      blosc   5490, 5490     915, 915         6, 6
      134      blosc   5490, 5490     915, 915         6, 6
      135      blosc   5490, 5490     915, 915         6, 6
      136      blosc   5490, 5490     915, 915         6, 6
      137      blosc   5490, 5490     915, 915         6, 6
      138      blosc         5490         5490            1
      139      blosc         5490         5490            1
      140      blosc   1830, 1830     305, 305         6, 6
      141      blosc   1830, 1830     305, 305         6, 6
      142      blosc   1830, 1830     305, 305         6, 6
      143      blosc         1830         1830            1
      144      blosc         1830         1830            1
      145       <NA>            1            1            1
      146      blosc   5490, 5490     915, 915         6, 6
      147      blosc   5490, 5490     915, 915         6, 6
      148      blosc         5490         5490            1
      149      blosc         5490         5490            1

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

