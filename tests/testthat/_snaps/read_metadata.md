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
      Compressor: zstd

# zarr_overview works with consolidated metadata store

    Code
      metadata_df
    Output
                                                        path          data_type
      1                           /conditions/geometry/angle fixed_length_utf32
      2                            /conditions/geometry/band fixed_length_utf32
      3                        /conditions/geometry/detector              int64
      4                 /conditions/geometry/mean_sun_angles            float64
      5   /conditions/geometry/mean_viewing_incidence_angles            float64
      6                      /conditions/geometry/sun_angles            float64
      7        /conditions/geometry/viewing_incidence_angles            float64
      8                               /conditions/geometry/x              int64
      9                               /conditions/geometry/y              int64
      10        /conditions/mask/detector_footprint/r10m/b02              uint8
      11        /conditions/mask/detector_footprint/r10m/b03              uint8
      12        /conditions/mask/detector_footprint/r10m/b04              uint8
      13        /conditions/mask/detector_footprint/r10m/b08              uint8
      14          /conditions/mask/detector_footprint/r10m/x              int64
      15          /conditions/mask/detector_footprint/r10m/y              int64
      16        /conditions/mask/detector_footprint/r20m/b05              uint8
      17        /conditions/mask/detector_footprint/r20m/b06              uint8
      18        /conditions/mask/detector_footprint/r20m/b07              uint8
      19        /conditions/mask/detector_footprint/r20m/b11              uint8
      20        /conditions/mask/detector_footprint/r20m/b12              uint8
      21        /conditions/mask/detector_footprint/r20m/b8a              uint8
      22          /conditions/mask/detector_footprint/r20m/x              int64
      23          /conditions/mask/detector_footprint/r20m/y              int64
      24        /conditions/mask/detector_footprint/r60m/b01              uint8
      25        /conditions/mask/detector_footprint/r60m/b09              uint8
      26        /conditions/mask/detector_footprint/r60m/b10              uint8
      27          /conditions/mask/detector_footprint/r60m/x              int64
      28          /conditions/mask/detector_footprint/r60m/y              int64
      29        /conditions/mask/l1c_classification/r60m/b00              uint8
      30          /conditions/mask/l1c_classification/r60m/x              int64
      31          /conditions/mask/l1c_classification/r60m/y              int64
      32        /conditions/mask/l2a_classification/r20m/scl              uint8
      33          /conditions/mask/l2a_classification/r20m/x              int64
      34          /conditions/mask/l2a_classification/r20m/y              int64
      35        /conditions/mask/l2a_classification/r60m/scl              uint8
      36          /conditions/mask/l2a_classification/r60m/x              int64
      37          /conditions/mask/l2a_classification/r60m/y              int64
      38                /conditions/meteorology/cams/aod1240            float32
      39                 /conditions/meteorology/cams/aod469            float32
      40                 /conditions/meteorology/cams/aod550            float32
      41                 /conditions/meteorology/cams/aod670            float32
      42                 /conditions/meteorology/cams/aod865            float32
      43               /conditions/meteorology/cams/bcaod550            float32
      44               /conditions/meteorology/cams/duaod550            float32
      45          /conditions/meteorology/cams/isobaricInhPa            float64
      46               /conditions/meteorology/cams/latitude            float64
      47              /conditions/meteorology/cams/longitude            float64
      48                 /conditions/meteorology/cams/number              int64
      49               /conditions/meteorology/cams/omaod550            float32
      50               /conditions/meteorology/cams/ssaod550            float32
      51                   /conditions/meteorology/cams/step              int64
      52               /conditions/meteorology/cams/suaod550            float32
      53                /conditions/meteorology/cams/surface            float64
      54                   /conditions/meteorology/cams/time              int64
      55             /conditions/meteorology/cams/valid_time              int64
      56                      /conditions/meteorology/cams/z            float32
      57         /conditions/meteorology/ecmwf/isobaricInhPa            float64
      58              /conditions/meteorology/ecmwf/latitude            float64
      59             /conditions/meteorology/ecmwf/longitude            float64
      60                   /conditions/meteorology/ecmwf/msl            float32
      61                /conditions/meteorology/ecmwf/number              int64
      62                     /conditions/meteorology/ecmwf/r            float32
      63                  /conditions/meteorology/ecmwf/step              int64
      64               /conditions/meteorology/ecmwf/surface            float64
      65                  /conditions/meteorology/ecmwf/tco3            float32
      66                  /conditions/meteorology/ecmwf/tcwv            float32
      67                  /conditions/meteorology/ecmwf/time              int64
      68                   /conditions/meteorology/ecmwf/u10            float32
      69                   /conditions/meteorology/ecmwf/v10            float32
      70            /conditions/meteorology/ecmwf/valid_time              int64
      71                  /measurements/reflectance/r10m/b02             uint16
      72                  /measurements/reflectance/r10m/b03             uint16
      73                  /measurements/reflectance/r10m/b04             uint16
      74                  /measurements/reflectance/r10m/b08             uint16
      75                    /measurements/reflectance/r10m/x              int64
      76                    /measurements/reflectance/r10m/y              int64
      77                  /measurements/reflectance/r20m/b01             uint16
      78                  /measurements/reflectance/r20m/b02             uint16
      79                  /measurements/reflectance/r20m/b03             uint16
      80                  /measurements/reflectance/r20m/b04             uint16
      81                  /measurements/reflectance/r20m/b05             uint16
      82                  /measurements/reflectance/r20m/b06             uint16
      83                  /measurements/reflectance/r20m/b07             uint16
      84                  /measurements/reflectance/r20m/b11             uint16
      85                  /measurements/reflectance/r20m/b12             uint16
      86                  /measurements/reflectance/r20m/b8a             uint16
      87                    /measurements/reflectance/r20m/x              int64
      88                    /measurements/reflectance/r20m/y              int64
      89                  /measurements/reflectance/r60m/b01             uint16
      90                  /measurements/reflectance/r60m/b02             uint16
      91                  /measurements/reflectance/r60m/b03             uint16
      92                  /measurements/reflectance/r60m/b04             uint16
      93                  /measurements/reflectance/r60m/b05             uint16
      94                  /measurements/reflectance/r60m/b06             uint16
      95                  /measurements/reflectance/r60m/b07             uint16
      96                  /measurements/reflectance/r60m/b09             uint16
      97                  /measurements/reflectance/r60m/b11             uint16
      98                  /measurements/reflectance/r60m/b12             uint16
      99                  /measurements/reflectance/r60m/b8a             uint16
      100                   /measurements/reflectance/r60m/x              int64
      101                   /measurements/reflectance/r60m/y              int64
      102                       /quality/atmosphere/r10m/aot             uint16
      103                       /quality/atmosphere/r10m/wvp             uint16
      104                         /quality/atmosphere/r10m/x              int64
      105                         /quality/atmosphere/r10m/y              int64
      106                       /quality/atmosphere/r20m/aot             uint16
      107                       /quality/atmosphere/r20m/wvp             uint16
      108                         /quality/atmosphere/r20m/x              int64
      109                         /quality/atmosphere/r20m/y              int64
      110                       /quality/atmosphere/r60m/aot             uint16
      111                       /quality/atmosphere/r60m/wvp             uint16
      112                         /quality/atmosphere/r60m/x              int64
      113                         /quality/atmosphere/r60m/y              int64
      114                   /quality/l2a_quicklook/r10m/band              int64
      115                    /quality/l2a_quicklook/r10m/tci              uint8
      116                      /quality/l2a_quicklook/r10m/x              int64
      117                      /quality/l2a_quicklook/r10m/y              int64
      118                   /quality/l2a_quicklook/r20m/band              int64
      119                    /quality/l2a_quicklook/r20m/tci              uint8
      120                      /quality/l2a_quicklook/r20m/x              int64
      121                      /quality/l2a_quicklook/r20m/y              int64
      122                   /quality/l2a_quicklook/r60m/band              int64
      123                    /quality/l2a_quicklook/r60m/tci              uint8
      124                      /quality/l2a_quicklook/r60m/x              int64
      125                      /quality/l2a_quicklook/r60m/y              int64
      126                             /quality/mask/r10m/b02              uint8
      127                             /quality/mask/r10m/b03              uint8
      128                             /quality/mask/r10m/b04              uint8
      129                             /quality/mask/r10m/b08              uint8
      130                               /quality/mask/r10m/x              int64
      131                               /quality/mask/r10m/y              int64
      132                             /quality/mask/r20m/b05              uint8
      133                             /quality/mask/r20m/b06              uint8
      134                             /quality/mask/r20m/b07              uint8
      135                             /quality/mask/r20m/b11              uint8
      136                             /quality/mask/r20m/b12              uint8
      137                             /quality/mask/r20m/b8a              uint8
      138                               /quality/mask/r20m/x              int64
      139                               /quality/mask/r20m/y              int64
      140                             /quality/mask/r60m/b01              uint8
      141                             /quality/mask/r60m/b09              uint8
      142                             /quality/mask/r60m/b10              uint8
      143                               /quality/mask/r60m/x              int64
      144                               /quality/mask/r60m/y              int64
      145                     /quality/probability/r20m/band              int64
      146                      /quality/probability/r20m/cld              uint8
      147                      /quality/probability/r20m/snw              uint8
      148                        /quality/probability/r20m/x              int64
      149                        /quality/probability/r20m/y              int64
          endianness compressor              dim       chunk_dim       nchunks
      1       little      blosc                2               2             1
      2       little      blosc               13              13             1
      3       little      blosc                6               6             1
      4       little      blosc                2               2             1
      5       little      blosc            13, 2           13, 2          1, 1
      6       little      blosc        2, 23, 23       2, 23, 23       1, 1, 1
      7       little      blosc 13, 6, 2, 23, 23 7, 3, 2, 23, 23 2, 2, 1, 1, 1
      8       little      blosc               23              23             1
      9       little      blosc               23              23             1
      10        <NA>      blosc     10980, 10980      1830, 1830          6, 6
      11        <NA>      blosc     10980, 10980      1830, 1830          6, 6
      12        <NA>      blosc     10980, 10980      1830, 1830          6, 6
      13        <NA>      blosc     10980, 10980      1830, 1830          6, 6
      14      little      blosc            10980           10980             1
      15      little      blosc            10980           10980             1
      16        <NA>      blosc       5490, 5490        915, 915          6, 6
      17        <NA>      blosc       5490, 5490        915, 915          6, 6
      18        <NA>      blosc       5490, 5490        915, 915          6, 6
      19        <NA>      blosc       5490, 5490        915, 915          6, 6
      20        <NA>      blosc       5490, 5490        915, 915          6, 6
      21        <NA>      blosc       5490, 5490        915, 915          6, 6
      22      little      blosc             5490            5490             1
      23      little      blosc             5490            5490             1
      24        <NA>      blosc       1830, 1830        305, 305          6, 6
      25        <NA>      blosc       1830, 1830        305, 305          6, 6
      26        <NA>      blosc       1830, 1830        305, 305          6, 6
      27      little      blosc             1830            1830             1
      28      little      blosc             1830            1830             1
      29        <NA>      blosc       1830, 1830        305, 305          6, 6
      30      little      blosc             1830            1830             1
      31      little      blosc             1830            1830             1
      32        <NA>      blosc       5490, 5490        915, 915          6, 6
      33      little      blosc             5490            5490             1
      34      little      blosc             5490            5490             1
      35        <NA>      blosc       1830, 1830        305, 305          6, 6
      36      little      blosc             1830            1830             1
      37      little      blosc             1830            1830             1
      38      little      blosc             9, 9            9, 9          1, 1
      39      little      blosc             9, 9            9, 9          1, 1
      40      little      blosc             9, 9            9, 9          1, 1
      41      little      blosc             9, 9            9, 9          1, 1
      42      little      blosc             9, 9            9, 9          1, 1
      43      little      blosc             9, 9            9, 9          1, 1
      44      little      blosc             9, 9            9, 9          1, 1
      45      little       <NA>                1               1             1
      46      little      blosc                9               9             1
      47      little      blosc                9               9             1
      48      little       <NA>                1               1             1
      49      little      blosc             9, 9            9, 9          1, 1
      50      little      blosc             9, 9            9, 9          1, 1
      51      little       <NA>                1               1             1
      52      little      blosc             9, 9            9, 9          1, 1
      53      little       <NA>                1               1             1
      54      little       <NA>                1               1             1
      55      little       <NA>                1               1             1
      56      little      blosc             9, 9            9, 9          1, 1
      57      little       <NA>                1               1             1
      58      little      blosc                9               9             1
      59      little      blosc                9               9             1
      60      little      blosc             9, 9            9, 9          1, 1
      61      little       <NA>                1               1             1
      62      little      blosc             9, 9            9, 9          1, 1
      63      little       <NA>                1               1             1
      64      little       <NA>                1               1             1
      65      little      blosc             9, 9            9, 9          1, 1
      66      little      blosc             9, 9            9, 9          1, 1
      67      little       <NA>                1               1             1
      68      little      blosc             9, 9            9, 9          1, 1
      69      little      blosc             9, 9            9, 9          1, 1
      70      little       <NA>                1               1             1
      71      little      blosc     10980, 10980      1830, 1830          6, 6
      72      little      blosc     10980, 10980      1830, 1830          6, 6
      73      little      blosc     10980, 10980      1830, 1830          6, 6
      74      little      blosc     10980, 10980      1830, 1830          6, 6
      75      little      blosc            10980           10980             1
      76      little      blosc            10980           10980             1
      77      little      blosc       5490, 5490        915, 915          6, 6
      78      little      blosc       5490, 5490        915, 915          6, 6
      79      little      blosc       5490, 5490        915, 915          6, 6
      80      little      blosc       5490, 5490        915, 915          6, 6
      81      little      blosc       5490, 5490        915, 915          6, 6
      82      little      blosc       5490, 5490        915, 915          6, 6
      83      little      blosc       5490, 5490        915, 915          6, 6
      84      little      blosc       5490, 5490        915, 915          6, 6
      85      little      blosc       5490, 5490        915, 915          6, 6
      86      little      blosc       5490, 5490        915, 915          6, 6
      87      little      blosc             5490            5490             1
      88      little      blosc             5490            5490             1
      89      little      blosc       1830, 1830        305, 305          6, 6
      90      little      blosc       1830, 1830        305, 305          6, 6
      91      little      blosc       1830, 1830        305, 305          6, 6
      92      little      blosc       1830, 1830        305, 305          6, 6
      93      little      blosc       1830, 1830        305, 305          6, 6
      94      little      blosc       1830, 1830        305, 305          6, 6
      95      little      blosc       1830, 1830        305, 305          6, 6
      96      little      blosc       1830, 1830        305, 305          6, 6
      97      little      blosc       1830, 1830        305, 305          6, 6
      98      little      blosc       1830, 1830        305, 305          6, 6
      99      little      blosc       1830, 1830        305, 305          6, 6
      100     little      blosc             1830            1830             1
      101     little      blosc             1830            1830             1
      102     little      blosc     10980, 10980      1830, 1830          6, 6
      103     little      blosc     10980, 10980      1830, 1830          6, 6
      104     little      blosc            10980           10980             1
      105     little      blosc            10980           10980             1
      106     little      blosc       5490, 5490        915, 915          6, 6
      107     little      blosc       5490, 5490        915, 915          6, 6
      108     little      blosc             5490            5490             1
      109     little      blosc             5490            5490             1
      110     little      blosc       1830, 1830        305, 305          6, 6
      111     little      blosc       1830, 1830        305, 305          6, 6
      112     little      blosc             1830            1830             1
      113     little      blosc             1830            1830             1
      114     little      blosc                3               3             1
      115       <NA>      blosc  3, 10980, 10980   1, 1830, 1830       3, 6, 6
      116     little      blosc            10980           10980             1
      117     little      blosc            10980           10980             1
      118     little      blosc                3               3             1
      119       <NA>      blosc    3, 5490, 5490     1, 915, 915       3, 6, 6
      120     little      blosc             5490            5490             1
      121     little      blosc             5490            5490             1
      122     little      blosc                3               3             1
      123       <NA>      blosc    3, 1830, 1830     1, 305, 305       3, 6, 6
      124     little      blosc             1830            1830             1
      125     little      blosc             1830            1830             1
      126       <NA>      blosc     10980, 10980      1830, 1830          6, 6
      127       <NA>      blosc     10980, 10980      1830, 1830          6, 6
      128       <NA>      blosc     10980, 10980      1830, 1830          6, 6
      129       <NA>      blosc     10980, 10980      1830, 1830          6, 6
      130     little      blosc            10980           10980             1
      131     little      blosc            10980           10980             1
      132       <NA>      blosc       5490, 5490        915, 915          6, 6
      133       <NA>      blosc       5490, 5490        915, 915          6, 6
      134       <NA>      blosc       5490, 5490        915, 915          6, 6
      135       <NA>      blosc       5490, 5490        915, 915          6, 6
      136       <NA>      blosc       5490, 5490        915, 915          6, 6
      137       <NA>      blosc       5490, 5490        915, 915          6, 6
      138     little      blosc             5490            5490             1
      139     little      blosc             5490            5490             1
      140       <NA>      blosc       1830, 1830        305, 305          6, 6
      141       <NA>      blosc       1830, 1830        305, 305          6, 6
      142       <NA>      blosc       1830, 1830        305, 305          6, 6
      143     little      blosc             1830            1830             1
      144     little      blosc             1830            1830             1
      145     little       <NA>                1               1             1
      146       <NA>      blosc       5490, 5490        915, 915          6, 6
      147       <NA>      blosc       5490, 5490        915, 915          6, 6
      148     little      blosc             5490            5490             1
      149     little      blosc             5490            5490             1

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

# zarr_overview works with consolidated v3 metadata

    Code
      zarr_overview(zarr_store_consolidated_v3, as_data_frame = FALSE)
    Output
      Type: Group of Arrays
      Path: <path>
      Arrays:
      ---
      Path: <path>
        Shape: 3 x 45087 x 11580
        Chunk Shape: 1 x 4096 x 4096
        No. of Chunks: 108 (3 x 12 x 3)
        Data Type: uint8
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 3 x 22543 x 5790
        Chunk Shape: 1 x 4096 x 4096
        No. of Chunks: 36 (3 x 6 x 2)
        Data Type: uint8
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 3 x 11271 x 2895
        Chunk Shape: 1 x 4096 x 2895
        No. of Chunks: 9 (3 x 3 x 1)
        Data Type: uint8
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 3 x 5635 x 1447
        Chunk Shape: 1 x 4096 x 1447
        No. of Chunks: 6 (3 x 2 x 1)
        Data Type: uint8
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 3 x 2817 x 723
        Chunk Shape: 1 x 2817 x 723
        No. of Chunks: 3 (3 x 1 x 1)
        Data Type: uint8
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 4 x 17098 x 51187
        Chunk Shape: 1 x 4096 x 4096
        No. of Chunks: 260 (4 x 5 x 13)
        Data Type: uint16
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 4 x 8549 x 25593
        Chunk Shape: 1 x 4096 x 4096
        No. of Chunks: 84 (4 x 3 x 7)
        Data Type: uint16
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 4 x 4274 x 12796
        Chunk Shape: 1 x 4096 x 4096
        No. of Chunks: 32 (4 x 2 x 4)
        Data Type: uint16
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 4 x 2137 x 6398
        Chunk Shape: 1 x 2137 x 4096
        No. of Chunks: 8 (4 x 1 x 2)
        Data Type: uint16
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 4 x 1068 x 3199
        Chunk Shape: 1 x 1068 x 3199
        No. of Chunks: 4 (4 x 1 x 1)
        Data Type: uint16
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 17098 x 51187
        Chunk Shape: 4096 x 4096
        No. of Chunks: 65 (5 x 13)
        Data Type: uint32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 8549 x 25593
        Chunk Shape: 4096 x 4096
        No. of Chunks: 21 (3 x 7)
        Data Type: uint32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 4274 x 12796
        Chunk Shape: 4096 x 4096
        No. of Chunks: 8 (2 x 4)
        Data Type: uint32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 2137 x 6398
        Chunk Shape: 2137 x 4096
        No. of Chunks: 2 (1 x 2)
        Data Type: uint32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 1068 x 3199
        Chunk Shape: 1068 x 3199
        No. of Chunks: 1 (1 x 1)
        Data Type: uint32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 17098 x 51187
        Chunk Shape: 4096 x 4096
        No. of Chunks: 65 (5 x 13)
        Data Type: uint32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 8549 x 25593
        Chunk Shape: 4096 x 4096
        No. of Chunks: 21 (3 x 7)
        Data Type: uint32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 4274 x 12796
        Chunk Shape: 4096 x 4096
        No. of Chunks: 8 (2 x 4)
        Data Type: uint32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 2137 x 6398
        Chunk Shape: 2137 x 4096
        No. of Chunks: 2 (1 x 2)
        Data Type: uint32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 1068 x 3199
        Chunk Shape: 1068 x 3199
        No. of Chunks: 1 (1 x 1)
        Data Type: uint32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 
        Chunk Shape: 
        No. of Chunks: 1 ()
        Data Type: bool
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 162254
        No. of Chunks: 1 (1)
        Data Type: string
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 40564
        No. of Chunks: 4 (4)
        Data Type: float64
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 162254
        No. of Chunks: 1 (1)
        Data Type: string
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 40564
        No. of Chunks: 4 (4)
        Data Type: int64
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 40564
        No. of Chunks: 4 (4)
        Data Type: int64
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 40564
        No. of Chunks: 4 (4)
        Data Type: int64
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 40564
        No. of Chunks: 4 (4)
        Data Type: int64
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 40564
        No. of Chunks: 4 (4)
        Data Type: float64
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 40564
        No. of Chunks: 4 (4)
        Data Type: float64
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 40564
        No. of Chunks: 4 (4)
        Data Type: int64
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 40564
        No. of Chunks: 4 (4)
        Data Type: int64
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 40564
        No. of Chunks: 4 (4)
        Data Type: int64
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 40564
        No. of Chunks: 4 (4)
        Data Type: float64
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254 x 2
        Chunk Shape: 40564 x 1
        No. of Chunks: 8 (4 x 2)
        Data Type: float64
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 377
        Chunk Shape: 377
        No. of Chunks: 1 (1)
        Data Type: string
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 377
        Chunk Shape: 377
        No. of Chunks: 1 (1)
        Data Type: string
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 377
        Chunk Shape: 377
        No. of Chunks: 1 (1)
        Data Type: string
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 377
        Chunk Shape: 377
        No. of Chunks: 1 (1)
        Data Type: string
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 4900367
        Chunk Shape: 153137
        No. of Chunks: 32 (32)
        Data Type: float32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 4900367
        Chunk Shape: 153137
        No. of Chunks: 32 (32)
        Data Type: int32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162255
        Chunk Shape: 81128
        No. of Chunks: 2 (2)
        Data Type: int32
        Endianness: little
        Compressor: zstd
      ---
      Path: <path>
        Shape: 1
        Chunk Shape: 1
        No. of Chunks: 1 (1)
        Data Type: string
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 162254
        Chunk Shape: 162254
        No. of Chunks: 1 (1)
        Data Type: int8
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 
        Chunk Shape: 
        No. of Chunks: 1 ()
        Data Type: string
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 
        Chunk Shape: 
        No. of Chunks: 1 ()
        Data Type: string
        Endianness: NA
        Compressor: zstd
      ---
      Path: <path>
        Shape: 
        Chunk Shape: 
        No. of Chunks: 1 ()
        Data Type: string
        Endianness: NA
        Compressor: zstd

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

