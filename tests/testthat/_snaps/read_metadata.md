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
      zarr_overview(complex_consolidated, as_data_frame = TRUE)
    Output
                                                                                                                                                 path
      1                           /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/geometry/angle
      2                            /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/geometry/band
      3                        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/geometry/detector
      4                 /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/geometry/mean_sun_angles
      5   /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/geometry/mean_viewing_incidence_angles
      6                      /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/geometry/sun_angles
      7        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/geometry/viewing_incidence_angles
      8                               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/geometry/x
      9                               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/geometry/y
      10        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r10m/b02
      11        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r10m/b03
      12        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r10m/b04
      13        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r10m/b08
      14          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r10m/x
      15          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r10m/y
      16        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r20m/b05
      17        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r20m/b06
      18        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r20m/b07
      19        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r20m/b11
      20        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r20m/b12
      21        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r20m/b8a
      22          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r20m/x
      23          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r20m/y
      24        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r60m/b01
      25        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r60m/b09
      26        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r60m/b10
      27          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r60m/x
      28          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/detector_footprint/r60m/y
      29        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/l1c_classification/r60m/b00
      30          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/l1c_classification/r60m/x
      31          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/l1c_classification/r60m/y
      32        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/l2a_classification/r20m/scl
      33          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/l2a_classification/r20m/x
      34          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/l2a_classification/r20m/y
      35        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/l2a_classification/r60m/scl
      36          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/l2a_classification/r60m/x
      37          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/mask/l2a_classification/r60m/y
      38                /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/aod1240
      39                 /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/aod469
      40                 /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/aod550
      41                 /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/aod670
      42                 /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/aod865
      43               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/bcaod550
      44               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/duaod550
      45          /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/isobaricInhPa
      46               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/latitude
      47              /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/longitude
      48                 /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/number
      49               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/omaod550
      50               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/ssaod550
      51                   /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/step
      52               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/suaod550
      53                /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/surface
      54                   /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/time
      55             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/valid_time
      56                      /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/cams/z
      57         /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/isobaricInhPa
      58              /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/latitude
      59             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/longitude
      60                   /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/msl
      61                /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/number
      62                     /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/r
      63                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/step
      64               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/surface
      65                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/tco3
      66                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/tcwv
      67                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/time
      68                   /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/u10
      69                   /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/v10
      70            /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/conditions/meteorology/ecmwf/valid_time
      71                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r10m/b02
      72                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r10m/b03
      73                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r10m/b04
      74                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r10m/b08
      75                    /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r10m/x
      76                    /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r10m/y
      77                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r20m/b01
      78                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r20m/b02
      79                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r20m/b03
      80                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r20m/b04
      81                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r20m/b05
      82                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r20m/b06
      83                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r20m/b07
      84                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r20m/b11
      85                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r20m/b12
      86                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r20m/b8a
      87                    /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r20m/x
      88                    /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r20m/y
      89                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/b01
      90                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/b02
      91                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/b03
      92                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/b04
      93                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/b05
      94                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/b06
      95                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/b07
      96                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/b09
      97                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/b11
      98                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/b12
      99                  /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/b8a
      100                   /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/x
      101                   /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/measurements/reflectance/r60m/y
      102                       /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/atmosphere/r10m/aot
      103                       /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/atmosphere/r10m/wvp
      104                         /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/atmosphere/r10m/x
      105                         /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/atmosphere/r10m/y
      106                       /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/atmosphere/r20m/aot
      107                       /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/atmosphere/r20m/wvp
      108                         /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/atmosphere/r20m/x
      109                         /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/atmosphere/r20m/y
      110                       /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/atmosphere/r60m/aot
      111                       /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/atmosphere/r60m/wvp
      112                         /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/atmosphere/r60m/x
      113                         /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/atmosphere/r60m/y
      114                   /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/l2a_quicklook/r10m/band
      115                    /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/l2a_quicklook/r10m/tci
      116                      /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/l2a_quicklook/r10m/x
      117                      /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/l2a_quicklook/r10m/y
      118                   /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/l2a_quicklook/r20m/band
      119                    /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/l2a_quicklook/r20m/tci
      120                      /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/l2a_quicklook/r20m/x
      121                      /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/l2a_quicklook/r20m/y
      122                   /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/l2a_quicklook/r60m/band
      123                    /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/l2a_quicklook/r60m/tci
      124                      /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/l2a_quicklook/r60m/x
      125                      /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/l2a_quicklook/r60m/y
      126                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r10m/b02
      127                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r10m/b03
      128                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r10m/b04
      129                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r10m/b08
      130                               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r10m/x
      131                               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r10m/y
      132                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r20m/b05
      133                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r20m/b06
      134                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r20m/b07
      135                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r20m/b11
      136                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r20m/b12
      137                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r20m/b8a
      138                               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r20m/x
      139                               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r20m/y
      140                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r60m/b01
      141                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r60m/b09
      142                             /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r60m/b10
      143                               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r60m/x
      144                               /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/mask/r60m/y
      145                     /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/probability/r20m/band
      146                      /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/probability/r20m/cld
      147                      /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/probability/r20m/snw
      148                        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/probability/r20m/x
      149                        /home/hgruson/Projects/Rarr/inst/extdata/zarr_examples/metadata/complex_consolidated.zarr/quality/probability/r20m/y
           data_type endianness compressor          dim    chunk_dim      nchunks
      1   unicode224     little      blosc            2            2            1
      2    unicode96     little      blosc           13           13            1
      3        int64     little      blosc            6            6            1
      4      float64     little      blosc            2            2            1
      5      float64     little      blosc        13, 2        13, 2         1, 1
      6      float64     little      blosc    2, 23, 23    2, 23, 23      1, 1, 1
      7      float64     little      blosc 13, 6, 2.... 7, 3, 2,.... 2, 2, 1,....
      8        int64     little      blosc           23           23            1
      9        int64     little      blosc           23           23            1
      10       uint8       <NA>      blosc 10980, 10980   1830, 1830         6, 6
      11       uint8       <NA>      blosc 10980, 10980   1830, 1830         6, 6
      12       uint8       <NA>      blosc 10980, 10980   1830, 1830         6, 6
      13       uint8       <NA>      blosc 10980, 10980   1830, 1830         6, 6
      14       int64     little      blosc        10980        10980            1
      15       int64     little      blosc        10980        10980            1
      16       uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      17       uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      18       uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      19       uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      20       uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      21       uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      22       int64     little      blosc         5490         5490            1
      23       int64     little      blosc         5490         5490            1
      24       uint8       <NA>      blosc   1830, 1830     305, 305         6, 6
      25       uint8       <NA>      blosc   1830, 1830     305, 305         6, 6
      26       uint8       <NA>      blosc   1830, 1830     305, 305         6, 6
      27       int64     little      blosc         1830         1830            1
      28       int64     little      blosc         1830         1830            1
      29       uint8       <NA>      blosc   1830, 1830     305, 305         6, 6
      30       int64     little      blosc         1830         1830            1
      31       int64     little      blosc         1830         1830            1
      32       uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      33       int64     little      blosc         5490         5490            1
      34       int64     little      blosc         5490         5490            1
      35       uint8       <NA>      blosc   1830, 1830     305, 305         6, 6
      36       int64     little      blosc         1830         1830            1
      37       int64     little      blosc         1830         1830            1
      38     float32     little      blosc         9, 9         9, 9         1, 1
      39     float32     little      blosc         9, 9         9, 9         1, 1
      40     float32     little      blosc         9, 9         9, 9         1, 1
      41     float32     little      blosc         9, 9         9, 9         1, 1
      42     float32     little      blosc         9, 9         9, 9         1, 1
      43     float32     little      blosc         9, 9         9, 9         1, 1
      44     float32     little      blosc         9, 9         9, 9         1, 1
      45     float64     little       <NA>                                       
      46     float64     little      blosc            9            9            1
      47     float64     little      blosc            9            9            1
      48       int64     little       <NA>                                       
      49     float32     little      blosc         9, 9         9, 9         1, 1
      50     float32     little      blosc         9, 9         9, 9         1, 1
      51       int64     little       <NA>                                       
      52     float32     little      blosc         9, 9         9, 9         1, 1
      53     float64     little       <NA>                                       
      54       int64     little       <NA>                                       
      55       int64     little       <NA>                                       
      56     float32     little      blosc         9, 9         9, 9         1, 1
      57     float64     little       <NA>                                       
      58     float64     little      blosc            9            9            1
      59     float64     little      blosc            9            9            1
      60     float32     little      blosc         9, 9         9, 9         1, 1
      61       int64     little       <NA>                                       
      62     float32     little      blosc         9, 9         9, 9         1, 1
      63       int64     little       <NA>                                       
      64     float64     little       <NA>                                       
      65     float32     little      blosc         9, 9         9, 9         1, 1
      66     float32     little      blosc         9, 9         9, 9         1, 1
      67       int64     little       <NA>                                       
      68     float32     little      blosc         9, 9         9, 9         1, 1
      69     float32     little      blosc         9, 9         9, 9         1, 1
      70       int64     little       <NA>                                       
      71      uint16     little      blosc 10980, 10980   1830, 1830         6, 6
      72      uint16     little      blosc 10980, 10980   1830, 1830         6, 6
      73      uint16     little      blosc 10980, 10980   1830, 1830         6, 6
      74      uint16     little      blosc 10980, 10980   1830, 1830         6, 6
      75       int64     little      blosc        10980        10980            1
      76       int64     little      blosc        10980        10980            1
      77      uint16     little      blosc   5490, 5490     915, 915         6, 6
      78      uint16     little      blosc   5490, 5490     915, 915         6, 6
      79      uint16     little      blosc   5490, 5490     915, 915         6, 6
      80      uint16     little      blosc   5490, 5490     915, 915         6, 6
      81      uint16     little      blosc   5490, 5490     915, 915         6, 6
      82      uint16     little      blosc   5490, 5490     915, 915         6, 6
      83      uint16     little      blosc   5490, 5490     915, 915         6, 6
      84      uint16     little      blosc   5490, 5490     915, 915         6, 6
      85      uint16     little      blosc   5490, 5490     915, 915         6, 6
      86      uint16     little      blosc   5490, 5490     915, 915         6, 6
      87       int64     little      blosc         5490         5490            1
      88       int64     little      blosc         5490         5490            1
      89      uint16     little      blosc   1830, 1830     305, 305         6, 6
      90      uint16     little      blosc   1830, 1830     305, 305         6, 6
      91      uint16     little      blosc   1830, 1830     305, 305         6, 6
      92      uint16     little      blosc   1830, 1830     305, 305         6, 6
      93      uint16     little      blosc   1830, 1830     305, 305         6, 6
      94      uint16     little      blosc   1830, 1830     305, 305         6, 6
      95      uint16     little      blosc   1830, 1830     305, 305         6, 6
      96      uint16     little      blosc   1830, 1830     305, 305         6, 6
      97      uint16     little      blosc   1830, 1830     305, 305         6, 6
      98      uint16     little      blosc   1830, 1830     305, 305         6, 6
      99      uint16     little      blosc   1830, 1830     305, 305         6, 6
      100      int64     little      blosc         1830         1830            1
      101      int64     little      blosc         1830         1830            1
      102     uint16     little      blosc 10980, 10980   1830, 1830         6, 6
      103     uint16     little      blosc 10980, 10980   1830, 1830         6, 6
      104      int64     little      blosc        10980        10980            1
      105      int64     little      blosc        10980        10980            1
      106     uint16     little      blosc   5490, 5490     915, 915         6, 6
      107     uint16     little      blosc   5490, 5490     915, 915         6, 6
      108      int64     little      blosc         5490         5490            1
      109      int64     little      blosc         5490         5490            1
      110     uint16     little      blosc   1830, 1830     305, 305         6, 6
      111     uint16     little      blosc   1830, 1830     305, 305         6, 6
      112      int64     little      blosc         1830         1830            1
      113      int64     little      blosc         1830         1830            1
      114      int64     little      blosc            3            3            1
      115      uint8       <NA>      blosc 3, 10980.... 1, 1830,....      3, 6, 6
      116      int64     little      blosc        10980        10980            1
      117      int64     little      blosc        10980        10980            1
      118      int64     little      blosc            3            3            1
      119      uint8       <NA>      blosc 3, 5490,....  1, 915, 915      3, 6, 6
      120      int64     little      blosc         5490         5490            1
      121      int64     little      blosc         5490         5490            1
      122      int64     little      blosc            3            3            1
      123      uint8       <NA>      blosc 3, 1830,....  1, 305, 305      3, 6, 6
      124      int64     little      blosc         1830         1830            1
      125      int64     little      blosc         1830         1830            1
      126      uint8       <NA>      blosc 10980, 10980   1830, 1830         6, 6
      127      uint8       <NA>      blosc 10980, 10980   1830, 1830         6, 6
      128      uint8       <NA>      blosc 10980, 10980   1830, 1830         6, 6
      129      uint8       <NA>      blosc 10980, 10980   1830, 1830         6, 6
      130      int64     little      blosc        10980        10980            1
      131      int64     little      blosc        10980        10980            1
      132      uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      133      uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      134      uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      135      uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      136      uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      137      uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      138      int64     little      blosc         5490         5490            1
      139      int64     little      blosc         5490         5490            1
      140      uint8       <NA>      blosc   1830, 1830     305, 305         6, 6
      141      uint8       <NA>      blosc   1830, 1830     305, 305         6, 6
      142      uint8       <NA>      blosc   1830, 1830     305, 305         6, 6
      143      int64     little      blosc         1830         1830            1
      144      int64     little      blosc         1830         1830            1
      145      int64     little       <NA>                                       
      146      uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      147      uint8       <NA>      blosc   5490, 5490     915, 915         6, 6
      148      int64     little      blosc         5490         5490            1
      149      int64     little      blosc         5490         5490            1

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

