#include "float16_conversion.h"

/* this function is based on the float16->float32 implementation found at
 *  https://gist.github.com/milhidaka/95863906fe828198f47991c813dbe233
 *  as well as the process described 
 *  https://fgiesen.wordpress.com/2012/03/28/half-to-float-done-quic/
 */
double float16_to_float64(uint16_t float16_value) {
  // float16=1bit: sign, 5bit: exponent, 10bit: fraction
  // float64=1bit: sign, 11bit: exponent, 52bit: fraction
  uint64_t sign = float16_value >> 15;
  uint64_t exponent = (float16_value >> 10) & 0x1F;
  uint64_t fraction = (float16_value & 0x3FF);
  uint64_t float64_value;
  if (exponent == 0) {
    if (fraction == 0) {
      /* zero */
      float64_value = (sign << 63);
    } else {
      /* denormalised number */
      exponent = 1023 - 14;
      while ((fraction & (1 << 10)) == 0) {
        exponent--;
        fraction <<= 1;
      }
      fraction &= 0x3FF;
      float64_value = (sign << 63) | (exponent << 52) | (fraction << 42);  
    }    
  } else if (exponent == 0x1F)  {
    /* Inf or NaN */
    float64_value = (sign << 63) | (0x7FFUL << 52) | (fraction << 42);
  } else {
    /* ordinary number */
    float64_value = (sign << 63) | ((exponent + (1023-15)) << 52) | (fraction << 42);
  }
  
  return *((double*)&float64_value);
}
