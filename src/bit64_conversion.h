#include <R.h>
#include <Rdefines.h>
#include <stdint.h>
#include <stddef.h>

int32_t uint32_to_int32(void* in_buf, size_t n, void* out_buf);
int8_t   int64_to_int32(void* in_buf, size_t n, void* out_buf, int is_signed);
