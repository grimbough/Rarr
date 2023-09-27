
.unicode_to_int <- function(input, typestr) {
  
  data_type <- .parse_datatype(typestr)
  
  nchar <- as.integer(data_type$nbytes / 4L)
  
  to <- ifelse(data_type$endian == "little", "UCS-4LE", "UCS-4BE")
  raw_list <- iconv(input, to = to, toRaw = TRUE)
  
  base64_strings <- vapply(raw_list, 
                     function(x) { 
                       if(length(x) < nchar * 4) {
                         x <- c(x, as.raw(rep(0, (nchar * 4) - length(x))))
                       }
                       jsonlite::base64_enc(x)
                     }, 
                     FUN.VALUE = character(1), 
                     USE.NAMES = FALSE)
  
  return(base64_strings)
  
}