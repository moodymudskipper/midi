

write_var_length <- function(n, con){
  b <- rep(NA_integer_, 4)
  b[1] <- n
  if(b[1] > 127){
    b[2] <- b[1] %/% 128
    b[1] <- b[1] %% 128
    if(b[2] > 127){
      b[3] <- b[2] %/% 128
      b[2] <- b[2] %% 128
      if(b[3] > 127){
        b[4] <- b[3] %/% 128
        b[3] <- b[3] %% 128
      }
    }
  }
  b <- b[!is.na(b)]
  b[-1] <-  b[-1] + 128
  sapply(rev(b), write_integer, con)
}


read_var_length <- function(con){
  # <delta_time> : number of 'ticks' from previous event
  # it's a succession of bytes where the first bit is 1 until we reach the last byte
  # e.g. 10000010 then 01111100 , for 82 7c
  # this means we have 2^7 = 128 values per byte, see read_var_length()
  # in principle should be recursive, but using 4 bytes is enough for practical uses

  b <- numeric(4)
  b[1] <- read_integer(con, signed = FALSE)
  bytes <- 1
  if(b[1] > 127){
    b[1] <- (b[1]-128) * 2^7
    b[2] <- read_integer(con, signed = FALSE)
    bytes <- 2
    if(b[2] > 127){
      b[2] <- b[2]-128
      b <- b*2^7
      b[3] <- read_integer(con, signed = FALSE)
      bytes <- 3
      if(b[3] > 127){
        b[3] <- b[3]-128
        b <- b*2^7
        b[4] <- read_integer(con, signed = FALSE)
        bytes <- 4
      }
    }
  }
  c(sum(b), bytes)
}


as_raw <- function(x, n = NA, signed){
  lapply(x, as_raw0, n, signed)
}

read_integer <- function(con, n = 1, size = 1, signed = TRUE) {
  res <- readBin(con, integer(0), n = n, size = size, signed = signed, endian = "big")
  #lapply(as_raw(res, size, signed), print)
  res
}

write_integer <- function(object, con, size = 1) {
  writeBin(as.integer(object), con, size = size, endian = "big")
}

read_raw <- function(con, n = 1) {
  res <- readBin(con, raw(0), n = n)
  #print(res)
  res
}

write_raw <- function(object, con) {
  writeBin(as.raw(object), con)
}

# for debugging purposes, from integer get a raw vector
as_raw0 <- function(x, n = NA, signed = TRUE){
  if (signed && x < 0) x <- 256 + x
  x <- as.character(as.hexmode(x))
  if(!is.na(n) && nchar(x) != n*2) {
    x <- paste0(strrep("0", n*2 - nchar(x)),x)
  }
  x <- strsplit(x, "")[[1]]
  x <- matrix(x, ncol = 2)
  x <- as.vector(apply(x, 1, function(x) as.raw(as.hexmode(paste(x, collapse= "")))))
  x
}

# readChar <- function (con, nchars, useBytes = FALSE) {
#   res <- base::readChar(con, nchars, useBytes)
#   print(charToRaw(res))
#   res
# }



