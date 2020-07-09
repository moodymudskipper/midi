#' @importFrom magrittr %>%
#' @importFrom stats setNames
#' @importFrom dplyr mutate filter group_by summarize select arrange bind_rows
#' @importFrom ggplot2 ggplot aes geom_segment labs theme theme_classic
#' @importFrom ggplot2 scale_y_continuous element_line element_blank
#' @importFrom ggplot2 scale_x_continuous
NULL

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

# # for debugging purposes, from integer get a raw vector
# as_raw0 <- function(x, n = NA, signed = TRUE){
#   if (signed && x < 0) x <- 256 + x
#   x <- as.character(as.hexmode(x))
#   if(!is.na(n) && nchar(x) != n*2) {
#     x <- paste0(strrep("0", n*2 - nchar(x)),x)
#   }
#   x <- strsplit(x, "")[[1]]
#   x <- matrix(x, ncol = 2)
#   x <- as.vector(apply(x, 1, function(x) as.raw(as.hexmode(paste(x, collapse= "")))))
#   x
# }
#
# as_raw <- function(x, n = NA, signed){
#   lapply(x, as_raw0, n, signed)
# }

# readChar <- function (con, nchars, useBytes = FALSE) {
#   res <- base::readChar(con, nchars, useBytes)
#   print(charToRaw(res))
#   res
# }


scale_x_duration <- function(..., units = c("s","min","h","day")){
  units <- match.arg(units, c("ms","seconds","minutes","hours","days", "weeks", "months", "years"),
                     several.ok = TRUE)
  # to get them in the right order
  units <- intersect(c("ms","seconds","minutes","hours","days", "weeks", "months", "years"), units)

  ms    <- 1/1000
  min   <- 60
  h     <- 60 *min
  day   <- 24 *h
  week  <- 7 * day
  year  <- 365.25 * day
  month <- year / 12

  labels <- function(x){
    max_x <- max(x, na.rm=TRUE)

    if("years" == units[[1]] || max_x > 3 * year && "years" %in% units){
      # > 3 year, use years
      x <- x /year
      x <- paste(round(x,2),"years")
    } else if("months" == units[[1]] || max_x > 3 * month && "months" %in% units){
      # > 3 months, use months
      x <- x /month
      x <- paste(round(x,2),"months")
    } else if("weeks" == units[[1]] || max_x > 3 * week && "weeks" %in% units){
      # > 3 weeks, use weeks
      x <- x /week
      x <- paste(round(x,2),"weeks")
    } else if("days" == units[[1]] || max_x > 3 * day && "days" %in% units){
      # > 3 days, use days
      x <- x /day
      x <- paste(round(x,2),"days")
    } else if("hours" == units[[1]] || max_x > 3 * h && "hours" %in% units){
      # > 3 h, use h
      x <- x /h
      x <- paste(round(x,2),"h")
    } else if("minutes" == units[[1]] || max_x > 3 * min && "minutes" %in% units){
      # > 3 min, use min
      x <- x /min
      x <- paste(round(x,2),"min")
    } else if("seconds" == units[[1]] || max_x > 3 * min && "seconds" %in% units){
      # > 3 min, use min
      x <- paste(round(x,2),"s")
    } else if("ms" %in% units){
      # > 3 min, use min
      x <- x /min
      x <- paste(round(x,2),"min")
    } else {
      stop("no unit was found for provided value of x")
    }
  }

  breaks <- function(x){
    max_x <- max(x, na.rm=TRUE)
    if("years" == units[[1]] || max_x > 3 * year && "years" %in% units){
      # > 3 year, use years
      scales:::extended_breaks()(x/year) * year
    } else if("months" == units[[1]] || max_x > 3 * month && "months" %in% units){
      # > 3 months, use months
      scales:::extended_breaks()(x/month) * month
    } else if("weeks" == units[[1]] || max_x > 3 * week && "weeks" %in% units){
      # > 3 weeks, use weeks
      scales:::extended_breaks()(x/week) * week
    } else if("days" == units[[1]] || max_x > 3 * day && "days" %in% units){
      # > 3 days, use days
      scales:::extended_breaks()(x/day) * day
    } else if("hours" == units[[1]] || max_x > 3 * h && "hours" %in% units){
      # > 3 h, use h
      scales:::extended_breaks()(x/h) * h
    } else if("minutes" == units[[1]] || max_x > 3 * min && "minutes" %in% units){
      # > 3 min, use min
      scales:::extended_breaks()(x/min) * min
    }  else if("seconds" == units[[1]] || max_x > 3 && "seconds" %in% units){
      # > 3 s, use s
      scales:::extended_breaks()(x)
    } else if("ms" %in% units){
      scales:::extended_breaks()(x/ms) * ms
    } else {
      stop("no unit was found for provided value of x")
    }
  }
  scale_x_continuous(..., labels = labels, breaks = breaks)
}



