midi <- R6::R6Class("midi", list(
  header = NULL,
  tracks = NULL,
  initialize = function(
    header = list(format = 1, n_tracks = 0, n_ticks_per_quarter_note = 960),
    tracks = list()){
    # we might want to be able define an empty midi with default headers etc
    self$header <- header
    self$tracks <- tracks
  },
  print = function(...) {
    message("header:")
    print(self$header)
    message("tracks:")
    print(self$tracks)
    invisible(self)
  }
))

# test <- midi$new(parsed$header, parsed$tracks)
# attributes(test$tracks[[1]])

midi$set("public", "track_names", function() {
  vapply(self$tracks, function(x) {
    subset(x, event == "Sequence/Track Name")[["params"]][[1]][["value"]]
  }, character(1), USE.NAMES = FALSE)
})




#' @method print midi_track
#' @export
print.midi_track <- function(x, ...){
  x_displayed <- x
  x_displayed$channel <- purrr::map_int(x$params, ~ as.integer(purrr::pluck(.,"channel", .default = NA)))
  x_displayed$time <- cumsum(x_displayed$deltatime)
  class(x_displayed) <- c("tbl_df", "tbl", "data.frame")
  x_displayed$params <- midi_params(x_displayed$params)
  print(x_displayed[c("time", "event", "channel", "params")])
  invisible(x)
}


#' @export
midi_params <- function(x) {
  vctrs::new_rcrd(list(params =x), class = "midi_params")
}


#' @export
format.midi_params <- function(x, ...) {
  ret <- purrr::map_chr(vctrs::field(x, "params"), ~ {
    .x[["channel"]] <- NULL
    .x[["length"]] <- NULL
    if(length(.x) == 1) return(as.character(.x))
    nms <- names(.x)

    if("value" %in% nms) {
      nms <- unique(c("value", nms))
      .x <- .x[nms]
    }
    paste(nms, format(.x, width = 3), sep = ": ", collapse = ", ")
  })

  ret <- sub("^value:", "", ret)
  ret <- sub("^key_number: ", "", ret)
  ret
}

#' @export
vec_ptype_abbr.midi_params  <- function(x) {
  "params"
}
