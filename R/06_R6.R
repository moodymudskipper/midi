midi <- R6::R6Class("midi", list(
  header = NULL,
  tracks = NULL,
  initialize = function(
    header = list(format = 1, n_tracks = 0, n_ticks_per_quarter_note = 960),
    tracks = list()){
    # we might want to be able define an empty midi with default headers etc
    self$header <- header
    self$tracks <- tracks
  }
))

midi$set("public", "print", function(..., n = NULL) {
  # so we can print tibbles with >20 rows
  opt <- options(tibble.print_min = n)
  on.exit(options(opt))
  message("header:")
  print(self$header, ...)
  message("tracks:")
  print(self$tracks, ...)
  invisible(self)
})

midi$set("public", "track_names", function() {
  vapply(self$tracks, function(x) {
    subset(x, event == "Sequence/Track Name")[["params"]][[1]][["value"]]
  }, character(1), USE.NAMES = FALSE)
})

midi$set("public", "encode", function(to){
  file.create(to)
  con <- file(description = to, open = "wb")
  on.exit(close(con))
  encode_header(self$header, con)
  encode_tracks(self$tracks, con)
})


#' #' @method print midi_track_list
#' #' @export
#' print.midi_track_list <- function(x, n= NULL){
#'   opt <- options(tibble.print_max = n)
#'   on.exit(options(opt))
#'   NextMethod()
#' }

#' @method print midi_track
#' @export
print.midi_track <- function(x, ...){
  x_displayed <- x
  x_displayed$channel <- purrr::map_int(x$params, ~ as.integer(purrr::pluck(.,"channel", .default = NA)))
  x_displayed$time <- cumsum(x_displayed$deltatime)
  class(x_displayed) <- c("tbl_df", "tbl", "data.frame")
  x_displayed$params <- midi_params(x_displayed$params)
  print(x_displayed[c("time",  "channel", "event", "params")], ...)
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
    .x[["controller_number"]] <- NULL
    if(!is.null(.x[["key_number"]])) {
      .x[["value"]] <- key_numbers$note2[key_numbers$key_number == .x[["key_number"]]]
      .x[["key_number"]] <- NULL
    }
    if(length(.x) == 1) return(as.character(.x))
    nms <- names(.x)

    if("value" %in% nms) {
      nms <- unique(c("value", nms))
      .x <- .x[nms]
    }
    paste(nms, format(.x, width = 3), sep = ": ", collapse = ", ")
  })

  ret <- sub("^value:", "", ret)
  #ret <- sub("^key_number: ", "", ret)
  ret
}

#' @export
vec_ptype_abbr.midi_params  <- function(x) {
  "params"
}
