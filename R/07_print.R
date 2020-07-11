#' Printing methods
#'
#' @method print midi_track
#' @param x midi_track object
#' @param ... additional parameters passed to the tibble print method
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

midi_params <- function(x) {
  vctrs::new_rcrd(list(params =x), class = "midi_params")
}
#' #' @method print midi_track_list
#' #' @export
#' print.midi_track_list <- function(x, n= NULL){
#'   opt <- options(tibble.print_max = n)
#'   on.exit(options(opt))
#'   NextMethod()
#' }

#' Methods for displaying midi parameters in midi object output
#' @rdname midi_params_methods
#' @param x object
#' @param ... additional parameters passed to other methods
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

#' @rdname midi_params_methods
#' @export
vec_ptype_abbr.midi_params  <- function(x) {
  "params"
}
