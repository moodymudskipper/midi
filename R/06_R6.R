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
  # vapply(self$tracks, function(x) {
  #   params <- x[["params"]][x$event == "Sequence/Track Name"]
  #   if(length(params)) params[[1]][["value"]] else ""
  # }, character(1), USE.NAMES = FALSE)
  names(self$tracks)
})

midi$set("public", "encode", function(to){
  file.create(to)
  con <- file(description = to, open = "wb")
  on.exit(close(con))
  encode_header(self$header, con)
  encode_tracks(self$tracks, con)
})


midi$set("public", "plot", function(){

  # tempo is in in microseconds per MIDI quarter-note
  # header has n_ticks_per_quarter_note (most of the time! There's another system, not supported)
  # deltatime is in ticks
  # so deltatime / n_ticks_per_quarter_note * tempo will give the time

  tempo <- subset(self$tracks[[1]], event == "Set Tempo")$params
  if(!length(tempo)) stop("plotting is not yet supported for this midi format, ",
  "please complain at http://www.github.com/moodymudskipper/midi")
  tempo <- tempo[[1]]$value

  notes <-
    self$tracks %>%
    setNames(ifelse(names(.) == "", seq_along(.), names(.))) %>%
    purrr::map(mutate, time = cumsum(deltatime) /
                 self$header$n_ticks_per_quarter_note *
                 tempo / 1e6) %>%
    bind_rows( .id = "track_name") %>%
    tibble::as_tibble() %>%
    filter(event %in% c("Note On", "Note Off")) %>%
    tidyr::hoist(params, "channel", "key_number") %>%
    select(track_name, time, event, channel, key_number) %>%
    arrange(track_name, channel, key_number, time) %>%
    mutate(note_id = ifelse(
      event == "Note On",
      cumsum(event == "Note On"),
      cumsum(event == "Note Off")),
      event = tolower(gsub(" ", "_", event)),
      "track name channel" = paste0(track_name, " (ch. ", channel, ")"),
    ) %>%
    tidyr::pivot_wider(names_from = event, values_from = time)

  notes %>%
    ggplot(aes(x = note_on, xend = note_off, y = key_number, yend = key_number,
               col = `track name channel`)) +
    geom_segment(size = 3) +
    scale_y_continuous(minor_breaks = 0:127, breaks = seq(0, 127, 12), labels = paste0("C", seq(-1,9))) +
    theme_classic() +
    theme(panel.grid.minor.y = element_line(size=.1),
          panel.grid.major.y = element_line(size=.1, colour = "grey"),
          legend.title=element_blank()) +
    labs(x= "time", y="", title = self$track_names()[[1]]) +
    scale_x_duration()
})







#' #' @method print midi_track_list
#' #' @export
#' print.midi_track_list <- function(x, n= NULL){
#'   opt <- options(tibble.print_max = n)
#'   on.exit(options(opt))
#'   NextMethod()
#' }

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
