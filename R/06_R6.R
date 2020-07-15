#' midi class constructor
#'
#' @export
midi <- R6::R6Class("midi", list(
  header = NULL,
  tracks = NULL #,
  # initialize = function(
  #   header = list(format = 1, n_tracks = 0, n_ticks_per_quarter_note = 960),
  #   tracks = list()){
  #   # we might want to be able define an empty midi with default headers etc
  #   self$header <- header
  #   self$tracks <- tracks
  # }
))


midi$set("public", "initialize", function(file){
  con <- file(description = file, open = "rb")
  on.exit(close(con))

  header <- parse_header(con)
  tracks <- parse_tracks(con, header$n_tracks)
  tracks <- lapply(tracks, as_midi_track)
  names(tracks) <- vapply(tracks, function(x) {
    params <- x[["params"]][x$event == "Sequence/Track Name"]
    if(length(params)) params[[1]][["value"]] else ""
  }, character(1), USE.NAMES = FALSE)

  self$header <- header
  self$tracks <- tracks
})


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

# $names
midi$set("active", "names", function() {
  # vapply(self$tracks, function(x) {
  #   params <- x[["params"]][x$event == "Sequence/Track Name"]
  #   if(length(params)) params[[1]][["value"]] else ""
  # }, character(1), USE.NAMES = FALSE)
  names(self$tracks)
})

# $encode()
midi$set("public", "encode", function(to){
  file.create(to)
  con <- file(description = to, open = "wb")
  on.exit(close(con))
  encode_header(self$header, con)
  encode_tracks(self$tracks, con)
})


# $cut()
midi$set("public", "cut", function(start = 0, end = Inf, unit = c("sec", "tick")) {
  mid <- self$clone(deep=TRUE)

  unit <- match.arg(unit)
  if(unit == "sec") {
  tempo <- subset(mid$tracks[[1]], event == "Set Tempo")$params
  if(!length(tempo)) stop("plotting is not yet supported for this midi format, ",
                          "please complain at http://www.github.com/moodymudskipper/midi")
  tempo <- tempo[[1]]$value
  conv_factor <- tempo / mid$header$n_ticks_per_quarter_note/ 1e6
  } else {
    conv_factor <-1
  }

  mid[["tracks"]][-1] <- lapply(mid[["tracks"]][-1], function(x) {
    x %>%
      # add time as cumsum of deltatime, converted to sec if relevant
      mutate(time = cumsum(deltatime) * conv_factor) %>%
      # add rowid to backup order
      rowid_to_column() %>%
      # convert back to tibble so it prints conveniently for debugging
      tibble::as_tibble() %>%
      # extract channel so we can sort and not have intertwined note_on note_off pairs
      tidyr::hoist(params, "channel", "key_number", .remove = FALSE) %>%
      arrange(channel, key_number, time) %>%
      # compute note_id for note events
      mutate(
        note_id = ifelse(
          event == "Note On",
          cumsum(event == "Note On"),
          ifelse(event == "Note Off", cumsum(event == "Note Off"), NA))) %>%
      # keep non note event and event happening after start
      filter(!event %in% c("Note On", "Note Off") | time >= start & time <= end) %>%
      # count remaining note_id pairs and remove unfinished sounds
      add_count(note_id) %>%
      filter(is.na(note_id) | n == 2) %>%
      # return to standard format
      arrange(rowid) %>%
      select(-rowid, -channel, -note_id) %>%
      as_midi_track})
  mid
})


# $plot()
midi$set("public", "plot", function(){

  notes <- get_notes(self)

  notes %>%
    ggplot(aes(x = note_on, xend = note_off, y = key_number, yend = key_number,
               col = `track name channel`)) +
    geom_segment(size = 3) +
    scale_y_continuous(minor_breaks = 0:127, breaks = seq(0, 127, 12), labels = paste0("C", seq(-1,9))) +
    theme_classic() +
    theme(panel.grid.minor.y = element_line(size=.1),
          panel.grid.major.y = element_line(size=.1, colour = "grey"),
          legend.title=element_blank()) +
    labs(x= "time", y="", title = self$names[[1]]) +
    scale_x_duration()
})




# $select()
midi$set("public", "select", function(...){
  mid <- self$clone(deep=TRUE)
  pos <- tidyselect::eval_select(rlang::expr(c(...)), mid$tracks, include=1)
  mid$tracks <- setNames(mid$tracks[pos], names(pos))
  # we must rename in the code too!
  mid$tracks[-1] <- Map(mid$tracks[-1], names(mid$tracks[-1]), f = function(track, nm){
    pos <- which(track$event == "Sequence/Track Name")
    if(length(pos)) {
      track$params[[pos]] <- list(value = nm, length = nchar(nm))
    } else if (nm != ""){
      track <- dplyr::add_row(
        track,
        deltatime = 0,
        event_type = "meta",
        event = "Sequence/Track Name",
        type = "03",
        params = list(list(value = nm, length = nchar(nm))),
        EventChannel = as.raw(0xff),
        .before = 1)
    }
    track
  })
  mid
})

# $rename()
midi$set("public", "rename", function(...){
  mid <- self$clone(deep=TRUE)
  pos <- tidyselect::eval_rename(rlang::expr(c(...)), mid$tracks)
  names(mid$tracks)[pos] <- names(pos)
  # we must rename in the code too!
  mid$tracks[-1] <- Map(mid$tracks[-1], names(mid$tracks[-1]), f = function(track, nm){
    pos <- which(track$event == "Sequence/Track Name")
    if(length(pos)) {
      track$params[[pos]] <- list(value = nm, length = nchar(nm))
    } else if (nm != ""){
      track <- dplyr::add_row(
        track,
        deltatime = 0,
        event_type = "meta",
        event = "Sequence/Track Name",
        type = "03",
        params = list(list(value = nm, length = nchar(nm))),
        EventChannel = as.raw(0xff),
        .before = 1)
    }
    track
  })
  mid
})


# $tempo
midi$set("active", "tempo", function(){
  pos <- which(mid$tracks[[1]]$event == "Set Tempo")
  tempo <- mid$tracks[[1]]$params[[pos]]$value
  tibble(bpm = 60*1e6/tempo, micro_seconds_per_quarter_note = as.integer(tempo))
})

# $set_tempo()
midi$set("public", "set_tempo", function(tempo, metric = c(
  "bpm", "relative", "micro_seconds_per_quarter_note")){
  metric <- match.arg(metric)
  mid <- self$clone(deep=TRUE)
  pos <- which(mid$tracks[[1]]$event == "Set Tempo")
  old_tempo <- mid$tracks[[1]]$params[[pos]]$value
  new_tempo <- switch(metric,
                      bpm = 60*1e6/tempo,
                      relative = old_tempo / tempo,
                      micro_seconds_per_quarter_note = tempo)
  new_tempo <- round(new_tempo)
  mid$tracks[[1]]$params[[pos]]$value <- new_tempo
  mid
})


# $shift_hstep()
midi$set("public", "shift_hstep", function(n, at = -1){
  mid <- self$clone(deep=TRUE)
  # if(is.character(to)) {
  #   if(!to %in% c("c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"))
  #     stop("`to` should be a valid note")
  #   if(!missing(from)){
  #     if(!is.character(from) || ! from %in% c("c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"))
  #       stop("`from` should be a valid note")
  #   }
  #   stop("not supported yet! please use a numeric value for `to`")
  # }
  pos <- tidyselect::eval_select(rlang::enquo(at), mid$tracks, include=1)
  mid$tracks[pos] <- lapply(mid$tracks[pos], function(track){
    track$params <- map(track$params, ~ {
      if(!is.null(.$key_number))
        .$key_number <- .$key_number + n
      .
    })
    track
  })
  mid
})

midi$set("active", "key", function(){
  pos <- which(mid$tracks[[1]]$event == "Key Signature")
  key <- map_chr(pos, ~mid$tracks[[1]]$params[[.]]$value)
  tibble(time = cumsum(mid$tracks[[1]]$deltatime)[pos],
         key = key)
})



# $shift_degree()
midi$set("public", "shift_degree", function(n, scale, at = -1, stay_home = FALSE){
  mid <- self$clone(deep=TRUE)
  scale <- as.character(scale)
  scale <- gsub("[,']", "", scale)
  root <- scale[[1]]
  chromatic <- gsub("[,']", "", as.character(scale_chromatic(root)))

  shifted_scale <- c(scale[-seq(n)], scale[seq(n)])
  deltas <-
    match(shifted_scale, chromatic) -
    match(scale, chromatic)
  deltas <- deltas %% 12
  deltas <- setNames(deltas, scale)
  if(stay_home) deltas <- deltas - deltas[1]
  # wat do we do with borrowed notes though ? a lazy solution would be to leave them where they are

  pos <- tidyselect::eval_select(rlang::enquo(at), mid$tracks, exclude=1)
  mid$tracks[pos] <- lapply(mid$tracks[pos], function(track){
    track$params <- map(track$params, ~ {
      if(!is.null(.$key_number)) {
        note <- subset(key_numbers, key_number == .$key_number)$note
        .$key_number <- .$key_number + deltas[note]

      }
      .
    })
    track
  })
  mid
})


# $negative_harmony()
midi$set("public", "negative_harmony", function(pivot = 60, at = -1, stay_home = FALSE){
  mid <- self$clone(deep=TRUE)
  # chromatic <- as.character(scale_chromatic(pivot, ignore_octave = TRUE))
  # deltas <- -2*(0:11)
  # if(!stay_home) deltas <- deltas + 7
  # deltas <- setNames(deltas, chromatic)
  if(is.character(pivot)){
    #convert note (e.g. a4) into key_number
    # maybe "auto" (default ?) can take a center that preserves the span ?
  }

  pos <- tidyselect::eval_select(rlang::enquo(at), mid$tracks, exclude=1)
  mid$tracks[pos] <- lapply(mid$tracks[pos], function(track){
    track$params <- map(track$params, ~ {
      if(!is.null(.$key_number)) {
        #note <- subset(key_numbers, key_number == .$key_number)$note
        .$key_number <- -.$key_number + 2*pivot
        if(.$key_number < 0 || .$key_number > 127)
          stop("tried to use key_number ", .$key_number, ". It should fall between 0 and 127, try another pivot!")
      }
      .
    })
    track
  })
  mid
})


# # $reverse()
# midi$set("public", "reverse", function(at = -1){
#   # the current way of reversing is not good enough, because source data is not
#   # note on note off note on..., so when we revert the delta times they're not
#   # always associated to the right thing, pairs have to be identified, then spread again
#   # quite more complicated!
#   mid <- self$clone(deep=TRUE)
#   pos <- tidyselect::eval_select(rlang::enquo(at), mid$tracks, exclude=1)
#   mid$tracks[pos] <- lapply(mid$tracks[pos], function(track){
#
#     notes_on_lgl <- track$event == "Note On"
#     notes_off_lgl <- track$event == "Note Off"
#
#     track[] <- map(track, ~{
#       #browser()
#     .[notes_on_lgl]  <- rev(.[notes_on_lgl])
#     .[notes_off_lgl] <- rev(.[notes_off_lgl])
#     .
#     })
#     track
#   })
#   mid
# })
