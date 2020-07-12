# adapted from code by David Solito
# https://www.davidsolito.com/post/midiplayer-avec-r-part-2/

midi$set("public", "play", function(
  left_track = NULL,
  right_track = left_track) {

  nms <- self$names
  if(is.null(left_track) || is.null(right_track))
    stop("left_track and right_track (defaulting to left_track) must be valid names or indices")

  if(is.character(left_track) && ! left_track %in% nms) {
    stop("'", left_track , "' is not a valid track name")
  }
  if(is.character(right_track) && ! right_track %in% nms) {
    stop("'", right_track , "' is not a valid track name")
  }

  notes <-get_notes(self) %>%
    mutate(note_length = note_off - note_on)
  t_last_note <- max(notes$note_on)
  track_length <- t_last_note +
    max(notes$note_length[notes$note_on == t_last_note])


  if(is.numeric(left_track))  left_track  <- nms[left_track]
  if(is.numeric(right_track)) right_track <- nms[right_track]

  tracks_wav <- notes %>%
    distinct(key_number, track_name) %>%
    filter(track_name %in% c(left_track, right_track)) %>%
    # generate wave notes for each track note pair
    mutate(morse = map2(key_number , track_name ,
                        ~ build_morse(notes, .x, .y, ..3),
                        track_length))

  min_synth_length <- min(lengths(tracks_wav$morse))

  # trim tracks
  tracks_wav <- tracks_wav %>%
    transmute(track_name, morse = map(morse, head, min_synth_length))

  left_channel <- tracks_wav %>%
    filter(track_name == left_track) %>%
    pull(morse) %>%
    Reduce(`+`, .)

  if(right_track == left_track) {
    right_channel <- left_channel
  } else {
    right_channel <- tracks_wav %>%
      filter(track_name == right_track) %>%
      pull(morse) %>%
      Reduce(`+`, .)
  }

  wav <-
    tuneR::stereo(right_channel, left_channel) %>%
    tuneR::normalize(unit = "16") %>%
    tuneR::play()
})

build_morse <- function(notes, kn, tn, track_length) {
  # to avoid notes :
  key_number <- track_name <- note_off <- note_on <- silence_off <-
    silence_on <- silence_length <- note_length <- freq <- synth <-
    . <- NULL

  sf <- 4000
  signal <- "sine"
  shape <- "sine"
  am <- c(0, 0, 0)
  fm <- c(0, 0, 0, 0, 0)
  harmonics <- 1

  notes <- filter(notes, key_number == kn, track_name == tn)

  # extract silence
  silence <- notes %>%
    mutate(
      silence_on = note_off,
      silence_off = lead(note_on),
      silence_off = replace_na(silence_off, track_length),
      silence_length = silence_off - silence_on) %>%
    filter(silence_length != 0)  %>%
    transmute(note_on = silence_on, note_length = silence_length) %>%
    mutate(synth = map(note_length, ~ seewave::addsilw(
      0, f = sf, d = ., at = "end", output = "Wave")[-1, ]))

  # extract note
  sound <- notes %>%
    left_join(pitch_table, by = "key_number") %>%
    select(note_on, note_length, freq) %>%
    mutate(synth = map2(freq, note_length, ~ seewave::synth(
      f = sf, d = .y, cf = .x, signal = signal, shape = shape, am = am, fm = fm,
      harmonics = harmonics, output = "Wave")))

  first_silence <- tibble(
    note_on = 0 ,
    note_length = first(sound$note_on),
    synth = list(seewave::addsilw(
      0, f = sf,d = note_length, output = "Wave", at = "end")[-1, ])
  )

  # merge note + silence
  note_and_silence <-
    bind_rows(sound, silence, first_silence) %>%
    arrange(note_on) %>%
    pull(synth) %>%
    Reduce(tuneR::bind, .)

  return(note_and_silence)
}

