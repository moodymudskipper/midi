

read_meta_event <- function(con, event, DT, EventChannel) {
  # In a MIDI file this is used as an escape to introduce <meta events>.
  type <- as.character(read_raw(con))

  # FIXME: We need a special case for each possible meta event. A data.frame is not the best possible solution...,
  # probably define separate S4 classes?
  elength <- read_var_length(con)
  eventName <- switch(
    type,
    "00" = "Sequence Number",
    "01" = "Text Event",
    "02" = "Copyright Notice",
    "03" = "Sequence/Track Name",
    "04" = "Instrument Name",
    "05" = "Lyric",
    "06" = "Marker",
    "07" = "Cue Point",
    "08" = "Program Name",
    "09" = "Device Name",
    "20" = "MIDI Channel Prefix",
    "21" = "MIDI Port",
    "2f" = "End of Track",
    "51" = "Set Tempo",
    "54" = "SMPTE Offset",
    "58" = "Time Signature",
    "59" = "Key Signature",
    "7f" = "Sequencer Specific",
    "Meta")

  params <- switch(
    eventName,
    "Text Event" =,
    "Copyright Notice" =,
    "Sequence/Track Name" =,
    "Instrument Name" =,
    "Lyric" =,
    "Marker" =,
    "Cue Point" =,
    "Program Name" =,
    "Device Name" = list(value = readChar(con, elength[1])),
    "Sequence Number" =,
    "MIDI Channel Prefix" = ,
    "MIDI Port" = list(value = read_integer(con, size = elength[1], signed = FALSE)),
    "Set Tempo" = list(value = as.vector(read_integer(con, n = 3, signed = FALSE) %*% c(256^2, 256, 1))),
    "Time Signature" = {
      temp <- read_integer(con, n = 4, signed = FALSE)
      list(time_signature = paste0(temp[1], "/", 2^temp[2]),
           clocks_per_tick = temp[3],
           n_32nd_notes_per_24_midi_clocks = temp[4])
    },
    "Key Signature" = {
      sharpflat <- read_integer(con)
      majorminor <- read_integer(con)
      ma <- paste(c("B (H)", "Gb", "Db", "Ab", "Eb", "Bb", "F", "C", "G", "D", "A", "E", "B (H)", "F#", "C#"), "major")
      mi <- paste(c("Ab","Eb", "Bb", "F",  "C",  "G",  "D", "A", "E", "B (H)", "F#","C#","G#","D#", "A#"), "minor")
      if(majorminor == 1) list(value =mi[sharpflat+8]) else list(value = ma[sharpflat+8])
    },
    "End of Track" =,
    "SMPTE Offset" =,
    "Sequencer Specific" =,
    "Meta" = list(value = read_raw(con, n = elength[1]))
  )

  params$length <- elength[[1]]

  list(deltatime=DT, event_type = "meta", event=eventName, type=type, params = params, EventChannel=EventChannel)
}

write_meta_event <- function(event, con) {
  # type
  raw_type <- as.raw(as.hexmode(event$type))
  write_raw(raw_type, con)

  # length
  write_var_length(event$params[[1]]$length, con)

  switch(
    event$event,
    "Text Event" =,
    "Copyright Notice" =,
    "Sequence/Track Name" =,
    "Instrument Name" =,
    "Lyric" =,
    "Marker" =,
    "Cue Point" =,
    "Program Name" =,
    "Device Name" = writeChar(event$params[[1]]$value, con, nchars = event$params[[1]]$length, eos = NULL),
    "Sequence Number" =,
    "MIDI Channel Prefix" = ,
    "MIDI Port" = write_integer(event$params[[1]]$value, con, size = elength[1]),
    "Set Tempo" = {
      val <- event$params[[1]]$value
      tempo <- integer(3)
      tempo[3] <- val %% 256
      val <- (val-tempo[3]) / 256
      tempo[2] <- val %% 256
      val <- (val-tempo[2]) / 256
      tempo[1] <- val
      write_integer(tempo, con)
    },
    "Time Signature" = {
      ts <- strsplit(event$params[[1]]$time_signature, "/")[[1]]
      write_integer(ts[[1]], con)
      write_integer(log(as.numeric(ts[[2]]), 2), con)
      write_integer(event$params[[1]]$clocks_per_tick, con)
      write_integer(event$params[[1]]$n_32nd_notes_per_24_midi_clocks, con)
    },
    "Key Signature" = {
      val <- event$params[[1]]$value
      if(endsWith(val, "major")) {
        ma <- paste(c("B (H)", "Gb", "Db", "Ab", "Eb", "Bb", "F", "C", "G", "D", "A", "E", "B (H)", "F#", "C#"), "major")
        write_integer(match(val, ma) - 8, con)
        write_integer(0, con)

      } else {
        mi <- paste(c("Ab","Eb", "Bb", "F",  "C",  "G",  "D", "A", "E", "B (H)", "F#","C#","G#","D#", "A#"), "minor")
        write_integer(match(val, mi) - 8, con)
        write_integer(1, con)
      }
    },
    "Sequencer Specific" =,
    "Meta" = write_raw(event$params[[1]]$value, con)
  )
}
