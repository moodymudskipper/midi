# for debugging purposes, from integer get a raw vector
as.raw2_ <- function(x, n = NA, signed = TRUE){
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

as.raw2 <- function(x, n = NA, signed){
  lapply(x, as.raw2_, n, signed)
}

read_integer <- function(con, n = 1, size = 1, signed = TRUE) {
  res <- readBin(con, integer(0), n = n, size = size, signed = signed, endian = "big")
  #lapply(as.raw2(res, size, signed), print)
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

# readChar <- function (con, nchars, useBytes = FALSE) {
#   res <- base::readChar(con, nchars, useBytes)
#   print(charToRaw(res))
#   res
# }



write_raw <- function(object, con) {
  writeBin(as.raw(object), con)
}

writeVarLength <- function(n, con){
  b <- numeric(4)
  b[1] <- n
  if(b[1] > 127){
    b[2] <- b[1] %/% 128
    b[1] <- b[1] %% 128 + 128
    write_integer(b[1], con)
    if(b[2] > 127){
      b[3] <- b[2] %/% 128
      b[2] <- b[2] %% 128 + 128
      write_integer(b[2], con)
      if(b[3] > 127){
        b[4] <- b[3] %/% 128
        b[3] <- b[3] %% 128 + 128
        write_integer(b[3], con)
        write_integer(b[4], con)
      } else {
        write_integer(b[3], con)
      }
    } else {
      write_integer(b[2], con)
    }
  } else {
    write_integer(b[1], con)
  }
}

readVarLength <- function(con){
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

read_channel_voice_event <- function(con, event, DT, EventChannel) {

  # shifting removes the event bits to leave only the channel bits
  # then we balance the multiplication that the shifting produced
  channel <- as.numeric(rawShift(EventChannel, 4)) / 2^4
  p1 <- read_integer(con)
  p2 <- if(event %in% c("c", "d")) NA else read_integer(con)

  out <- list(deltatime=DT, message_type = "channel_voice")

  # This is this table, transcribed in hexa quartet :
  # http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html#BMA1_
  out$event <- switch(event,
                      "8" = "Note Off",
                      "9" = "Note On",
                      "a" = "Note Aftertouch",
                      "b" = "Controller",
                      "c" = "Program Change",
                      "d" = "Channel Aftertouch",
                      "e" = "Pitch Bend",
                      "f" = "Meta or System")
  out$params <- switch(event,
                       "8" =,
                       "9" = list(channel = channel, key_number=p1, velocity=p2),
                       "a" = list(channel = channel, key_number=p1, pressure=p2),
                       "b" = list(channel = channel, controller=p1, value=p2),
                       "c" = list(channel = channel, program=p1),
                       "d" = list(channel = channel, pressure=p1),
                       "e" = list(channel = channel, pitch_wheel=p2*128+p1))
  # we could also include note here, by looking at table, but we choose to limit redundancies here
  out$EventChannel <- EventChannel
  out
}

write_channel_voice_event <- function(event, con) {
  switch(event$event,
         "Note Off" =,
         "Note On" = {
           write_integer(event$params$key_number, con)
           write_integer(event$params$velocity, con)
         },
         "Note Aftertouch" = {
           write_integer(event$params$key_number, con)
           write_integer(event$params$pressure, con)
         },
         "Controller" = {
           write_integer(event$params$controller, con)
           write_integer(event$params$value, con)
         },
         "Program Change" =
           write_integer(event$params$program, con),
         "Channel Aftertouch" =
           write_integer(event$params$pressure, con),
         "Pitch Bend" ={
           write_integer(event$params$pitch_wheel %% 128, con)
           write_integer(event$params$pitch_wheel %/% 128, con)
         })
}

read_meta_event <- function(con, event, DT, EventChannel) {
  # In a MIDI file this is used as an escape to introduce <meta events>.
  type <- as.character(read_raw(con))

  # FIXME: We need a special case for each possible meta event. A data.frame is not the best possible solution...,
  # probably define separate S4 classes?
  elength <- readVarLength(con)
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

  list(deltatime=DT, message_type = "meta_event", event=eventName, type=type, params = params, EventChannel=EventChannel)
}

write_meta_event <- function(event, con) {
  # type
  raw_type <- as.raw(as.hexmode(event$type))
  write_raw(raw_type, con)

  # length
  writeVarLength(event$params$length, con)

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
    "Device Name" = writeChar(event$params$value, con, nchars = event$params$length, eos = NULL),
    "Sequence Number" =,
    "MIDI Channel Prefix" = ,
    "MIDI Port" = write_integer(event$params$value, con, size = elength[1]),
    "Set Tempo" = {
      val <- event$params$value
      tempo <- integer(3)
      tempo[3] <- val %% 256
      val <- (val-tempo[3]) / 256
      tempo[2] <- val %% 256
      val <- (val-tempo[2]) / 256
      tempo[1] <- val
      write_integer(tempo, con)
      },
    "Time Signature" = {
      ts <- strsplit(event$params$time_signature, "/")[[1]]
      write_integer(ts[[1]], con)
      write_integer(log(as.numeric(ts[[2]]), 2), con)
      write_integer(event$params$clocks_per_tick, con)
      write_integer(event$params$n_32nd_notes_per_24_midi_clocks, con)
    },
    "Key Signature" = {
      val <- event$params$value
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
    "Meta" = write_raw(event$params$value, con)
  )
  }

read_system_message_event <- function(con, event, DT, EventChannel) {
  eventName <- "System"
  elength <- readVarLength(con)
  # on.exit(undebug(as.raw2_))
  # debug(as.raw2_)
  value <- read_integer(con, n = elength[[1]])
  # tuneR was just ignoring those!
  # seek(con, where = elength[1], origin = "current")
  return(list(deltatime = DT,  message_type = "system_message", event=eventName,
              EventChannel = EventChannel, params = list(value = value, length = elength[[1]])))
}

write_system_message_event <- function(event, con) {
  #browser()
  writeVarLength(event$params$length, con)
  write_integer(event$params$value, con)
}


read_track_chunk_event <- function(con, lastEventChannel = NA){
  DTtemp <- readVarLength(con)
  DT <- DTtemp[1]

  n_DT_bytes <- DTtemp[2]

  EventChannel <- read_raw(con)
  # message("EventChannel: ", EventChannel)
  # event is the first quartet of the event data (right after delta time)
  # it can describe a channel (>=8, i.e. >= 1000, i.e. first bit is 1), or not
  # if it doesn't, we consider the previous channel, and roll our cursor one byte back
  event <- substr(EventChannel, 1, 1)
  #backseeked <- 0

  if(event < "8"){
    seek(con, where = -1, origin = "current")
    EventChannel <- lastEventChannel
    event <- substr(EventChannel, 1, 1)
    #backseeked <- 1
  }

  if(EventChannel == "ff")
    read_meta_event(con, event, DT, EventChannel)
  else if(event == "f")
    read_system_message_event(con, event, DT, EventChannel)
  else
    read_channel_voice_event(con, event, DT, EventChannel)
}

write_track_chunk_event <- function(event, con, lastEventChannel = NA){

  # Delta time
  writeVarLength(event$deltatime, con)

  # EventChannel
  # message("EventChannel: ", event$EventChannel)
  write_raw(event$EventChannel, con)

  # deal with redundancy
  # if(event < "8"){
  #   seek(con, where = -1, origin = "current")
  #   EventChannel <- lastEventChannel
  #   event <- substr(EventChannel, 1, 1)
  #   #backseeked <- 1
  # }

  if(event$EventChannel == "ff")
    write_meta_event(event, con)
  else if(substr(event$EventChannel,1,1) == "f")
    write_system_message_event(event, con)
  else
    write_channel_voice_event(event, con)
}

read_midi_chunk_type <- function(con) {
  # http://www.personal.kent.edu/~sbirch/Music_Production/MP-II/MIDI/midi_file_format.htm
  # A 4-byte chunk type (ASCII)
  readChar(con, 4)
}

parse_header <- function(con){
  # As comments We copy and paste or paraphrase pieces of doc from :
  # http://www.personal.kent.edu/~sbirch/Music_Production/MP-II/MIDI/midi_file_format.htm
  # MSB first means endian = "big" in readBin(), see https://en.wikipedia.org/wiki/Endianness

  # MIDI files are structured into chunks.
  # There are two types of chunks: Header Chunks, Track Chunks
  # A MIDI file consists of a single header chunk followed by one or more track chunks.

  # |-----------------------------------------------------------------|
  # |                         Header Chunk                            |
  # |-----------------------------------------------------------------|
  # | Chunk Type |      length     |             Data                 |
  # |------------|-----------------|----------------------------------|
  # |   4 bytes  |     4 bytes     |    <-- length (= 6 bytes) -->    |
  # |   (ASCII)  | (32-bit binary) |  16-bit  |  16-bit  |   16-bit   |
  # |------------|-----------------|----------|----------|------------|
  # |     MThd	 |     <length>	   | <format> |	<tracks> | <division> |
  # |------------|-----------------|----------|----------|------------|

  # Chunk type is always "MThd"
  MThd <- readChar(con, 4)
  if(MThd != "MThd")
    stop("No Header Chunk in this Midi (?)")

  # <length> : length in bytes of the chunk data part.
  # 32-bit binary number, MSB first.
  # Exactly 6 (bytes) for any MIDI file created under the MIDI 1.0 specification.

  MThd_length <- read_integer(con, size = 4)
  if(MThd_length != 6)
    stop("Unexpected Header Chunk size")

  # <format>: The MIDI file format.
  # 16-bit binary number, MSB first.
  # only 0, 1 or 2.
  # 0 : A single track-chunk contains all note and tempo information
  # 1 : 1+ track chunks, tracks are played simultaneously. 1st track called 'Tempo Map' contains meta events
  # 2 : 1+ track chunks, each an independent sequence (less encountered)

  MThd_format <- read_integer(con, size = 2, signed = FALSE)
  if(!(MThd_format %in% 0:2))
    stop("Unexpected Midi file format")

  # <tracks> :The number of track chunks contained in this MIDI file.
  # 16-bit binary number, MSB first.

  MThd_tracks <- read_integer(con, size = 2, signed = FALSE)

  # <division> : the default unit of delta-time.
  # 16-bit binary value, MSB first.
  # two possible formats, depending on MS bit, only first format supported so far

  # |--------------------------------------------------|
  # |    Bit:    | 15 | 14 ... 8       |    7 ... 0    |
  # |------------|----|----------------|---------------|
  # |            |  0 |     ticks per quarter note     |
  # | <division> |----|----------------|---------------|
  # |            |  1 | -frames/second | ticks / frame |
  # |------------|----|----------------|---------------|

  # ticks per quarter note : number of delta-time units in each a quarter-note.
  # ticks / frame : number of delta-time units per SMTPE frame
  # -frames/second : number of SMTPE frames per second
  # -24 = 24 fps
  # -25 = 25 fps
  # -29 = 30 fps, drop frame
  # -30 = 30 fps, non-drop frame

  MThd_division <- read_integer(con, size = 2)
  if(MThd_division < 0){
    stop("Midi representation of timing: Frames per second / ticks per frame not yet implemented, please ask the author")
  }

  list(format = MThd_format,
       n_tracks = MThd_tracks,
       n_ticks_per_quarter_note = MThd_division)
}

encode_header <- function(header, con){
  # Mthd
  writeChar("MThd", con, eos = NULL)
  # MThd_length
  write_integer(6, con, size = 4)
  # format
  write_integer(header$format, con, size = 2)
  # n_tracks
  write_integer(header$n_tracks, con, size = 2)
  # division
  write_integer(header$n_ticks_per_quarter_note, con, size = 2)
}

encode_tracks <- function(tracks, con){
  for(track in tracks){

    # Chunk Type
    writeChar("MTrk", con, nchars = 4, eos = NULL)
    # length
    track_length <- attr(track, "length")

    write_integer(track_length, con, size = 4)
    # data
    for (i in seq(length(track))){
      last_event <- if(i>1)  track[[i-1]][["EventChannel"]] else NA
      write_track_chunk_event(track[[i]], con) #, last_event)
    }
  }
}


parse_tracks <- function(con, n_tracks){
  # |-----------------------------------------------------------------|
  # |                         Track Chunk                             |
  # |-----------------------------------------------------------------|
  # | Chunk Type |      length     |             Data                 |
  # |------------|-----------------|----------------------------------|
  # |   4 bytes  |     4 bytes     |       <-- length bytes) -->      |
  # |   (ASCII)  | (32-bit binary) |           (binary data)          |
  # |------------|-----------------|--------------|---------|---------|
  # |     MTrk	 |     <length>	   | <delta_time> |	<event> |  <...>  |
  # |------------|-----------------|--------------|---------|---------|

  # <delta_time> : number of 'ticks' from previous event
  # it's a succession of bytes where the first bit is 1 until we reach the last byte
  # e.g. 10000010 then 01111100 , for 82 7c
  # this means we have 2^7 = 128 values per byte, see readVarLength()

  # <event> : one of: <midi_event>, <sysex_event>, <meta_event>

  allTracks <- list()
  for(track in seq(n_tracks)){
    MTrk <- readChar(con, 4)
    if(MTrk != "MTrk")
      stop("No Track Chunk in this Midi")
    MTrk_length <- read_integer(con, size = 4)
    MeventList <- list()
    bytes <- 0
    i <- 0
    repeat {
      #browser()
      i <- i+1
      last_event <- if(i>1)  MeventList[[i-1]][["EventChannel"]] else NA
      MeventList[[i]] <- read_track_chunk_event(con, last_event)
      #bytes <- bytes + MeventList[[i]][["bytes"]]
      last_type <-  MeventList[[i]][["type"]]
      if(!is.null(last_type) &&  last_type == "2f")
        break
    }

    allTracks[[track]] <- MeventList
    attr(allTracks[[track]], "length") <- MTrk_length
  }
  allTracks
}

#' @export
parse_midi <- function(file){
  con <- file(description = file, open = "rb")
  on.exit(close(con))

  message("parsing header")
  header <- parse_header(con)
  message("parsing tracks")
  tracks <- parse_tracks(con, header$n_tracks)

  list(header= header, tracks = tracks)
}


#' @export
encode_midi <- function(mid, file){
  file.create(file)
  con <- file(description = file, open = "wb")
  on.exit(close(con))
  encode_header(mid$header, con)
  encode_tracks(mid$tracks, con)
}
