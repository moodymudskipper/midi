# references for midi format
# http://www.personal.kent.edu/~sbirch/Music_Production/MP-II/MIDI/midi_file_format.htm
# http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html
# https://www.jchr.be/linux/midi-format.htm
# https://github.com/colxi/midi-parser-js/wiki/MIDI-File-Format-Specifications

# about endian arg and MSB
# https://en.wikipedia.org/wiki/Endianness

# build test files :
# https://onlinesequencer.net/

#' Load a midi file into a midi object
#'
#' @param file path to midi file
#'
#' @export
parse_midi <- function(file){
  con <- file(description = file, open = "rb")
  on.exit(close(con))

  message("parsing header")
  header <- parse_header(con)
  message("parsing tracks")
  tracks <- parse_tracks(con, header$n_tracks)
  tracks <- lapply(tracks, function(x) {
    class(x) <- c("midi_track", class(x))
    x
    })
  names(tracks) <- vapply(tracks, function(x) {
    params <- x[["params"]][x$event == "Sequence/Track Name"]
    if(length(params)) params[[1]][["value"]] else ""
  }, character(1), USE.NAMES = FALSE)
  #class(tracks) <- c("midi_track_list", class(tracks))

  midi$new(header, tracks)
}

parse_header <- function(con){
  # As comments We copy and paste or paraphrase pieces of doc from :

  # MSB first means endian = "big" in readBin(), see

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

  tibble::tibble(format = MThd_format,
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

  tracks <- list()
  for(i_track in seq(n_tracks)){
    MTrk <- readChar(con, 4)
    if(MTrk != "MTrk")
      stop("No Track Chunk in this Midi")
    MTrk_length <- read_integer(con, size = 4)
    track <- list()
    i_event <- 0
    repeat {
      i_event <- i_event + 1
      last_EventChannel <- if(i_event>1) track[[i_event-1]][["EventChannel"]] else NA
      track[[i_event]] <- read_track_event(con, last_EventChannel)
      last_type <-  track[[i_event]][["type"]]
      if(!is.null(last_type) &&  last_type == "2f")
        break
    }
    track <- purrr::map_dfr(track, ~{.$params <- list(.$params);tibble::as_tibble(.)})
    attr(track, "length") <- MTrk_length
    tracks[[i_track]] <- track
  }
  tracks
}

encode_tracks <- function(tracks, con){
  for(track in tracks){

    # Chunk Type
    writeChar("MTrk", con, nchars = 4, eos = NULL)
    # length
    track_length <- attr(track, "length")
    write_integer(track_length, con, size = 4)
    # data
    for (i in seq(nrow(track))){
      #<last_event <- if(i>1)  track[[i-1]][["EventChannel"]] else NA
      write_track_event(track[i,], con) #, last_event)
    }
  }
}


