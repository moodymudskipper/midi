
read_track_event <- function(con, lastEventChannel = NA){
  DTtemp <- read_var_length(con)
  DT <- DTtemp[1]
  n_DT_bytes <- DTtemp[2]

  EventChannel <- read_raw(con)
  # event is the first quartet of the event data (right after delta time)
  # it can describe a channel (>=8, i.e. >= 1000, i.e. first bit is 1), or not
  # if it doesn't, we consider the previous channel, and roll our cursor one byte back
  event <- substr(EventChannel, 1, 1)

  if(event < "8"){
    warning(
      "This file uses 'running status', we support it for reading in the file, ",
      "but we lose the related information (the midi will still sound the same, ",
      "the file will just be a bit bigger)")
    seek(con, where = -1, origin = "current")
    EventChannel <- lastEventChannel
    event <- substr(EventChannel, 1, 1)
  }

  if(EventChannel == "ff")
    read_meta_event(con, event, DT, EventChannel)
  else if(event == "f")
    read_system_message_event(con, event, DT, EventChannel)
  else
    read_channel_voice_event(con, event, DT, EventChannel)
}

write_track_event <- function(event, con, lastEventChannel = NA){
  # Delta time
  write_var_length(event$deltatime, con)

  # EventChannel
  write_raw(event$EventChannel, con)

  if(event$EventChannel == "ff")
    write_meta_event(event, con)
  else if(substr(event$EventChannel,1,1) == "f")
    write_system_message_event(event, con)
  else
    write_channel_voice_event(event, con)
}
