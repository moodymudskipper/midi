
read_channel_voice_event <- function(con, event, DT, EventChannel) {

  # shifting removes the event bits to leave only the channel bits
  # then we balance the multiplication that the shifting produced
  channel <- as.numeric(rawShift(EventChannel, 4)) / 2^4
  p1 <- read_integer(con)
  p2 <- if(event %in% c("c", "d")) NA else read_integer(con)

  out <- list(deltatime=DT, event_type = "channel_voice")

  # This is this table, transcribed in hexa quartet :
  # http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html#BMA1_
  out$event <- switch(event,
                      "8" = "Note Off",
                      "9" = "Note On",
                      "a" = "Note Aftertouch",
                      "b" = "Control Change",
                      "c" = "Program Change",
                      "d" = "Channel Aftertouch",
                      "e" = "Pitch Bend",
                      "f" = "Meta or System")
  out$params <- switch(event,
                       "8" =,
                       "9" = list(channel = channel, key_number=p1, velocity=p2),
                       "a" = list(channel = channel, key_number=p1, pressure=p2),
                       "b" = list(channel = channel, controller_number=p1, value=p2),
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
           write_integer(event$params[[1]]$key_number, con)
           write_integer(event$params[[1]]$velocity, con)
         },
         "Note Aftertouch" = {
           write_integer(event$params[[1]]$key_number, con)
           write_integer(event$params[[1]]$pressure, con)
         },
         "Control Change" = {
           write_integer(event$params[[1]]$controller, con)
           write_integer(event$params[[1]]$value, con)
         },
         "Program Change" =
           write_integer(event$params[[1]]$program, con),
         "Channel Aftertouch" =
           write_integer(event$params[[1]]$pressure, con),
         "Pitch Bend" ={
           write_integer(event$params[[1]]$pitch_wheel %% 128, con)
           write_integer(event$params[[1]]$pitch_wheel %/% 128, con)
         })
}
