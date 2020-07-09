
read_channel_voice_event <- function(con, nibble1, DT, byte1) {

  # shifting removes the event bits to leave only the channel bits
  # then we balance the multiplication that the shifting produced
  channel <- as.numeric(rawShift(byte1, 4)) / 2^4

  # filter lookup table on relevant nibble
  cvm <- channel_voices_messages
  cvm <- cvm[cvm$nibble1 == nibble1,]
  event <- cvm$event[1]

  #feed the relevant named params
  params <- list(channel = channel)
  for (byte_descr in cvm$byte_descr){
    params[[byte_descr]] <- read_integer(con)
  }

  # For control change we edit the event
  if(nibble1 == "b"){
    #browser()
    descr <- controller_messages$descr[
      controller_messages$byte2_dec == params$controller_number]
    event <- paste0(event, ": ", descr)
  }

  # convert pitch bend from 2 to to 1 param
  if(nibble1 == "e") # pitch bend
    params <- list(channel = channel, pitch_wheel =
                     params$most_significant_7_bits*128 +
                     params$least_significant_7_bits)

  list(deltatime=DT, event_type = "channel_voice", event = event, params = params, EventChannel = byte1)
}

write_channel_voice_event <- function(event, con) {
  if(startsWith(event$event, "Control Change")) {
    event$event <- "Control Change"
  }
  cvm <- channel_voices_messages
  cvm <- cvm[cvm$event == event$event,]

  # convert back pitch bend from 1 to to 2 params
  if(event$event == "Pitch Bend") {
    event$params[[1]] <- list(
      event$params[[1]][[1]],
      event$params[[1]]$pitch_wheel %% 128,
      event$params[[1]]$pitch_wheel %/% 128)
  }
  for (i in seq(nrow(cvm))){
    write_integer(event$params[[1]][[1+i]], con) # +1 because channel is the first param
  }

}
