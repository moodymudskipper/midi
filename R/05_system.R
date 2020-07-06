read_system_message_event <- function(con, event, DT, EventChannel) {
  eventName <- "System"
  elength <- read_var_length(con)
  # on.exit(undebug(as.raw2_))
  # debug(as.raw2_)
  value <- read_integer(con, n = elength[[1]])
  # tuneR was just ignoring those!
  # seek(con, where = elength[1], origin = "current")
  return(list(deltatime = DT,  event_type = "system_message", event=eventName,
              EventChannel = EventChannel, params = list(value = value, length = elength[[1]])))
}

write_system_message_event <- function(event, con) {
  #browser()
  write_var_length(event$params[[1]]$length, con)
  write_integer(event$params[[1]]$value, con)
}
