

getWeeks <- function(data) {

  # get a vector of weeks found in the tibble of bugs

  weeks.vector <- data %>%
    select(week) %>%
    unique() %>%
    .$week

  return(weeks.vector)

}

dissectWeeks <- function(w) {

  # get a vector of weeks by chopping the list of week labels
  #
  # input is a list of character strings ('week23'...) or integers ('23' ...)
  # output is a list of character strings ('23' ...) that will be used
  # as the x-axis of a graph

  weeks.vector <- list()

  for (i in 1:length(w)) {

    if (class(w[[i]]) == "character") {
      # input format is 'weekXX' (a list of character strings)
      weeks.vector[i] <- substring(w[[i]], 5, 6)
    }
    else {
      # input format is a two digit week (a list of integers)
      # yes, one bracket
      weeks.vector[i] <- as.character(w[i])
    }

  }

  return(weeks.vector)

}
