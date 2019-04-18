NUMBER_OF_SIMULATIONS <- 100000
NUMBER_OF_PEOPLE <- 60

round_to_day <- function(random_day) {
  if (random_day <= 365) {
    random_day <- max(ceiling(random_day), 1)  # In case it's exactly 0 (probability 0 of happening though since it's continuous)
  } else {  # Leap day birthday
    random_day <- 366
  }
  random_day
}

birthdays_has_duplicate <- function(x) {
    current_birthdays <- runif(n=NUMBER_OF_PEOPLE, min=0, max=365.2425)  # In the Gregorian calendar years divisible by 100 are not leap years, unless they are divisible by 400.
                                                                 # So the average number of days per year works out to be 365.2425 over the long term
    current_birthdays <- sapply(current_birthdays, round_to_day)
    min(anyDuplicated(current_birthdays), 1)
}

birthday_vector <- sapply(1:NUMBER_OF_SIMULATIONS, birthdays_has_duplicate)
mean(birthday_vector)