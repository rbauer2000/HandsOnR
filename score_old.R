score = function(symbols) {
  prize <- 0
  
  all_bars <- FALSE
  bars <- c("B", "BB", "BBB", "DD")
  if (sum(symbols %in% bars) == 3) {
    all_bars <- TRUE
  }
}

if (sum(symbols %in% c("B", "DD"))  == 3  &  sum(symbols %in% c("B")) >= 1) {
  prize <- 10
} else if (sum(symbols %in% c("C", "DD")) == 3 & sum(symbols %in% c("C")) >= 1) {
  prize <- 10
} else if (sum(symbols %in% c("BB", "DD")) == 3 & sum(symbols %in% c("BB")) >= 1) {
  prize <- 25
} else if (sum(symbols %in% c("BBB", "DD")) == 3 & sum(symbols %in% c("BBB")) >= 1) {
  prize <- 40
} else if (sum(symbols %in% c("7", "DD")) == 3) {
  prize <- 80
} else if (sum(symbols %in% "DD") == 3) {
  prize <- 100
} else if (all_bars) {
  prize <- 5
} else if (any(symbols == "C")) {
  num_of_Cs <- sum(symbols %in% "C")
  if (num_of_Cs == 1) {
    prize <- 2
  } else if (num_of_Cs ==2) {
    prize <- 5
  }
  
  mult <- sum(symbols %in% "DD")
  prize <- prize * 2 ^ mult
  
  prize
}