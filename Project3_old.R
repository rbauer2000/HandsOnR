get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

score = function(symbols) {
  prize <- 0
  
  all_bars <- FALSE
  bars <- c("B", "BB", "BBB", "DD")
  if (sum(symbols %in% bars) == 3) {
    all_bars <- TRUE
  }
  
  if ((sum(symbols %in% c("B", "DD")) == 3) | (sum(symbols %in% c("C", "DD")) == 3)) {
    prize <- 10
  } else if (sum(symbols %in% c("BB", "DD")) == 3) {
    prize <- 25
  } else if (sum(symbols %in% c("BBB", "DD")) == 3) {
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
  }
  
  mult <- sum(symbols %in% "DD")
  prize <- prize * 2 ^ mult
    
  prize
}

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}

slot_display <-function(prize) {
  # extract symbols
  symbols <- attr(prize, "symbols")
  
  # collapse symbols in a stigle string
  symbols <- paste(symbols, collapse = " ")
 
  # combine synbols with prize as regular expression
  string <- paste(symbols, prize, sep = "\n$")
 
  # display regular expression in console without quotes
  cat(string)
  
}

print.slots <- function(x, ...) {
  slot_display(x)
}

wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
combos <- expand.grid(wheel, wheel, wheel)
prob = c("DD" = 0.03, "7" = 0.03, "BBB"= 0.06, "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3

for (i in 1:nrow(combos)) {
  symbols <- c(combos$Var1[i], combos$Var2[i], combos$Var3[i])
  combos$prize[i] <- score(symbols)
}

sum(combos$prize * combos$prob)
