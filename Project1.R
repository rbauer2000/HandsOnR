library("ggplot2")

roll <- function() {
  die <- 1:6
  dice <- sample(die, 2, replace = TRUE)
  sum(dice)
}

roll2 <- function() {
  die <- 1:6
  dice <- sample(die, 2, replace = TRUE, prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}

roll3 <- function(bones = 6) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}

rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)

rolls2 <- replicate(10000, roll2())
qplot(rolls2, binwidth = 1)

rolls3 <- replicate(10000, roll3(bones = 20))
qplot(rolls3, binwidth = 1)

# calculate expected value of weighted dice.
die <- 1:6
rolls <- expand.grid(die, die)
rolls$value <- rolls$Var1 + rolls$Var2

prob <- c("1" = 1/8, "2" = 1/8, "3" = 1/8, "4" = 1/8, "5" = 1/8, "6" = 3/8)

rolls$prob1 <- prob[rolls$Var1]
rolls$prob2 <- prob[rolls$Var2]
rolls$prob <- rolls$prob1 * rolls$prob2

sum(rolls$value * rolls$prob)
