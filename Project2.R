deck <- read.csv("cards.csv")

setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = parent.env(environment()))
    card
  }
  
  SHUFFLE <- function() {
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = parent.env(environment()))
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)
deal <- cards$deal
shuffle <- cards$shuffle
  
war <- function(cards) {
  cards$value[cards$face == "ace"] <- 14
  cards
}

hearts <- function(cards) {
  queenOfSpades <- deck4$suit == "spades" & deck4$face == "queen"
  cards$value <- 0
  cards$value[cards$suit == "hearts"] <- 1
  cards$value[queenOfSpades] <- 13
  cards
} 

blackjack <- function(cards) {
  facecard <- cards$face %in% c("king", "queen", "jack")
  cards$value[facecard] <- 10
  cards$value[cards$face == "ace"] <- NA
  cards
}

show_env <- function(){
  list(ran.in = environment(), 
       parent = parent.env(environment()), 
       objects = ls.str(environment()))
}

