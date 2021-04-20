
# Load deck csv file
deck <- read.csv("deck.csv", TRUE,",")

# Show the head
head(deck)

#-------------------- task1-----------------------
# Create function to Shuffle the deck
shuffle <- function(cards) { 
  random <- sample(1:52, size = 52)
  cards[random, ]
}

# save the randomized card desk in a new data frame
shuffle_deck <- shuffle(deck)

# check
shuffle_deck
# -------------------- task2-----------------------
# Create function to deal the cards n times
# the number of deals (num_deal)
deal <- function(cards,num_deal) { 
  random <- sample(1:52, num_deal, replace = FALSE)
  cards[random, ]
  
}

# check: deal 3 in shuffle deck
deal(shuffle_deck,3)
  # -------------------- task3-----------------------
# Create a function that accepts two input “number of players” and " number of deals"
# than distribute the cards to each player 

deal_for_num_players <- function(num_players,num_deal){
  
  # the total cards will distribute depending on the number of players
  total_cards <- num_players*num_deal
  
  #if the total number of cards is less than or equal to 52 we will distribute, Otherwise, print(‘No enough cards’)  
  if (total_cards > 52){
    print ('No enough cards')
    
  } else {
    # Initialize the condition variables 
    cards <- sample(x=deck$X, size = total_cards, replace = FALSE)
    count <- 1
    start_card <- 1
    end_card <- num_deal
    
    # create an empty list
    player_list <- vector(mode = "list",1)
    
    # create a while loop to print cards for each player
    while (count <= num_players) {
      # This is the code that will execute while the condition is satisfied
      
      player_cards <- deck[deck$X %in% cards[start_card:end_card],]
      player_list[[count]]<- player_cards
      
      count = count +1
      start_card = start_card + num_deal
      end_card = end_card + num_deal
    }
    return(player_list)
  }
}


# distribute 5 deck cards for 10 players 
deal_for_num_players(3,5)

# check : distribute 5 deck cards for 11 players 
deal_for_num_players(11,5)


# -------------------- task4-----------------------
# save the cards each player will receive in a data frame

player1 <- deal_for_num_players(3,5)[1]
player1

player2 <- deal_for_num_players(3,5)[2]
player2

player3 <- deal_for_num_players(3,5)[3]
player3

# ---------------- Optional task-------------------
# Note: we have already created the function above

# distribute 5 deck cards for 4 players 
deal_for_num_players(4,5)

# distribute 7 deck cards for 4 players 
deal_for_num_players(4,7)

# distribute 10 deck cards for 4 players
deal_for_num_players(4,10)
