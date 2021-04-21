# Load deck csv file
deck <- read.csv("deck.csv", TRUE,",")

# Create function to Shuffle the deck
shuffle <- function(cards) { 
  random <- sample(1:52, size = 52)
  cards[random, ]
}

# save the randomized card desk in a new data frame
shuffle_deck <- shuffle(deck)


#---------------------------------------- task1-----------------------------------------------------

# Create a function that hands two players there cards
game_two_players<- function(num_deal) {
  
  player1<- head(shuffle_deck,num_deal)   # player1 take cards from the head of shuffle_deck
  player2<- tail(shuffle_deck, num_deal)  # player2 take cards from the tail of shuffle_deck
  
  # Add a new column and specify every card to which player
  player1$player <- c("player1") 
  player2$player <- c("player2")          
  
  print("---- player1----")
  print(player1)               # print the cards for the first player
  print("----player2----")
  print(player2)               # print the cards for the second player
  
  # combine vectors rows to give us a matrix by using rbind 
  hands_two_players <- rbind(player1 ,player2)
}

# hands 10 cards for each player 
game_two_players(10)

# Assign game_two_players(10) to hands_two_players as dataframe 
hands_two_players  <- data.frame(game_two_players(10))


# Create a function for two players that compares players' points, it determines who is the winner
winner_two_players <- function(){
  
  # find the sum of column 'value' for each player to calculate score game
  points_player1 <- sum(hands_two_players[hands_two_players$player == "player1",]$value)
  points_player2 <- sum(hands_two_players[hands_two_players$player == "player2",]$value)
  
  # if-else statement to compare scores and print the results
  if (points_player1 > points_player2) {
    print("Player 1 has more points than Player 2")
    
  }else if(points_player1 < points_player2){
    print("Player 2 has more points than Player 1")
    
  }else{
    print("It's a tie! points for player1 equal to points for player2")
    
  }
  print("Total score for player1:")
  print(points_player1)  
  print("Total score for player2:")
  print(points_player2)
}

# the winner
winner_two_players()


#----------------------------------------------- task2-------------------------------------------------

# Create a function that hands three players there cards
game_three_players<- function(num_deal) {
  
  player1<- head(shuffle_deck,num_deal)               # player1 take cards from the head of shuffle_deck
  player2<- tail(shuffle_deck, num_deal)              # player2 take cards from the tail of shuffle_deck
  player3<- shuffle_deck[c(17:(17 + num_deal-1)),]    # player3 take cards from the middle of shuffle_deck
  
  # Add a new column and specify every card to which player
  player1$player <- c("player1")              
  player2$player <- c("player2")
  player3$player <- c("player3")
  
  print("---- player1----")
  print(player1)                # print the cards for the first player
  print("----player2----")
  print(player2)                # print the cards for the second player
  print("----player3----")
  print(player3)                # print the cards for the third player
  
  # combine vectors rows to give us a matrix by using rbind
  hands_three_players <- rbind(player1 ,player2, player3)
}

# hands 11 cards for each player
game_three_players(11)

# Assign game_three_players(11) to hands_three_players as dataframe
hands_three_players <- data.frame(game_three_players(11))

# find the sum of column 'value' for each player to calculate score game
points_player1 <- sum(hands_three_players[hands_three_players$player == "player1",]$value)
points_player2 <- sum(hands_three_players[hands_three_players$player == "player2",]$value)
points_player3 <- sum(hands_three_players[hands_three_players$player == "player3",]$value)

# Create a function for three players that compares players' points, it determines who is the winner 
winner_three_players <- function(){
  
  # if-else statement to compare scores and print the results
  if (points_player1 > points_player2 & points_player1 > points_player3) {
    print("Player1 has more points than Player2 and Player3")
    
  }else if(points_player2 > points_player1 & points_player2 > points_player3){
    print("Player2 has more points than Player1 and Player3")
    
  }else if(points_player3 > points_player1 & points_player3 > points_player2){
    print("Player3 has more points than Player1 and Player2")
    
    }else if(points_player3 == points_player1 & points_player3 == points_player2){
      print("It's a tie! points for player1, player2 and player3 are equal")
    }
    
  print("Total score for player1:")
  print(points_player1) 
  print("Total score for player2:")
  print(points_player2)
  print("Total score for player3:")
  print(points_player3)
}

# the winner
winner_three_players()

#-------------------------------------------- Optional task1----------------------------------------------

# create a function to count the number of pairs in 'face' column for each player
pairs <- function(players){
  return(sum(duplicated(hands_three_players[hands_three_players$player == players,]$face)))
}


pairs("player1")   # find the number of pairs in the column 'face' for player1
pairs("player2")   # find the number of pairs in the column 'face' for player2
pairs("player3")   # find the number of pairs in the column 'face' for player3



#-------------------------------------------- Optional task3----------------------------------------------
  
# Create a function for three players that compares players' points
# And it determines whether there is a tie or not, Who is the player who tied with the other player?

is_there_tie <- function(){ 

# if-else statement to compare scores and find the tie then print the results
if (points_player1 == points_player2 & points_player1 != points_player3) {
  print("There's a tie between player1 and player2")
  
}else if(points_player1 == points_player3 & points_player1 != points_player2){
  print("There's a tie between player1 and player3")
  
}else if(points_player2 == points_player3 & points_player2 != points_player1){
  print("There's a tie between player2 and player3")
  
}else {
  print("There's No tie! between the players")
}

print("Total score for player1:")
print(points_player1) 
print("Total score for player2:")
print(points_player2)
print("Total score for player3:")
print(points_player3)
}

# the result
is_there_tie()
