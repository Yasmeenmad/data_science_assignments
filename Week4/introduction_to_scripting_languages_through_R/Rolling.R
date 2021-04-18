# -------------------- task1-----------------------
# we have created function for number of dice sided (num_side)
# and to roll one dice n number of times (num_roll)
# then show the result and return the sum of rolling result

roll_dice_result_sum <- function(num_side,num_roll) {
  roll_dice <- sample(1:num_side,  size = num_roll, replace = T)
  print(roll_dice)
  return(sum(roll_dice))
}


# Create a 10 sided dice and roll one dice 6 times 
# and calculate the sum of the result

roll_dice_result_sum(10,6)



# -------------------- task2-----------------------
# Create a 20 sided dice and roll one dice 10 times
# than calculated the sum of the result (not in the task)

roll_dice_result_sum(20,10)



# -------------------- task3-----------------------
# we have created function for number of dice sided (num_side)
# and to roll one dice n number of times (num_roll)
# then show the result and calculate how many dice rolled more 
# than particular number(num)

count_dice_result <- function(num_side,num_roll,num){
  roll_dice <- sample(1:num_side, size = num_roll, replace = TRUE)
  print(roll_dice)
  return(sum(roll_dice > num))
}


# calculate how many dice rolled more than 6 (for the 10 sided, roll 10 times)

count_dice_result(10,10,6)


# calculate how many dice rolled more than 16 (for the 20 sided, roll 10 times)

count_dice_result(20,10,16)