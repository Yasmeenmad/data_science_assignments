    # ===========================================================================================================================================
    # ------------------------------------------- Ideal Solution For The Kim Birthday Party Decision Tree --------------------------------------
    # ===========================================================================================================================================
    
    # library we need
    library('tidyverse')

    # values for each column in kim birthday party
    location_output    <- c("Outdoors", "Porch", "Indoors")
    weather_output     <- c("Sunny", "Rainy")

    # repeat our variables to complete our decision tree
    locations       <- rep(location_output, 2, replace = FALSE)
    weathers        <- rep (weather_output, 3, replace= FALSE)
    
    # create dataframe for kim birthday party contian the party location and the weather
    kim_party <- data.frame(locations, weathers)
    
    
    # adding a new column depending on the values of weathers column
    kim_party <- kim_party %>% mutate(weather_probability = case_when(weathers == "Sunny" ~ 0.4,
                                                                              weathers == "Rainy" ~ 0.6 ))
    
    # adding a new column depending on the values of locations and weathers columns 
    kim_party <- kim_party %>% mutate(weight = case_when((locations == "Outdoors" & weathers == "Sunny") ~ 100,
                                                      (locations == "Outdoors" & weathers == "Rainy") ~ 0,
                                                      (locations == "Porch" & weathers == "Sunny") ~ 90,
                                                      (locations == "Porch" & weathers == "Rainy") ~ 20,
                                                      (locations == "Indoors" & weathers == "Sunny") ~ 40,
                                                      (locations == "Indoors" & weathers == "Rainy") ~ 50))

    
    #create a function to print out the ideal solution which's the maximum of value_path
    ideal_solution <- function(){
      
      # Initial list
      mylist <- list()
      
      #for-loop to printing the all values for each party location
      # find the value for each path by calculating the sum of the probability in each location
      for(location in location_output){
        
        # save the output for the each location to same_location to print it
        same_location <- kim_party[kim_party$locations == location,]
        
        # determine how much each party location is worth based on the values provided by Kim
        # save the sum of the decision tree probability for the same location
        value_path           <- list(sum(same_location$weather_probability * same_location$weight))
        
        # calculate how much each uncertainty decision is worth then append it to a list
        mylist[[length(mylist)+1]] <- list(sum(same_location$weather_probability * same_location$weight))
         
        # show rows that have the same location 
         print (same_location)
         
         # print out the value of much each uncertainty decision is worth
         print(value_path)
      
      }
      # appand the list
      mylist
      
      print("The ideal solution for kim is to choose the location for her party whose value will be:")
      
      # choose the ideal solution for the kim party decision tree
      # which is the maximum value based on the calculated results of the decision tree
      max(unlist(mylist))
    }
    
    # show the final result
    ideal_solution()
    