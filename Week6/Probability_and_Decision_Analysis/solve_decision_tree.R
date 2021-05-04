    library('tidyverse')
    library('plyr'); library('dplyr')
    library('reshape')
    
    # values for each column in kim birthday party
    location_output    <- c("Outdoors", "Porch", "Indoors")
    weather_output     <- c("Sunny", "Rainy")

    # repeat our variables to complete our decision tree
    locations       <- rep(location_output, 2, replace = FALSE)
    weathers        <- rep (weather_output, 3, replace= FALSE)
    
    # create dataframe for kim birthday party contian the party location and the weather
    kim_party <- data.frame(locations, weathers)
    
    
    # create 
    kim_party <- kim_party %>% mutate(weather_probability = case_when(weathers == "Sunny" ~ 0.4,
                                                                              weathers == "Rainy" ~ 0.6 ))
    
    kim_party <- kim_party %>% mutate(weight = case_when((locations == "Outdoors" & weathers == "Sunny") ~ 100,
                                                      (locations == "Outdoors" & weathers == "Rainy") ~ 0,
                                                      (locations == "Porch" & weathers == "Sunny") ~ 90,
                                                      (locations == "Porch" & weathers == "Rainy") ~ 20,
                                                      (locations == "Indoors" & weathers == "Sunny") ~ 40,
                                                      (locations == "Indoors" & weathers == "Rainy") ~ 50))

    # create a 
    ideal_solution <- function(){
      
      for(location in location_output){
        
      same_location <- kim_party[kim_party$locations == location,]
      value_path           <- list(sum(same_location$weather_probability * same_location$weight))
     #mylist <- list(sum(kim_party[kim_party$location == i,]$weather_probability * kim_party[kim_party$location == i,]$weight))
     #new_mat <- rbind(mylist[i])
     
    print (same_location)
    print(value_path)
    
    #lapply(sum[i],FUN=max)
    #print(new_mat)
      }
      #mylist <- list(sum)
      #max(sapply(new_mat, max))
      max(unlist(value_path))
      #max(unlist(lapply(sum[i],FUN=max)))
      #lapply(sum[i],FUN=function(x)apply(x,MARGIN=1,FUN=max))
      #lapply(sum[i],FUN=max)
      #max(sum)
    }
    ideal_solution()
    