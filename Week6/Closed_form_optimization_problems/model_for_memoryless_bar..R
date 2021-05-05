    # Memoryless Soda Shop, on the other hand, is tired of running out of inventory and missing a lot of potential sales. 
    # They have hired you to help them figure out what they need to do to have a probability of 60% or higher of having 2 kegs on
    # hand in the long run. There are several things that you can change to attempt this task: changing the order rules, 
    # expanding the chain to allow for more inventory to accumulate. Experiment with a few of these and make a recommendation to the bar owners,
    #your recommendation should include a diagram, a transition matrix, and the steady-state values
    
    # ======================================================================================================================================
    # ----------------------------------------------------- Model For Memoryless Soda Shop -------------------------------------------------
    # ======================================================================================================================================
    install.packages("markovchain")
    library(markovchain)
    
    
    # --------------------------------------------------------- Plan Number 1 --------------------------------------------------------------
    # changing the order rule to have a probability of 60% or higher of having 2 batches on hand in the long run
    prob <- matrix(c(0, 0, 0.6, 0.4,
                     0, 0, 0.85, 0.15,
                     0, 0, 0.65, 0.35,
                     0, 0, 0.9, 0.1),
                   nrow = 4, byrow = TRUE)
    
    soda_batches <- new("markovchain", states = c('0','1','2','3'),
                        transitionMatrix = prob, name= 'Inventory')
    print(soda_batches)
    plot(soda_batches)
    
    # after 6 months the inventory is  
    current_inventory <- soda_batches ^6
    current_inventory
    round((current_inventory[1:4]),2)
    
 
    # --------------------------------------------------------- Plan Number 2 --------------------------------------------------------------
    
    # expanding the chain to allow for more inventory to accumulate
    prob  <- matrix(c(0, 0, 0.6 , 0.1 , 0.2 , 0.1,
                      0, 0, 0.65, 0.15, 0.09, 0.11,
                      0, 0, 0.7 , 0.05, 0.05, 0.2,
                      0, 0, 0.8 , 0.03, 0.07, 0.1,
                      0, 0, 0.6 , 0.33, 0.05 , 0.02,
                      0, 0, 0.77, 0.12, 0.05, 0.06),
                   nrow = 6, byrow = TRUE)
    
    soda_batches <- new("markovchain", states = c('0','1','2','3','4','5'),
                        transitionMatrix = prob, name= 'Inventory')
    print(soda_batches)
    plot(soda_batches)
    
    #after 8 months the inventory is:
    
    current_inventory <- soda_batches ^8
    current_inventory
    round((current_inventory[1:4]),2)
    
