    #LP Brewery is coming up with a hot new brew called Grape Soda. Because it is a special craft beer, 
    #it has a margin of $30, It uses 10 ounces of water, 4g of CO2, and 15 pounds of flavoring. 
    #With the addition of this new soda, LP Brewery has to now think about allocating its personnel to the different manufacturing processes. 
    #Each of the batches takes 5 (Strawberry Soda), 10 (Orange Soda), and 20 (Grape Soda) hours of labor to make and we have only 5 employees
    #full-time. If this is the production planning for a month of brewing, what is the optimal amount of each beer that must be produced 
    #to maximize profit
    
    
    # ======================================================================================================================================
    # ------------------------------------------------- Model For LP Brewery Manufacturing -------------------------------------------------
    # ======================================================================================================================================
    install.packages("lpSolve")
    library(lpSolve)
    
    # objective function info the lecture example
    # Max (13 X1 + 23 X2 + 30 X3)
    # Constraints matrix ---> A coefficient matrix, RHS matrix
    # 5 X1 + 15 X2 + 4 X3   <= 480    CO2
    # 4 X1 + 4 X2 + 10 X3   <= 160    Water
    # 35 X1 + 20 X2 + 15 X3 <= 1190   Flavor
    # 5 X` + 10 X2 + 20 X3  <= 800    hours of labor, 5employees, 4 Weeks, 40 hour/week
    
    # A = [5,15,4 ; 4,4,10; 35,20,15; 5,10,20]
    # B = [480;160;1190;800]
    
    # lp(direction='max', objective.in, const.mat, const.dir, const.rhs)
    
    objective.in = c (13, 23, 30)
    const.mat = matrix( c(5,15,4 ,4,4,10, 35,20,15, 5,10,20), nrow = 4, byrow = TRUE)
    const.dir = c("<=", "<=", "<=", "<=")
    const.rhs = c(480, 160, 1190, 800)
    
    prob_1 = lp(direction='max', objective.in, const.mat, const.dir, const.rhs)
    
    ans = prob_1$solution
    cat('The optimal amount of each beer that must be produced to maximize profit is: ',
        '\n', floor(ans)[1] , 'Strawberry Soda', '\n', floor(ans)[2], 'Orange Soda',
        '\n', floor(ans)[3], 'Grape Soda')
    val = prob_1$objval
    floor(val)
    