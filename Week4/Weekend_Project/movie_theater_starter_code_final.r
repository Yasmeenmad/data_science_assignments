    
    # ===================================================================================================================================
    # ------------------------------------------------------Movie Theaters simulations---------------------------------------------------
    # ===================================================================================================================================
    
    theaters                  <- c('Screens Come True', 'Like Home Theater')
    screens                   <- c('Kids (30 Seats)', 'Standard (70 Seats)', 'Max (100 Seats)')
    kids_standard_ticket_cost <- 55 # Kids and Standard ticket cost
    max_ticket_cost           <- 75 # Max ticket cost
    not_allowed_movies        <- c('Little Women', 'Parasite', 'In the Heart of the Sea', 'Life of PI', 'The Help') 
    snacks_prices             <- c('Popcorn'= 15, 'Nachos'= 20, 'Nugget'= 10, 'French Fries'= 7, 'Cheese Sticks'= 11,
                                   'Hot Dog'= 13, 'Onion Rings'= 8) 
    quantities                <- 1:20
    week_days_f_theater       <- rep(0, 7) # to save days' revenue of first theater (Screens Come True)
    week_days_s_theater       <- rep(0, 7) # to save days' revenue of second theater (Like Home Theater)
    employee_id               <- 1001:1040
    individual                <- c('Children','Adults')
    
    # create an empty dataframe to save our results
    theater_dataframe = NULL
    
    # iterate through theaters
    for(theater in theaters) {
      Description  <- c("Theater's Name") # first column in theater_dataframe
      Value        <- c(theater)          # second column in theater_dataframe
      theater_dataframe = rbind(theater_dataframe,data.frame(Description, Value, stringsAsFactors = FALSE)) # add first row in theater_dataframe
      
      # iterate through the week
      for (day in 1:7) {
        theater_dataframe = rbind(theater_dataframe, c("Day", day)) # save the day in theater_dataframe
        all_screen_revenue <- 0 # initial value of Screens Revenue
        
        # iterate through screens on a particular day
        for (screen in screens) {
          # sample of adults and children
          sample_categories     <- c('Adults','Children')
          # create a sample according to number of seat in each screen (Kids)
          if (screen == 'Kids (30 Seats)') { 
            sample_seats  <- sample(1:30, size = 1, replace = TRUE) 
            # if the day is a weekend day, the sample will start from half of the seats
            if (day == 5  | day == 6 | day == 7) { # Thu, Fri, Sat
              sample_seats  <- sample(15:30, size = 1, replace = TRUE)
            }
            sample_people <- sample(sample_categories, size = sample_seats, replace = TRUE)
          }
          # create a sample according to number of seat in each screen (Standard)
          else if (screen == 'Standard (70 Seats)') {
            sample_seats  <- sample(1:70, size = 1, replace = TRUE)
            # if the day is a weekend day, the sample will start from half of the seats
            if (day == 5  | day == 6 | day == 7) {
              sample_seats  <- sample(35:70, size = 1, replace = TRUE)
            }
            sample_people <- sample(sample_categories, size = sample_seats, replace = TRUE)
            
          }
          else {
            # create a sample according to number of seat in each screen (Max)
            sample_seats  <- sample(1:100, size = 1, replace = TRUE) 
            # if the day is a weekend day, the sample will start from half of the seats
            if (day == 5  | day == 6 | day == 7) {
              sample_seats  <- sample(50:100, size = 1, replace = TRUE)
            }
            sample_people <- sample(sample_categories, size = sample_seats, replace = TRUE) 
          }
          
          Adults_Counter        <- 0    # initial number of Adults
          Children_Counter      <- 0    # initial number of Children
          
          # sample prices and quantities for snacks
          sample_snacks_prices  <- sample(snacks_prices, size = 7, replace = FALSE)
          sample_quantities     <- sample(quantities, size = 7, replace = TRUE)
          
          for (individual in sample_people) { # To count #of Adults & Children
            if (individual == 'Adults') {
              Adults_Counter = Adults_Counter + 1
            }
            else {
              Children_Counter = Children_Counter + 1
            }
            
            # calculate the revenue of sold tickets
            sold_tickets     <- Adults_Counter + Children_Counter
            tickets_revenue  <- 0
            # calculate the price of tickets according to the type of screen
            if (screen == 'Kids (30 Seats)' | screen == 'Standard (70 Seats)') {
              tickets_revenue <- sold_tickets * kids_standard_ticket_cost
            }
            else {
              tickets_revenue <- sold_tickets * max_ticket_cost    
            }
            
            # calculate the revenue for the snacks
            Snacks_Revenue  <- 0
            Snacks_Revenue  <- t(sample_quantities)%*%sample_snacks_prices
            
            # calculate the revenue of a screen 
            screen_revenue  <- 0
            screen_revenue  <- Snacks_Revenue + tickets_revenue
          }
          
          # calculate the revenue of all three screens in a current day 
          all_screen_revenue = all_screen_revenue + screen_revenue
          
          # save our output in theater_dataframe 
          # determine titles for all the values in the first column
          Description <- c("Screen","Number of Adults ", "Number of Children", "Sold Tickets",
                           "Total Tickets Revenue", "Total Snacks Revenue", "Total Screen Revenue")
          # the values in the second column
          Value <- c(screen, Adults_Counter, Children_Counter, sold_tickets, 
                     tickets_revenue, Snacks_Revenue, screen_revenue)
          # save first and second columns to theater_dataframe using rbind 
          theater_dataframe = rbind(theater_dataframe,data.frame(Description, Value,stringsAsFactors = FALSE))
        }
        
        # save total revenue of the corresponding day
        if (theater == 'Screens Come True') { 
          week_days_f_theater[day] = all_screen_revenue } # first theater = Screens Come True
        else {
          week_days_s_theater[day] = all_screen_revenue } # second theater = Like Home Theater
        
        theater_dataframe = rbind(theater_dataframe, c("Total Day Revenue", all_screen_revenue))
      } # end days loop
      
      # save total revenue of a week
      if (theater == 'Screens Come True') { # first theater = Screens Come True
        theater_dataframe = rbind(theater_dataframe, c("Total Week Revenue", sum(week_days_f_theater))) }
      else { # second theater = Like Home Theater
        theater_dataframe = rbind(theater_dataframe, c("Total Week Revenue", sum(week_days_s_theater))) }
    } # end theaters loop
    
    # ===================================================================================================================================
    # --------------------------------------------------------------Charts--------------------------------------------------------------
    # ==================================================================================================================================
    
    # a bar chart shows total revenue per day for the first theater (Screens Come True)
    barplot(week_days_f_theater, 
            main= "Revenue of a Week (Screens Come True)",
            xlab = "Days",
            ylab = "Revenue",
            names.arg = list('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'),
            col = c("#FFA07A","#FA8072","#E9967A", "#F08080","#CD5C5C", "#DC143C","#B22222" ))
    
    # a bar chart shows total revenue per day for the second theater (Like Home Theater)
    barplot(week_days_s_theater, 
            main= "Revenue of a Week (Like Home Theater)",
            xlab = "Days",
            ylab = "Revenue",
            names.arg = list('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'),
            col = c("#FFA07A","#FA8072","#E9967A", "#F08080","#CD5C5C", "#DC143C","#B22222" ))
    
    # a pie chart compares total revenue of the two theaters
    revenue_two_theaters <- c(sum(week_days_f_theater), sum(week_days_s_theater))
    pie(revenue_two_theaters,
        labels = list('Screens Come True', 'Like Home Theater'),
        main   = "Revenue Comparison of the Two Theaters",
        col    = c("#F08080","#CD5C5C"))
    
    # ==================================================================================================================================
    # ---------------------------------------------------- Empty Seats Function---------------------------------------------------------
    # ==================================================================================================================================
    
    
    # empty_seats function simulates the hall and gives you information about empty seats 
    # the function takes (theater name, day, screen) and returns data frame with empty seats
    empty_seats <- function(theater_name, day, screen) {
      sold_seats <- 0
      # to get the sold seats (sold_tickets) from theater_dataframe
      if (theater_name == "Screens Come True" ) { 
        for (f_theater in 1:163) {  # loop through the first theater
          if (theater_dataframe[f_theater, 1] == "Day" & theater_dataframe[f_theater, 2] == day){ 
            start_day <- f_theater # to get first row of the current day
            end_day <-start_day+22 # to get last row of the current day
            for (d in start_day:end_day) { # loop through the day
              if (theater_dataframe[d, 2] == screen){ # to get the required screen
                sold_seats <- strtoi(theater_dataframe[d+3, 2]) # to get Sold Tickets
                break;}
            }}}}
      if (theater_name == "Like Home Theater" ) { 
        for (s_theater in 164:326) {  # loop through the second theater
          if (theater_dataframe[s_theater, 1] == "Day" & theater_dataframe[s_theater, 2] == day){ 
            start_day <- s_theater # to get first row of the current day
            end_day <-start_day+22 # to get last row of the current day
            for (d in start_day:end_day) { # loop through the day
              if (theater_dataframe[d, 2] == screen){ # to get the required screen
                sold_seats <- strtoi(theater_dataframe[d+3, 2]) # to get Sold Tickets
                break;}
            }}}}
      
      # to visualize the hall as a matrix each seat with code such as A1, D6, etc.
      seat_code   <- c('A','B','C','D','E','F','G','H','I','J')
      seat_number <- c('1','2','3','4','5','6','7','8','9','10')
      
      # to draw the matrix according to the size of screen (Kids (30 Seats))
      if (screen == 'Kids (30 Seats)') {
        screan_seats <- matrix(nrow = 6, ncol = 5, dimnames = list(seat_code[1:6],seat_number[1:5])) # 6*5 = 30 seats
        for (letter in seat_code[1:6]) { # loop through letters from A to F
          theater_row <- c(paste0(letter, seat_number[1:5])) # to combine between letter and number such as A3
          row_number  <- strtoi(letter, 20L)-9 # convert a letter to number to get row of the matrix
          screan_seats[row_number, ] <- theater_row # assign special code to each seat
        }
        # to mark occupied seats with x sign 
        occupied_seat <- sample(1:30, size = sold_seats, replace = FALSE) # occupied_seat equals sold_seat
        for (seat in occupied_seat) {
          screan_seats[seat] <- "X"
        }
        kids_df <- data.frame(screan_seats) # to save the matrix as df
        row.names(kids_df) <- seat_code[1:6]
        names(kids_df)     <- seat_number[1:5]
        View (kids_df)
      }
      
      # to draw the matrix according to the size of screen (Standard (70 Seats))
      else if (screen == 'Standard (70 Seats)') {
        screan_seats <- matrix(nrow = 7, ncol = 10, dimnames = list(seat_code[1:7],seat_number[1:10])) # 7*10 = 70 seats
        for (letter in seat_code[1:7]) { # loop through letters from A to G
          theater_row <- c(paste0(letter, seat_number[1:10]))  # to combine between letter and number such as A3
          row_number  <- strtoi(letter, 20L)-9 # convert a letter to number to get row of the matrix
          screan_seats[row_number, ] <- theater_row # assign special code to each seat
        }
        # to mark occupied seats with x sign 
        occupied_seat <- sample(1:70, size = sold_seats, replace = FALSE) # occupied_seat equals sold_seat
        for (seat in occupied_seat) {
          screan_seats[seat] <- "X"
        }
        standard_df <- data.frame(screan_seats) # to save the matrix as df
        row.names(standard_df) <- seat_code[1:7]
        names(standard_df)     <- seat_number[1:10]
        View (standard_df)
      }
      
      # to draw the matrix according to the size of screen (Max (100 Seats))
      else {
        screan_seats <- matrix(nrow = 10, ncol = 10, dimnames = list(seat_code,seat_number)) # 10*10 = 100 seats
        for (letter in seat_code) { # loop through letters from A to J
          theater_row <- c(paste0(letter, seat_number)) # to combine between letter and number such as A3
          row_number  <- strtoi(letter, 20L)-9 # convert a letter to number to get row of the matrix
          screan_seats[row_number, ] <- theater_row # assign special code to each seat
        }
        # to mark occupied seats with x sign 
        occupied_seat <- sample(1:100, size = sold_seats, replace = FALSE) # occupied_seat equals sold_seat
        for (seat in occupied_seat) {
          screan_seats[seat] <- "X"
        }
        max_df <- data.frame(screan_seats) # to save the matrix as df
        row.names(max_df) <- seat_code
        names(max_df)     <- seat_number
        View (max_df)
      }
    }
    
    # to test empty_seats function that takes (theater name, day, screen)
    empty_seats("Screens Come True", "1", "Kids (30 Seats)")
    empty_seats("Like Home Theater", "2", "Standard (70 Seats)")
    empty_seats("Screens Come True", "3", "Max (100 Seats)")
    
   
    # ==================================================================================================================================
    # ------------------------------------------------------Employee Evaluation Function------------------------------------------------
    # ==================================================================================================================================
    
    # employee_evaluation function simulates the employee performance evaluation for
    # each quarter of the year, the function takes (employee_id) and returns the 
    # employee performance evaluation
    
    # create a function to evaluate our employees performance
    employee_evaluation <- function(employee_id) {
      
      # create a sample for all our key performance indicators employee evaluation
      communication_sample  <- sample(1:5,size=1,replace = TRUE)
      productivity_sample   <- sample(1:5,size=1,replace = TRUE)
      creativity_sample     <- sample(1:5,size=1,replace = TRUE)
      integrity_sample      <- sample(1:5,size=1,replace = TRUE)
      punctuality_sample    <- sample(1:5,size=1,replace = TRUE)
      attendance_sample     <- sample(1:5,size=1,replace = TRUE)
      
      # calculate the grade of each employee  
      # the average method with weighting – Competencies 
      # bases on this website https://clockify.me/blog/business/performance-rating-calculator/
      grade <- ((communication_sample/5) * 30) + ((productivity_sample/5) * 25)+((creativity_sample/5) * 15) +
        ((integrity_sample/5) * 15)+((punctuality_sample/5) * 5) + ((attendance_sample/5) * 10) 
      
      # if-else statement to classify employees' grades and print the evaluation result
      if (grade >= 95 & grade <= 100){
        print('Exceptional Performer')
      } else if (grade >= 85 & grade < 95){
        print('Highly Effective Performer')
      } else if (grade >= 78 & grade < 85){
        print('Effective Performer')
      } else if (grade >= 70 & grade < 78){
        print('Minimally Effective Performer ')
      } else {
        print('Non-Effective Performer ')
      }
      sprintf('%d This is your employee performance evaluation for this quarter of the year', grade)
      
    }
    
    # print the employee performance evaluation for particular employee
    employee_evaluation(1001)
    
    
    # call readxl library
    library(readxl)
    
    # create a function to print the employee performance evaluation for our employees
    # in each movie theater as a dataframe
    employee_evaluation_dataframe <- function(theater_name){
      
      # fill in the table with random values from 1-5 to simulate employee evaluation
      dataframe <- cbind(sample(3:5,size=100,replace = TRUE),sample(2:5,size=100,replace = TRUE),
                         sample(2:5,size=100,replace = TRUE),sample(1:5,size=100,replace = TRUE),
                         sample(4:5,size=100,replace = TRUE),sample(1:5,size=100,replace = TRUE))
      
      # add column for employees id as number one column
      dataframe <- data.frame(ID = 1001:1100, dataframe)
      # add column names for the dataframe
      colnames(dataframe) <- c('ID','Communication','Productivity','Creativity','Integrity','Punctuality','Attendance')
      dataframe <- as.data.frame.matrix(dataframe)
      
      # add a new column for the employees grades
      dataframe <- dataframe %>% rowwise() %>%
        mutate(Grade = ((Communication/5) * 30) + ((Productivity/5) * 25)+((Creativity/5) * 15) +
                 ((Integrity/5) * 15)+((Punctuality/5) * 5) + ((Attendance/5) * 10))
      
      # add a new column for the employees final evaluation
      dataframe <- dataframe %>% mutate(Evaluation =
                                          case_when(Grade >= 95 ~ "Exceptional Performer", 
                                                    (Grade >= 85 & Grade < 95) ~ "Highly Effective Performer",
                                                    (Grade >= 78 & Grade < 85) ~ "Effective Performer",
                                                    (Grade >= 70 & Grade < 78) ~ "Minimally Effective Performer",
                                                    Grade < 70 ~"Non-Effective Performer"))
      return(dataframe)
    }
    
    # print the employee evaluation for the two theaters
    theater_one_employee_evaluation <- employee_evaluation_dataframe('Screens Come True')
    theater_two_employee_evaluation <- employee_evaluation_dataframe('Like Home Theater')
    
    # ===================================================================================================================================
    # --------------------------------------------------------------Charts--------------------------------------------------------------
    # ==================================================================================================================================
    
      # create bar chart shows the number of employees in each category of employee performance evaluation in theater 1
    plot1 <- ggplot(data=theater_one_employee_evaluation, aes(x=Evaluation)) + geom_bar(fill="steelblue") +
        geom_text(stat='count', aes(label=..count..), vjust=-1,) + ggtitle("Screens Come True Theater Employee Evaluation by Category")  
      
      # create bar chart shows the number of employees in each category of employee performance evaluation in theater 2
    plot2 <- ggplot(data=theater_two_employee_evaluation, aes(x=Evaluation)) + geom_bar(fill="steelblue") +
        geom_text(stat='count', aes(label=..count..), vjust=-1)+ ggtitle("Like Home Theater Employee Evaluation by Category")
      
    
     # calculate average grades for employees performance evaluation in theater 1, 2
     mean_theater_one <- mean(theater_one_employee_evaluation$Grade)
     mean_theater_two <- mean(theater_two_employee_evaluation$Grade)
     
     # set the column values and names for the bar chart  
     column_value <- c(mean_theater_one,mean_theater_two)
     column_name <- c("Screens Come True","Like Home Theater")
     
     # create bar chart shows the average for employee performance evaluation in theater 1, 2
     barplot(column_value,names.arg=column_name,xlab="",ylab="Average Grade",col="steelblue",
             main="Average Employee Evaluation Grade for each Theaters")
   
   
    
    # ==================================================================================================================================
    # ---------------------------------------------------Not Allowed Movies Function----------------------------------------------------
    # ==================================================================================================================================
    
    # not_allowed_movies function Simulates whether children are allowed into this film
    # based on the movie rating, the function takes (movie_name,individual) and returns
    # if the children are allowed or not
    
    # not_allowed_movies_function
    not_allowed_movies_fun <- function(movie_name,individual) {
      if( (individual == 'Children') & (movie_name %in% not_allowed_movies)) {
        print("Not allowed to watch")
      } 
      else {
        print("Allowed to watch")
      }
    }
    not_allowed_movies_fun('Parasite', 'Children')
    not_allowed_movies_fun('Parasite', 'Adults')
    
    
