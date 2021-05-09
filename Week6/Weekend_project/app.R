
# Prerequisites:
library('shiny')
library('shinythemes')
library('tidyverse')
library('plyr')
library('dplyr')
library('png')
library('stringr')
library('grid')
library('ggplot2')
library('fiftystater')

#setwd("C:\\Users\\-\\Downloads\\munging_final")
# color palette: maroon(#9a031e), orange (#E2833E), yellow(#ffbe0b), gray(#979A9A), black(383838)
# read cleaned data (SO MUCH CANDY DATA)
clean_candy    <- read.csv("https://raw.githubusercontent.com/Yasmeenmad/data_science_bootcamp/main/Week6/Weekend_project/clean_candy.csv", TRUE, ",")
tablecandy     <- read.csv("https://raw.githubusercontent.com/Yasmeenmad/data_science_bootcamp/main/Week6/Weekend_project/clean_candy.csv", TRUE, ",", fileEncoding = "UTF-8")
countrydata    <- read.csv("https://raw.githubusercontent.com/Yasmeenmad/data_science_bootcamp/main/Week6/Weekend_project/filter_data%20_insight1_2.csv", TRUE, ",")
agedata        <- read.csv("https://raw.githubusercontent.com/Yasmeenmad/data_science_bootcamp/main/Week6/Weekend_project/filter_data%20_insight3.csv", TRUE, ",")
mapdata        <- read.csv("https://raw.githubusercontent.com/Yasmeenmad/data_science_bootcamp/main/Week6/Weekend_project/state-candy-count.csv", header=TRUE, stringsAsFactors=FALSE) 
mapdata        <- mapdata %>% glimpse()
data("fifty_states")

# =======================================================================================================================
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> User Interface (ui) <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ======================================================================================================================= 

ui <- shinyUI(navbarPage(title = "", theme = shinytheme("cosmo"),
                         tabPanel("Home", br(),br(),
                                  h2('Shiny App - Candy Hierarchy 2017 Dataset', align="center"),
                                  h3(a ('- By Data Scientists Team -'), align="center")),
                         tabPanel("About the Data", 
                                  h2('- About Candy Hierarchy 2017 Dataset -', align="center"),
                                  h4('Our dataset "Candy Hierarchy 2017" was acquired from the SCQ website. Originally, the dataset was collected from an', align="center"),
                                  h4('official survey, the main purpose is writing a humour creative nonfiction science piece to be published at "BoingBoing".', align="center"),
                                  h4('The dataset is about the traditional Halloween custom "Trick-or-Treating", where the children in costumes travel from', align="center"),
                                  h4('house to house, asking for treats with the phrase "Trick or treat". The "Treat" is usually some form of candy, while the', align="center"),
                                  h4('"Trick" refers to a threat to perform mischief on the homeowners or their property if no treat is given. Therefore, the', align="center"),
                                  h4('survey questions about the feeling you get when you receive a specific candy in your Halloween haul. Does it make', align="center"),
                                  h4('you really happy (joy)? Or despair? Or Meh? (Meh for indifference).', align="center"),
                                  h3('- Resources -', align="center"),
                                  h4(a ('Info about "Trick or Treating"', href="https://en.wikipedia.org/wiki/Trick-or-treating"), align="center"),
                                  h4(a ("Dataset Website", href="https://www.scq.ubc.ca/so-much-candy-data-seriously/"), align="center"),
                                  h4(a ("Survey Link", href="https://www.scq.ubc.ca/wp-content/uploads/2017/10/candyhierarchysurvey2017.pdf"), align="center")),
                         
 #---------------------------------- Our Team Tab -------------------------------------------------------------------
                         tabPanel("Our Team",
                                  h2('- Data Scientists Team -', align="center"),
                                  h3(a ('Halah Almodarra'), align="center"),
                                  h4('Data Cleaning and Preparing', align="center"),
                                  h4('Insight Three and Four', align="center"),
                                  h3(a ('Nourah AlMutlaq'), align="center"),
                                  h4('Data Cleaning and Preparing', align="center"),
                                  h4('Insights Five,Six,Seven and Eight', align="center"),
                                  #h4('Theme and Coloring', align="center"),
                                  h3(a ('Yasmeen Aldossary'), align="center"),
                                  h4('Data Cleaning and Preparing', align="center"),
                                  h4('Insights One, Two and Three', align="center"),
                                  #h4('Theme and Coloring', align="center")
                         ),
   
#---------------------------------- First Insight ---------------------------------------------------------------------
#Insight 1 - The Child Feeling Of Receiving This Kind Of Candy Bar In Three Different Countries By Gender                    
                       tabPanel("Insight One",
                                sidebarLayout(
                                    sidebarPanel(
                                        selectInput("candy_type", "Choose The Candy Bar That You Wish To Plot",
                                                    choices= c("Kit kat", "Candy Corn", "Mars", "Snickers", "Twix")),
                                        textInput("PlotTitle_insight1", "Enter The Plot Title", 
                                                  "The Child Feeling Of Receiving This Kind Of Candy Bar In Three Different Countries By Gender"),
                                        sliderInput("fontsize1", "Select The Font Size", min = 12, max = 25, value = 16, step = 1), ),
                                    mainPanel(plotOutput("dispPlot1")))),

#---------------------------------- Second Insight ---------------------------------------------------------------------
#Insight 2 - The Most Preferable Day For Each Age Stage By Gender              
                       tabPanel("Insight Two",
                                sidebarLayout(
                                    sidebarPanel(
                                        radioButtons("age_stage", "What Stages Of Life Are You Interested In?",
                                                    choices= c("All Stages" = "all", "Senior Adult" = "sadult",
                                                               "Middle Age Adult" = "madult", "Adult" = "adult",
                                                               "Teen" = "teen", "Child" = "child")),
                                        textInput("PlotTitle_insight2", "Enter The Plot Title",
                                                  "The Most Preferable Day For Each Age Stage By Gender"),
                                        sliderInput("fontsize2", "Select The Font Size",
                                                    min = 12, max = 18, value = 14, step = 1), ),
                                    mainPanel(plotOutput("dispPlot2")))),



#---------------------------------- Third Insight ---------------------------------------------------------------------
# Insight 3 - Display the Number of people when they receive a Butterfinger Candy in Halloween by states
                                                    
                        tabPanel("Insight Three",
                                 sidebarLayout(
                                     sidebarPanel(
                                         selectInput("butter_feeling", "What Butterfinger Feeling Interested In?",
                                                     choices= c("JOY", "MEH", "DESPAIR")),
                                     textInput("PlotTitle_insight3", "Enter The Plot Title", 
                                               "The Number Of People When They Receive A Butterfinger Candy In Halloween By US States")),
                                     mainPanel(plotOutput("dispPlot3")))),
#---------------------------------- Forth Insight ---------------------------------------------------------------------
# Insight 4 - Display the Number of people when they receive a Butterfinger Candy in Halloween with options
                        tabPanel("Insight Four",
                                 sidebarLayout(
                                     sidebarPanel(
                                         radioButtons("Butterfinger_Feelings", "Plot color", c("Maroon", "Orange", "Yellow")),
                                         sliderInput("fontsize4", "Select The Font Size", min = 5, max = 20, value = 15, step = 1),
                                         textInput("Plot_Title", "Plot Title", "Butterfinger Feelings Plot")),
                                     mainPanel(plotOutput("dispPlot4")))),

#---------------------------------- Fifth Insight ---------------------------------------------------------------------
# Insight 5 - Which color would people most see?
tabPanel("Insight Five",
         sidebarLayout(
             sidebarPanel(
                 selectInput("Dress_Image_Color", "Plot Color",choices = c("Orange", "Gray", "Black")),
                 h5("Bar Width"), h6("Suggested Range (0.1 - 0.9)"),
                 textInput("Bar_Width", "" , 0.5),
                 textInput("Dress_Image_Title", "Plot Title", "Most Seen Color of the Dress Image")),
             mainPanel(plotOutput("dispPlot5")))),

#---------------------------------- More Tab ---------------------------------------------------------------------                
navbarMenu("More",

#---------------------------------- Sixth Insight ---------------------------------------------------------------------
# Insight 6 - Most Seen Color of the Dress based on Gender
                       tabPanel("Insight Six",
                                sidebarLayout(
                                    sidebarPanel(
                                        checkboxGroupInput("Gender_Group", "Select the Gender", 
                                                           choices = list("Male" = 1, "Female" = 2,
                                                                          "Other" = 3, "I'd rather not say" = 4), selected = list (1,2,3,4)),
                                        selectInput("Gender_Color", "Plot Color",choices = c("Maroon Theme", "Black Theme")),
                                        textInput("Gender_Dress_Title", "Plot Title", "Most Seen Color of the Dress based on Gender")),
                                    mainPanel(plotOutput("dispPlot6")))),

#---------------------------------- Seventh Insight ---------------------------------------------------------------------
# Insight 7 - Most Seen Color of the Dress based on Age
                        tabPanel("Insight Seven",
                                 sidebarLayout(
                                     sidebarPanel(
                                         radioButtons("Dress_radio", "Color of the Dress",
                                                      choices = list("All" = 1, "Blue and black" = 2, "White and gold" = 3), selected = 1),
                                         sliderInput("Age_Dress_slider", "Age Filter", min = 1, max = 99, value = c(25, 75)),
                                         textInput("Age_Dress_Title", "Plot Title", "Most Seen Color of the Dress based on Age"),
                                         h4("Order the count by"), actionButton("descending", "Descending"),actionButton("ascending", "Ascending "), 
                                         br(), br()),
                                     mainPanel(plotOutput("dispPlot7")))),

#---------------------------------- Eighth Insight -------------------------------------------------------------------
# Insight 8 - Most Clicked Websites
                        tabPanel("Insight Eight",
                                 sidebarLayout(
                                     sidebarPanel(
                                         radioButtons("Websites_radio", "Select the Website",
                                                      choices = list("All Websites" = 1, "DAILY DISH" = 2, "ESPN" = 3,"Science" = 4, "YAHOO!" = 5), selected = 1),
                                         textInput("web_point_size", "Point Size" , 5),  
                                         textInput("Most_Clicked_Websites", "Plot Title", "Most Clicked Websites")),
                                     mainPanel(plotOutput("dispPlot8")))),
#---------------------------------- Our clean data Tab -------------------------------------------------------------------
                        tabPanel("Data", dataTableOutput("data"))
), # end tab more
#---------------------------------- Background image -------------------------------------------------------------------
                        img (src="https://png.pngtree.com/thumb_back/fw800/background/20190813/pngtree-trick-or-treat-vector-design-banner-background-orange-moon-image_299033.jpg",
                            width="100%", height="20%")


                       
) # navbarPage end
) # ui end
    
   

 
# =======================================================================================================================
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Server <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ======================================================================================================================= 
    
server <- shinyServer(function(input, output, session) {
        
        # for display of clean candy dataset in the "Data Page"
        output$data <- renderDataTable({tablecandy})
        
#---------------------------------- First Insight ---------------------------------------------------------------------
#Insight 1 - The Child Feeling Of Receiving This Kind Of Candy Bar In Three Different Countries By Gender   
        output$dispPlot1 <- renderPlot({
            candytype    <- input$candy_type
                if (candytype == 'Kit kat'){
                    candy_name <- countrydata$kit.kat
                    
                } else if (candytype == 'Candy Corn'){
                    candy_name  <- countrydata$candy.corn
                    
                } else if (candytype == 'Mars'){
                    candy_name  <- countrydata$mars
                    
                } else if (candytype == 'Snickers'){
                    candy_name  <- countrydata$snickers
                    
                } else {
                    candy_name  <- countrydata$twix
                }
                
                ggplot(countrydata) +
                    geom_bar(aes(x = gender, fill = candy_name )) +
                    scale_fill_manual(values=c("#E2833E","#ffbe0b", "#979A9A")) +
                    theme_bw()+
                    labs(title = input$PlotTitle_insight1, x = "Countries & Gender") +
                    guides(fill=guide_legend(title="Feelings")) +
                    theme(text = element_text(size=input$fontsize1))+
                    facet_wrap(~ country)})
        
#---------------------------------- Second Insight ---------------------------------------------------------------------
#Insight 2 - The Most Preferable Day For Each Age Stage By Gender  
        output$dispPlot2 <- renderPlot({
            
            agestages    <- input$age_stage
            
            if (agestages == 'all'){
                agedata <- agedata
                
            } else if (agestages == 'sadult'){
                agedata  <- filter(agedata, agegroup == "Senior Adult")
                
            } else if (agestages == 'madult'){
                agedata  <- filter(agedata, agegroup == "Middle Age Adult")
                
            } else if (agestages == 'adult'){
                agedata  <- filter(agedata, agegroup == "Adult")
                
            } else if (agestages == 'teen'){
                agedata  <- filter(agedata, agegroup == "Teen")
                
            } else{
                agedata  <- filter(agedata, agegroup == "Child")
            }
            
            ggplot(agedata, aes(x = agegroup, fill = day)) +
                geom_bar(na.rm = TRUE, position = position_dodge()) + 
                scale_fill_manual(values=c("#E2833E","#ffbe0b"))+
                labs(title = input$PlotTitle_insight2, x = "Age Stages & Gender") +
                guides(fill=guide_legend(title="Day")) +
                theme_bw()+
                theme(text = element_text(size=input$fontsize2))+
                facet_wrap(~ gender) })
        
#---------------------------------- Third Insight ---------------------------------------------------------------------
# Insight 3 - Display the Number of people when they receive a Butterfinger Candy in Halloween by states
        
        output$dispPlot3 <- renderPlot({
            
            butterfeeling <- input$butter_feeling
            
            if (butterfeeling == 'JOY'){
                
                plot <- ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill="grey92" ) + 
                    geom_point(data=mapdata, aes(x=lon, y=lat, size = JOY), color="#9a031e") + 
                    scale_size(name="", range = c(2, 15)) + 
                    guides(size=guide_legend("Number of People Feelings JOY")) +
                    theme_void()
                
            } else if (butterfeeling == 'MEH'){
                plot <- ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill="grey92" ) + 
                    geom_point(data=mapdata, aes(x=lon, y=lat, size = MEH), color="#979A9A") + 
                    scale_size(name="", range = c(2, 15)) + 
                    guides(size=guide_legend("Number of People Feelings MEH")) +
                    theme_void()
                
            } else {
                plot <- ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill="grey92" ) + 
                    geom_point(data=mapdata, aes(x=lon, y=lat, size = DESPAIR), color="#E2833E") + 
                    scale_size(name="", range = c(2, 15)) + 
                    guides(size=guide_legend("Number of People Feelings DESPAIR")) +
                    theme_void()
            }
            
            plot + labs(title = input$PlotTitle_insight3)
        })
        
#----------------------------------  Forth Insight ---------------------------------------------------------------------
# Insight 4 - Display the Number of people when they receive a Butterfinger Candy in Halloween
        
        output$dispPlot4 <- renderPlot({
            if (input$Butterfinger_Feelings == 'Maroon')    {butterfinger_color = '#9A031E'} 
            else if (input$Butterfinger_Feelings == 'Orange') {butterfinger_color = '#E2833E'} 
            else {butterfinger_color = '#FFBE0B'}
            
            clean_candy %>% select(butterfinger) %>% filter(!is.na(butterfinger)) %>% ggplot(mapping = aes(butterfinger)) + 
                geom_bar(stat="count", width = as.numeric(input$Bar_Width), fill = butterfinger_color) + 
                labs(title = input$Plot_Title, x = "Feeling when you receive a Butterfinger", y = "Number of People")+ 
                guides(fill=guide_legend(title="butterfinger")) +
                theme_bw() + 
                theme(text = element_text(size=input$fontsize4)) 
            })
        
#----------------------------------  Fifth Insight ---------------------------------------------------------------------
# Insight 5 - Which color would people most see?
        
        output$dispPlot5 <- renderPlot({
            
            if (input$Dress_Image_Color == 'Orange')    {New_Image_Color = '#E2833E'} 
            else if (input$Dress_Image_Color == 'Gray') {New_Image_Color = '#979A9A'} 
            else {New_Image_Color = '#383838'}
            
            clean_candy %>% select(dress) %>% filter(!is.na(dress)) %>% ggplot(mapping = aes(dress)) + 
                # stat >> to count the number of cases at x position
                geom_bar(stat="count", width = as.numeric(input$Bar_Width), fill = New_Image_Color) + 
                labs(title = input$Dress_Image_Title, x = "Dress Color", y = "Number of People")+ 
                theme_bw() + # theme is a white background with grid lines
                theme(plot.title = element_text(size=16, hjust = 0.5), axis.text.x= element_text(size=10), # hjust to center the title
                      axis.text.y= element_text(size=10), axis.title=element_text(size=14)) })

#---------------------------------- Sixth Insight ---------------------------------------------------------------------
# Insight 6 - Most Seen Color of the Dress based on Gender
        
        output$dispPlot6 <- renderPlot({
            gender_lst = list()
            for (i in input$Gender_Group){
                if (i == '1') {gender_lst <- append(gender_lst,'Male') }
                else if (i == '2') {gender_lst <- append(gender_lst,'Female')}
                else if (i == '3') {gender_lst <- append(gender_lst,'Other')}
                else if (i == '4') {gender_lst <- append(gender_lst,"I'd rather not say")} }
            
            if (input$Gender_Color == "Maroon Theme"){
                low_color  = "#ffbe0b" 
                high_color = "#9a031e" }
            else if (input$Gender_Color == "Black Theme"){
                low_color  = "#979A9A" 
                high_color = "#383838" }
            
            clean_candy %>% select(dress, gender) %>% filter(!is.na(dress), !is.na(gender), gender == gender_lst) %>%
                ggplot(mapping = aes(dress, gender) ) + geom_bin2d() +
                scale_fill_gradient(low = low_color, high = high_color) + theme_bw() + # viridis refers to the scale of colors
                labs(x="Dress Color", y="Gender", title = input$Gender_Dress_Title)+  
                theme(plot.title = element_text(size=16, hjust = 0.5),axis.text.x= element_text(size=10),
                      axis.text.y= element_text(size=10), axis.title=element_text(size=14)) })
        
#---------------------------------- Seventh Insight ---------------------------------------------------------------------
# Insight 7 - Most Seen Color of the Dress based on Age
        output$dispPlot7 <- renderPlot({
            if (input$Dress_radio =="1") { color_selection = list('Blue and black', 'White and gold') }
            else if (input$Dress_radio =="2"){ color_selection = 'Blue and black' }
            else {color_selection = 'White and gold' }
            
            clean_candy %>% select(dress, age) %>%  
                filter(!is.na (dress), !is.na(age), dress == color_selection, 
                       age >=input$Age_Dress_slider[1] & age <=input$Age_Dress_slider[2]) %>%
                
                ggplot(aes(age, fill = dress)) +
                geom_bar(stat="count", position='stack', width = 0.5) + # Stack for stacked chart
                scale_fill_manual(values=c("#E2833E","#979A9A")) +
                labs(x="Age", y="Number of People", title=input$Age_Dress_Title)+ 
                theme_bw() +
                theme(plot.title = element_text(size=16, hjust = 0.5),axis.text.x= element_text(size=10,angle=90),
                      axis.text.y= element_text(size=10), axis.title=element_text(size=14)) })
        
#---------------------------------- Eighth Insight -------------------------------------------------------------------
# Insight 8 - Most Clicked Websites
        output$dispPlot8 <- renderPlot({
            
            if (input$Websites_radio == '1'){
                clicked_coordinates <- clean_candy %>% select(xy_coordinates = `click.Coordinates..x..y.`) %>% 
                    filter(!is.na(xy_coordinates)) %>% # to remove NA values
                    separate(xy_coordinates, into = c("x", "y"), sep = ",") %>%  # separate x and y at ","
                    mutate(x = str_replace(x, "\\(", "") %>% as.numeric()) %>% # remove right circle bracket
                    mutate(y = str_replace(y, "\\)", "") %>% as.numeric()) %>% # remove left circle bracket
                    mutate(Website = case_when(x<=50 & y <= 50 ~ "DAILY DISH", # assign the position for the coordinates
                                               x<=50 & y > 50  ~ "ESPN",
                                               x>50 & y <= 50 ~ "Science",
                                               x>50 & y > 50  ~ "YAHOO!")) }
            
            if (input$Websites_radio == '2'){
                clicked_coordinates <- clean_candy %>% select(xy_coordinates = `click.Coordinates..x..y.`) %>% 
                    filter(!is.na(xy_coordinates)) %>% # to remove NA values
                    separate(xy_coordinates, into = c("x", "y"), sep = ",") %>%  # separate x and y at ","
                    mutate(x = str_replace(x, "\\(", "") %>% as.numeric()) %>% # remove right circle bracket
                    mutate(y = str_replace(y, "\\)", "") %>% as.numeric()) %>% # remove left circle bracket
                    filter(x<=50 & y <= 50 ) %>%
                    mutate(Website = case_when(x<=50 & y <= 50 ~ "DAILY DISH", # assign the position for the coordinates
                                               x<=50 & y > 50  ~ "ESPN",
                                               x>50 & y <= 50 ~ "Science",
                                               x>50 & y > 50  ~ "YAHOO!")) }
            
            if (input$Websites_radio == '3'){
                clicked_coordinates <- clean_candy %>% select(xy_coordinates = `click.Coordinates..x..y.`) %>% 
                    filter(!is.na(xy_coordinates)) %>% # to remove NA values
                    separate(xy_coordinates, into = c("x", "y"), sep = ",") %>%  # separate x and y at ","
                    mutate(x = str_replace(x, "\\(", "") %>% as.numeric()) %>% # remove right circle bracket
                    mutate(y = str_replace(y, "\\)", "") %>% as.numeric()) %>% # remove left circle bracket
                    filter(x<=50 & y > 50 ) %>%
                    mutate(Website = case_when(x<=50 & y <= 50 ~ "DAILY DISH", # assign the position for the coordinates
                                               x<=50 & y > 50  ~ "ESPN",
                                               x>50 & y <= 50 ~ "Science",
                                               x>50 & y > 50  ~ "YAHOO!")) }
            
            if (input$Websites_radio == '4'){
                clicked_coordinates <- clean_candy %>% select(xy_coordinates = `click.Coordinates..x..y.`) %>% 
                    filter(!is.na(xy_coordinates)) %>% # to remove NA values
                    separate(xy_coordinates, into = c("x", "y"), sep = ",") %>%  # separate x and y at ","
                    mutate(x = str_replace(x, "\\(", "") %>% as.numeric()) %>% # remove right circle bracket
                    mutate(y = str_replace(y, "\\)", "") %>% as.numeric()) %>% # remove left circle bracket
                    filter(x>50 & y <= 50 ) %>%
                    mutate(Website = case_when(x<=50 & y <= 50 ~ "DAILY DISH", # assign the position for the coordinates
                                               x<=50 & y > 50  ~ "ESPN",
                                               x>50 & y <= 50 ~ "Science",
                                               x>50 & y > 50  ~ "YAHOO!")) }
            
            if (input$Websites_radio == '5'){
                clicked_coordinates <- clean_candy %>% select(xy_coordinates = `click.Coordinates..x..y.`) %>% 
                    filter(!is.na(xy_coordinates)) %>% # to remove NA values
                    separate(xy_coordinates, into = c("x", "y"), sep = ",") %>%  # separate x and y at ","
                    mutate(x = str_replace(x, "\\(", "") %>% as.numeric()) %>% # remove right circle bracket
                    mutate(y = str_replace(y, "\\)", "") %>% as.numeric()) %>% # remove left circle bracket
                    filter(x>50 & y > 50 ) %>%
                    mutate(Website = case_when(x<=50 & y <= 50 ~ "DAILY DISH", # assign the position for the coordinates
                                               x<=50 & y > 50  ~ "ESPN",
                                               x>50 & y <= 50 ~ "Science",
                                               x>50 & y > 50  ~ "YAHOO!")) }
            
            
            Websites_img <- readPNG("news_websites.png") # image of the websites from the survey 
            loc_img <- rasterGrob(Websites_img, interpolate=TRUE) # Render an image at the given location, interpolate the image linearly
            img_ratio = 755/586 # to identify the size of the image
            
            # to present the image after plotting the clicks on it 
            clicked_coordinates %>% mutate(y_scaled = img_ratio*y) %>% # y_scaled to identify place of points
                ggplot(aes(x, -y_scaled, colour = Website)) +  # colored according to the quarters
                annotation_custom(loc_img, xmin=0, xmax=100, ymax=0, ymin=-img_ratio*100) + # x (0-100), y (-100,0)
                geom_point(alpha = 1, shape = 20, stroke = 1, size = as.numeric(input$web_point_size)) + # points' style
                scale_y_continuous(limits = c(-img_ratio*100, 0), expand = c(0, 0)) +
                scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
                # color palette: maroon(#9a031e), orange (#E2833E), yellow(#ffbe0b), gray(#979A9A), black(383838)
                
                scale_colour_manual(values = c("#ffbe0b", "#E2833E", "#9a031e", "#979A9A"))+
                # coord_fixed specified ratio between the representation of data on the axes
                coord_fixed() + theme_void() + 
                labs(title = input$Most_Clicked_Websites) +
                theme(plot.title = element_text(size=16, hjust = 0.5)) })
}) # server end
    
shinyApp(ui = ui, server = server)
