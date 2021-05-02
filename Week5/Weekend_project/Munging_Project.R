
# ===========================================================================================================================================
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Munging Project - EDA - SO MUCH CANDY DATA, SERIOUSLY <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ===========================================================================================================================================

# Prerequisites:
library('tidyverse')
library('plyr')
library('dplyr')
library('readxl')
library('janitor')
library('data.table')
library('naniar')
library('stringr')
library('jpeg')
library('grid')
library('likert')
library('reshape2')

# ===========================================================================================================================================

# load so much candy dataset
candy       <- read.csv("C:\\Users\\-\\Downloads\\candyhierarchy2017.csv", TRUE,",", na.strings=c("","NA"))

# create a copy to apply EDA pipeline
clean_candy <- candy

# clean & prepare column names
names(clean_candy) <- substring(names(clean_candy),5)
clean_candy = clean_names(clean_candy)
clean_candy        <- setNames(clean_candy,gsub("_"," ",names(clean_candy)))
setnames(clean_candy, old = c('rnal id','state province county etc', 'x100 grand bar','k coordinates x y'), 
         new = c('id','state','100 grand bar','click Coordinates (x, y)'))

# delete the empty rows starting with gender column
clean_candy <- clean_candy[!is.na(clean_candy[,3]) | !is.na(clean_candy[,120]),]

# drop unnecessary columns
clean_candy$x <- NULL


# ===================================================== Age column pre-processing ===========================================================

clean_candy %>% select(age) %>% nrow() # to find number of rows in age column >> 2439 rows (before cleaning)
clean_candy %>% select(age) %>% filter(is.na(age)) %>% nrow() # to find number NA (blank) values in age column >> 63 rows
sapply(clean_candy$age, class) # to check type of age column >> character

# replace specific values such as 'sixty-nine' to 69
clean_candy$age = recode(clean_candy$age, "312"= "31", "45-55" = "50",
                         "39.4"="39","24-50"="37", "Over 50"="51",
                         "sixty-nine" ="69", "46 Halloweens." ="46",
                         "70.5"="70", "59 on the day after Halloween"= "59",
                         "60+"="61", "?"="1000","5u"="1000", "no"="1000", "1"="1000")

# replace all long strings with NA value
clean_candy$age[nchar(clean_candy$age) > 2] <- NA 

# change the type of age column from character to numeric
clean_candy$age = lapply(clean_candy$age, function(x) if(is.character(x)) as.numeric(x) else x) 
sapply(clean_candy$age, class) # to check type of age column >> numeric



# ===================================================== Country column pre-processing =======================================================

# print countries' names
setNames(as.data.frame(table(clean_candy$country)), c("country", "n"))

# the invalid values in column `country`
invalid_values <- c("1", "32", "35", "45", "46", "A", "CAN", "subscribe to dm4uz3 on youtube", "Earth",
                    "Europe", "Fear and Loathing", "Atlantis", "Can", "I don't know anymore", "insanity lately",
                    "Trumpistan", "soviet canuckistan", "UD", "Narnia", "cascadia")

# replacing invalid values with NA in column `country` 
clean_candy$country[clean_candy$country %in% invalid_values] <- NA

# replace empty cells with NA in column `country`
clean_candy$country[clean_candy$country == ""]               <- NA

# check
setNames(as.data.frame(table(clean_candy$country)), c("country", "n"))

# standardization of country names
united_states  <- c("USSA", "USAUSAUSA", "usas", "USAA", "USA? Hard to tell anymore..", "USA! USA! USA!", "USA", "Usa",
                    "USa", "usa", "US of A", "US", "us", "United ststes", "United Statss", "United States of America",
                    "united states of america", "united States", "united states", "United Stated", "United Statea",
                    "United staes", "'merica", "Ahem....Amerca", "Alaska", "america", "America", "California", "U S",
                    "I pretend to be from Canada, but I am really from the United States.", "N. America", "New Jersey",
                    "North Carolina", "Pittsburgh", "New York","The United States", "The United States of America", "U S ",
                    "u s a", "u.s.", "U.S.", "u.s.a.", "U.S.A.", "unhinged states", "Unied States", "unite states", "USA ",
                    "U.S. ", "United Sates", "United State", "United states", "united States ", "United States ", "Usa ",
                    "United States of America ", "united ststes", "Unites States", "Us", "Usa", "USA", "USA USA USA!!!!")
united_kingdom <- c("United kingdom", "United Kingdom", "UK ", "UK", "uk", "U.K. ", "endland", "England", "Scotland", 
                    "Scotland ", "Uk")
netherlands    <- c("The Netherlands")
canada         <- c("canada", "CANADA", "canada ", "Canada ", "Canada`", "Canae")
australia      <- c("australia")
france         <- c("france", "France ")
germany        <- c("germany")
spain          <- c("Murica", "murrika", "spain")
hong_kong      <- c("hong kong")
ireland        <- c("Ireland ")

# unify all the different inputs to one input
clean_candy$country[clean_candy$country %in% united_states]    <- 'United States'
clean_candy$country[clean_candy$country %in% united_kingdom]   <- 'United Kingdom'
clean_candy$country[clean_candy$country %in% netherlands]      <- 'Netherlands'
clean_candy$country[clean_candy$country %in% canada]           <- 'Canada'
clean_candy$country[clean_candy$country %in% australia]        <- 'Australia'
clean_candy$country[clean_candy$country %in% france]           <- 'France'
clean_candy$country[clean_candy$country %in% netherlands]      <- 'Netherlands'
clean_candy$country[clean_candy$country %in% germany]          <- 'Germany'
clean_candy$country[clean_candy$country %in% spain]            <- 'Spain'
clean_candy$country[clean_candy$country %in% hong_kong]        <- 'Hong Kong'
clean_candy$country[clean_candy$country %in% ireland]          <- 'Ireland'

# check
setNames(as.data.frame(table(clean_candy$country)), c("country", "n"))

# ====================================================== State column pre-processing ========================================================

# print States' names
setNames(as.data.frame(table(clean_candy$country)), c("state", "n"))

# the invalid values in column `state`
invalid_values <- c("1", "48", "|NC", "A", "America", "Canada", "cascadia", "Emerald City, Petrolia", "na", "Nope", "NOYB", 
                    "oblivion if things keep going this way", "Psychotic", "see question 2", "Sub Earth", "Tree Town, USA", 
                    "United States", "ur mom", "USA")

# replacing invalid values with NA in column `state` 
clean_candy$state[clean_candy$state %in% invalid_values] <- NA

# replace empty cells with NA in column `state`
clean_candy$state[clean_candy$state == ""] <- NA

# check
setNames(as.data.frame(table(clean_candy$state)), c("state", "n"))

# standardization of state names 
#states in USA
california <- c("Los Angeles, California", "CA", "alameda county, california", "Santa Clara County, California", 
                "California, San Francisco", "Glendora, Los Angeles, California", "California, San Diego", 
                "California, Ventura County", "California, Contra Costa County, Pleasant Hill", "Tehama",
                "California, Mendocino County", "orange county, ca","BERKELEY, CA", "California, USA", "ca",
                "California, San Diego county", "california", "California", "CA, Alameda", "Santa Cruz County, California", 
                "Cali", "Oakland, California", "cailifornia", "oakland,ca","Santa Barbara co. California", "calif", "Califor",
                "La","alameda county, california", "Ca", "California, Alameda")

new_york <- c("Astoria NY" , "Brooklyn", "Brooklyn, kings county ny", "DUTCHESS COUNTY NY", "Kings, New York", "Monroe county", 
              "New York County, New York City, New York State", "New York, NY","New York, New York", "NY", "ny", "Ny", "NY", 
              "Tompkins County", "NYC", "nyc", "Queens", "suffolk county ny", "ulster county, NY", "NY, Tompkins County")

washington <- c("Issaquah, WA, King County", "King County, Washington", "renton,wa ", "Vancouver, Wa", "Washingto", 
                "WA", "Wa", "wa", "Washington state",  "Washington, Clark ", "Washington, King" , "Washington, King County" ,
                "Washington, Seattle, king" , "Washington state, Kitsap County" , "Washington State, King County, City of Seattle",
                "Washington, Island county", "Whatcom County, WA","renton,wa", "Washington, seattle, king", "Washington, Clark", 
                "Washington State")

district_of_columbia <- c("Washington, DC", "WASHINGTON DC", "Washington DC", "DC", "dc", "Dc")
alaska <- c("Alaska, Matanuska-Susitna Borough", "alaska")
arizona  <- c("ar", "Ar", "Az", "az", "AZ", "phoenix, maricopa county, arizona")
virginia <- c("Arlington, VA", "arlington, va", "Virginia, Arlington", "Henrico, VA", "Rappahannock County, VA", "va","VA", "Va" )
west_virginia  <- c( "west virginia", "WV, Kanawha County", "WV", "wv")
georgia <- c("Atlanta, GA", "GA - Georgia", "GA", "Marietta, GA", "Ga", "georgia")
indiana <- c("Bloomington, IN", "in", "IN")
rhode_island  <- c("Bristol County, Rhode Island", "RI", "ri", "Ri", "rhode island")
north_carolina  <- c("Buncombe County, North Carolina", "Durham County, North Carolina", "NC","Nc","nc", "north carolina",
                     "The Democratic People's Republic of North Carolina", "North carolina")
south_carolina  <- c("SC", "sc")
minnesota  <- c("Carver county, MN", "minnesota", "MN", "Mn", "mn")
mexico_city   <- c("CDMX")
illinois  <- c("Chicago, IL", "Chicago, Illinois", "Cook", "il","IL", "ill","Il", "Northbrook, IL")
colorado <- c("Colorado, Jefferson county ", "CO, Larimer County", "colorado, boulder",
              "Colorado larimer county", "Colorado (CO)", "Co", "CO", "Colorado, Jefferson county", "COLORADO")
connecticut <- c("CT", "ct")
ohio <- c("Cuyahoga County, Ohio", "OH", "ohio")
delaware <- c("delaware", "Devonshire")
new_mexico <- c("Dona Ana county, New Mexico", "New Mexico, USA", "nm", "NM", "Nm")
florida <- c("FL", "Fl", "Orange County", "Orlando, FL" , "st. augustine florida", "FL, Orange County", "florida" )
texas <- c("Harris County, Texas", "TEXAS", "texas", "Tx.", "TX", "tx", "Tx")
hawaii <- c("hi","HI")
iowa <- c("Ia","IA", "iowa")
idaho <- c("Idaho ada")
michigan <- c("Kalamazoo County, MI", "MI","mi","Mi", "Michigan, Shiawassee County","Michigin","Mich", "Oakland County, Michigan")
tennessee <- c("Knox county, Tennessee", "Tennessee, Knox County", "tennessee", "tn", "Tn", "TN")
kansas <- c("KS", "Ks")
kentucky<- c("KY", "Ky")
louisiana<- c("louisiana", "New Orleans la")
massachusetts <- c("MA","ma","Ma", "Massachusetts, Plymouth county", "Massachussets ")
maryland <- c("MARYLAND", "maryland", "MD", "md", "Md")
mississippi <- c("mississippi", "MS")
missouri<- c("Missouri, Miller County", "missouri", "Missoure", "mo","Mo","MO", "St Louis mo", "St. Louis County, Missouri")
montana<- c("montana", "Mt","mt")
north_dakota <- c("ND")
south_dakota <- c("SD")
new_hampshire <- c("NH", "nh","Nh")
new_jersey <- c("NJ", "nj", "Nj", "NJ, Gloucester", "NJ; Essex", "The Shore")
pennsylvania <- c("Northampton County, PA", "Pennsylvania, Montgomery County", "philadelphia", "Philadelphia", 
                  "Philadelphia PA", "Pa.   Northampton county", "pennsylvania, northampton couny, easton", 
                  "Philadelphia, PA", "Pa", "pa", "PA", "Pittsburgh pa", "exton pa", "pennsylvania")
nebraska <- c("NE")
nevada <- c("NV")
oklahoma <- c("OK", "oklahoma")
oregon <- c("OR", "or", "Oregon - but will be in Washington for Halloween","Oregon, Multnomah County", "oregon", "OREGON", "Or")
maine <- c("Portland, maine", "maine")
puerto_rico <- c("pr")
utah <- c("UT", "ut", "UT, Salt Lake", "utah", "Ut")
vermont <- c("VT, Chittenden", "VT", "vt","Vt")
wisconsin <- c("Wisconsin, 53705", "WI", "wi", "Wi", "wisconsin")
wyoming <- c("wyoming")

#states in Canada 
british_columbia <- c("BC","bc", "Bc" , "British Columbia, Canada", "vancouver, bc")
alberta <- c("Albeqrrta", "AB" )
new_brunswick <- c("NB")
newfoundland_and_labrador <- c("newfoundland", "Newfoundland", "NL")
nova_scotia <- c("nova scotia", "NS")
ontario <- c("ON","On", "ontario", "ONTARIO", "ont", "Ont", "Ontario,  Canada", "Oshawa, Ontario, Canada", "Toronto, Ontario")
quebec <- c("QC", "QuÃ©bec","quÃ©bec", "Quenec")
saskatchewan <- c("sk")
yukon <- c("yukon")

#states in Germeny 
mecklenburg_vorpommern <- c("Greifswald")
hessen <- c("Hesse", "hesse" )
Baden <- c("Mannheim, Baden-WÃ¼rttemberg")

#states in France 
ile_de_france <- c("Idf")

#states in UK
south_east <- c("kent", "Oxfordshire", "Surrey", "Sussex", "OXFORDSHIRE")
north_west <- c("Greater Manchester", "Lancasire", "Manchester")
east_midlands  <- c("Derbyshire")
scotland <- c("aberdeenshire", "Fife, Scotland", "Glasgow", "Midlothian", "Strathclyde")
northern_ireland <- c("Kildare", "Munster")
greater_london <- c("Londom", "London")

#states in Netherlands
north_brabant<- c("N-Brabant")

#states in Australia
new_south_wales <- c("NSW", "Sydney")
queensland<- c("qld")

#states in china
changning_district<- c("Shanghai, Chang Ning district")

#states in Swedeen
Södermanland<- c("Stockholm")

# unify all the different inputs to one input
clean_candy$state[clean_candy$state %in% california]    <- 'California'
clean_candy$state[clean_candy$state %in% new_york]    <- 'New York'
clean_candy$state[clean_candy$state %in% washington]    <- 'Washington'
clean_candy$state[clean_candy$state %in% district_of_columbia]    <- 'District of Columbia'
clean_candy$state[clean_candy$state %in% alaska]    <- 'Alaska'
clean_candy$state[clean_candy$state %in% arizona]    <- 'Arizona'
clean_candy$state[clean_candy$state %in% virginia]    <- 'Virginia'
clean_candy$state[clean_candy$state %in% west_virginia]    <- 'West Virginia'
clean_candy$state[clean_candy$state %in% georgia]    <- 'Georgia'
clean_candy$state[clean_candy$state %in% indiana]    <- 'Indiana'
clean_candy$state[clean_candy$state %in% rhode_island]    <- 'Rhode Island'
clean_candy$state[clean_candy$state %in% north_carolina]    <- 'North Carolina'
clean_candy$state[clean_candy$state %in% south_carolina ]    <- 'South Carolina '
clean_candy$state[clean_candy$state %in% minnesota]    <- 'Minnesota'
clean_candy$state[clean_candy$state %in% mexico_city ]    <- 'Mexico city '
clean_candy$state[clean_candy$state %in% illinois]    <- 'Illinois'
clean_candy$state[clean_candy$state %in% colorado]    <- 'Colorado'
clean_candy$state[clean_candy$state %in% connecticut]    <- 'Connecticut'
clean_candy$state[clean_candy$state %in% ohio]    <- 'Ohio'
clean_candy$state[clean_candy$state %in% delaware]    <- 'Delaware'
clean_candy$state[clean_candy$state %in% new_mexico]    <- 'New Mexico'
clean_candy$state[clean_candy$state %in% florida]    <- 'Florida'
clean_candy$state[clean_candy$state %in% texas]    <- 'Texas'
clean_candy$state[clean_candy$state %in% hawaii]    <- 'Hawaii'
clean_candy$state[clean_candy$state %in% iowa]    <- 'Iowa'
clean_candy$state[clean_candy$state %in% idaho]    <- 'Idaho'
clean_candy$state[clean_candy$state %in% michigan]    <- 'Michigan'
clean_candy$state[clean_candy$state %in% tennessee]    <- 'Tennessee'
clean_candy$state[clean_candy$state %in% kansas]    <- 'Kansas'
clean_candy$state[clean_candy$state %in% kentucky]    <- 'Kentucky'
clean_candy$state[clean_candy$state %in% louisiana]    <- 'Louisiana'
clean_candy$state[clean_candy$state %in% massachusetts]    <- 'Massachusetts'
clean_candy$state[clean_candy$state %in% maryland]    <- 'Maryland'
clean_candy$state[clean_candy$state %in% mississippi]    <- 'Mississippi'
clean_candy$state[clean_candy$state %in% missouri]    <- 'Missouri'
clean_candy$state[clean_candy$state %in% montana]    <- 'Montana'
clean_candy$state[clean_candy$state %in% north_dakota]    <- 'North Dakota'
clean_candy$state[clean_candy$state %in% south_dakota ]    <- 'South Dakota '
clean_candy$state[clean_candy$state %in% new_hampshire]    <- 'New Hampshire'
clean_candy$state[clean_candy$state %in% new_jersey]    <- 'New Jersey'
clean_candy$state[clean_candy$state %in% pennsylvania ]    <- 'Pennsylvania '
clean_candy$state[clean_candy$state %in% nebraska]    <- 'Nebraska'
clean_candy$state[clean_candy$state %in% nevada]    <- 'Nevada'
clean_candy$state[clean_candy$state %in% oklahoma ]    <- 'Oklahoma'
clean_candy$state[clean_candy$state %in% oregon]    <- 'Oregon'
clean_candy$state[clean_candy$state %in% maine]    <- 'Maine'
clean_candy$state[clean_candy$state %in% puerto_rico ]    <- 'Puerto_Rico'
clean_candy$state[clean_candy$state %in% utah]    <- 'Utah'
clean_candy$state[clean_candy$state %in% vermont]    <- 'Vermont'
clean_candy$state[clean_candy$state %in% wisconsin]    <- 'Wisconsin'
clean_candy$state[clean_candy$state %in% wyoming]    <- 'Wyoming'
clean_candy$state[clean_candy$state %in% british_columbia]    <- 'British Columbia'
clean_candy$state[clean_candy$state %in% alberta]    <- 'Alberta'
clean_candy$state[clean_candy$state %in% new_brunswick ]    <- 'New Brunswick '
clean_candy$state[clean_candy$state %in% newfoundland_and_labrador]    <- 'Newfoundland and Labrador'
clean_candy$state[clean_candy$state %in% nova_Scotia]    <- 'Nova Scotia'
clean_candy$state[clean_candy$state %in% ontario]    <- 'Ontario'
clean_candy$state[clean_candy$state %in% quebec]    <- 'Quebec'
clean_candy$state[clean_candy$state %in% saskatchewan]    <- 'Saskatchewan'
clean_candy$state[clean_candy$state %in% yukon]    <- 'Yukon'
clean_candy$state[clean_candy$state %in% mecklenburg_vorpommern]    <- 'Mecklenburg Vorpommern'
clean_candy$state[clean_candy$state %in% hessen]    <- 'Hessen'
clean_candy$state[clean_candy$state %in% Baden ]    <- 'Baden-Württemberg'
clean_candy$state[clean_candy$state %in% ile_de_france ]    <- 'Ile de France '
clean_candy$state[clean_candy$state %in% south_east]    <- 'South East'
clean_candy$state[clean_candy$state %in% north_west]    <- 'North West'
clean_candy$state[clean_candy$state %in% east_midlands]    <- 'East Midlands'
clean_candy$state[clean_candy$state %in% scotland]    <- 'Scotland'
clean_candy$state[clean_candy$state %in% northern_ireland]    <- 'Northern Ireland'
clean_candy$state[clean_candy$state %in% greater_london]    <- 'Greater London'
clean_candy$state[clean_candy$state %in% north_brabant]    <- 'North Brabant'
clean_candy$state[clean_candy$state %in% new_south_wales]    <- 'New South Wales'
clean_candy$state[clean_candy$state %in% queensland]    <- 'Queensland'
clean_candy$state[clean_candy$state %in% changning_district]    <- 'Changning District'
clean_candy$state[clean_candy$state %in% Södermanland]    <- 'SÃ¶dermanland and Uppland'

# check
setNames(as.data.frame(table(clean_candy$state)), c("state", "n"))


# =========================================== First Insight: KITKAT & CANDY CORN IN TRICK OR TREATING =======================================

# filter the dataframe to Countries that responded most to the survey : United States, United Kingdom and Canada
# and show the appropriate age for trick or treating: from 1 to 15, then only show the male and female gender
filter_data    <- filter(clean_candy, (country == "United States" | country == "United Kingdom" |
                                         country == "Canada") & (age >= 1 & age <= 15) &
                           (gender == "Male"| gender == "Female"))

# create a function with two inputs: dataframe, column name
# the function remove the na values for a specific column
remove_na    <- function(dataframe, desiredColumn) {
  clean_na  <- complete.cases(dataframe[, desiredColumn])
  return(dataframe[clean_na, ])
}

# remove NA's values for the kitkat, candy_corn columns
candy_kitkat   <- remove_na(filter_data, 'kit kat')
candy_corn     <- remove_na(filter_data, 'candy corn')

# ploting a chart for The child feeling of receiving a KitKat in three different countries by gender
ggplot(candy_kitkat) +
  geom_bar(aes(x = gender, fill = `kit kat`)) +
  labs(title = "The Child Feeling Of Receiving A KitKat In Three Different Countries By Gender",
       x = "Countries & Gender") +
  guides(fill=guide_legend(title="Feelings")) +
  facet_wrap(~ country) 


## First of all, we can see from the plot that there are no female children in the United Kingdom nor males in Canada
## who have filled out the survey
## And the plot shows that the KitKat candy is popular candy bar between children for trick or treating in these countries
## Also, in the united states the number of females who feel joy when they are receiving the kitkat candy bar is higher 
## than males


# ploting a chart for The child feeling of receiving a Candy Corn in three different countries by gender
ggplot(candy_corn) +
  geom_bar(aes(x = gender, fill = `candy corn`)) +
  labs(title = "The Child Feeling Of Receiving A Candy Corn In Three Different Countries By Gender",
       x = "Countries & Gender") +
  guides(fill=guide_legend(title="Feelings")) +
  facet_wrap(~ country) 


## First of all, we can see from the plot that there are no female children in the United Kingdom nor males in Canada 
## who have filled out the survey
## And the plot shows that the candy corn is popular candy bar for the females in united states and Not popular for 
## the males 
## And for the unites kingdom the males don't care about the candy corn

# =============================================== Second Insight: MOST DAY PREFERABLE =======================================================

# create a new column to determine the age stages
age_data <- clean_candy %>% mutate(agegroup = case_when(age >= 5  & age <= 12 ~ 'Child',
                                                        age >= 13  & age <= 19 ~ 'Teen',
                                                        age >= 20  & age <= 39 ~ 'Adult',
                                                        age >= 40  & age <= 59 ~ 'Middle Age Adult',
                                                        age >= 60 ~ 'Senior Adult'))

# filter the age_data to only show the male and female gender
filter_age_data    <- filter(age_data, (gender == "Male"| gender == "Female"))

# remove NA's values form the day, agegroup columns
age_data_na_day   <- remove_na(filter_age_data , 'day')
age_data_na       <- remove_na(age_data_na_day , 'agegroup')

# ploting a chart for The Most Preferable Day For Each Age Stage By Gender
ggplot(age_data_na, aes(x = agegroup, fill = day)) +
  geom_bar(na.rm = TRUE, position = position_dodge()) + 
  labs(title = "The Most Preferable Day For Each Age Stage By Gender", x = "Age Stages & Gender") +
  guides(fill=guide_legend(title="Day")) +
  theme_minimal()+
  facet_wrap(~ gender)  


## As we can see from the plot that both genders in all age groups prefer Friday

# ========================================== Third Insight: Feelings when Receive Butterfinger ===============================================

img <- readJPEG("/Users/halmodarra/Desktop/butterfinger.jpeg") #Candy Corn background from clipart library
data_orig2 <- read_csv("/Users/halmodarra/Downloads/candyhierarchy20171.csv")
data2     <- data_orig2


# Clean candy names
colnames(data) <- colnames(data) %>%
  iconv(to = "ASCII//TRANSLIT") %>% #convert for special characters
  str_replace_all(c("\\\\xd5" = "",
                    "Q6 \\| |Q\\d{1,2}\\: " = "",
                    "\\?\\?" = "")) %>%
  str_trunc(39, side="right")
# Re-order levels of Candy Corn for future plotting purposes
data$`Butterfinger` <- factor(data$`Butterfinger`, levels = c("DESPAIR", "MEH","JOY", NA))

cc_pert <- table(data$`Butterfinger`, useNA = "ifany") %>%
  prop.table() %>% #ignoring NAs
  data.frame()
stderror <- function(p) {qnorm(.975) * sqrt(p * (1-p) / length(data$`Butter finger`))} #95% CI
cc_pert$se <- sapply(cc_pert$Freq, stderror)

g <- ggplot(cc_pert, aes(x = Var1, y = Freq))
g + annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf) +
  geom_col(fill = "#ffdb99") +
  geom_errorbar(data=cc_pert, aes(ymin = Freq-se, ymax = Freq+se), width = 0.15, color = "white") +
  geom_text(label = round(cc_pert$Freq, 3) * 100) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Feelings when Receive Butterfinger", x = "Feelings", y = "") +
  theme_minimal()

# ============================================== Fourth Insight: Most Seen Color of Dress Image =============================================

# Q10- Which color would people most see?
clean_candy %>% select(dress) %>% filter(!is.na(dress)) %>%
  ggplot(mapping = aes(dress)) + 
  geom_bar(stat="count", width = 0.5, fill="darkblue") + # stat >> to count the number of cases at x position
  labs(title = "Distribution of Most Seen Color", x = "Dress Color", y = "Number of People")+ 
  theme_bw() + # theme is a white background with grid lines
  theme(plot.title = element_text(size=16, hjust = 0.5), axis.text.x= element_text(size=10), # hjust to center the title
        axis.text.y= element_text(size=10), axis.title=element_text(size=14))

# Distribution of Most Seen Color based on Gender
clean_candy %>% select(dress, gender) %>% filter(!is.na(dress), !is.na(gender)) %>%
  ggplot(mapping = aes(dress, gender) ) + geom_bin2d() +
  scale_fill_continuous(type = "viridis") + theme_bw() + # viridis refers to the scale of colors
  labs(x="Dress Color", y="Gender", title="Distribution of Most Seen Color based on Gender")+  
  theme(plot.title = element_text(size=16, hjust = 0.5),axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10), axis.title=element_text(size=14))
# to check if the numbers correct or not
clean_candy %>% select(dress, gender) %>% filter(!is.na(dress), !is.na(gender)) %>%
  group_by(dress,gender) %>% count()

# Distribution of Most Seen Color based on Age
clean_candy %>% select(dress, age) %>% filter(!is.na(dress), !is.na(age), age != 4, age != 6, age != 7, age != 8, age != 9) %>%
  ggplot(aes(age,fill=dress))+
  geom_bar(stat="count",position='stack', width = 0.5) + # Stack for stacked chart
  labs(x="Age", y="Number of People", title="Distribution of Most Seen Color based on Age ")+ 
  theme_bw() +
  theme(plot.title = element_text(size=16, hjust = 0.5),axis.text.x= element_text(size=5,angle=90),
        axis.text.y= element_text(size=10), axis.title=element_text(size=14))

# =================================================== Fifth Insight: Most Clicked Websites ===================================================

# Q12- Which website would people most likely check out?

clicked_coordinates <- clean_candy %>% select(xy_coordinates = `click Coordinates (x, y)`) %>% 
  filter(!is.na(xy_coordinates)) %>% # to remove NA values
  separate(xy_coordinates, into = c("x", "y"), sep = ",") %>%  # separate x and y at ","
  mutate(x = str_replace(x, "\\(", "") %>% as.numeric()) %>% # remove right circle bracket
  mutate(y = str_replace(y, "\\)", "") %>% as.numeric()) %>% # remove left circle bracket
  mutate(Website = case_when(x<=50 & y <= 50 ~ "DAILY DISH", # assign the position for the coordinates
                             x<=50 & y > 50  ~ "ESPN",
                             x>50 & y <= 50 ~ "Science",
                             x>50 & y > 50  ~ "YAHOO!"))

Websites_img <- readPNG("/Users/nourah/Desktop/SDA/Week 5/5/news_websites.png") # image of the websites from the survey 
loc_img <- rasterGrob(Websites_img, interpolate=TRUE) # Render an image at the given location, interpolate the image linearly
img_ratio = 755/586 # to identify the size of the image

# to present the image before plotting the clicks on it 
clicked_coordinates %>% mutate(y_scaled = img_ratio*y) %>% 
  ggplot() + annotation_custom(loc_img) + # render the image
  # to scales for continuous x and y, image_ratio to identify the size
  # limits takes input and returns breaks as output
  scale_y_continuous(limits = c(-img_ratio*100, 0), expand = c(0, 0)) +  
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  theme_void() # to remove the number of coordinates


# to present the image after plotting the clicks on it 
clicked_coordinates %>% mutate(y_scaled = img_ratio*y) %>% # y_scaled to identify place of points
  ggplot(aes(x, -y_scaled, colour = Website)) +  # colored according to the quarters
  annotation_custom(loc_img, xmin=0, xmax=100, ymax=0, ymin=-img_ratio*100) + # x (0-100), y (-100,0)
  geom_point(alpha = 1, shape = 20, stroke = 1, size = 5) + # points' style
  scale_y_continuous(limits = c(-img_ratio*100, 0), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  # coord_fixed specified ratio between the representation of data on the axes
  coord_fixed() + theme_void() + 
  labs(title="Most Clicked Websites") +
  theme(plot.title = element_text(size=16, hjust = 0.5))
