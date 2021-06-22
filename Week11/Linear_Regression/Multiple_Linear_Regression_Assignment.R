#----------------------------------------------------Multiple Linear Regression-------------------------------------------------------

# Load dataset
dataset = read.csv('https://raw.githubusercontent.com/Yasmeenmad/data_science_bootcamp/main/Week11/Linear_Regression/BostonHousing.csv')

View(dataset)

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$medv, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

View(training_set)
View(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = medv ~ .,
               data = training_set)

# coefficients summary
summary(regressor)

# To get the coefficients 
regressor$coefficients

# Predicting the Test set results
y_pred <- predict(regressor, newdata = test_set)
y_actual <- test_set$medv
error <- y_pred - test_set$medv
percent_error <- abs(error)/y_pred
percent_error <- round(percent_error,2)
df_multi <- data.frame(y_pred, y_actual, error, percent_error)
View(df_multi)

# Lets try building our model with  only two columns "rm" and "medv"
avg_num_room <- dataset$rm
profits <- dataset$medv
new_dataset <- data.frame(avg_num_room, profits)
View(new_dataset)

set.seed(123)

split = sample.split(new_dataset$profits, SplitRatio = 0.8)
simple_training_set = subset(new_dataset, split == TRUE)
simple_test_set = subset(new_dataset, split == FALSE)

View(simple_training_set)
View(simple_test_set)

simple_regressor = lm(formula = profits ~ avg_num_room,
               data = simple_training_set)

summary(simple_regressor)

simple_y_pred <- predict(simple_regressor, newdata = simple_test_set)

simple_error <- simple_y_pred - simple_test_set$profits
simple_percent_error <- abs(simple_error)/simple_y_pred

simple_percent_error <- round(simple_percent_error,2)

comparison_df <- data.frame(df_multi, simple_y_pred, simple_percent_error)
View(comparison_df)


# Visualizing the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = simple_training_set$avg_num_room, y = simple_training_set$profits)) +
  geom_line(aes(x = simple_training_set$avg_num_room, y = predict(simple_regressor, newdata = simple_training_set))) +
  ggtitle('Profits vs Average Number Of Rooms Per Dwelling (Training set)') +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('Average Number Of Rooms Per Dwelling') +
  ylab('Profits')

# Visualizing the Test set results
ggplot() +
  geom_point(aes(x = simple_test_set$avg_num_room, y = simple_test_set$profits)) +
  geom_line(aes(x = simple_training_set$avg_num_room, y = predict(simple_regressor, newdata = simple_training_set))) +
  ggtitle('Profits vs Average Number Of Rooms Per Dwelling (Test set)') + 
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('Average Number Of Rooms Per Dwelling') +
  ylab('Profits')
