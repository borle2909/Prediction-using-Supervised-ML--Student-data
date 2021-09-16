#Step-1 Reading the Given Data Set.

## Importing The CSV Data

student_data <- read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv", header = TRUE)
head(student_data)

summary(student_data)

#Step-2 Plotting the Given Data set.
## Plotting the given data

plot(x = student_data$Hours, y = student_data$Scores, xlab = "Hours", ylab = "Scores", main = "Score of student by study Hours", col = "Red")

#Step-3 Running Linear Regression on the data as there are only Two variables.
## Linear regression

student_data_regression <- lm(formula = Scores~Hours , data = student_data)
plot(x = student_data$Hours, y = student_data$Scores, xlab = "Hours", ylab = "Scores", main = "Score of student by study Hours", col = "red")
abline(student_data_regression, col= "Blue")

summary(student_data_regression)

#Step-4 Splitting Data into Test and Training Data
## Splitting Data Into Test and Training data

library(caTools) # package "caTools" is used for splitting the data

split = sample.split(Y = student_data$Scores, SplitRatio = 0.75)
training_set = subset(student_data, split == TRUE)
test_set = subset(student_data, split==FALSE)

training_set # checking the training data

test_set# checking the test data

#Step-5 Training the dataset
## training the dataset

result <- lm(formula = Scores~Hours, data = training_set)

summary(result)

result$coefficients

#Step-6 Using the Test Data to predict the outcome

pred <- predict(result, test_set)
head(pred) #printing the predicted result

head(test_set) # printing the head of test set to compare with predicted values

#Step-7 Calculating the Mean Absolute Error
##taking the head values of original test set

tset <- head(training_set)
tset

##taking the head values of predicted set

pset <- head(test_set)
pset

##Calculating Mean Absolute Error

library(ie2misc)

mae(pset$Hours, tset$Hours)

#Step-8 What will be predicted score if a student studies for 9.25 hrs/ day?

predicted_result <- predict(result, data.frame(Hours = 9.25))
predicted_result