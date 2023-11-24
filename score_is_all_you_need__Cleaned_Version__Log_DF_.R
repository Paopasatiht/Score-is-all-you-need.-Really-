# Install from CRAN
#Insatll required packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages('bestglm')
install.packages('MASS')
install.packages("caTools")
install.packages('caret')


#Import required library
library(caret)
library(caTools)
library(tidyverse)
library(dplyr)
library(MASS)
library(bestglm)

#make this example reproducible
set.seed(1)
####################################################################
# Step 0 : Import data to the note
data <- read_csv("./morning_star_score_filter_1.csv")
# Explore data
head(data, 5)
colnames(data)


####################################################################
# Step 1 : Data Manipulation
# 1.1 remove outlier
outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(data, cols = names(data)) {
  for (col in cols) {
    data <- data[!outliers(data[[col]]),]
  }
  data
}

data <- remove_outliers(data, c('equity_style_score', 
                                'equity_size_score', 
                                'environmental_score',
                                'social_score',
                                'governance_score',
                                'sustainability_score'))




# 1.2 Split the data
#use 70% of dataset as training set and 30% as test set
sample <- sample.split(data$fund_return_2020_q3, SplitRatio = 0.7)
train  <- subset(data, sample == TRUE)
test   <- subset(data, sample == FALSE)

# replace for the code will not change a lot
data <- train


####################################################################
# 2. Explore the train set
#boxplot compare between style_score and fund_return
boxplot(data$equity_style_score~data$fund_return_2020_q3,
        main= "Box plot for morningstar",
        ylab = "equity_style_score",
        xlab = "fund_return_2020_q3",
        col="purple")

#boxplot compare between size_score and fund_return
boxplot(data$equity_size_score~data$fund_return_2020_q3,
        main= "Box plot for morningstar",
        ylab = "equity_size_score~data",
        xlab = "fund_return_2020_q3",
        col="red")

#boxplot compare between social_score and fund_return
boxplot(data$social_score~data$fund_return_2020_q3,
        main= "Box plot for morningstar",
        ylab = "social_score",
        xlab = "fund_return_2020_q3",
        col="cyan")

#boxplot compare between governance_score and fund_return
boxplot(data$governance_score~data$fund_return_2020_q3,
        main= "Box plot for morningstar",
        ylab = "governance_score",
        xlab = "fund_return_2020_q3",
        col="orange")

#boxplot compare between sustainability_score and fund_return
boxplot(data$sustainability_score~data$fund_return_2020_q3,
        main= "Box plot for morningstar",
        ylab = "sustainability_score",
        xlab = "fund_return_2020_q3",
        col="green")
####################################################################
# Function to log model performance
log_model_performance <- function(name, model) {
  
  # Calculate AIC score
  aic_score <- model$aic
  
  # Create a new row for the performance log dataframe
  new_row <- data.frame(
    Model = name,
    AIC_Score = aic_score,
    stringsAsFactors = FALSE
  )
  
  # Return the new row
  return(new_row)
}

# Create an empty dataframe or use an existing one
performance_log <- data.frame(
  Model = character(),
  AIC_Score = numeric(),
  stringsAsFactors = FALSE
)

# Print the updated performance_log dataframe
print(performance_log)

####################################################################
# Step 3 : Fitting the model
# : Baseline
# : Quadratic tearms explore


# model Base line
model1 <- glm(fund_return_2020_q3 ~ equity_style_score + equity_size_score + environmental_score + social_score + governance_score + sustainability_score , 
              data = data,
              family="binomial")

performance_row <- log_model_performance("baseline_model 1", model1)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)


#quadratic #test 
model_quad <- glm(formula = fund_return_2020_q3 ~ equity_style_score + I(equity_style_score^2) + 
                    equity_size_score + 
                    environmental_score + 
                    social_score + 
                    governance_score + 
                    sustainability_score, 
                  family = "binomial", 
                  data = data) 

performance_row <- log_model_performance("model_quad 0", model_quad)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad1 <- glm(formula = fund_return_2020_q3 ~ equity_style_score + I(equity_style_score^2) + I(equity_style_score^3) +
                    equity_size_score + 
                    environmental_score + 
                    social_score + 
                    governance_score + 
                    sustainability_score, 
                  family = "binomial", 
                  data = data) 

performance_row <- log_model_performance("model_quad1", model_quad1)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad2 <- glm(formula = fund_return_2020_q3 ~ equity_style_score + I(equity_style_score^2) + I(equity_style_score^3) + I(equity_style_score^4) +
                                         equity_size_score + 
                                         environmental_score + 
                                         social_score + 
                                         governance_score + 
                                         sustainability_score, 
                                       family = "binomial", 
                                       data = data) 

performance_row <- log_model_performance("model_quad2", model_quad2)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad3 <- glm(formula = fund_return_2020_q3 ~ equity_style_score + I(equity_style_score^2) + I(equity_style_score^3) + I(equity_style_score^4) + I(equity_style_score^5) +
                   equity_size_score + 
                     environmental_score + 
                     social_score + 
                     governance_score + 
                     sustainability_score, 
                   family = "binomial", 
                   data = data) 

performance_row <- log_model_performance("model_quad3", model_quad3)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)



model_quad4 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                     equity_size_score + I(equity_size_score^2) + 
                      environmental_score + 
                     social_score + 
                    governance_score + 
                     sustainability_score, 
                   family = "binomial", 
                  data = data) 

performance_row <- log_model_performance("model_quad4", model_quad4)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad5 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                     equity_size_score + I(equity_size_score^2) + I(equity_size_score^3) + 
                     environmental_score + 
                     social_score + 
                     governance_score + 
                     sustainability_score, 
                   family = "binomial", 
                   data = data) 

performance_row <- log_model_performance("model_quad5", model_quad5)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad6 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                     equity_size_score + I(equity_size_score^2) + I(equity_size_score^3) + I(equity_size_score^4) +
                     environmental_score + 
                     social_score + 
                     governance_score + 
                     sustainability_score, 
                   family = "binomial", 
                   data = data) 

performance_row <- log_model_performance("model_quad6", model_quad6)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad7 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                     equity_size_score + I(equity_size_score^2) + I(equity_size_score^3) + I(equity_size_score^4) + I(equity_size_score^5) +
                     environmental_score + 
                     social_score + 
                     governance_score + 
                     sustainability_score, 
                   family = "binomial", 
                   data = data) 

performance_row <- log_model_performance("model_quad7", model_quad7)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad8 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                     equity_size_score + 
                     environmental_score + I(environmental_score^2) + 
                     social_score + 
                     governance_score + 
                     sustainability_score, 
                   family = "binomial", 
                   data = data) 

performance_row <- log_model_performance("model_quad8", model_quad8)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad9 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                     equity_size_score + 
                     environmental_score + I(environmental_score^2) + I(environmental_score^3) + 
                     social_score + 
                     governance_score + 
                     sustainability_score, 
                   family = "binomial", 
                   data = data) 

performance_row <- log_model_performance("model_quad9", model_quad9)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad10 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                     equity_size_score + 
                     environmental_score + I(environmental_score^2) + I(environmental_score^3) +  I(environmental_score^4) + 
                     social_score + 
                     governance_score + 
                     sustainability_score, 
                   family = "binomial", 
                   data = data) 

performance_row <- log_model_performance("model_quad10", model_quad10)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad11 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                      equity_size_score + 
                      environmental_score + I(environmental_score^2) + I(environmental_score^3) +  I(environmental_score^4) +  I(environmental_score^5) +
                      social_score + 
                      governance_score + 
                      sustainability_score, 
                    family = "binomial", 
                    data = data) 

performance_row <- log_model_performance("model_quad11", model_quad11)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad12 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                     equity_size_score + 
                     environmental_score + 
                     social_score + I(social_score^2) +
                     governance_score + 
                     sustainability_score, 
                   family = "binomial", 
                   data = data) 

performance_row <- log_model_performance("model_quad12", model_quad12)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad13 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                      equity_size_score + 
                      environmental_score + 
                      social_score + I(social_score^2) + I(social_score^3) + 
                      governance_score + 
                      sustainability_score, 
                    family = "binomial", 
                    data = data) 

performance_row <- log_model_performance("model_quad13", model_quad13)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad14 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                      equity_size_score + 
                      environmental_score + 
                      social_score + I(social_score^2) + I(social_score^3) + I(social_score^4) + 
                      governance_score + 
                      sustainability_score, 
                    family = "binomial", 
                    data = data) 

performance_row <- log_model_performance("model_quad14", model_quad14)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad15 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                      equity_size_score + 
                      environmental_score + 
                      social_score + I(social_score^2) + I(social_score^3) + I(social_score^4) + I(social_score^5) + 
                      governance_score + 
                      sustainability_score, 
                    family = "binomial", 
                    data = data) 

performance_row <- log_model_performance("model_quad15", model_quad15)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)


model_quad16 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                     equity_size_score + 
                     environmental_score + 
                     social_score + 
                     governance_score + I(governance_score^2) +
                     sustainability_score, 
                     family = "binomial", 
                   data = data) 

performance_row <- log_model_performance("model_quad16", model_quad16)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad17 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                      equity_size_score + 
                      environmental_score + 
                      social_score + 
                      governance_score + I(governance_score^2) + I(governance_score^3) +
                      sustainability_score, 
                    family = "binomial", 
                    data = data) 

performance_row <- log_model_performance("model_quad17", model_quad17)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad18 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                      equity_size_score + 
                      environmental_score + 
                      social_score + 
                      governance_score + I(governance_score^2) + I(governance_score^3) + I(governance_score^4) +
                      sustainability_score, 
                    family = "binomial", 
                    data = data) 

performance_row <- log_model_performance("model_quad18", model_quad18)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad19 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                      equity_size_score + 
                      environmental_score + 
                      social_score + 
                      governance_score + I(governance_score^2) + I(governance_score^3) + I(governance_score^4) + I(governance_score^5) +
                      sustainability_score, 
                    family = "binomial", 
                    data = data) 

performance_row <- log_model_performance("model_quad19", model_quad19)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)


model_quad20 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                     equity_size_score + 
                     environmental_score + 
                     social_score + 
                     governance_score + 
                     sustainability_score + I( sustainability_score^2 ),
                   family = "binomial", 
                   data = data) 

performance_row <- log_model_performance("model_quad20", model_quad20)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad21 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                      equity_size_score + 
                      environmental_score + 
                      social_score + 
                      governance_score + 
                      sustainability_score + I( sustainability_score^2 ) + I( sustainability_score^3 ),
                    family = "binomial", 
                    data = data) 

performance_row <- log_model_performance("model_quad21", model_quad21)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)

model_quad22 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                      equity_size_score + 
                      environmental_score + 
                      social_score + 
                      governance_score + 
                      sustainability_score + I( sustainability_score^2 ) + I( sustainability_score^3 ) + I( sustainability_score^4 ) , 
                    family = "binomial", 
                    data = data) 

performance_row <- log_model_performance("model_quad22", model_quad22)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)


model_quad23 <- glm(formula = fund_return_2020_q3 ~ equity_style_score +  
                      equity_size_score + 
                      environmental_score + 
                      social_score + 
                      governance_score + 
                      sustainability_score + I( sustainability_score^2 ) + I( sustainability_score^3 ) + I( sustainability_score^4 )  + I( sustainability_score^5 ), 
                    family = "binomial", 
                    data = data) 

performance_row <- log_model_performance("model_quad23", model_quad23)
performance_log <- rbind(performance_log, performance_row)
print(performance_log)
####################################################################

# step 4 Sort the dataframe by the 'Score' column in descending order
your_data_sorted <- performance_log %>%
  arrange(AIC_Score)

# Print the sorted dataframe
print(your_data_sorted)


####################################################################

log_model_accuracy <- function(name, test_pred, ratio, test_label) {
  
  demo <- table(test_pred > ratio,test_label$fund_return_2020_q3)
  
  # Calculate True Positives, True Negatives, and Total Observations
  TP <- demo[2, 2]  # True Positives
  TN <- demo[1, 1]  # True Negatives
  Total <- sum(demo)  # Total Observations
  
  # Calculate Accuracy
  accuracy <- (TP + TN) / Total
  
  # Create a new row for the performance log dataframe
  new_row <- data.frame(
    Model = name,
    Cut_off = ratio,
    Accuracy = accuracy,
    stringsAsFactors = FALSE
  )
  
  # Return the new row
  return(new_row)
}

# Create an empty dataframe or use an existing one
accracy_log <- data.frame(
  Model = character(),
  Cut_off = numeric(),
  Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# Print the updated performance_log dataframe
print(accracy_log)

########################### Prediction For Evaluation ###############
test_filter <- test[,c('equity_style_score', 
        'equity_size_score', 
        'environmental_score',
        'social_score',
        'governance_score',
        'sustainability_score')]


# Predict the new data
Test_pred <- predict(model_quad11, 
                                 newdata = test_filter,
                                 type = 'response')

accracy_row <- log_model_accuracy("model_quad11", Test_pred, 0.5, test)
accracy_log <- rbind(accracy_log, accracy_row)
print(accracy_log)

accracy_row <- log_model_accuracy("model_quad11", Test_pred, 0.7, test)
accracy_log <- rbind(accracy_log, accracy_row)
print(accracy_log)

accracy_row <- log_model_accuracy("model_quad11", Test_pred, 0.9, test)
accracy_log <- rbind(accracy_log, accracy_row)
print(accracy_log)



# Predict the new data
Test_pred <- predict(model_quad19, 
                     newdata = test_filter,
                     type = 'response')

accracy_row <- log_model_accuracy("model_quad19", Test_pred, 0.5, test)
accracy_log <- rbind(accracy_log, accracy_row)
print(accracy_log)

accracy_row <- log_model_accuracy("model_quad19", Test_pred, 0.7, test)
accracy_log <- rbind(accracy_log, accracy_row)
print(accracy_log)

accracy_row <- log_model_accuracy("model_quad19", Test_pred, 0.9, test)
accracy_log <- rbind(accracy_log, accracy_row)
print(accracy_log)



# Predict the new data
Test_pred <- predict(model_quad18, 
                     newdata = test_filter,
                     type = 'response')


accracy_row <- log_model_accuracy("model_quad18", Test_pred, 0.5, test)
accracy_log <- rbind(accracy_log, accracy_row)
print(accracy_log)

accracy_row <- log_model_accuracy("model_quad18", Test_pred, 0.7, test)
accracy_log <- rbind(accracy_log, accracy_row)
print(accracy_log)

accracy_row <- log_model_accuracy("model_quad18", Test_pred, 0.9, test)
accracy_log <- rbind(accracy_log, accracy_row)
print(accracy_log)


#########################################################################
# step 6 Sort the dataframe by the 'Score' column in descending order
your_data_sorted <- accracy_log %>%
  arrange(desc(Accuracy))

# Print the sorted dataframe
print(your_data_sorted)



#########################################################################

# e ) Prediction
new_data <- data.frame(
  equity_style_score = -51.42,
  equity_size_score = 78,
  environmental_score = 0.07,
  social_score = 2.98,
  governance_score = 4.53,
  sustainability_score = 10.43
)


# Predict the new data
predicted_probability <- predict(model_quad19, 
                     newdata = new_data,
                     type = 'response')


# interpretation
result <- ifelse(predicted_probability > 0.5, 
                 "Model Predicted : This fund will be Profit", 
                 "Model Predicted : This fund will be Loss")

print(result)



