#Pima Indians Diabetes 
#A data frame with 768 observations on 9 variables.

#Load the packages 

library(mlbench)
library(tidyverse)
library(caTools)
library(MASS)
library(gmodels)


#Load the data
data("PimaIndiansDiabetes2")
df <- PimaIndiansDiabetes2


# inspect the data --------------------------------------------------------

head(df)
str(df)
summary(df)

#Remove all NAs and keep all 9 features/variables
#we sacrifice  half of our dataset 

df_2 <- na.omit(df)


#Alternatively we could remove the two variables with the highest frequency of NAs
#remove inuslin and triceps and do a separate analyses.
#We would be left with a lot more data

df_3 <- df
df_3$triceps <- NULL
df_3$insulin <- NULL
df_3 <- na.omit(df_3)

#However we will proceed with the first option


#  Explorotary Analysis --------------------------------------------------

plot(df_2)
table(df_2$diabetes)

#Implement Histograms
#age,pregnant and insulin columns are skewed

par(mfrow = c(3,3))
for (i in 1:8) {
  hist(df_2[,i], main = colnames(df_2)[i], 
       xlab = colnames(df_2)[i], col = "red")
}


#Implement Boxplots
#There are outliers for age,pregnant and insulin columns

par(mfrow = c(3,3))
for (i in 1:8) {
  boxplot(df_2[,i]~diabetes, ylab = colnames(df_2)[i], col = "red", xlab = "diabetes", 
          data = df_2)
}

#the older the patient the more likely he/she is positive
par(mfrow = c(3,3))
for (i in 1:7) {
  plot(df_2$age, df_2[,i], 
       main = colnames(df_2)[i], 
       ylab  = colnames(df_2)[i],
       xlab = "Age", col = df_2$diabetes)
}


# Transforming Features ---------------------------------------------------

summary(df_2$age)
df_2$age_cap <- as.factor(ifelse(df_2$age<=24, "20-24",
                                  ifelse(df_2$age<=30,"25-30",
                                         ifelse(df_2$age<=40, "31-40",
                                                ifelse(df_2$age<=50,"41-50", "50+")))))


summary(df_2$pregnant)
df_2$pregnant_cap <- as.factor(ifelse(df_2$pregnant==0, "0",
                                       ifelse(df_2$pregnant<=2, "1-2",
                                              ifelse(df_2$pregnant<=5,"3-5",
                                                     ifelse(df_2$pregnant<=10,"6-10", "10+")))))

summary(df_2$insulin)
df_2$insulin_cap <- as.factor(ifelse(df_2$insulin<=75, "0-75",
                                      ifelse(df_2$insulin<=150, "76-150",
                                             ifelse(df_2$insulin<=180,"151-180",
                                                    ifelse(df_2$insulin<=400,"181-400", "401+")))))

#Check for equal distibutions
table(df_2$diabetes,df_2$insulin_cap)
table(df_2$diabetes,df_2$pregnant_cap)
table(df_2$diabetes, df_2$age_cap)


#Remove columns
df_2[,c("age","insulin","pregnant")] <- NULL



# Further Exploratory Analysis --------------------------------------------

ggplot(df_2, aes(x = age_cap, fill = diabetes)) +
  geom_bar() +
  xlab("Age_cap") +
  ylab("Count") +
  labs(fill = "diabetes")

#the majority of negatives are from people aged 20-30
ggplot(df_2, aes(x = glucose, y = diabetes, color = age_cap)) +
  geom_jitter(width = 0, height = 0.15, alpha = 0.5)


ggplot(df_2 ,aes(x = mass, y = diabetes, color = insulin_cap))+
  geom_jitter(width = 0, height = 0.15, alpha = 0.5)


#create training and test set -------------------------------------------

splitcc <- sample.split(df_2, SplitRatio = 0.7)
train <- subset(df_2, splitcc == "TRUE")
test <- subset(df_2, splitcc == "FALSE")


# Testing Models GLM ------------------------------------------------------

#Minimum accuracy to beat
table(df_2$diabetes)
minimum <- 262/394

#Implement General Linear Model
#glucose and age_cap appear to be good predictors according to this model
glm_1 <- glm(data = train, diabetes ~., family = "binomial")
summary(glm_1)

# Variable selection
#use stepAIC to select the best model (minimum AIC value)
glm_2 <- stepAIC(glm_1)
summary(glm_2)

#Further Analysis of the model
summary(glm_2$fitted.values)

par(mfrow = c(1,1))
hist(glm_2$fitted.values,main = " Histogram ",
     xlab = "Probability of Positive diabetes", col = "blue")


#Classify the prediction as pos IF > 0.5 (threshold = 0.5)

train$predicted <- ifelse(glm_2$fitted.values > 0.5,
                          "pos","neg")

#Compare with actual data
#Accuracy % 79.3
CrossTable(x = train$diabetes, y = train$predicted, prop.chisq = FALSE)




# Graphing the predicted probabilities ------------------------------------

visual <- data.frame(
  probability_of_diabetes = glm_2$fitted.values,
  diabetes = train$diabetes
)

visual <- visual[
  order(visual$probability_of_diabetes, decreasing = FALSE),
  ]

visual$rank <- 1:nrow(visual)

#the model seems to classify the majority of the values correctly 
#however there is always room for improvement (i.e better feature engineering)

ggplot(visual,aes(x=rank, y= probability_of_diabetes)) +
  geom_point(aes(color = diabetes), alpha = 1, shape = 4, stroke = 2)+
  xlab("index")+
  ylab("Predicted probablity of getting diabetes")



# Apply the model on test set ---------------------------------------------

test$predicted <- predict.glm(glm_2, newdata = test, type = "response")
test$predicted <- ifelse(test$predicted > 0.5,"pos","neg")

#Compare with actual data
#Accuracy % 82.4 (better than our training set accuracy)

CrossTable(x = test$diabetes, y = test$predicted, prop.chisq = FALSE)





