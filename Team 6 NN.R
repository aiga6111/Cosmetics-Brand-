#setwd("~/Desktop/Spring 2021/APRD6343 Advanced Advertising Stats")

library(psych)
library(neuralnet)

fb.raw <- read.csv("dataset_Facebook-1.csv")

#EXPLORATORY
View(fb.raw)
head(fb.raw)
describe(fb.raw)
summary(fb.raw)

length(fb.raw) # 19 Columns in the data.
length(fb.raw$Page.total.likes)
length(fb.raw$Post.Month)  # 500 rows in the data.

#RENAME
install.packages('tidyverse')
library(tidyverse)
names(fb.raw)[8] <- "LPoTotReach" #Lifetime.Post.Total.Reach
names(fb.raw)[9] <- "LPoTotImp" #Lifetime.Post.Total.Impressions
names(fb.raw)[10] <- "LEUsers" #Lifetime.Engaged.Users
names(fb.raw)[11] <- "LPCon" #Lifetime.Post.Consumers
names(fb.raw)[12] <- "LPCons" #Lifetime.Post.Consumptions
names(fb.raw)[13] <- "LPIBPWHLYP" #Lifetime.Post.Impressions.by.people.who.have.liked.your.Page
names(fb.raw)[14] <- "LPRBWLYP" #Lifetime.Post.reach.by.people.who.like.your.Page
names(fb.raw)[15] <- "LPWHLYPEWYP" #Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post
View(fb.raw)

sum(is.na(fb.raw)) # We have 6 NULL values.
fb.clean <- na.omit(fb.raw) # fb_clean removed 5 rows with missing/NULL values.
row.names(fb.clean) <- NULL # Restarts the numbering on the row index.
tail(fb.clean) # The last row is 495 so we know we did the previous step right.

fb.clean$link= ifelse(fb.clean$Type=="Link",1,0)
fb.clean$Photo= ifelse(fb.clean$Type=="Photo",1,0)
fb.clean$Status= ifelse(fb.clean$Type=="Status",1,0)
fb.clean$Video= ifelse(fb.clean$Type=="Video",1,0)


#NEURAL NETWORK
set.seed(5415) # Set random state.

  #Scale
fb.clean1 <- fb.clean[, c( "Total.Interactions","Category","LPoTotReach", 
                          "Post.Weekday", "Post.Month", "Post.Hour", 
                          "Page.total.likes",
                          "LPoTotImp","LEUsers", "LPCon",
                          "LPCons", "LPIBPWHLYP",
                          "LPRBWLYP", "LPWHLYPEWYP", "link", "Photo", "Status", "Video")]

maxs <- apply(fb.clean1, 2, max)
mins <- apply(fb.clean1, 2, min)
fb.scaled <- scale(fb.clean1, center = mins, scale = maxs - mins)
View(fb.scaled)
describe(fb.scaled)

#Test/Train
train_test_split_index <- 0.8 * nrow(fb.scaled)
train <- fb.scaled[1:train_test_split_index,]
test <- fb.scaled[(train_test_split_index+1): nrow(fb.scaled),]

fb.net_train <- neuralnet(Total.Interactions ~  Category + LPoTotReach + 
                           Post.Weekday + Post.Month + Post.Hour + Page.total.likes
                          + LPoTotImp + LEUsers + LPCon
                          + LPCons + LPIBPWHLYP
                          + LPRBWLYP + LPWHLYPEWYP + link + Photo + Status + Video, 
                          data = train, hidden = 3)
plot(fb.net_train)
fb.net_train$result.matrix

#fb.net_test <- neuralnet(Total.Interactions ~  Category + LPoTotReach + 
#                            Post.Weekday + Post.Month + Post.Hour + Page.total.likes
#                          + LPoTotImp + LEUsers + LPCon
#                          + LPCons + LPIBPWHLYP
#                          + LPRBWLYP + LPWHLYPEWYP, 
#                           data = test, hidden = 3)
#plot(fb.net_test)
#fb.net_test$result.matrix

#Plots
result_test <- subset(test, select = c( "Category","LPoTotReach", 
                                       "Post.Weekday", "Post.Month", "Post.Hour", 
                                       "Page.total.likes",
                                       "LPoTotImp","LEUsers", "LPCon",
                                       "LPCons", "LPIBPWHLYP",
                                       "LPRBWLYP", "LPWHLYPEWYP", "link", "Photo", "Status", "Video"))
head(result_test)
fb4 <- compute(fb.net_train, result_test)

results <- data.frame(actual = test[,1], prediction = fb4$net.result)
results
plot(results)
abline(0,1)


#Making predictions using the neural network results.
predict_train <- predict(fb.net_train, fb.scaled)
#predict_test <- predict(fb.net_test, fb.scaled)
(predict_train)
#(predict_test)

#Compute the mean squared error for the prediction.
install.packages('Metrics')
library(Metrics)
rmse(test[,1], fb4$net.result)




