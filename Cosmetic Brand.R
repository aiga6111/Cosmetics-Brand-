df = read.csv('C://Users//Lorenzo//Desktop//Facebook Dummy.csv')

head(df)

colnames(dfclean)[1] <- 'Page.total.likes'
dfclean = na.omit(df)
head(dfclean)

library(psych)
describe(dfclean)
str(dfclean)
dfclean = subset(dfclean, select = -c(comment,like,share))


dfclean$link= ifelse(dfclean$Type=="Link",1,0)
dfclean$Photo= ifelse(dfclean$Type=="Photo",1,0)
dfclean$Status= ifelse(dfclean$Type=="Status",1,0)
dfclean$Video= ifelse(dfclean$Type=="Video",1,0)


cor(dfclean)
head(dfclean)

maxs <- apply(dfclean, 2, max)
mins <- apply(dfclean, 2, min)

df.scaled <- scale(dfclean, center = mins, scale = maxs - mins)
head(df.scaled)

results <- data.frame(df.scaled)
head(results)

lm1 = lm(Total.Interactions~.,data=results)
summary(lm1)

notsignificant = c('Post.ID..','Page.total.likes','Category', 'Post.Month', 'Post.Weekday','Post.Hour','Paid','Lifetime.Post.Consumers',
                   'Lifetime.Post.Consumptions ')

lm2 = lm(Total.Interactions~.-Post.ID..-Page.total.likes-Category-Post.Month-Post.Weekday-Post.Hour-Paid-Lifetime.Post.Consumers-Lifetime.Post.Consumptions,data = results)
summary(lm2)

head(dfclean)
#tree
library(tree)
tree.df = tree(Total.Interactions~.,data=dfclean)

colSums(is.na(dfclean))

dfclean = na.omit(df)

plot(tree.df)
text(tree.df)

summary(tree.df)

#predict the class of each observation
tree.pred <- predict(tree.df)

#building a larger tree
tree.df = tree(Total.Interactions~ . -like ,data=dfclean, control = tree.control(nobs = nrow(dfclean), mindev = 0.001))

tree.df

summary(tree.df)

plot(tree.df)
text(tree.df, pretty = 0)


#aileen tree

tree.state <- tree(Total.Interactions~.-like-comment-share-Lifetime.Engaged.Users-Lifetime.Post.Consumptions-Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,data=dfclean)
plot(tree.state)
text(tree.state)


tree.appt <- tree(Total.Interactions~.-like-comment-share-Lifetime.Engaged.Users-Lifetime.Post.Consumptions-Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,data=dfclean, 
                  control = tree.control(nobs = nrow(dfclean), mindev = 0.01))
plot(tree.appt)
text(tree.appt, pretty = 0)


#validation set approach

set.seed(200)
train <- sample(nrow(dfclean), 0.6 * nrow(df))
appt.test <- appt[-train, ]
tree.train <- tree(Status ~ . - MRN, data = appt, subset = train, 
                   control = tree.control(nobs = length(train), mindev = 0.001))