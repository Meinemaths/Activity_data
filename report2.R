url_train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

read.csv(url_train)

setwd("C:/Users/corin/Desktop/coursera/course8/week4")
url_train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(url_train, destfile= ".train.csv")
download.file(url_test, destfile= ".test.csv")

train <- read.csv(".train.csv")
test <- read.csv(".test.csv")

# looking at the data of the test set, there are a lot of columns which were not filled. Due to this I got an error when I tried to predict the data. 

# For a quick and dirty way to get rid of this, I select the columns of the test and train set where not all the data is na:
train <- train[colSums(!is.na(test)) > 0]
test <- test[colSums(!is.na(test)) > 0]

# split the train data in a train and test set, so that we see how good the model is
set.seed(1)

library(caret)
n = nrow(train)
selection = sample(1:n, size = round(0.7*n), replace=FALSE)

training = train[selection, ]
testing = train[-selection, ]

er is een heleboel missende data. Met na.action = na.exclude worden die regels er wrs utigehaald, maar de uiteindelijke testset doet het dan niet.
Opties
- Aanvullen van de missende data
- de kolommen die in de testset niet gevuld zijn, uit de data halen. Dit is minder mooi maar wel zo praktisch

# modelling a simple decision tree
mytree <- train(classe ~ ., data = training, method = "rpart", na.action = na.exclude)

# modelling knn
myknn <- train(classe ~ ., data = training, method = "knn", na.action = na.exclude)


# predict the decision tree with the testing data
testing$outcome_tree <- predict(mytree, testing)

# this tree can predict classe A and B pretty good, C, D and E are going wrong
table(testing$classe, testing$outcome_tree)

# predict knn with the testing data
testing$outcome_knn <- predict(myknn, testing)

# knn does not predict the classes very well, but C, D and E are going better with knn then with trees
table(testing$classe, testing$outcome_knn)


outcome_combined <- ifelse(testing$outcome_tree == "A" | testing$outcome_tree == "B", testing$outcome_tree, testing$outcome_knn)

#deze duurt een paar uur
myrf <- train(classe ~ ., data = training, method = "rf", na.action = na.exclude)

#gaat snel
mytree <- train(classe ~ ., data = training, method = "rpart", na.action = na.exclude)

#dit duurt een half uurtje
myknn <- train(classe ~ ., data = training, method = "knn", na.action = na.exclude)


# mygbm <- train(classe ~ ., data = training, method = "gbm", na.action = na.exclude)

# die doet het niet
mynn <- train(classe ~ ., data = training, method = 'neuralnet', na.action = na.exclude)

outcome_rf <- predict(myrf, testing)
outcome_tree <- predict(mytree, testing)
outcome_knn <- predict(myknn, testing)
outcome_gbm <- predict(mygbm, testing)


# A en B gaan vrij goed, het model kan geen onderscheid tussen C, D en E maken.
table(testing$classe, outcome_tree)

# hier gaat het niet heel goed met de naastgelegen classes
table(testing$classe, outcome_knn)


mytree2 <- train(classe ~ ., data = training, method = "rpart")

# looking at the data of the train and test set, there are a lot of columns on the test set which are not filled




train2 <- train %>% select_if(is.numeric)
train2$classe <- train$classe
train2$user_name <- train$user_name

test2 <- test %>% select_if(is.numeric)
test2$classe <- test$classe
test2$user_name <- test$user_name


myrf <- train(classe ~ ., data = train2, method = "rf", na.action = na.exclude)
mytree <- train(classe ~ ., data = train, method = "rpart", na.action = na.exclude)

myknn <- train(classe ~ ., data = train, method = "knn", na.action = na.exclude)

mytest <- train(classe ~ magnet_forearm_x + magnet_forearm_y + magnet_forearm_z, data = train, method = "rpart", na.action = na.exclude)


myglm <- glm(classe~., data = train2, na.action = na.exclude)



test$outcome <- predict(mytest, test)



table(train$classe)

fit_glm <- glm(classe ~ ., data = train)
fit_rf <- train(classe ~., data = train, model = "rf")

plot(train$accel_belt_x, train$accel_forearm_x, col = train$classe)

library(dplyr)
# throw away all the variables with more than half of the lines is div/0


train2 <- train %>% select_if(is.numeric)
train2$classe <- train$classe
train2$user_name <- train$user_name

library(caret)
myrf <- train(classe ~ ., data = train2, method = "rf", na.action = na.exclude)

myglm <- glm(classe~., data = train2, na.action = na.exclude)


% plot(train$pitch_belt, train$roll_arm, col = train$classe)
