# Load the package
library(rpart)
library(e1071)
library(rpart)
library(dbscan)
library(fpc)
library(factoextra)

# Loading dataset mushrooms into R
mushrooms <- read.csv("mushrooms.csv")
smp_size <-  nrow(mushrooms) * 2 / 3

## Set the seed to make your partition reproductible
set.seed(123)

train_ind <- sample(seq_len(nrow(mushrooms)), size = smp_size)

train_mushrooms <- mushrooms[train_ind, ]
test_mushrooms <- mushrooms[-train_ind, ]

# Naive Bayes Theory
model <- naiveBayes(test_mushrooms$class ~ . , data = test_mushrooms)

pred <- predict(model, newdata = test_mushrooms, type = 'class')
p <- table(pred, test_mushrooms$class)

accuracy_naive <- sum(p[row(p) == col(p)]) / sum(p)

sprintf("Percentage dat naive bayes goed heeft is: %s%s",
        round(accuracy_naive * 100, 1),
        "%")

# Decision Tree Theory
fit <- rpart(train_mushrooms$class ~ ., data = train_mushrooms)
fit
class.pred <- table(predict(fit, newdata = test_mushrooms, type = 'class'), test_mushrooms$class)
accuracy_tree <- sum(diag(class.pred)) / sum(class.pred)

sprintf("Percentage dat decision tree goed heeft is: %s%s",
        round(accuracy_tree * 100, 1),
        "%")

#DB_SCAN
stars <- read.csv('stars.csv')

## Set the seed to make your partition reproductible
smp_size <- nrow(stars) * 2 / 3

set.seed(123)
train_ind <- sample(seq_len(nrow(stars)), size = smp_size)

test_stars <- stars[train_ind,]
train_stars <- stars[-train_ind,]

df <- train_stars[, 1:2]

res.fpc <- fpc::dbscan(df, eps = 0.25, MinPts = 4)
res.db <- dbscan::dbscan(df, 0.25, 4)

fviz_cluster(res.fpc, df, geom = "point")
