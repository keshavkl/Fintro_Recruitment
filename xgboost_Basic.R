
train_new <- newdata[1:nrow(train), ]
test_new <- newdata[-(1:nrow(train)), ]
train_new$Business_Sourced <- train.y

train_new <- sparse.model.matrix(Business_Sourced ~ ., data = train_new)

dtrain <- xgb.DMatrix(data = train_new, label = train.y)
watchlist <- list(train_new = dtrain)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.03,
                max_depth           = 5,
                subsample           = 0.7,
                colsample_bytree    = 0.675,
                early.stop.round    = 2
)

xcv <- xgb.cv(  params = param,
                data = dtrain,
                nrounds = 700,
                nfold = 2,
                metrics = {'auc'}
)
which.max(xcv$test.auc.mean)
plot(xcv$test.auc.mean)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = which.max(xcv$test.auc.mean), 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)
importance_matrix <- xgb.importance(train_new@Dimnames[[2]], model = clf)
xgb.plot.importance(importance_matrix)


#Accuracy on Training Set
tpred <- predict(clf, train_new)
table(round(tpred), train.y)
sum(diag(table(round(tpred), train.y)))/sum(table(round(tpred), train.y))

test_new$Business_Sourced <- -1
test_new <- sparse.model.matrix(Business_Sourced ~ ., data = test_new)

preds <- predict(clf, test_new)
submission <- data.frame(ID = test$ID, Business_Sourced = preds)
write.csv(submission, "submission_15var562trees.csv", row.names = F)

### Ensemble
a <- read.csv("submission_62trees.csv")
b <- read.csv("submission_115trees.csv")
c <- read.csv("submission_333trees.csv")

final <- merge(a,b, by = "ID")
final <- merge(final, c, by = "ID")
colnames(final) <- c("ID", "a", "b", "c")
final$Business_Sourced <- (final$a*0.34)+(final$b*0.33)+(final$c*0.33)
submission <- data.frame(ID = final$ID, Business_Sourced = final$Business_Sourced)
write.csv(submission, "submission_Ensemble.csv", row.names = F)





