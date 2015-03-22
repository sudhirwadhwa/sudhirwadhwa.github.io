trainingdataset <- read.csv("/Users/sudhir/pml-training.csv", na.strings=c("NA","#DIV/0!", ""))     
testingdataset <- read.csv('/Users/sudhir/pml-testing.csv', na.strings=c("NA","#DIV/0!", "")) 
set.seed(12345)
S_Trainset <- createDataPartition(trainingdataset$classe, p = 0.8, list = FALSE)
S_Training <- trainingdataset[S_Trainset , ]
S_Validation <- trainingdataset[-S_Trainset , ]
nzvcol <- nearZeroVar(S_Training)
S_Training <- S_Training[, -nzvcol]


cntlength <- sapply(S_Training, function(x) {
  sum(!(is.na(x) | x == ""))
})

nullcol <- names(cntlength[cntlength < 0.6 * length(S_Training$classe)])
descriptcol <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
                 "cvtd_timestamp", "new_window", "num_window")

excludecols <- c(descriptcol, nullcol)
S_Training <- S_Training[, !names(S_Training) %in% excludecols]



S_rfModel <- randomForest(classe ~ ., data = S_Training, importance = TRUE, ntrees = 10)
S_predict <- predict(S_rfModel, S_Training)
print(confusionMatrix(S_predict, S_Training$classe))

S_pvalidation <- predict(S_rfModel, S_Validation)
print(confusionMatrix(S_pvalidation, S_Validation$classe))

S_FINALPredictTest <- predict(S_rfModel, testingdataset)
S_FINALPredictTest
answers <- as.vector(S_FINALPredictTest)


