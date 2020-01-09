
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
library(cowplot)

# Loading data

## The data can be download in repos and automatic download
dl <- tempfile()
download.file("https://raw.githubusercontent.com/RawinChanpitak/rawin-student-prediction/master/student-mat.csv", dl)
student <- data.frame(read.csv(dl))
coltemp <- strsplit(as.character(names(student)), "\\.")[[1]]
colnames(student) <- "temp"
student <- student %>% separate(temp, into = coltemp, sep = ";")

# Change the data from character to factor and numeric

for (i in seq(1:30)) {
  student[,i] <- as.factor(student[,i])
}
student$absences <- as.numeric(as.character(student$absences))
student$G1 <- as.numeric(as.character(student$G1))
student$G3 <- as.numeric(as.character(student$G3))
student <- student %>% select(-G2)

# Exploring classes of variables

sapply(student,class)

# Factorize the output value from numeric to factor by quatile. There will be 3 level of output vector

hist(student$G3)
per_value <- quantile(student$G3,probs = c(0.33,0.67))
student <- student %>% mutate(finalG = ifelse(G3<per_value[1],0,
                                              ifelse(G3<per_value[2],1,2)))
student$finalG <- factor(student$finalG, labels = c("low","average","high"))
student <- student %>% select(-G3)

# Explore data to find any significant different from visualise

plot1 <- student %>% ggplot(aes(sex, fill = finalG)) + geom_bar(position = "dodge")
plot2 <- student %>% ggplot(aes(age, fill = finalG)) + geom_bar(position = "dodge")
plot3 <- student %>% ggplot(aes(Medu, fill = finalG)) + geom_bar(position = "dodge")
plot4 <- student %>% ggplot(aes(G1, fill = finalG)) + geom_bar(position = "dodge")
plot_grid(plot1,plot2,plot3,plot4,nrow = 2)
student$finalG <- factor(student$finalG, labels = c("1","2","3"))

# Divind the data into 3 parts. Training set, Validation set and testing set

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = student$finalG, times = 1, p = 0.15, list = FALSE)
f_train_stu <- student[-test_index,]
test_stu <- student[test_index,]

v_index <- createDataPartition(y = f_train_stu$finalG, times = 1, p = 0.15, list = FALSE)
train_stu <- f_train_stu[-v_index,]
val_stu <- f_train_stu[v_index,]

# Train model which include 6 machine learning algorythms

models <- c( "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "rf")
ctrl <- trainControl(method="repeatedcv",
                     repeats=10)
fits <- lapply(models, function(model){ 
  print(model)
  train(finalG ~ .,
        method = model, 
        data = train_stu,
        trControl=ctrl)
}) 
names(fits) <- models

# Calculate accuracy from training data

acc_val_result <- data_frame(method = "", Acc = "")
acc_val_result <- acc_val_result[-1,]
for (i in models) {
  y_hat <- predict(fits[[i]],val_stu)
  acc <- confusionMatrix(data = y_hat, reference = val_stu$finalG)$overall["Accuracy"]
  acc_val_result <- bind_rows(acc_val_result,
                    data_frame(method = models[i],Acc = as.character(acc)))
}
acc_val_result$method <- models
acc_val_result

# Exploring the accuracy to find the best performer

acc_val_result %>% arrange(desc(Acc)) %>% select(method) %>% slice(1:3)

# Constructing the ensemble model

target <- acc_val_result %>% arrange(desc(Acc)) %>% select(Acc) %>% slice(3)
top_index <- which(acc_val_result$Acc>(as.numeric(target)-0.001))
top_models <- models[top_index]
top_fits <- fits[top_index]

y_hat_top <- sapply(seq(1:3),function(i){
  y_hat <- predict(top_fits[[i]],val_stu)
})
colnames(y_hat_top) <- models[top_index]
y_hat_top <- as_data_frame(y_hat_top)

# Calculate average of top three model

en_train <- sapply(seq(1:nrow(y_hat_top)),function(i){
  mean(as.numeric(y_hat_top[i,]))
})

# Calculating accuracy for ensemble model

en_train <- as.factor(round(en_train,digits = 0))
acc <- confusionMatrix(data = en_train, reference = val_stu$finalG)$overall["Accuracy"]
acc_val_result <- bind_rows(acc_val_result,
                            data_frame(method = "Ensemble",Acc = as.character(acc)))

# tuned knn

knnFit <- train(finalG ~ ., 
                data = train_stu, 
                method = "knn", 
                trControl = ctrl, 
                tuneLength = 20)

y_hat <- predict(knnFit,val_stu)
acc <- confusionMatrix(data = y_hat, reference = val_stu$finalG)$overall["Accuracy"]
acc_val_result <- bind_rows(acc_val_result,
                            data_frame(method = "Tuned KNN",Acc = as.character(acc)))
acc_val_result


# Testing the tuned knn model

ctrl <- trainControl(method="repeatedcv",
                     repeats=5)
knnFit <-  train(finalG ~ .,
                 method = "knn", 
                 data = f_train_stu,
                 trControl=ctrl,
                 tuneLength = 20)

y_hat <- predict(knnFit,test_stu)
acc <- confusionMatrix(data = y_hat, reference = test_stu$finalG)$overall["Accuracy"]
acc




