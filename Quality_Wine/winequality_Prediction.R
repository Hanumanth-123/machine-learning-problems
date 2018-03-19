input <- read.csv("E:\\Bosch_Choclate_Project\\winequality\\winequality-red.csv")
head(input)
unique(input$quality)
table(input$quality)
hist(input$quality)
summary(input)
str(input)
input$quality <- as.factor(input$quality)
prop.table(table(input$quality))
install.packages("caTools",dependencies = T)
library(caTools)
set.seed(123)
split <- sample.split(input,SplitRatio = 0.66)
train_red <- subset(input,split == T)
test_red <- subset(input,split == F)
table(train_red$quality)
prop.table(table(train_red$quality))
prop.table(table(test_red$quality))
library(rpart)
library(rpart.plot)
model_control <- rpart.control(xval = 10, cp = 0.00001)
fit_original <- rpart(quality~.,control = model_control,data = train_red)
train_red$pred_quality <- predict(fit_original,train_red,type = "class")
x <- predict(fit_original,train_red,type = "prob")
library(caret)
confusionMatrix(train_red$pred_quality,train_red$quality)
test_red$pred_quality <- predict(fit_original,test_red,type = "class")
confusionMatrix(test_red$quality,test_red$pred_quality)

# Binning the data i.e balancing the data

input1 <- input
input1$quality <- ifelse(input1$quality %in% c(3,4),4,input1$quality)
input1$quality <- ifelse(input1$quality %in% c(7,8),7,input1$quality)
unique(input1$quality)
table(input1$quality)
prop.table(table(input1$quality))
input1$quality <- as.factor(input1$quality)

# Balancing the Binned dataset using ROSE

library(ROSE)

input1_un <- ovun.sample(quality~.,data = input1,method = "under",N = 300,seed = 1)$data
upsample_input1 <- upSample(input1,input1$quality)
head(upsample_input1)
set.seed(123)
split_input1 <- sample.split(input1,SplitRatio = 0.66)
train_input1 <- subset(input1,split_input1 == T)
test_input1 <- subset(input1,split_input1 == F)
prop.table(table(train_input1$quality))
prop.table(table(test_input1$quality))

upsample_train <- upSample(train_input1,train_input1$quality)
head(upsample_train)
names(upsample_train)
upsample_train <- upsample_train[,-13]
prop.table(table(upsample_train$quality))
dim(upsample_train)
fit_upsample <- rpart(quality~.,control = model_control,data = upsample_train)
upsample_train$pred_quality <- predict(fit_upsample,upsample_train,type = "class")
confusionMatrix(upsample_train$pred_quality,upsample_train$quality)
test_input1$pred_quality <- predict(fit_upsample,test_input1,type = "class")
confusionMatrix(test_input1$pred_quality,test_input1$quality)
table(test_input1$quality)
table(train_input1$quality)
table(upsample_train$quality)
upsample_test1 <- upSample(test_input1,test_input1$quality)
table(upsample_test1$quality)
upsample_test1$pred_quality <- predict(fit_upsample,upsample_test1,type = "class")
confusionMatrix(upsample_test1$pred_quality,upsample_test1$quality)

# Upsampling

split2 <- sample.int(n=nrow(train_input1),size = 0.5*nrow(train_input1))
binned_new <- input1[split2,]
binned_old <- input1[-split2,]
newdata <- upSample(binned_new,binned_new$quality)
newdata$Class <- NULL
upsample_data <- rbind(newdata,binned_old)
prop.table(table(upsample_data$quality))
model_control <- rpart.control(xval = 10,minsplit = 12,cp = 0.00001)
fit_final <- rpart(quality~.,control = model_control,data = upsample_data)
plot(fit_final)
text(fit_final)

upsample_data$pred_quality <- predict(fit_final,upsample_data,type = "class")
confusionMatrix(upsample_data$pred_quality,upsample_data$quality)
test_input1$pred_quality <- predict(fit_final,test_input1,type = "class")
confusionMatrix(test_input1$pred_quality,test_input1$quality)

rf1 <- randomForest(quality~.,data = upsample_data)
pred_train_rf1 <- predict(rf1,newdata = upsample_data)
pred_test_rf1 <- predict(rf1,newdata = test_input1)
confusionMatrix(pred_train_rf1,upsample_data$quality)
confusionMatrix(pred_test_rf1,test_input1$quality)

# Using Synthitic Minority Oversampling technique

install.packages("DMwR",dependencies = T)
library(DMwR)

split3 <- sample.split(train_input1,SplitRatio = 0.5)
binned_new1 <- subset(input1,split3 == T)
binned_old1 <- subset(input1,split3 == F)

newdata1 <- SMOTE(quality~.,data = binned_new1,perc.over = 600,perc.under = 500)
table(newdata1$quality)
prop.table(table(newdata1$quality))
newdata1 <- rbind(newdata1,binned_old1)
model_control <- rpart.control(xval = 10, minsplit = 12, cp = 0.00001)
fit_final2 <- rpart(quality~.,control = model_control,data = newdata1)
plot(fit_final2)
text(fit_final2)
summary(fit_final2)

newdata1$pred_quality <- predict(fit_final2,data = newdata1,type = "class")
confusionMatrix(newdata1$pred_quality,newdata1$quality)

test_pred_quality <- predict(fit_final2,newdata = test_input1,type = "class")
confusionMatrix(test_pred_quality,test_input1$quality)


rf2 <- randomForest(quality~.,data = newdata1)
train_pred_quality <- predict(rf2,newdata = newdata1)
confusionMatrix(train_pred_quality,newdata1$quality)

test_pred_quality1 <- predict(rf2,newdata = test_input1)
confusionMatrix(test_pred_quality1,test_input1$quality)

# Using Downsampling

split4 <- sample.split(train_input1,SplitRatio = 0.5)
binned_new2 <- subset(input1, split4 == T)
binned_old2 <- subset(input1, split4 == F)

newdata2 <- downSample(train_input1,train_input1$quality)
newdata2 <- newdata2[,-13]
table(train_input1$quality)
table(newdata2$quality)
newdata2 <- rbind(newdata2,binned_old2)
prop.table(table(newdata2$quality))
fit_final3 <- rpart(quality~.,control = model_control,data = newdata2)
pred_train <- predict(fit_final3,newdata = newdata2,type = "class")
confusionMatrix(pred_train,newdata2$quality)
pred_test <- predict(fit_final3,newdata = test_input1,type = "class")
confusionMatrix(pred_test,test_input1$quality)


rf3 <- randomForest(quality~.,data = newdata2)
pred_train_rf <- predict(rf3,newdata = newdata2)
pred_test_rf <- predict(rf3,newdata = test_input1)
confusionMatrix(pred_train_rf,newdata2$quality)
confusionMatrix(pred_test_rf,test_input1$quality)
