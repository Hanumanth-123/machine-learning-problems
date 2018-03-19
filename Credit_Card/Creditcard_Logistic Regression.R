library(data.table)
cr <- fread("C:\\Users\\Hanu\\Documents\\Kaggle_creditcard.csv")
str(cr)
install.packages("ROSE")
library(ROSE)
cr_un <- ovun.sample(Class~.,data=cr,method = "under",N = 1000,seed = 1)$data
table(cr_un$Class)
cr_ov <- ovun.sample(Class~.,data=cr,method = "over",N = 568630)$data
table(cr_ov$Class)
cr_both <- ovun.sample(Class~.,data = cr,method = "both",N = 284807,p = 0.5,seed =1)$data
table(cr_both$Class)
cr_rose <- ROSE(Class~.,data = cr, seed = 1)$data
table(cr_rose$Class)

# Univariate Analysis :
str(cr_rose)
summary(cr_rose)
sum(is.na(cr_rose))
table(cr_rose$Class)


# Bivariate Analysis :

t.test(cr_rose$Time~cr_rose$Class)
t.test(cr_rose$V1~cr_rose$Class)
t.test(cr_rose$V2~cr_rose$Class)
t.test(cr_rose$V3~cr_rose$Class)
t.test(cr_rose$V4~cr_rose$Class)
t.test(cr_rose$V5~cr_rose$Class)
t.test(cr_rose$V6~cr_rose$Class)
t.test(cr_rose$V7~cr_rose$Class)
t.test(cr_rose$V8~cr_rose$Class)
t.test(cr_rose$V9~cr_rose$Class)
t.test(cr_rose$V10~cr_rose$Class)
t.test(cr_rose$V11~cr_rose$Class)
t.test(cr_rose$V12~cr_rose$Class)
t.test(cr_rose$V13~cr_rose$Class)
t.test(cr_rose$V14~cr_rose$Class)
t.test(cr_rose$V15~cr_rose$Class)
t.test(cr_rose$V16~cr_rose$Class)
t.test(cr_rose$V17~cr_rose$Class)
t.test(cr_rose$V18~cr_rose$Class)
t.test(cr_rose$V19~cr_rose$Class)
t.test(cr_rose$V20~cr_rose$Class)
t.test(cr_rose$V21~cr_rose$Class)
t.test(cr_rose$V22~cr_rose$Class) # Bit less significant
t.test(cr_rose$V22~cr_rose$Class) # Bit less significant
t.test(cr_rose$V23~cr_rose$Class)
t.test(cr_rose$V24~cr_rose$Class)
t.test(cr_rose$V25~cr_rose$Class)
t.test(cr_rose$V26~cr_rose$Class)
t.test(cr_rose$V27~cr_rose$Class)
t.test(cr_rose$V28~cr_rose$Class)
t.test(cr_rose$Amount~cr_rose$Class)

class(cr_rose$Class)

cr_rose1 <- unique(cr_rose)
sum(is.na(cr_rose1))

cr_rose1$Class <- as.factor(cr_rose1$Class)
class(cr_rose1$Class)

sampl <- sample.int(n=nrow(cr_rose1),size = 0.7*nrow(cr_rose1))
train_cr <- cr_rose1[sampl,]
test_cr <- cr_rose1[-sampl,]

table(cr_rose1$Class)
table(train_cr$Class)
table(test_cr$Class)

fit1 <- glm(Class~.,family = binomial,data = train_cr)
summary(fit1)
step(fit1)
library(car)
vif(fit1)
train_cr$Class_Predicted <- predict(fit1,train_cr,type = "response")
train_cr$predict_grp <- ifelse(train_cr$Class_Predicted>=0.5,1,0)
x <- table(train_cr$predict_grp,train_cr$Class)
prop.table(x,1)
sum(diag(x))/sum(x)

test_cr$Class_Predicted <- predict(fit1,test_cr,type = "response")
test_cr$predict_grp <- ifelse(test_cr$Class_Predicted>=0.5,1,0)
x <- table(test_cr$predict_grp,test_cr$Class)
prop.table(x,1)
sum(diag(x))/sum(x)

library(Hmisc)
somers2(x=train_cr$predict_grp,
        y=ifelse(train_cr$predict_grp=='1', TRUE, FALSE))


prediction_Train <- prediction(predictions=train_cr$predict_grp,
                               labels=train_cr$Class, label.ordering=c("0", "1"))
perf <- performance(prediction.obj = prediction_Train,
                    measure = "tpr", x.measure = "fpr") 
plot(perf)


