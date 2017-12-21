# SET WORKING DIRECTORY AND FILE PATH

MY_WD <-("~/Desktop/BIGDATA/final_project")
PATH <-("/Users/Giacomo/Desktop/BIGDATA/final_project/HR_comma_sep.csv")

# INSTALL ALL DEPENDENCIES
install.packages(rpart.plot)
install.packages("e1071")

# RUN THE CODE

setwd(MY_WD)
myData <- read.csv(file=PATH)
head(myData)

# class frequencies (distribution)
table(myData$left)


#changing categorical data to numerical data
myData$sales<- factor(myData$sales,levels = c("sales","accounting","hr","technical","support","management","IT","product_mng","marketing","RandD"),labels = c(1,2,3,4,5,6,7,8,9,10))
myData$salary <- factor(myData$salary,levels=c("low","medium","high"),labels =c(0,1,2))


library("rpart.plot")

set.seed(1)
train.idx <- sample(1:nrow(myData), size = round(0.8 * nrow(myData)), replace = FALSE)
train.set <- myData[train.idx,]
test.set <- myData[-train.idx,]

dt <- rpart(left ~ ., data =train.set , method = 'class')
plot(dt)
text(dt, pretty = 0)
rpart.plot(dt)

dtPred <- predict(dt,train.set,type='class')
table(dtPred,train.set$left)



#SVM not working
library("e1071")
svm.model <- svm(formula = left ~ ., data = train.set, kernel = "radial", cost = 1, probability = TRUE)
svm.pred <-predict(svm.model, newdata = test.set,probability = TRUE) 

class.probs <- attr(svm.pred, "probabilities")
pred2 <- prediction(class.probs[,2], test.set$left)

