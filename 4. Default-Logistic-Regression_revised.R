######################################################
############# Example: Default -- Logistic Regression
#######################################################

## TASK 1 ##

# load library ISLR and see what the dataframe Default looks like
library(ISLR)
View(Default)

# check the names of the columns in Default
names(Default)

# check the dimensions of Default (how many rows, how many columns)
dim(Default)

# check how many missing values there are in the dataframe Default
sum(is.na(Default))


## TASK 2 ##

#Retrieve the dummy variables that glm is using in the model. Note that glm uses "0" as the default reference and hence it finds the probability of the response as equal to 1. 
#Recall that odds are p(x)/1-p(x). The event for the denominator is called the reference, which in this case is the response category denote as 0.
contrasts(Default$default)#？？？

# run logistic regression with default as the response variable and all others as predictors
logreg.fit <- glm(default~balance+income+student,data=Default,family=binomial)

# see the summary output of logistic regression
summary(logreg.fit) 

## TASK 3 ##  

# given we have the logistic regression results stored in logreg.fit, we call predict()
# with logreg.fit as the first argument, the data for predcitors in the second argument,
# type = "response" to clarify we want to predict the probabilities for the response being 1 
# (in this case, predicting the probability of default)
predict(logreg.fit,newdata=data.frame(balance=c(2000,2000),income=c(8000,40000), student=c("Yes","No")),type="response")
#这题同时预测两个人的default概率

## TASK 4 ##
logreg.fit2 <- glm(default~balance+student,data=Default,family=binomial)
summary(logreg.fit2)

predict(logreg.fit2,newdata=data.frame(balance=c(2000,2000), student=c("Yes","No")),type="response")

## TASK 5 ##
#Create a new dataset
Default.v2<-Default

#Divide balance into three categories: low balance, med balance, high balance
Default.v2$balance_cat<-"High"#建立一个新列, 都是high，格式特殊，[ ]。
Default.v2$balance_cat[Default.v2$balance<=500]<-"Low"
Default.v2$balance_cat[(Default.v2$balance>500) & (Default.v2$balance<=1000)]<-"Med"

## TASK 6 ##

#Turn the balance category column into factors
Default.v2$balance_cat<-as.factor(Default.v2$balance_cat) 

#Check which one is the baseline category
contrasts(Default.v2$balance_cat) #high是baseline，都是0的那个左侧的是。 

#Apply relevel so that "Low" is the baseline category
Default.v2$balance_cat<- relevel(Default.v2$balance_cat, ref = "Low")

#Verify that "Low" is the baseline category
contrasts(Default.v2$balance_cat)

## TASK 7 ##
#Run a logistic regression model with "student" and "balance_cat"
logreg.fit3 <- glm(default~balance_cat+student,data=Default.v2,family=binomial)
summary(logreg.fit3)

## TASK 8 ##

#Create a new dataset
Default.v3<-Default #现在有v3了。

#Divide balance into three categories: low balance, med balance, high balance
Default.v3$balance_cat<-"High"
Default.v3$balance_cat[Default.v2$balance<=900]<-"Low"
Default.v3$balance_cat[(Default.v2$balance>900) & (Default.v2$balance<=1800)]<-"Med"

#Turn the balance category column into factors
Default.v3$balance_cat<-as.factor(Default.v3$balance_cat)

#Check which one is the baseline category
contrasts(Default.v3$balance_cat)

#Apply relevel so that "Low" is the baseline category
Default.v3$balance_cat<- relevel(Default.v3$balance_cat, ref = "Low")

#Verify that "Low" is the baseline category
contrasts(Default.v3$balance_cat)

#Run a logistic regression model with "student" and "balance_cat"
logreg.fit4 <- glm(default~balance_cat+student,data=Default.v3,family=binomial)
summary(logreg.fit4)

## TASK 9 ##

# randomly split data into two subsets
train=sample(1:nrow(Default.v3), nrow(Default.v3)/2) 

# the variable test will store the numbers of remaining rows, which will be our test data
test=(-train)

# we create the part of Default.v3 that will be our training data
Default.v3.train=Default.v3[train,] #针对default这个数据组执行train和test函数。

# we create the part of Default.v3 that will be our testing data
Default.v3.test=Default.v3[test,] 

# store the output of the testing set in a different data frame
Default.v3.test.output=Default.v3$default[test] #拿出test中的y值，后面看吻合度用。 

## TASK 10 ##
# run logistic regression, but only on the training data
logreg.fit5 <- glm(default~balance_cat+student,data=Default.v3.train,family=binomial)
summary(logreg.fit5)

## TASK 11 ##
# predict the probability of default for the test data
logreg.fit5.prob=predict(logreg.fit5,Default.v3.test,type="response")
# check the first 6 values of the predicted response
head(logreg.fit5.prob)

# create a vector logreg.fit5.pred, which initially consists of Yes for 5000 rows
logreg.fit5.pred=rep("No",5000)

# update the vector to Yes, for any row in which the probability of Default is greater than 50%
logreg.fit5.pred[logreg.fit5.prob>.5]="Yes"#一个新列承载结果。 

# compute the percentage of time the prediction was correct
mean(logreg.fit5.pred==Default.v3.test.output)
