### Initial Steps -----------------------------------------------------------------------

# ...Remove all Environment variables..#

rm(list = ls(all.names=T))

# Install Required Libraries
install.packages("XLConnect")
install.packages("pROC")
install.packages("usdm") #VIF
install.packages("ResourceSelection") #Hosmer-lemeshow test
install.packages("aod") #wald-test
#install.packages("DMwR") #data mining
install.packages("partykit") #VIF
install.packages("foreign") # read data from SPSS/SAS..
install.packages("outliers")
install.packages("data.table")
install.packages("caTools") #AUC ROC
install.packages("ROCR")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("randomForest")
install.packages('RColorBrewer')

# load relevant libraries
library(data.table)
library(caTools)
library(corrplot)
library(ROCR)
library(ggplot2)
library(XLConnect)
library(pROC)
library(usdm)
library(ResourceSelection)
library(aod)
library(fmsb)
library(foreign)
library(outliers)
library(rpart)
library(randomForest)
library(rpart.plot)
library(partykit) #treeplots
library(rattle) # Providing Graphical user interface to very many other R pckgs that provide functionality for data mining
library(rpart.plot)
library(RColorBrewer)
install.packages("rattle")
install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)

# load the data
dat=read.csv("D:/hp/dtree/term_data.csv", stringsAsFactors = FALSE)
# summarize the data
summary(dat) # also shows which variable has NAs

str(dat)

### Variable Sanitization ---------------------------------------------------------------
# print out the variable classes and see which ones need changing
# ...notice how int_rate and revol_util are read as 'character'
# ...so are grade and emp_length (which need to be converted to factor)
data.frame(variable = names(dat),
           variable_type = as.character(sapply(dat, class)))
# make certain variables 'factor' (categorical)
dat$job <- factor(dat$job)
dat$marital <- factor(dat$marital)
dat$education <- factor(dat$education)
dat$default<-factor(dat$default)
dat$housing<-factor(dat$housing)
dat$loan<-factor(dat$loan)
dat$contact<-factor(dat$contact)
dat$month<-factor(dat$month)
dat$day_of_week<-factor(dat$day_of_week)
dat$poutcome<-factor(dat$poutcome)
dat$Term_Dep<-factor(dat$Term_Dep)
str(dat)
# check the variable classes now...
data.frame(variable = names(dat),
           variable_type = as.character(sapply(dat, class)))

### Missing Value Treatment -------------------------------------------------------------

# Check fraction of each variable having missing values
data.frame(variable = names(dat),
           pctg_missing = 100*as.numeric(sapply(dat, 
                                                function(x) sum(is.na(x))/
                                                  length(x))))

# ...indicating we can remove mths_since_last_delinq which has > 60% NAs
#dat$mths_since_last_delinq <- NULL



### Splitting the Data ------------------------------------------------------------------

library(caTools)


set.seed(5)
spl <- sample.split(dat$Term_Dep, SplitRatio = 0.7)
train <- subset(dat, spl == T); test <- subset(dat, spl == F)

#prop.table(table(dat$Term_Dep)
#CART Decision Tree on Train Dataset
CART_Model_Train <-rpart(formula = Term_Dep~.,data=train,method = "class")
CART_Model_Train

#Important Variables
CART_Model_Train$variable.importance

#Cross Validation
#The value of cp should be least,so that the cross-validated error rate is minimum
bestcp=CART_Model_Train$cptable[which.min(CART_Model_Train$cptable[,"xerror"]),"CP"]
bestcp
# Prune the tree using the best cp.
tree.pruned <- prune(CART_Model_Train, cp = bestcp)
tree.pruned
# confusion matrix (training data)
conf.matrix <- table(train$Term_Dep, predict(tree.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
#Visualise the Results
printcp(tree.pruned) #Display the results
summary(tree.pruned) # Detailed summary of the split

#....................................................#

#Call Library

jpeg("tree.pruned.jpg",width = 800,height = 600)
rpart.plot(x=CART_Model_Train,type=4,main="Plot of CART Model")
dev.off()


par(mfrow=c(1,2))
#Fancy plot
fancyRpartPlot(CART_Model_Train,sub="CART Classification Tree")
drawTreeNodes(CART_Model_Train)

prp(CART_Model_Train,main="",type=2,extra=101,box.col = c("palegreen3","lightblue")[CART_Model_Train$frame$yval])

summary(train$Term_Dep)
#PRP
prp(CART_Model_Train)
prp(CART_Model_Train,main="type=4,extra=6",type = 4,extra=6,faclen = 0)


#Rpart Plot
rpart.plot(x=CART_Model_Train,main="Rpart plot for Term Deposit",type=4)
rpart.plot(CART_Model_Train,main="extra=106,under=TRUE",extra=106,under=TRUE,faclen = 0)


## Test Set Performance --------------------------------------------
predictions_test <- predict(CART_Model_Train, newdata = test, type = 'class')

predictions_test
confusion_test=table(test$Term_Dep,predictions_test)

confusion_test

Accuracy=((10481+777)/12356)*100
Accuracy
# Sensitivity/recall/True positive rate
Sensitivity=((777)/(sum(615,777)))*100
Sensitivity

Tyep_2error=((615/sum(615,777)))*100
Tyep_2error

