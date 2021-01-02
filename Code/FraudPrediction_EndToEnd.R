rm(list = ls())

# Consolidated code for PHD Batch 32 Rohil Rao Insurance Fraud Detection

## Loading Required libraries;

library(caret)
library(DMwR)
library(h2o)
library(ROCR)
library(ggplot2)
library(vegan)


#***********************************************************************************************************************  

  
## Read The Train and Test Data into Enviornment and defining NAs for each data
  
train_data1 = read.csv(file = "Train.csv", header = T, sep = ',')
train_data2 = read.csv(file = "Train_Claim.csv", header = T, sep = ',',na.strings = c('?','-5','MISSINGVALUE','MISSEDDATA'))
train_data3 = read.csv(file = "Train_Demographics.csv", header = T, sep = ',')
train_data4 = read.csv(file = "Train_Policy.csv", header = T, sep = ',',na.strings = c('-1','MISSINGVAL','NA'))
train_data5 = read.csv(file = "Train_Vehicle.csv", header = T, sep = ',',na.strings = '???')
  
  
test_data1 = read.csv(file = "Test.csv", header = T, sep = ',')
test_data2 = read.csv(file = "Test_Claim.csv", header = T, sep = ',',na.strings = c('?','-5','MISSINGVALUE','MISSEDDATA'))
test_data3 = read.csv(file = "Test_Demographics.csv", header = T, sep = ',')
test_data4 = read.csv(file = "Test_Policy.csv", header = T, sep = ',',na.strings = c('-1','MISSINGVAL','NA'))
test_data5 = read.csv(file = "Test_Vehicle.csv", header = T, sep = ',',na.strings = '???')



#***********************************************************************************************************************  

###### train vehicle data pre processing #######
  
  
  
train_VehicleID = subset(train_data5, VehicleAttribute == "VehicleID")
train_VehicleMake = subset(train_data5, VehicleAttribute == "VehicleMake")
train_VehicleModel = subset(train_data5, VehicleAttribute == "VehicleModel")
train_VehicleYOM = subset(train_data5, VehicleAttribute == "VehicleYOM")
  
  
  
df1 = merge(x = train_VehicleID, y = train_VehicleMake, by = "CustomerID", all = TRUE)
df2 = merge(x = train_VehicleModel, y = train_VehicleYOM, by = "CustomerID", all = TRUE)
train_data5_final <- merge(x = df1, y = df2 , by = "CustomerID", all =TRUE)
train_data5_final <- train_data5_final[,!(colnames(train_data5_final) %in% c("VehicleAttribute.x.x","VehicleAttribute.y.x","VehicleAttribute.x.y","VehicleAttribute.y.y"))]
names(train_data5_final) <- c("CustomerID","VehicleID","VehicleMake","VehicleModel","VehicleYOM")
nrow(train_data5_final)
str(train_data5_final)
  

#***********************************************************************************************************************  


##Converting datatypes to adjust for the differenet levels in Vehicle data
  
train_data5_final$VehicleID = as.factor(as.character(train_data5_final$VehicleID))
train_data5_final$VehicleMake = as.factor(as.character(train_data5_final$VehicleMake))
train_data5_final$VehicleModel = as.factor(as.character(train_data5_final$VehicleModel))
train_data5_final$VehicleYOM = as.factor(as.character(train_data5_final$VehicleYOM))
str(train_data5_final)
  
#***********************************************************************************************************************  
  
##Similarly doing pre processing on test vehicle data
  
###### test vehicle data pre processing #######
  
  
  
test_VehicleID = subset(test_data5, VehicleAttribute == "VehicleID")
test_VehicleMake = subset(test_data5, VehicleAttribute == "VehicleMake")
test_VehicleModel = subset(test_data5, VehicleAttribute == "VehicleModel")
test_VehicleYOM = subset(test_data5, VehicleAttribute == "VehicleYOM")
  
df3 = merge(x = test_VehicleID, y = test_VehicleMake, by = "CustomerID", all = TRUE)
df4 = merge(x = test_VehicleModel, y = test_VehicleYOM, by = "CustomerID", all = TRUE)
  
test_data5_final <- merge(x = df3, y = df4 , by = "CustomerID", all =TRUE)
test_data5_final <- test_data5_final[,!(colnames(test_data5_final) %in% c("VehicleAttribute.x.x","VehicleAttribute.y.x","VehicleAttribute.x.y","VehicleAttribute.y.y"))]
names(test_data5_final) <- c("CustomerID","VehicleID","VehicleMake","VehicleModel","VehicleYOM")
nrow(test_data5_final)
str(test_data5_final)
  

  
##Adjusting for levels
  
test_data5_final$VehicleID = as.factor(as.character(test_data5_final$VehicleID))
test_data5_final$VehicleMake = as.factor(as.character(test_data5_final$VehicleMake))
test_data5_final$VehicleModel = as.factor(as.character(test_data5_final$VehicleModel))
test_data5_final$VehicleYOM = as.factor(as.character(test_data5_final$VehicleYOM))
str(test_data5_final)
  
  
## Merging the data
  
train_data=Reduce(function(x, y) merge(x, y, all=TRUE), list(train_data1,train_data2,train_data3,train_data4,train_data5_final))
df=Reduce(function(x,y) merge(x,y, all=TRUE), list(test_data2,test_data3,test_data4,test_data5_final))
test_data1$id=1:nrow(test_data1)
test_datax=merge(test_data1,df,by = "CustomerID",sort = FALSE)
test_data=test_datax[order(test_datax$id),]

sum(is.na(train_data))

train_data <- centralImputation(data = train_data)
test_data <- centralImputation(data = test_data)


#***********************************************************************************************************************  


#******************************************************************************************************************************

VISUALIZATIONS:
  
  
  
  #Corelation heatmap
  cormat <- round(cor(train_data_n),2)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
upper_tri
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

#No surprising co-relations found (age and customer loyalt period naturally co-related)
melted_cormat[(melted_cormat$value> 0.2 | melted_cormat$value< -0.2) & melted_cormat$value != 1.00,]

# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


#Histogram



ggplot(train_data, aes(x = UmbrellaLimit)) +
  geom_histogram(bins = 20)+ geom_bar()+ facet_wrap(~ ReportedFraud)


ggplot(train_data, aes(x = CapitalGains)) +
  geom_histogram(bins = 20)+ geom_bar()+ facet_wrap(~ ReportedFraud)

ggplot(train_data, aes(x = CapitalLoss)) +
  geom_histogram(bins = 20)+ geom_bar()+ facet_wrap(~ ReportedFraud)

x <- train_data$CapitalGains - train_data$CapitalLoss
temp <- train_data
temp <- cbind(temp,x)

ggplot(temp, aes(x = x)) +
  geom_histogram(bins = 20)+ geom_bar()+ facet_wrap(~ ReportedFraud)


ggplot(train_data, aes(x = InsuredAge)) +
  geom_histogram(bins = 20)+ geom_bar()+ facet_wrap(~ ReportedFraud)

ggplot(train_data, aes(x = IncidentTime)) +
  geom_histogram(bins = 24)+ geom_bar()+ facet_wrap(~ ReportedFraud)

ggplot(train_data, aes(x = AmountOfTotalClaim)) +
  geom_histogram(bins = 50)+facet_wrap(~ReportedFraud)

ggplot(train_data, aes(x = AmountOfInjuryClaim)) +
  geom_histogram(bins = 50)+geom_bar()+facet_wrap(~ReportedFraud)         # AmountOfInjuryClaim seems to have spikes below 2500.Examine further.

ggplot(train_data, aes(x = AmountOfVehicleDamage)) +
  geom_histogram(bins = 50)+geom_bar()+facet_wrap(~ReportedFraud) # AmountOfVehicleDamage seems to have spikes below 10000.Examine further.

table(train_data$ReportedFraud)

500/7785



ggplot(train_data, aes(x = InsuredZipCode)) +
  geom_histogram(bins = 30)             #InsuredZipCode convert to bins maybe.

train_data$InsuredZipCode <- as.numeric(as.character(train_data$InsuredZipCode)) 
test_data$InsuredZipCode<- as.numeric(as.character(test_data$InsuredZipCode))

train_data$InsuredZipCode=ifelse(train_data$InsuredZipCode < 500000,'A1','A2')
test_data$InsuredZipCode=ifelse(test_data$InsuredZipCode < 500000,'A1','A2')


# Insured Age,CustmerLoyaltyPeriod fine,

unique(train_data$PolicyAnnualPremium)


##Barplots 


geom_bar() 


ggplot(train_data, aes(x = InsuredEducationLevel)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)
geom_bar() 

ggplot(train_data, aes(x = TypeOfCollission)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)


ggplot(train_data, aes(x = DateOfIncident)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)

ggplot(train_data, aes(x = IncidentAddress)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)

ggplot(train_data, aes(x = IncidentTime)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)

ggplot(train_data, aes(x = InsuredEducationLevel)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)

ggplot(train_data, aes(x = VehicleMake)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)

ggplot(train_data, aes(x = VehicleYOM)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)

ggplot(train_data, aes(x = InsuredRelationship)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)

#Major amount of the fraud reportings were reported as Major Damage.Distribution skewed to left for Fraud cases.
ggplot(train_data, aes(x = SeverityOfIncident)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)

#seeming to be a pattern: Most of the fraud cases tend to be Collisions
ggplot(train_data, aes(x = TypeOfIncident)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)


##Authorities are almost always contacted in fraud case.Conversely chances of fraud when no authorities are contacted are low.
ggplot(train_data, aes(x = AuthoritiesContacted)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)


ggplot(train_data, aes(x = IncidentState)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)

ggplot(train_data, aes(x = IncidentCity)) +
  geom_bar()+facet_wrap( ~ ReportedFraud)


#Scatterplots
ggplot(data = train_data, mapping = aes(x = AmountOfTotalClaim, y = CustomerID,color = ReportedFraud)) + 
  geom_point()+facet_wrap( ~ InsuredEducationLevel)



qplot(InsuredGender,InsuredAge, data = train_data, geom = "boxplot",col = ReportedFraud)

qplot(InsuredEducationLevel,CustomerLoyaltyPeriod, data = train_data, geom = "boxplot",col = ReportedFraud)


qplot(PolicyAnnualPremium,data = train_data, facets = InsuredEducationLevel ~ ReportedFraud)



# Boxplots to analyze any outliers
boxplot(train_data$AmountOfTotalClaim)
boxplot(train_data$AmountOfInjuryClaim)   # Outliers beyond the 20k mark.
boxplot(train_data$AmountOfPropertyClaim)  # Outliers beyong 18k mark
boxplot(train_data$AmountOfVehicleDamage)  # Outliers above 70k and below 5k
boxplot(train_data$InsuredAge) #Outliers above 60.
boxplot(train_data$CapitalGains)
boxplot(train_data$CapitalLoss)
boxplot(train_data$CustomerLoyaltyPeriod)
boxplot(train_data$Policy_Deductible)












#*************************************************************************************************************************
#### Dropping unwanted variables 

train_data$Country <- NULL
train_data$CustomerID <- NULL
train_data$VehicleID <- NULL
train_data$IncidentAddress <- NULL
train_data$InsuredHobbies <- NULL
train_data$InsurancePolicyNumber <- NULL
train_data$Policy_CSL1 <- NULL # Dropping interaction term.Leaving CSL2
train_data$DateOfPolicyCoverage <- NULL
train_data$VehicleModel <- NULL # too many levels
#train_data$InsuredZipCode <- NULL
train_data$DateOfIncident <- NULL

test_data$Country <- NULL
test_data$DateOfPolicyCoverage <- NULL
test_data$DateOfIncident <- NULL
test_data$CustomerID <- NULL
test_data$VehicleID <- NULL
test_data$IncidentAddress <- NULL
test_data$InsuredHobbies <- NULL
#test_data$InsuredZipCode <- NULL
test_data$InsurancePolicyNumber <- NULL
test_data$Policy_CSL1 <- NULL
test_data$VehicleModel <- NULL # too many levels
test_data$id <- NULL
  

test_b <- test_data
train_b <- train_data
  
sum(is.na(train_data))
#***********************************************************************************************************************  

### Standardization
  

library(caret)

preProc1 = preProcess(train_data[, setdiff(names(train_data),"ReportedFraud")],method=c("center", "scale"))

train_data = predict(preProc1, train_data)

summary(train_data)

str(train_data)

preProc3 = preProcess(test_data[, setdiff(names(test_data),"ReportedFraud")],method=c("center", "scale"))

test_data = predict(preProc3, test_data)

summary(test_data)

str(test_data)



### Normalization

preProc2 = preProcess(train_data[, setdiff(names(train_data),"ReportedFraud")],method = c("range"))

train_data = predict(preProc2, train_data)

summary(train_data)

str(train_data)

preProc4 = preProcess(test_data[, setdiff(names(test_data),"ReportedFraud")],method = c("range"))

test_data = predict(preProc4, test_data)

summary(test_data)

str(test_data)

#***********************************************************************************************************************  


###### MODEL BUILDING #######

####################### RANDOM FOREST


library(DMwR)
library(randomForest)



# Split dataset into train and test
set.seed(9585)

train_RowIDs = sample(1:nrow(train_data), nrow(train_data)*0.7)
train_D = train_data[train_RowIDs,]
Val_D = train_data[-train_RowIDs,]
rm(train_RowIDs)


# Build the classification model using randomForest
model = randomForest(ReportedFraud ~ ., data=train_D, 
                     keep.forest=TRUE, ntree=50) 

# Print and understand the model
print(model)

# Important attributes
model$importance  
round(importance(model), 2)   

# Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(model$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]

# plot (directly prints the important attributes) 
varImpPlot(model)



# Predicton Test Data
pred_Test = predict(model, Val_D[,setdiff(names(Val_D),
                                          "ReportedFraud")],
                    type="response", 
                    norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Test = table("actual"=Val_D$ReportedFraud, "predicted"=pred_Test);
accu_Test= sum(diag(cm_Test))/sum(cm_Test)
rm(pred_Test, cm_Test)


# Build randorm forest using top 5 important attributes. 
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:5])

# Build the classification model using randomForest
model_Imp = randomForest(ReportedFraud~.,
                         data=train_D[,c(top_Imp_Attr,"ReportedFraud")], 
                         keep.forest=TRUE,ntree=50) 

# Print and understand the model
print(model_Imp)

# Important attributes
model_Imp$importance  

# Predict on Train data 
pred_Train = predict(model_Imp, train_D[,top_Imp_Attr],
                     type="response", norm.votes=TRUE)


# Build confusion matrix and find accuracy   
cm_Train = table("actual" = train_D$ReportedFraud, 
                 "predicted" = pred_Train);
accu_Train_Imp = sum(diag(cm_Train))/sum(cm_Train)
rm(pred_Train, cm_Train)

# Predicton Test Data
pred_Test = predict(model_Imp, Val_D[,top_Imp_Attr],
                    type="response", norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Test = table("actual" = Val_D$ReportedFraud, 
                "predicted" = pred_Test);
accu_Test_Imp = sum(diag(cm_Test))/sum(cm_Test)
rm(pred_Test, cm_Test)

accu_Train
accu_Test
accu_Train_Imp
accu_Test_Imp








########### CART

library(rpart)
rpart_tree <- rpart(ReportedFraud ~ . , data = train_data,method="class")
rpart_tree$variable.importance
preds_rpart <- predict(rpart_tree, test_data,type="class")
write.csv(preds_rpart,"SubCARTO.csv")

printcp(rpart_tree)

asRules(rpart_tree)

# ####RESULTS
# Your answer passed the tests! Your score is 60.88%
# Auxiliary metrics => Precision=64.97929% and Recall=57.25872%


###########  H2O Random Forest
 

  h2o.init(nthreads = -1, max_mem_size = '4g', ip = "127.0.0.1", port = 50001)
  
  which ( colnames(train_data) == "ReportedFraud")
  
  train1=train_data
  train_data$ReportedFraud=NULL
  train_data=cbind(train_data,ReportedFraud=train1$ReportedFraud)
  
  
  
  train2.h2o <- as.h2o(train_data)
  test2.h2o  <- as.h2o(test_data)
  
  
  y.dep <- 33
  x.indep <- c(1:32)
  
  rforest.model.h2o <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train2.h2o,
                                        ntrees = 50, mtries = 3, 
                                        max_depth = 5, seed = 007)
  
  predict.rforest.h2o <- as.data.frame(h2o.predict(rforest.model.h2o, test2.h2o))
  
  predictionsH2O<- predict.rforest.h2o$predict
  write.csv(predictionsH2O,'submissionRfinal.csv')
  
  
  # # RESULT #
  # Your answer passed the tests! Your score is 64.82%
  # Auxiliary metrics => Precision=59.77405% and Recall=70.80292%
  
  #***********************************************************************************************************************  
  
  
  ########
  
  library(C50)
  
  # Running the C5.0 on non standardized data to get more understandable rules.
  c5_tree = C5.0(train_data$ReportedFraud ~ . , train_b)
  c5_rules = C5.0(train_data$ReportedFraud ~ . , train_b, rules = T)
  print(summary(c5_rules))
  
  
  #Running on standardized data
  c5_tree = C5.0(train_data$ReportedFraud ~ . , train_data)
  c5_rules = C5.0(train_data$ReportedFraud ~ . , train_data, rules = T)
  print(summary(c5_rules))
  
  
  out <- capture.output(summary(c5_rules))
  cat("My title", out, file="summary_of_my_very_long_DT2.txt", sep="///", append=TRUE)
  
  
  preds <- predict(c5_tree, test_data)
  write.csv(preds,"subC51.csv")
  
  # ####RESULTS#######
  # Your answer passed the tests! Your score is 72.06%
  # Auxiliary metrics => Precision=80.83712% and Recall=65.00406%
  
  
  #***********************************************************************************************************************  
  
  ############
  
  ###### GBM #######
  
  df <- as.h2o(train_data)
  
  response <- "ReportedFraud"
  predictors <- setdiff(names(train_data), c(response))
  
  splits <- h2o.splitFrame(
    data = df, 
    ratios = c(0.75),   ## only need to specify 2 fractions, the 3rd is implied
    destination_frames = c("train.hex", "test.hex"), seed = 1234
  )
  train <- splits[[1]]
  test  <- splits[[2]]
  
  
  gbm <- h2o.gbm(x = predictors, y = response, training_frame = train)
  
  h2o.auc(h2o.performance(gbm, newdata = test))
  
  #cross-validation
  gbm <- h2o.gbm(x = predictors, y = response, training_frame = train, nfolds = 4, seed = 0xDECAF)
  gbm@model$cross_validation_metrics_summary
  h2o.auc(h2o.performance(gbm, xval = TRUE))
  
  
  # I feel lucky parameters
  
  gbm <- h2o.gbm(
    ## standard model parameters
    x = predictors, 
    y = response, 
    training_frame = train, 
    validation_frame = test,
    
    ## more trees is better if the learning rate is small enough 
    ## here, use "more than enough" trees - we have early stopping
    ntrees = 10000,                                                            
    
    ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
    learn_rate=0.01,                                                         
    
    ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
    stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC", 
    
    ## sample 80% of rows per tree
    sample_rate = 0.8,                                                       
    
    ## sample 80% of columns per split
    col_sample_rate = 0.8,                                                   
    
    ## fix a random number generator seed for reproducibility
    seed = 1234,                                                             
    
    ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
    score_tree_interval = 10                                                 
  )
  
  h2o.auc(h2o.performance(gbm, newdata =  test))
  
  preds <- h2o.predict(gbm, test)
  
  test1 <- as.h2o(test_data)
  
  preds <- h2o.predict(gbm, test1)
  
  
  head(preds)
  
  gbm@model$validation_metrics@metrics$max_criteria_and_metric_scores
  
  
  h2o.exportFile(preds, "SubC.csv", force=TRUE)
  
  # ### Result
  # Your answer passed the tests! Your score is 64.92%
  # Auxiliary metrics => Precision=59.77482% and Recall=71.04623%
  
  
  #***********************************************************************************************************************  
  
  
#################################################
  
  
  ################### NaiveBayes
  
  require(e1071)
  
  #Building the Naive Bayes model
  model_nb<-naiveBayes(train_data$ReportedFraud~.,train_data)
  
  #Response of the model
  model_nb
  
  #Predict the Flight Status on the test data
  pred<-predict(model_nb,test_data)
  
  write.csv(pred, "SubNB.csv")
  
  
  ####Your answer passed the tests! Your score is 59.05%
  # Auxiliary metrics => Precision=51.62367% and Recall=68.9781%
  #   Your score may be scaled post submission deadline.

  
  #***********************************************************************************************************************  
  
  
  