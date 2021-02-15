#Dr. Mary Regina Boland
#Random Forest Model Building 
#GINI Index Importance Modeling
#Using Dummy Patient-Level Data
#BMIN505 Spring 2021
#Script 3: Runs random forest models on 2 fake patient datasets (created by script #1),
#          one with 'baked-in' correlation and one with spurious correlation
#          Uses Gini-index and Mean-Decrease in Accuracy (MDA) as a measure of feature importance

#Libraries
library(caret)
library(randomForest)

#it is important to set the seed to ensure reproducible results later
set.seed(1234)  

#Read in dummy data
#set working directory
fldr = "/Users/insertfilepathhere/"

#read in file
#file below has 'built-in' correlation
fake_pt_df = read.csv(paste(fldr, "fake_pts_for_classpractice.csv", sep=""))

#file below does not have 'built-in' correlation, but some correlations could occur randomly
BIRTHfake_pt_df = read.csv(paste(fldr, "random_pts_nocorrelation_for_classpractice.csv", sep=""))


#view data
head(fake_pt_df)
head(BIRTHfake_pt_df)

#removing the 'junk' first column for both files
BIRTHfake_pt_df = BIRTHfake_pt_df[ ,-1]
fake_pt_df = fake_pt_df[ ,-1]

##CONSTRUCTING A RANDOM FOREST MODEL
#Start first model with 200 trees 
#if significant results then increase the number of trees
#2000 trees
colnames(fake_pt_df)
model <- randomForest(as.factor(as.character(outcome_ADE)) ~ hypertension +
                        depression + 
                        cyp2c9 + 
                        diabetes,
                      data=fake_pt_df, 
                      importance=TRUE, 
                      ntree=200)
print(model)

#Significance is determined by: GINI-Index Importance Metric
#if features MDA>=10 then there are significant features
importance <- varImp(model, scale=FALSE)
plot(importance)
print(importance)
VI_F=importance(model)
VI_F
#testing to see if there are features with MDA>=10
which(VI_F[ , 3]>=10)
#2 are returned -> Depression and Diabetes

#therefore increase the number of trees in the random forest model

#2000 trees
colnames(fake_pt_df)
model <- randomForest(as.factor(as.character(outcome_ADE)) ~ hypertension +
                        depression + 
                        cyp2c9 + 
                        diabetes,
                      data=fake_pt_df, 
                      importance=TRUE, 
                      ntree=2000)
print(model)
#OOB estimate of  error rate: 24%
#   0  1 class.error
#0 57 15   0.2083333
#1  9 19   0.3214286


#GINI-Index Importance Metric
importance <- varImp(model, scale=FALSE)
plot(importance)
print(importance)
VI_F=importance(model)
VI_F

#again testing to see if there are features with MDA>=10
which(VI_F[ , 3]>=10)
#there are still 2 features with MDA>=10
#depression and diabetes are informative for this model
#this is good because these are the 'baked in' features

#make pretty plots of MDA and feature importance
#uncomment pdf and set file path if interested in saving
pdf(paste(fldr, "GINI_INDEX_PLOT_classpractice.pdf", sep=""))
  varImpPlot(model,type=1, col="darkblue", pch=16, main="ADE Prediction")
  abline(v=10, lty=2, col="black")
dev.off()

#saving importance of variables to external file
write.csv(VI_F, paste(fldr, "random_pts_withcorrelation_GINIimportance_RFmodeloutput_classpractice.csv", sep=""))
#Note: Mean Decrease Accuracy >=10 is an important cutoff
#features with >=10 MDA are important in the model!
#therefore they should be retained in the models


#adding age into the model 
#don't include an interaction in the random forest model
colnames(fake_pt_df)
set.seed(1234)
model <- randomForest(as.factor(as.character(outcome_ADE)) ~ hypertension +
                        depression + 
                        cyp2c9 + 
                        diabetes +
                        age_above50 ,
                      data=fake_pt_df, 
                      importance=TRUE, 
                      ntree=2000)
print(model)
#OOB estimate of  error rate: 13%

#GINI-Index Importance Metric
importance <- varImp(model, scale=FALSE)
plot(importance)
print(importance)
VI_F=importance(model)
VI_F

#again testing to see if there are features with MDA>=10
which(VI_F[ , 3]>=10)
#same 2 features are important!

#uncomment pdf and set file path if interested in saving
pdf(paste(fldr, "GINI_INDEX_PLOT_wage_classpractice.pdf", sep=""))
  varImpPlot(model,type=1, col="darkblue", pch=16, main="ADE Prediction")
  abline(v=10, lty=2, col="black")
dev.off()
#Age is close to the 10 cutoff but still <10 MDA
#Our fake dataset had age only be randomly correlated with the outcome
#therefore it is a good sign that we did not detect this feature as significant!


###SECOND FAKE DATASET
#No correlation 'baked in' to fake variables
head(BIRTHfake_pt_df)

#predicting outcome: C-section
#set number of trees to 200 first time running the model
#large numbers are better (e.g., 2000) - more stable models
#however can take a while to compute
#therefore I recommend starting with a smaller number first (e.g., 200)
set.seed(1234)
birthmodel <- randomForest(as.factor(as.character(CSECTION)) ~ MULTIPLE_BIRTH +
                             PRETERM + 
                             STILLBIRTH +
                             NH_AFAM+
                             NH_WHITE+
                             NH_ASIAN+
                             HISPANIC+
                             WEIGHT_LBS+
                             ENC_AGE_DEC,
                           data=BIRTHfake_pt_df, 
                           importance=TRUE, 
                           ntree=200)
print(birthmodel)

#if anything is significant from the 200 tree model, then increase the ntrees to 2000
#OOB estimate of  error rate: 30.03%
#Confusion matrix:
#  0 1  class.error
#0 44311 17 0.0003835048
#1 19002  4 0.9997895401
#error rate is close to the outcome incidence - not a good model

#GINI-Index Importance Metric
importance <- varImp(birthmodel, scale=FALSE)
plot(importance)
print(importance)
VI_F=importance(birthmodel)
VI_F

#again testing to see if there are features with MDA>=10
which(VI_F[ , 3]>=10)
#no features had an MDA>=10

#saving importance results to external file similar to before
write.csv(VI_F, paste(fldr, "random_pts_withoutcorrelation_GINIimportance_RFmodeloutput_classpractice.csv", sep=""))

#uncomment pdf and set file path if interested in saving
pdf(paste(fldr, "GINI_INDEX_PLOT_nocorrelation_classpractice.pdf", sep=""))
  varImpPlot(birthmodel,type=1, col="darkblue", pch=16, main="C-Section Prediction")
  abline(v=10, lty=2, col="black")
dev.off()
#negative values for mean decrease in accuracy indicate that the variable is definitely not important
#10 is a good cutoff and therefore only values greater then or equal to 10 should be included
#none of these variables should be included because all were insignificant


#there is no correlation in this model - so increasing the number of trees will make the model
#very slow - also not needed since the 200 tree model did not find any correlation
#however, ..
#we can try 500, but going up to 2000 takes a very long time to compute
birthmodel <- randomForest(as.factor(as.character(CSECTION)) ~ MULTIPLE_BIRTH +
                             PRETERM + 
                             STILLBIRTH +
                             NH_AFAM+
                             NH_WHITE+
                             NH_ASIAN+
                             HISPANIC+
                             WEIGHT_LBS+
                             ENC_AGE_DEC,
                           data=BIRTHfake_pt_df, 
                           importance=TRUE, 
                           ntree=500)
print(birthmodel)

pdf(paste(fldr, "GINI_INDEX_PLOT_500trees_nocorrelation_classpractice.pdf", sep=""))
  varImpPlot(birthmodel,type=1, col="darkblue", pch=16, main="C-Section Prediction")
  abline(v=10, lty=2, col="black")
dev.off()
VI_F=importance(birthmodel)
VI_F

#again testing to see if there are features with MDA>=10
which(VI_F[ , 3]>=10)

#features still all less then 10 MDA - therefore not important in the model
#this is good because we created this second fake dataset to have purely random features
#and we did not 'bake-in' correlation between the variables