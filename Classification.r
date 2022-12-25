##################################################
### PROG8430                                    ##
### Assignment 3                                ## 
##################################################
#                                               ##
##################################################
# Written by Manthan Ravi Pillai
# ID: 8744053
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("C:/Users/Manthan/Documents/R Programs")

options(scipen=9)


##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(pROC)){install.packages("pROC")}
library(pROC)

#For N-B Analysis

if(!require(klaR)){install.packages("klaR")}
library("klaR")

# For LDA

if(!require(MASS)){install.packages("MASS")}
library("MASS")


##################################################
### Rename and Clean Variables                  ##
##################################################

#Loading the data

load("Tumor_21S.Rdata")

#Renaming the R Data 

Tumor_diagnosis_MRP<-Tumor_21S

str(Tumor_diagnosis_MRP)

#Rename the Variables 

names(Tumor_diagnosis_MRP)<-c("Age_MRP", "Sex_MRP","Bone_MRP","Marrow_MRP","Lung_MRP","Pleura_MRP", "Liver_MRP", 
                              "Brain_MRP","Skin_MRP","Neck_MRP","Supra_MRP","Axil_MRP","Media_MRP","Out_MRP")

head(Tumor_diagnosis_MRP)


###################################################
## Find Outliers                                 ##
###################################################

#statastical finding
summary(Tumor_diagnosis_MRP)

#graphical finding
box_plot_MRP<-Tumor_diagnosis_MRP

par(mfrow=c(3,3))

# BoxPlot for all variables
# loop over column *names* instead of actual columns
sapply(names(box_plot_MRP), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(box_plot_MRP[[cname]]))
    # use the `main` param to put column name as plot title
    print(boxplot(box_plot_MRP[[cname]], main=cname))
})

par(mfrow=c(1,1))


###################################################
## Exploratory  Analysis                         ##
###################################################

Tum_MRP <- cor(Tumor_diagnosis_MRP, method="spearman")
round(Tum_MRP, 2)

#supra, Brain and marrow on Tumor


#Check the Chi Squared Test for Out~Brain

chisqRct_MRP <- chisq.test(Tumor_diagnosis_MRP$Out_MRP, Tumor_diagnosis_MRP$Brain_MRP, correct=FALSE)      
chisqRct_MRP

chisqRct_MRP$observed   # What we observed
chisqRct_MRP$expected   # If there were no relationship

#Check the Chi Squared Test for Out~Marrow

chisqRct1_MRP <- chisq.test(Tumor_diagnosis_MRP$Out_MRP, Tumor_diagnosis_MRP$Marrow_MRP, correct=FALSE)      
chisqRct1_MRP

chisqRct1_MRP$observed   # What we observed
chisqRct1_MRP$expected   # If there were no relationship

#########################################
## Creating Model                     ##
#########################################

#Forward selection

min_model_MRP <- glm( Out_MRP~ 1, data=Tumor_diagnosis_MRP, na.action=na.omit)

Fwd_Den_lm = step(min_model_MRP, direction="forward", scope =(
  ~ Age_MRP + Sex_MRP + Bone_MRP + Marrow_MRP + Lung_MRP + Pleura_MRP +
    Liver_MRP + Brain_MRP + Skin_MRP + Neck_MRP + Supra_MRP + Axil_MRP +
    Media_MRP), details=TRUE)

summary(Fwd_Den_lm)

#User Model1 (Dropping Media_MRP)

User_Model1_MRP = glm(Out_MRP ~ Brain_MRP  + Marrow_MRP + Supra_MRP  + Neck_MRP  + Skin_MRP 
                      + Age_MRP + Lung_MRP + Sex_MRP     
                      ,
                      family="binomial", data=Tumor_diagnosis_MRP, na.action=na.omit)

summary(User_Model1_MRP)

#User Model2 (Dropping Media_MRP and Sex_MRP)

User_Model2_MRP = glm(Out_MRP ~ Brain_MRP  + Marrow_MRP + Supra_MRP  + Neck_MRP  + Skin_MRP 
                      + Age_MRP  + Lung_MRP  
                      ,
                      family="binomial", data=Tumor_diagnosis_MRP, na.action=na.omit)

summary(User_Model2_MRP)


#########################################
## Model Evaluation                    ##
#########################################


#Confusion Matrix for User Model1

resp_um1_MRP <- predict(User_Model1_MRP, type="response")   # creates probabilities
head(resp_um1_MRP,10)
Class_um1_MRP <- ifelse(resp_um1_MRP > 0.5,1,0)           # Classifies probablities (i.e. >50% then likely have tumor)
head(Class_um1_MRP)
Table_MRP <- table(Tumor_diagnosis_MRP$Out_MRP, Class_um1_MRP, dnn=list("Act Tumor","Predicted") )  # Creates a Contingency Table

Table_MRP

#Create True Positives, Negatives, etc
TP_MRP <-Table_MRP[2,2]
TN_MRP <-Table_MRP[1,1]
FP_MRP <-Table_MRP[1,2]
FN_MRP <-Table_MRP[2,1]

#Calculates Accuracy
Acc_MRP <- (TP_MRP + TN_MRP)/sum(Table_MRP)
Acc_MRP
#Calculates Specificity
Mis_MRP <- TN_MRP/(TN_MRP+FP_MRP)
Mis_MRP
#Calculates Sensitivity
Sens_MRP <- TP_MRP/(TP_MRP+FN_MRP)
Sens_MRP
#Calculate Precision
Pre_MRP <- TP_MRP/(TP_MRP+FP_MRP)
Pre_MRP

#Confusion Matrix for User Model2

resp_um2_MRP <- predict(User_Model2_MRP, type="response")   # creates probabilities
head(resp_um2_MRP,10)
Class_um2_MRP <- ifelse(resp_um2_MRP > 0.5,1,0)           # Classifies probablities (i.e. >50% then likely have tumor)
head(Class_um2_MRP)
Table1_MRP <- table(Tumor_diagnosis_MRP$Out_MRP, Class_um2_MRP, dnn=list("Act Tumor","Predicted") )  # Creates a Contingency Table

Table1_MRP

#Create True Positives, Negatives, etc
TP1_MRP <-Table1_MRP[2,2]
TN1_MRP <-Table1_MRP[1,1]
FP1_MRP <-Table1_MRP[1,2]
FN1_MRP <-Table1_MRP[2,1]

#Calculates Accuracy
Acc1_MRP <- (TP1_MRP + TN1_MRP)/sum(Table1_MRP)
Acc1_MRP
#Calculates Specificity
Mis1_MRP <- TN1_MRP/(TN1_MRP+FP1_MRP)
Mis1_MRP
#Calculates Sensitivity
Sens1_MRP <- TP1_MRP/(TP1_MRP+FN1_MRP)
Sens1_MRP
#Calculate Precision
Pre1_MRP <- TP1_MRP/(TP1_MRP+FP1_MRP)
Pre1_MRP

#ROC Curve for User Model1(and Area Under the Curve)

plot(roc(Tumor_diagnosis_MRP$Out_MRP,resp_um1_MRP, direction="<"),
     col="red", lwd=2, main='ROC Curve for Logistic, Tumor for UM1')

auc(Tumor_diagnosis_MRP$Out_MRP, resp_um1_MRP)


#ROC Curve for User Model2(and Area Under the Curve)

plot(roc(Tumor_diagnosis_MRP$Out_MRP,resp_um2_MRP, direction="<"),
     col="red", lwd=2, main='ROC Curve for Logistic, Tumor for UM2')

auc(Tumor_diagnosis_MRP$Out_MRP, resp_um2_MRP)

##################################################
### Building the Stepwise Model                 ##
##################################################

start_time_MRP <- Sys.time()

min_model_MRP <- glm( Out_MRP~ 1, data=Tumor_diagnosis_MRP, na.action=na.omit)

Fwd_Den_lm_MRP = step(min_model_MRP, direction="forward", scope =(
  ~ Age_MRP + Sex_MRP + Bone_MRP + Marrow_MRP + Lung_MRP + Pleura_MRP +
    Liver_MRP + Brain_MRP + Skin_MRP + Neck_MRP + Supra_MRP + Axil_MRP +
    Media_MRP), details=TRUE)

end_time_MRP <- Sys.time()

FSW_Time_MRP <- end_time_MRP - start_time_MRP

summary(Fwd_Den_lm_MRP)

#Confusion matrix
resp_fm_MRP <- predict(Fwd_Den_lm_MRP, type="response")   # creates probabilities
head(resp_fm_MRP,10)
Class_fm_MRP <- ifelse(resp_fm_MRP > 0.5,1,0)           # Classifies probablities (i.e. >50% then likely have tumor)
head(Class_um2_MRP)
Table_fm_MRP <- table(Tumor_diagnosis_MRP$Out_MRP, Class_fm_MRP, dnn=list("Act Tumor","Predicted") )  # Creates a Contingency Table

Table_fm_MRP

##################################################
### NAIVE BAYES                                 ##
##################################################

#Checking for variables to transform
str(Tumor_diagnosis_MRP)

#transforming it into a factor
Tumor_diagnosis_MRP$Out_MRP<- as.factor(Tumor_diagnosis_MRP$Out_MRP)

start_time1_MRP <- Sys.time()

Tumor_Naive_MRP <- NaiveBayes(Out_MRP  ~ Age_MRP + Sex_MRP + Bone_MRP + Marrow_MRP + Lung_MRP + Pleura_MRP +
                                Liver_MRP + Brain_MRP + Skin_MRP + Neck_MRP + Supra_MRP + Axil_MRP +
                                Media_MRP
                              , 
                              data = Tumor_diagnosis_MRP, na.action=na.omit)

end_time1_MRP <- Sys.time()

NB_Time_MRP <- end_time1_MRP - start_time1_MRP



#Classifies
pred_bay_MRP <- predict(Tumor_Naive_MRP,Tumor_diagnosis_MRP)

#Confusion matrix For naive
CF_NB_MRP <- table(Actual=Tumor_diagnosis_MRP$Out_MRP, Predicted=pred_bay_MRP$class)

##################################################
### LDA                                         ##
##################################################

start_time2_MRP <- Sys.time()

Tumor_discrim_MRP <- lda(Out_MRP  ~ Age_MRP + Sex_MRP + Bone_MRP + Marrow_MRP + Lung_MRP + Pleura_MRP +
                           Liver_MRP + Brain_MRP + Skin_MRP + Neck_MRP + Supra_MRP + Axil_MRP +
                           Media_MRP
                         , 
                         data = Tumor_diagnosis_MRP, na.action=na.omit)

end_time2_MRP <- Sys.time()

LDA_Time_MRP <- end_time2_MRP - start_time2_MRP

#Classifies
pred_disc_MRP <- predict(Tumor_Naive_MRP,Tumor_diagnosis_MRP)

#Confusion matrix For naive
CF_LDA_MRP <- table(Actual=Tumor_diagnosis_MRP$Out_MRP, Predicted=pred_disc_MRP$class)

#Comparing all three

CF_NB_MRP
CF_LDA_MRP
Table_fm_MRP

NB_Time_MRP
LDA_Time_MRP
FSW_Time_MRP