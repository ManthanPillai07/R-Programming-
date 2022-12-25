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
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")

##################################################
### Rename and Clean Variables                  ##
##################################################

#Loading the data

load("PROG8430_Assign_MLR_21S.Rdata")

#Renaming the R Data 

political_engagement_MRP<-PROG8430_Assign_MLR_21S

#Rename Variables to something meaningful

str(political_engagement_MRP)

names(political_engagement_MRP) <- c( "id_MRP","group_MRP", "hs.grad_MRP", "nation_MRP" ,
                                      "gender_MRP", "age_MRP","m.status_MRP","political_MRP",
                                      "n.child_MRP","income_MRP","food_MRP","housing_MRP", 
                                      "other_MRP","score_MRP" ,"scr_MRP", "time1_MRP", 
                                      "time2_MRP" ,"time3_MRP"  ,"Pol_MRP")



##################################################
### Coverting categorical data to Num           ##
##################################################

#Converting group_MRP to dummy variables
group_dummies_MRP<-model.matrix(~group_MRP -1,data = political_engagement_MRP)
head(group_dummies_MRP)

#Combining group_dummies_MRP to dataset
political_engagement_MRP<-cbind(political_engagement_MRP,group_dummies_MRP)
head(political_engagement_MRP)


#Converting hs.grad_MRP to dummy variables
hs.grad_dummies_MRP<-model.matrix(~hs.grad_MRP -1,data = political_engagement_MRP)
head(hs.grad_dummies_MRP)

#Combining hs.grad_MRP to dataset
political_engagement_MRP<-cbind(political_engagement_MRP,hs.grad_dummies_MRP)
head(political_engagement_MRP)


#Converting nation_MRP to dummy variables
nation_dummies_MRP<-model.matrix(~nation_MRP -1,data = political_engagement_MRP)
head(nation_dummies_MRP)

#Combining nation_MRP to dataset
political_engagement_MRP<-cbind(political_engagement_MRP,nation_dummies_MRP)
head(political_engagement_MRP)

#Converting gender_MRP to dummy variables
gender_dummies_MRP<-model.matrix(~gender_MRP -1,data = political_engagement_MRP)
head(gender_dummies_MRP)

#Combining gender_MRP to dataset
political_engagement_MRP<-cbind(political_engagement_MRP,gender_dummies_MRP)
head(political_engagement_MRP)

#Converting m.status_MRP to dummy variables
m.status_dummies_MRP<-model.matrix(~m.status_MRP -1,data = political_engagement_MRP)
head(m.status_dummies_MRP)

#Combining m.status_MRP to dataset
political_engagement_MRP<-cbind(political_engagement_MRP,m.status_dummies_MRP)
head(political_engagement_MRP)

#Converting political_MRP  to dummy variables
political_dummies_MRP<-model.matrix(~political_MRP  -1,data = political_engagement_MRP)
head(political_dummies_MRP)

#Combining political_MRP  to dataset
political_engagement_MRP<-cbind(political_engagement_MRP,political_dummies_MRP)
head(political_engagement_MRP)

names(political_engagement_MRP) <- c( "id_MRP","group_MRP", "hs.grad_MRP", "nation_MRP" ,
                                      "gender_MRP", "age_MRP","m.status_MRP","political_MRP",
                                      "n.child_MRP","income_MRP","food_MRP","housing_MRP", 
                                      "other_MRP","score_MRP" ,"scr_MRP", "time1_MRP", 
                                      "time2_MRP" ,"time3_MRP"  ,"Pol_MRP","group_control_MRP","group_treat_MRP",
                                      "hs.grad_NO_MRP","hs.grad_YES_MRP","nation_Asia_MRP","nation_Europe_MRP",
                                      "nation_NorthAmerica_MRP","nation_Southern_MRP","gender_female_MRP",
                                      "gender_male_MRP","m.status_divorced_MRP","m.status_married_MRP",
                                      "m.status_never_MRP","m.status_widowed_MRP","political_Conservative_MRP",
                                      "political_Liberal_MRP","political_NewDemocrat_MRP","political_Other_MRP")


#dropping categorical data

political_engagement_MRP <- political_engagement_MRP[-c(2,3,4,5,7,8)]

##############################
## Reduce Number of values    ##
##############################

# Find missing values
# Identifying cols> 95% missing

summary(political_engagement_MRP)

#Dropping column time1_MRP

political_engagement_MRP <- political_engagement_MRP[-c(10)]


# Identify Low Variance
stat.desc(political_engagement_MRP)  #Consider coef of var
summary(political_engagement_MRP$n.child_MRP)
boxplot(political_engagement_MRP$n.child_MRP, main ="Box plot of no. of child")
#Based on the above Evidence n.child_MRP is likely

#to see the count of the values
table(political_engagement_MRP$n.child_MRP)


#dropping n.child due to low variance
political_engagement_MRP <- political_engagement_MRP[-c(3)]
head(political_engagement_MRP)

#Identify High Correlation using stastical evidence

cor(political_engagement_MRP[1:11],method="spearman")

#graphical evidence
plot(political_engagement_MRP$time2_MRP,political_engagement_MRP$time3_MRP,
     main = "Plot for time2_MRP and time3_MRP")


#Removing time3_MRP due to high correlation with time2_MRP
political_engagement_MRP <- political_engagement_MRP[-c(10)]
head(political_engagement_MRP,5)




###################################################
## Find Outliers                                 ##
###################################################
#making a dataframe without binary variables
summary(political_engagement_MRP[1:10])
box_plot_MRP<-political_engagement_MRP[1:10]

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
corrgram(political_engagement_MRP, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Political Engagement")

res_MRP <- cor(political_engagement_MRP, method="spearman")
round(res_MRP, 2)


##################################################
### SLR Model                                   ##
##################################################

#Simple Linear Model  using Pol as the dependent variable and age as the independent.

Slr_model_MRP<- lm(Pol_MRP ~ age_MRP, data=political_engagement_MRP)

Slr_model_MRP

y <- political_engagement_MRP$Pol_MRP
x <- political_engagement_MRP$age_MRP
plot(x, y,main = "Pol_MRP by age_MRP (with Regression Line)",
     xlab = "age_MRP",ylab = "Pol_MRP",
     pch = 20, frame = TRUE)
abline(Slr_model_MRP, col = "blue")

summary(Slr_model_MRP)

#Simple Linear Model  using Pol as the dependent variable and income as the independent.

Slr_model1_MRP<- lm(Pol_MRP ~ income_MRP, data=political_engagement_MRP)

Slr_model1_MRP

y <- political_engagement_MRP$Pol_MRP
x <- political_engagement_MRP$income_MRP
plot(x, y, main = "Pol_MRP by income_MRP (with Regression Line)",
     xlab = "income_MRP",ylab = "Pol_MRP",
     pch = 20, frame = TRUE)
abline(Slr_model1_MRP, col = "blue")

summary(Slr_model1_MRP)

#########################################
## Creating Baseline Model             ##
#########################################

# id_MRP was just used for just indexing
#last variable of categorical data is not included in model input

Base_PE_MRP=lm(Pol_MRP ~  age_MRP + income_MRP +food_MRP+housing_MRP+ other_MRP+ score_MRP + scr_MRP
          +time2_MRP +group_control_MRP+ hs.grad_NO_MRP + nation_Asia_MRP +nation_Europe_MRP
          +nation_NorthAmerica_MRP +gender_female_MRP +m.status_divorced_MRP+m.status_married_MRP
          +m.status_never_MRP +political_Conservative_MRP+political_Liberal_MRP +political_NewDemocrat_MRP,
          data=political_engagement_MRP, na.action=na.omit)


Base_PE_MRP
summary(Base_PE_MRP)

#########################################
## Creating Backward Selection Model   ##
#########################################

Bck_PE_MRP = step(Base_PE_MRP, direction="backward", details=TRUE)

Bck_PE_MRP
summary(Bck_PE_MRP)


###########################################
## Creating Model and Residual vectors    #
###########################################

BckPEFit <- predict(Bck_PE_MRP)
BckPERes <- residuals(Bck_PE_MRP)

BasePEFit <- predict(Base_PE_MRP)
BasePERes <- residuals(Base_PE_MRP)

#Numerically

shapiro.test(BckPERes)
shapiro.test(BasePERes)


#graphical evidence

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(Base_PE_MRP)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section



par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(Bck_PE_MRP)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section






