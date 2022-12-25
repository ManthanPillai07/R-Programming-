##################################################
### PROG8430                                    ##
### Assignment 1                                ## 
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

#Set work directory to an appropriate location
setwd("C:/Users/Manthan/Documents/R Programs")

options(scipen=9)
##################################################
### Install Libraries                           ##
##################################################
if(!require(lattice)){install.packages("lattice")}
library("lattice")

if(!require(HSAUR)){install.packages("HSAUR")}
library("HSAUR")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

if(!require(vcd)){install.packages("vcd")}
library("vcd")


##################################################
### Loading the Data                                ##
##################################################

load("PROG8430_Assign_Explore.Rdata")
head(PROG8430_Assign_Explore,10)

#Exploring the structure of file

str(PROG8430_Assign_Explore)

##################################################
### 1. Summarizing Data                               ##
##################################################

#1.1.a Table for each political affiliation based on their total income 


Table1_MRP <- aggregate(PROG8430_Assign_Explore[,10], 
              by=list(PROG8430_Assign_Explore$political), FUN=sum, na.rm=TRUE)
Table1_MRP

#1.1.b Total income based on marital status

Table2_MRP <- aggregate(PROG8430_Assign_Explore[,10], 
              by=list(PROG8430_Assign_Explore$m.status), FUN=sum, na.rm=TRUE)
Table2_MRP

#1.2.a Creating a seprate table for Asian with age and n.child values

Table3_MRP<-PROG8430_Assign_Explore[PROG8430_Assign_Explore$nation=='Asia'
                                    , c("nation", "age", "n.child")]

#Mean age of respondents born in Asia

Table4_MRP <- aggregate(Table3_MRP[,2],
                        by=list(Table3_MRP$nation), FUN=mean, na.rm=TRUE)
Table4_MRP

#1.2.b mean age of respondent born in Asia weighted by no of children they have

weighted.mean(Table3_MRP$age,w=Table3_MRP$n.child)

#1.3.a mean score of both the genders

Table6_MRP <- aggregate(PROG8430_Assign_Explore$score, 
                  by=list(PROG8430_Assign_Explore$gender), FUN=mean, na.rm=TRUE)
Table6_MRP

#1.3.b Highest score from gender

Table7_MRP <- aggregate(PROG8430_Assign_Explore$score, 
                  by=list(PROG8430_Assign_Explore$gender), FUN=max, na.rm=TRUE)
Table7_MRP

#1.4.a The 31st and 61st percentiles of percentage of time taken on the test.

quantile(PROG8430_Assign_Explore$time1, c(.31, .61))

##################################################
### 2. Organizing Data                             ##
##################################################

                          
##################################################
### 2.1.a Pie Chart                            ##
##################################################
x_pie_MRP<-table(PROG8430_Assign_Explore$m.status)
x_pie_MRP

pie(x_pie_MRP, main='The number of respondents by marital status')

##################################################
### 2.2.a Summary Table                         ##
##################################################

ST_MRP <- table(PROG8430_Assign_Explore$nation,
                    PROG8430_Assign_Explore$hs.grad)
ST_MRP

x_MRP<-prop.table(ST_MRP,1)
x_MRP*100


##################################################
### 2.3.a Bar Plot                              ##
##################################################


Bar_MRP<-as.matrix(aggregate(PROG8430_Assign_Explore$scr,
                         by=list(PROG8430_Assign_Explore$nation), FUN=mean))
head(Bar_MRP)
rownames(Bar_MRP) <- Bar_MRP[,1] 
head(Bar_MRP)
Bar_MRP <- Bar_MRP[,-1]
head(Bar_MRP)

barplot(t(Bar_MRP),
        legend=TRUE,
        ylim=c(0.0,1.0),
        main="Standardize test score by region", 
        ylab="Mean of standardize test")

##################################################
### 2.4 Histograms                            ##
##################################################

Histogram_MRP<-hist(PROG8430_Assign_Explore$housing, breaks=5,
               xlab='Percent of income spent on housing',freq = TRUE,
               main="Frequeny of percentage of income spent on housing")
Histogram_MRP

##################################################
### 2.5 Box Plots                             ##
##################################################

Box_MRP<-boxplot(income ~ m.status, data=PROG8430_Assign_Explore, 
        main="Distribution of Income by Marital status",
        xlab="Marital status",  pch = 20)
Box_MRP

##################################################
### 2.6 Scatter Plots                         ##
##################################################

#2.6.a
Histo1_MRP<-hist(PROG8430_Assign_Explore$income, prob=FALSE,xlab='Income',
     main="pct of people based on their income")

#2.6.b
Histo1_MRP

Histo2_MRP<-hist(PROG8430_Assign_Explore$scr, breaks=5, prob=FALSE
                 ,xlab='Standardized Score', main="pct of 
                 people based on their standardized score")

Histo2_MRP

#2.6.c Scatter plot for income and standardized score
plot(income ~ scr, data=PROG8430_Assign_Explore, col=2, pch=20,
     main="Income by Standardized Score")


#2.6.a Correlation Coefficient

cor.test(PROG8430_Assign_Explore$income, PROG8430_Assign_Explore$scr)
#Pearson defaults, but assumes normality

cor.test(PROG8430_Assign_Explore$income, 
         PROG8430_Assign_Explore$scr, method="spearman")


##################################################
### 3.1.a QQ normal plot                        ##
##################################################

qqnorm(PROG8430_Assign_Explore$score, main="QQ normal test for score")
qqline(PROG8430_Assign_Explore$score)

#statistical test for normality on the Political Awareness Test Score
shapiro.test(PROG8430_Assign_Explore$score)

##################################################
### 3.2 Hypothesis Test                       ##
##################################################
#3.2.a Comparing box plot of two groups
bwplot(score ~ group, data=PROG8430_Assign_Explore, 
       main="Score by groups",
       xlab="Groups",  pch = '|')

#F-Test to see the ratio of variance
FT.ftest_MRP <- var.test(score ~ group, data = PROG8430_Assign_Explore)
FT.ftest_MRP

#T-test to see the group has significant difference or not
TT_MRP <- t.test(score ~ group, data = PROG8430_Assign_Explore,
                  var.equal = TRUE)
TT_MRP

##################################################
### 3.3 ANOVA                                 ##
##################################################

#3.3.a ANOVA for score and nations 
ANOVA_score_MRP <- aov(score~nation, data=PROG8430_Assign_Explore)
summary(ANOVA_score_MRP)

#Comparing Nation on basis of score
boxplot(score~nation, data=PROG8430_Assign_Explore,
        main="Political Awareness Test Score by Nation ",
        range=0)

#3.3.b ANOVA 
ANOVA_pol_MRP <- aov(Pol~political, data=PROG8430_Assign_Explore)
summary(ANOVA_pol_MRP)

#Comparing Political Affiliation on basis of Pol
boxplot(Pol~political, data=PROG8430_Assign_Explore,
        main="Political Involvement by Political Affiliations ",
        range=0)