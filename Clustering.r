##################################################
### PROG8430                                    ##
### K-Means Clustering                          ## 
##################################################
#                                               ##
##################################################
# Written by David Marsh
# ID: 8643279
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

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")


if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

##################################################
### Read in Data                                ##
##################################################

##################################################
### Read data and do preliminary data checks    ##

load("PROG8430_Clst_21S.Rdata")

Income_kmean_MRP<-PROG8430_Clst_21S

str(Income_kmean_MRP)

names(Income_kmean_MRP)<-c("Food_MRP", "Entr_MRP", "Educ_MRP","Tran_MRP","Work_MRP","Hous_MRP","Othr_MRP")


###################################################
## Standardize Data                              ##
###################################################

#Create a quick standardization function
norm01_MRP <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Standardizing all variable
#standardizing Food 
Income_kmean_MRP$Food_MRP_MinMax <- norm01_MRP(Income_kmean_MRP$Food_MRP)
summary(Income_kmean_MRP$Food_MRP_MinMax)

#standardazing Entr
Income_kmean_MRP$Entr_MRP_MinMax<-norm01_MRP(Income_kmean_MRP$Entr_MRP)
summary(Income_kmean_MRP$Entr_MRP_MinMax)

#Standardizing Educ
Income_kmean_MRP$Educ_MRP_MinMax<-norm01_MRP(Income_kmean_MRP$Educ_MRP)
summary(Income_kmean_MRP$Educ_MRP_MinMax)

#Standardizing Tran
Income_kmean_MRP$Tran_MRP_MinMax<-norm01_MRP(Income_kmean_MRP$Tran_MRP)
summary(Income_kmean_MRP$Tran_MRP_MinMax)

#Standardizing Work
Income_kmean_MRP$Work_MRP_MinMax<-norm01_MRP(Income_kmean_MRP$Work_MRP)
summary(Income_kmean_MRP$Work_MRP_MinMax)

#Standardizing Hous
Income_kmean_MRP$Hous_MRP_MinMax<-norm01_MRP(Income_kmean_MRP$Hous_MRP)
summary(Income_kmean_MRP$Hous_MRP_MinMax)

#Standardizing Othr
Income_kmean_MRP$Othr_MRP_MinMax<-norm01_MRP(Income_kmean_MRP$Othr_MRP)
summary(Income_kmean_MRP$Othr_MRP_MinMax)

summary(Income_kmean_MRP)
str(Income_kmean_MRP)

###################################################
## Descriptive Data Analysis                     ##
###################################################

#creating a histogram
hist_plot_MRP<-Income_kmean_MRP
str(hist_plot_MRP)

par(mfrow=c(3,3))

# Histogram for all variables
# loop over column *names* instead of actual columns
sapply(names(hist_plot_MRP), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(hist_plot_MRP[[cname]]))
    # use the `main` param to put column name as plot title
    print(hist(hist_plot_MRP[[cname]], main=cname))
})

par(mfrow=c(1,1))

#creating a box plot
box_plot_MRP<-Income_kmean_MRP

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
##CLlustering                                   ##
###################################################
str(Income_kmean_MRP)
IncomeClstrData_MRP <- Income_kmean_MRP[c(9,13)]   
str(IncomeClstrData_MRP)

# Cluster with k=2
ClstrInc_MRP <- kmeans(IncomeClstrData_MRP, iter.max=10, centers=2, nstart=10)
ClstrInc_MRP

Income_kmean_MRP$cluster <- factor(ClstrInc_MRP$cluster)   # Adding Cluster tags to variables
head(Income_kmean_MRP)

# Cluster with k=3
ClstrInc3_MRP<- kmeans(IncomeClstrData_MRP, iter.max=10, centers=3, nstart=10)
ClstrInc3_MRP

# Cluster with k=4
ClstrInc4_MRP<- kmeans(IncomeClstrData_MRP, iter.max=10, centers=4, nstart=10)
ClstrInc4_MRP

# Cluster with k=5
ClstrInc5_MRP<- kmeans(IncomeClstrData_MRP, iter.max=10, centers=5, nstart=10)
ClstrInc5_MRP

# Cluster with k=6
ClstrInc6_MRP<- kmeans(IncomeClstrData_MRP, iter.max=10, centers=6, nstart=10)
ClstrInc6_MRP

###################################################
##Evalution of cluster                           ##
###################################################

#scatter plot for k=3
Income_kmean_MRP$cluster <- factor(ClstrInc3_MRP$cluster)   # Adding Cluster tags to variables
head(Income_kmean_MRP)

centers <- data.frame(cluster=factor(1:3), ClstrInc3_MRP$centers)

ggplot(data=Income_kmean_MRP, aes(x=Entr_MRP_MinMax, y=Hous_MRP_MinMax, color=cluster)) + geom_point()

ggplot(data=Income_kmean_MRP, aes(x=Entr_MRP_MinMax, y=Hous_MRP_MinMax, color=cluster, shape=cluster)) + 
  geom_point(alpha=.8) +
  geom_point(data=centers, aes(x=Entr_MRP_MinMax, y=Hous_MRP_MinMax), size=3, stroke=2)

#dropping cluster column before plotting next cluster 
Income_kmean_MRP<-Income_kmean_MRP[c(-15)]

#scatter plot for k=5
Income_kmean_MRP$cluster <- factor(ClstrInc5_MRP$cluster)   # Adding Cluster tags to variables
head(Income_kmean_MRP)

centers <- data.frame(cluster=factor(1:5), ClstrInc5_MRP$centers)

ggplot(data=Income_kmean_MRP, aes(x=Entr_MRP_MinMax, y=Hous_MRP_MinMax, color=cluster)) + geom_point()

ggplot(data=Income_kmean_MRP, aes(x=Entr_MRP_MinMax, y=Hous_MRP_MinMax, color=cluster, shape=cluster)) + 
  geom_point(alpha=.8) +
  geom_point(data=centers, aes(x=Entr_MRP_MinMax, y=Hous_MRP_MinMax), size=5, stroke=2)

#dropping cluster column before plotting next cluster 
Income_kmean_MRP<-Income_kmean_MRP[c(-15)]

#scatter plot for k=4
Income_kmean_MRP$cluster <- factor(ClstrInc4_MRP$cluster)   # Adding Cluster tags to variables
head(Income_kmean_MRP)

centers <- data.frame(cluster=factor(1:4), ClstrInc4_MRP$centers)

ggplot(data=Income_kmean_MRP, aes(x=Entr_MRP_MinMax, y=Hous_MRP_MinMax, color=cluster)) + geom_point()

ggplot(data=Income_kmean_MRP, aes(x=Entr_MRP_MinMax, y=Hous_MRP_MinMax, color=cluster, shape=cluster)) + 
  geom_point(alpha=.8) +
  geom_point(data=centers, aes(x=Entr_MRP_MinMax, y=Hous_MRP_MinMax), size=4, stroke=2)

 

# creating the summary table

str(Income_kmean_MRP)
Income_spend_MRP <- Income_kmean_MRP %>% 
  group_by(cluster) %>% 
  summarise(food = mean(Food_MRP), Entr = mean(Entr_MRP), Edu=mean(Educ_MRP), Tran=mean(Tran_MRP), Work=mean(Work_MRP),
            Hous=mean(Hous_MRP), Other=mean(Othr_MRP), N=n() )

Income_spend_MRP
