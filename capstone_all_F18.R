library(readxl)
library(cluster)
library(factoextra)
library(tidyverse)
library(corrplot)
library(caret)
library(factoextra)
library(gridExtra)
library(NbClust)
library(rworldmap)
library(rworldxtra)
library(rsconnect)
library(foreign)

#getting excel data
df <- read_excel("capstone_data_1_all_F18.xlsx",sheet = 1)
df
str(df)

# Renaming the vairbles to be human readable :)
names(df)[2:23]<- c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12", "F13", "F14", "F15", "F16", "F17","F18", "F19", "F20","F21","F22","F23")

# Exploratory data analysis: visualisng and standardizing basic elements of the data

# checking correlations

cor_matrix = cor(df[,c(-1)],use="complete.obs")
corrplot(cor_matrix,method ="color",type="upper",tl.cex=0.7)

# Box plot for all varibles
par(cex.axis=0.52)
boxplot(df[,c(-1)], las=2)

#box plot of standardized data
boxplot(scale(df[,c(-1)]), las=2)

#Finding Null values:Method 1
#NA's in rows
null_rows = apply(df,1,function(x) sum(is.na(x)))

#add Company name
row_nulls = data.frame(df$Company, null_rows)

#select where value is not equal to zero
row_nulls[as.numeric(row_nulls[,2])>0,]

#checking for nulls in columns
apply(df,2,function(x) sum(is.na(x)))
#setting seed so our imputation results are reproducible
set.seed(20)

# Impute missing values with a random fores or omitting missing data: Method1
#imputation_model = preProcess(x=df[,c(-1)],method = "bagImpute")
#imputated_data =predict(object=imputation_model,newdata=df[,c(-1)])

#Adding country names to rows
df=df[,c(-1)]
row.names(df)<-df[[2]]

#Checking out this fresh imputated data
#head(imputated_data)
#str(imputated_data)
#we check nulls in imputated data, successful if there are none
apply(df,2,function(x) sum(is.na(x)))
str(df)

library(devtools)
#PCA analysis
pca.out<-prcomp(df, scale=FALSE)
pca.out
biplot(pca.out,scale=0,cex=0.75)

#Creating data table to store and plot the PCA no. of Principal Components Vs Cumulative Variance explained
vexplained<-as.data.frame(pca.out$sdev^2/sum(pca.out$sdev^2))
vexplained<-cbind(c(1:23),vexplained,cumsum(vexplained[,1]))
colnames(vexplained)<-c("No_of_Pricipal_Components","Individual_Variance_Explained","Cumulative_Variance")
#Table showing the amount of variance explained by the principal components
vexplained

library(factoextra)
# contributions 
plot1 <- fviz_contrib(pca.out, choice="var", axes = 1, top = 23, color = "blue")
plot2 <- fviz_contrib(pca.out, choice="var", axes = 2, top = 23, color = "blue")

library(gridExtra)
grid.arrange(plot1, plot2, nrow=2)

###Ends###