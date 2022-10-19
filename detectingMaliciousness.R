#Generating the data set----

#Import the data set into R Studio.
dat <- read.csv("MLDATASET_PartiallyCleaned.csv",
               na.strings="", stringsAsFactors=TRUE)
set.seed()
#Randomly select 500 rows
selected.rows <- sample(1:nrow(dat),size=500,replace=FALSE)
#Your sub-sample of 500 observations and excluding the 1st and last column
mydata <- dat[selected.rows,2:16]
dim(mydata) #check the dimension of your sub-sample

#================================================================================================================================================
#Lording Libraries ----
library(tidyverse)
#This library allows to get the skewness.
library(moments)
#To get plots
library(factoextra)
#To get ggplots
library(ggplot2)
#To get 3D scatterplots
library(scatterplot3d)

#================================================================================================================================================
#Part 1 ----

#This for loop answers to part 1 question 1 and question 2
for(i in 1:ncol(mydata)){
  Feature.Name <- names(mydata[i])
  if (unlist(lapply(mydata[i], is.numeric)) == FALSE){
    print( paste("categorical  Value -> ", Feature.Name)) 
    CategoricalNumbers <- mydata %>%
      pull(i) %>%
      table() %>%
      prop.table() * 100
    print(CategoricalNumbers)
  }
  else {
    print( paste("Numeric  Value -> ", Feature.Name)) 
    Numberic.Value.Summaries <- mydata %>%
      pull(i) %>%
      summary()
    Skweness <- mydata %>%
      pull(i) %>%
      skewness() %>%
      round(4)
    print(Numberic.Value.Summaries)
    print(paste("Skweness -> ", Skweness))
  }
}

#================================================================================================================================================
#Part 2 ----

#################################################################################################################################################
# Question 2:

#2.1 Compare No.Executable.Code.Found.In.Headers and Executable.Code.Maybe.Present.in.Headers:-

# convert YEA and No into binary values
tidy.mydata <- mydata %>%
  mutate(Executable.Code.Maybe.Present.in.Headers = ifelse(Executable.Code.Maybe.Present.in.Headers=='YES', 1, 0))

# convert all the YES values into 0
tidy.mydata <- tidy.mydata %>%
  mutate(No.Executable.Code.Found.In.Headers = ifelse(No.Executable.Code.Found.In.Headers=='YES', 0, No.Executable.Code.Found.In.Headers))

# convert all the NA values into 1
tidy.mydata$No.Executable.Code.Found.In.Headers[is.na(tidy.mydata$No.Executable.Code.Found.In.Headers)] <- 1

# Check whether they are identical or not.
if(identical(tidy.mydata$Executable.Code.Maybe.Present.in.Headers, tidy.mydata$No.Executable.Code.Found.In.Headers) == TRUE)
{
  print("There the are identical to each other.")
  print("removing the column No.Executable.Code.Found.In.Headers")
  # Delete the entire No.Executable.Code.Found.In.Headers column
  tidy.mydata <- subset(mydata, select = -No.Executable.Code.Found.In.Headers)
} else{
  print("they are not identical to each other")
}

#################################################################################################################################################
#Dealing with NA values:-

#Check the location of missing data in mydata data frame
which(is.na(mydata))

#Get the sum of missing data
sum(is.na(mydata))

#identify the columns with missing data.
colSums(is.na(tidy.mydata))

#check the number of Emails in Download.Source
table(tidy.mydata$Download.Source)

#Checking all 36 NA values in 
NA.Values <- tidy.mydata %>%
  filter(is.na(Calls.To.Low.level.System.Libraries));NA.Values

#Excluding missing values
#tidy.mydata <- na.omit(tidy.mydata)

#either this command can be used to remove NA values.
#tidy.mydata <- tidy.mydata[complete.cases(mydata), ]

#################################################################################################################################################
#Dealing with Outliers:- 

# raw 487 looks like an outlier
ol.num.mydata <- tidy.mydata %>%
  select_if(is.numeric)
m.data <- lm(Ping.Time.To.Server~Characters.in.URL, data=ol.num.mydata)
plot(m.data)

#Dealing with outliers -> Function to find outlier percentage ----
OutlierPerecentage <- function(x){
  Q1 <- quantile(x, 0.25, na.rm=T)
  Q3 <- quantile(x, 0.75, na.rm=T)
  IQR <- Q3 - Q1
  bench <- Q3 + 1.5*IQR
  extreme.threshold.upper <- (IQR * 3) + Q3
  extreme.threshold.lower <- (IQR * 1.5) - Q1
  result1 <- length(x[x > extreme.threshold.upper]);result1
  result2 <- length(x[x < extreme.threshold.lower]);result2
  Num.Outliers <- result1+result2
  Precentage <- (Num.Outliers * 100) / length(x)
  print(paste('Precentage of outliers -> ', Precentage))
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> Ping time to server----

#Checking any outliers with boxplot
boxplot(tidy.mydata$Ping.Time.To.Server)
summary(tidy.mydata$Ping.Time.To.Server)

#Get outlier percentage
OutlierPerecentage(tidy.mydata$Ping.Time.To.Server)

#try get the idea about download speed and ping time to server.
boxplot(tidy.mydata$Ping.Time.To.Server~tidy.mydata$Download.Speed)

# remove 800 value
tidy.mydata <- tidy.mydata %>%
  mutate(Ping.Time.To.Server=ifelse(Ping.Time.To.Server>600 & Download.Speed == "Greater than 10MB/s",NA,Ping.Time.To.Server)); tidy.mydata

tidy.mydata <- tidy.mydata %>%
  mutate(Ping.Time.To.Server=ifelse(Ping.Time.To.Server>600 & Download.Speed == "Less than 100 KB/s",NA,Ping.Time.To.Server)); tidy.mydata

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> How.Many.Times.File.Seen----

#Checking any outliers with boxplot
boxplot(tidy.mydata$How.Many.Times.File.Seen)

#getting the percentage of outliers
OutlierPerecentage(tidy.mydata$How.Many.Times.File.Seen)

#filtering higher and weird value
tidy.mydata %>%
  filter(How.Many.Times.File.Seen>50000)

#Get the mean values of the this feature
GetMeanOFHMTFS <- round(mean(tidy.mydata$How.Many.Times.File.Seen),0); GetMeanOFHMTFS

# since this feature does not have any NA and we have to add a value for that i use the mean
tidy.mydata <- tidy.mydata %>%
  mutate(How.Many.Times.File.Seen=ifelse(How.Many.Times.File.Seen>50000,GetMeanOFHMTFS,How.Many.Times.File.Seen))

# Or you could either remove that raw completely.
tidy.mydata <- tidy.mydata[!(tidy.mydata$How.Many.Times.File.Seen > 50000),]

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> Threads.Started----

#Checking any outliers with boxplot
boxplot(tidy.mydata$Threads.Started)

#Getting the percentage of outliers
OutlierPerecentage(tidy.mydata$Threads.Started)

#filtering outliers.
tidy.mydata %>%
  filter(Threads.Started>3.5)

round(log(tidy.mydata$Threads.Started),0)

#removing values which are more than 3.5
tidy.mydata <- tidy.mydata %>%
  mutate(Threads.Started=ifelse(Threads.Started>5,round(log(tidy.mydata$Threads.Started),0),Threads.Started))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> Characters.in.URL----

#boxplot Calls.To.Low.level.System.Libraries
boxplot(tidy.mydata$Characters.in.URL)

#percentage of outlier
OutlierPerecentage(tidy.mydata$Characters.in.URL)

tidy.mydata %>%
  filter(Characters.in.URL>110)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> Mean.Word.Length.of.Extracted.Strings----

#Get the boxplot to see outliers
boxplot(tidy.mydata$Mean.Word.Length.of.Extracted.Strings)

#get the percentage of ouliers
OutlierPerecentage(tidy.mydata$Mean.Word.Length.of.Extracted.Strings)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> File.Size..Bytes.----

boxplot(tidy.mydata$File.Size..Bytes.)
#get the percentage of ouliers
OutlierPerecentage(tidy.mydata$File.Size..Bytes.)

summary(tidy.mydata$File.Size..Bytes.)

tidy.mydata %>%
  filter(File.Size..Bytes.>37146187)
#################################################################################################################################################

#Question 2 ----

#Write to a csv file
write.csv(tidy.mydata, "mydata.csv")

######################################################### PCA #####################################################################################

#Question 3 ----


#Extracting only continues values----
num.mydata <- tidy.mydata %>%
  select_if(is.numeric) 

#Replacing NA values with 0
num.tidy.mydata <- num.mydata %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  cbind(tidy.mydata$Actually.Malicious)

#Check the first few line of the num.mydata dataframe. 
head(num.tidy.mydata)

# (...Optional...) removing na values to calculate PCA (...Optional...)
na.num.mydata <- tidy.mydata %>%
  select_if(is.numeric) %>%
  na.omit(num.mydata)


#Calculate the PCA with prcomp function.
my.pca <- prcomp(num.tidy.mydata[, 1:8], scale=TRUE)

#Checking my.pca 
my.pca
str(my.pca)

#Get the summary of the my.pca
summary(my.pca)

#Ladings of the first 4 components
my.pca$rotation[,1:4]


## ---------------------------------------------------------------------
#checking the variances in each principal components
plot(my.pca, type="l")
#---OR---
fviz_eig(my.pca)
#---OR---
biplot(my.pca, scale = 0)
pca.var <- my.pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

#2.3.3. barplot with 12.4 threshold ----
barplot(pca.var.per, main="The Scree Plot", xlab="Principal Component",
        ylab="Percent Variation") +
  abline(12.5, 0, col = "red") #creating the red line thought the 12.5 value.

fviz_pca_ind(my.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

## ---------------------------------------------------------------------
#use the base R plot function to check PC1 and PC2
my.pca$x
plot(my.pca$x[,1], my.pca$x[,2])

#Check the correlation of the extracted numeric data set.
view(cor(num.tidy.mydata[,1:8]))

# Extract PC scores----
num.mydata.pca <- cbind(num.tidy.mydata, my.pca$x[,1:2])
#Rename Actually.Malicious column
colnames(num.mydata.pca)[9] = "Actually.Malicious"

#checking the success of the above codes.
head(num.mydata.pca)


#Creating the Plots ----

## ---------------------------------------------------------------------
#Scatter plot matrix
pairs(num.mydata.pca[,1:8],
      pch=21,
      col=as.numeric(num.mydata.pca$Actually.Malcious)+1,  
      bg=alpha(as.numeric(num.mydata.pca$Actually.Malicious)+1,0.4), 
      cex=1.5,  
      upper.panel=NULL,  
      labels=gsub("[[:punct:]]"," ",colnames(num.mydata.pca[,1:8])))  

## ---------------------------------------------------------------------
ggplot(num.mydata.pca, aes(PC1, PC2, col = Actually.Malicious, fill=Actually.Malicious)) +
  stat_ellipse(geom = "polygon", col="black", alpha=0.45) +
  geom_point(shape = 21, col = "black")


## ---------------------------------------------------------------------
#visualizing with PC1 and PC2 with 2 dimensions.
ggplot(num.mydata.pca,aes(x=PC1,y=PC2))+
  geom_point(aes(colour=Actually.Malicious),alpha=0.5,size=2)+
  theme_minimal(base_size=10)+
  theme(legend.position = "bottom")+
  xlab("PC1")+
  ylab("PC2")

## ---------------------------------------------------------------------
#3D plot
scatterplot3d(my.pca$x[,1:3],pch=21,
              color=as.numeric(num.mydata.pca$Actually.Malicious)+1,
              bg=alpha(as.numeric(num.mydata.pca$Actually.Malicious)+1,0.4),                  
              cex.symbols=2,
              col.grid="steelblue",
              col.axis="steelblue",
              angle=45);

## Biplot 2.5.1 ---------------------------------------------------------------------
fviz_pca_biplot(my.pca,
                axes = c(1,2),   
                col.ind=num.mydata.pca$Actually.Malicious,  
                fill.ind=num.mydata.pca$Actually.Malicious, 
                alpha=0.8, #adding the transparency to the Biplot
                pointsize=4, 
                pointshape=21,# Defining shape of the points
                col.var="red", #adding the color to the Points 
                label="var",  
                repel=TRUE,  #Avoiding the overlapping
                addEllipses=TRUE,  
                legend.title=list(colour="Actually.Malicious",fill="Actually.Malicious",alpha="Actually.Malicious"))

## Biplot 2.5.2 ---------------------------------------------------------------------

fviz_pca_var(my.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
