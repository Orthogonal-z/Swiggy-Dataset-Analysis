# Working Directory
setwd("C:\\Users\\kapil\\Desktop\\GitHub\\Swiggy Dataset")
getwd()


#Load the Dataset 
swiggy <- read.csv("C:\\Users\\kapil\\Desktop\\GitHub\\Swiggy Dataset\\Swiggy.csv")


#Load the Packages 
library("ggplot2")
library("dplyr")
library("car")


#Check How many Columns are there 
colnames(swiggy)

#Select only 3 columns from the entire dataset 
Data <- swiggy %>% 
        select("Price.range", "Aggregate.rating", "Votes")


#Dimensions in the Dataset 
dim(Data)

#Took 4000  samples from entire dataset 
Data.2 <- Data[sample(nrow(Data), 4000), ]

#Table to see diffrent categories in the Price range 
table(Data.2$Price.range)


#Plot the Price Range and Counts of Restaurants 
ggplot(data = Data.2, aes(factor(x = Data.2$Price.range))) +
  geom_bar(col = "black", fill = "steelblue") +
  ggtitle("Number of restaurants in each price range") + 
  xlab ("Price Range") + ylab ("Count")


#Plot the density curve to check what our customers rated most 
plot(density(Data.2$Aggregate.rating), col = "darkblue", 
     main = "Density Curve of Ratings")

#Plot the Boxplot to check most rated price range restaurants 
qplot(factor(Data.2$Price.range), y = Data.2$Aggregate.rating, 
      geom = "boxplot", main = "Aggregate ratings by price range", 
      data = Data.2, xlab = "Price Range", ylab = "rating", 
      fill = I("darkgreen"))

#Anova Test to see 
#Whether these diffrences are significant that more expensive food meanse more ratings
#P.value is lower than 0.05
#It meanse we reject the null hypothesis. This proves that 
#There is diffrence in the group the aggregate rating at diffrent price range 
anova <- aov(Data.2$Aggregate.rating ~ factor(Data.2$Price.range))
summary(anova)


#As we have multiple groups here to prove which  groups are significantly diffrent from one another 
#So we will use the TukeyHSD Test here 
#Only combination of 3-4 restaurants and their price ratings are not significantly diffrent 
TukeyHSD <- TukeyHSD(anova)
TukeyHSD

#Plot to check whether our data is normally distributed or not  
#As you can see in the plot our data is not normally distributed 
Nor.Plot <- plot(anova, 2, col = "red", cex = 0.5)
Nor.Plot

#Calculate Residuals from the anova 
aov_residuals <- residuals(object = anova)


#To confirm its normally distributed or not we will use statistical test like shapiro wilk test 
shapiro.test(x = aov_residuals)

#Check whether our diffrent categorical have equal variance or not 
LT <-leveneTest(Data$Aggregate.rating ~ factor(Data$Price.range))
LT
                       

#One Way Test to check whether there is diffrence in restaurants rating at diffrent price range 
#As our P.valu is lower than 0.05, we reject thhe null hypothesis here 
#Yes there is some diffrence between the restraunts at diffrent range 
oneway.test(Data.2$Aggregate.rating ~factor(Data.2$Price.range))

#As our P.value is lower than 0.05 in each row 
#it meanse we reject the null hypothesis in each row, it meanse there is no significance is there 
pairwise.t.test(Data$Aggregate.rating, Data$Price.range, p.adjust.method = "BH", pool.sd = F)

#Kruskall wallis test to check whether the diffrence between the groups are statistically significant or not 
#As our p.value is lower than 0.05 we reject the null hypothesis 
#It meanse the diffrence between the groups are statistically significant 
K.T <- kruskal.test(Data.2$Votes, Data.2$Price.range)
K.T



























































