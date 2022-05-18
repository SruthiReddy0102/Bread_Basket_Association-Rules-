# Performing Association Rules on Bread-Basket dataset

# Importing Required Libraries
library(dplyr)     # Used for data manipulation challenges
library(plyr)      # split data apart, do stuff to it, and mash it back together.
library(ggplot2)   # create complex plots from data in a data frame
library(arules)    # Mining Association Rules and Frequent Itemsets with R
library(arulesViz) # Visualizing Association Rules and Frequent Itemsets
library(knitr)     # a general-purpose literate programming engine
library(tidyverse) # data manipulation
library(lubridate) # R package that makes it easier to work with dates and times
library(gridExtra) # arranging ggplot in grid
library(data.table)# extension of data. frame package in R
library(splitstackshape) # Stack and Reshape Datasets After Splitting Concatenated Values

# Loading the Dataset
Bread_df <- read.csv("F:/Salford Uni/CourseWork/ASDM/Task 2/bread basket.csv")

dim(Bread_df)
str(Bread_df)

summary(Bread_df)

#creating a data frame  with 0 columns and 20507 rows
Bread_df[,colSums(is.na(Bread_df)) > 0]
View(Bread_df)

# Ten most popular items sold by the bakery
x <- as.data.frame(plyr::count(Bread_df, 'Item'))
x <- x %>% arrange(desc(freq))
x[1:10,]

# Most popular period of day for bakery sale
y <- as.data.frame(plyr::count(Bread_df, 'period_day'))
y <- y %>% arrange(desc(freq))
y


# Breaking down date_time column in date column, time coulumn, year column, month column and day column
temp <- as.POSIXlt(Bread_df$date_time, format="%d-%m-%Y %H:%M")
Bread_df$year <- year(temp)
Bread_df$month <- month(temp)
Bread_df$date <- date(temp)
Bread_df$time <- as.ITime(temp, format = "%H:%M")
Bread_df$day <- weekdays(date(temp))
Bread_df$month <- month.abb[Bread_df$month]
head(Bread_df)

Bread_df$month <- as.factor(Bread_df$month)
Bread_df$Date <- as.Date(Bread_df$date)


# Univariate Analysis
# Transaction Data for Weekday and Weekend
ggplot(data=Bread_df, aes(x = weekday_weekend)) +
  geom_bar(color= "black",fill="deeppink4") + 
  ggtitle("Transaction Data for Weekday and Weekend")

# Bivariate Analysis
ggplot(Bread_df, aes(x=weekday_weekend, fill = period_day)) +
  scale_fill_manual(values=c("darkmagenta","lightsalmon","forestgreen","darkred"))+
  geom_bar(position = "dodge") +
  ggtitle("Transaction Data for Weekday and Weekend")

#Creating an item list and converting data into transaction data
Bread_df <- Bread_df[complete.cases(Bread_df),]
Bread_df$Transaction <- as.numeric(Bread_df$Transaction)
Bread_df_sorted <- Bread_df[order(Bread_df$Transaction), ]


itemlist <- ddply(Bread_df, c("Transaction"),
                  function(df1)paste(df1$Item, collapse = ","))

itemlist$Transaction <- NULL
colnames(itemlist) <- c("items")

# Write to csv file
write.csv(itemlist, "F:/Salford Uni/CourseWork/ASDM/Task 2/bread_basket.csv", quote = FALSE,row.names = TRUE)

#Model Evaluation

# Importing the data
trans <- read.transactions("F:/Salford Uni/CourseWork/ASDM/Task 2/bread_basket.csv",format = "basket", sep = ",")

trans
summary(trans)
glimpse(trans)

str(trans)

#Absolute Item Frequency Plot
itemFrequencyPlot(trans, topN=15, type="absolute", col=rainbow(15),xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")

# Relative Item Frequency Plot
itemFrequencyPlot(trans, topN=15, type="relative", col=rainbow(15), xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot")

# Inspecting the data for further analysis
cat("No of baskets:", length(trans)) # concatenate string & integer value
cat("No of unique items:", sum(size(trans)))


#Apriori algorithm

# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers 
rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup1 <- integer(length=9)
rules_sup0.5 <- integer(length=9)

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup10[i] <- length(apriori(trans, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup5[i] <- length(apriori(trans, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup1[i] <- length(apriori(trans, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup0.5[i] <- length(apriori(trans, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
}


# Number of rules found with a support level of 10%
plot1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 10%") +
  theme_bw()

# Number of rules found with a support level of 5%
plot2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 5%") + 
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  theme_bw()

# Number of rules found with a support level of 1%
plot3 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 1%") + 
  scale_y_continuous(breaks=seq(0, 50, 10)) +
  theme_bw()

# Number of rules found with a support level of 0.5%
plot4 <- qplot(confidenceLevels, rules_sup0.5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 0.5%") + 
  scale_y_continuous(breaks=seq(0, 130, 20)) +
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_rules, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=rules_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=rules_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=rules_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=rules_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())

# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
rules_sup1_conf50 <- apriori(trans, parameter=list(sup=supportLevels[3], 
                                                   conf=confidenceLevels[5], target="rules"))

# Association rules
inspect(rules_sup1_conf50)

# Scatter plot
plot(rules_sup1_conf50, measure=c("support","lift"), shading="confidence")

# Parallel coordinates plot
plot(rules_sup1_conf50, method="paracoord", control=list(reorder=TRUE))

# Graph
plot(rules_sup1_conf50, method="graph")

# Graph
plot(rules_sup1_conf50, method="graph", control=list(layout=igraph::in_circle()))

# Grouped matrix plot
plot(rules_sup1_conf50, method="grouped")

plot(rules_sup1_conf50, method = "graph", measure = "lift", shading = "confidence", engine = "htmlwidget")

# Rules for Coffee
coffee_rules <- apriori(data=trans,  parameter=list(supp=0.01, conf = 0.5, target="rules"), appearance = list(default="lhs", rhs="Coffee"), control=list(verbose=F)) 
coffee_rules_byconf <- sort(coffee_rules, by="confidence", decreasing=TRUE)
inspect(coffee_rules_byconf)

plot(coffee_rules, method="graph", cex=0.7, shading="lift",control=list(col=rainbow(7)))# graph based on confidence
plot(coffee_rules , method="paracoord")# parallel coordinates plot based on support


# Apriori algorithm execution with a support level of 0.5% and a confidence level of 10%
rules_sup0.5_conf10 <- apriori(trans, parameter=list(sup=supportLevels[4], conf=confidenceLevels[9], target="rules"))

# Graph
plot(rules_sup0.5_conf10, method="graph", control=list(layout=igraph::in_circle()))


# Parallel coordinates plot
plot(rules_sup0.5_conf10, method="paracoord", control=list(reorder=TRUE))

# Grouped matrix plot
plot(rules_sup0.5_conf10, method="grouped")

# Scatter plot
plot(rules_sup0.5_conf10, measure=c("support","lift"), shading="confidence", jitter=0)



ruleexploer