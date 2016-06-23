library(dplyr)
library(plyr)
library(readr)
library(moments)
library(ggplot2)
library(reshape2)
library(car)
library(psych)
library(corrplot)
library(PerformanceAnalytics)

# read training data
raw <- read_csv("moneyball-training-data.csv")
str(raw)

train <- raw
head(train)
str(train)

# rename columns
names(train) <- c("index", "wins", "base_hits", "doubles", "triples", "homeruns", 
                  "walks", "b_strikeouts", "stolen_bases", "caught_stealing", 
                  "hit_by_pitch", "hits_allowed", "homeruns_allowed", 
                  "walks_allowed", "p_strikeouts", "errors", "double_plays")


# delete index column
train <- train[-1]


## DATA EXPLORATION ##

# descriptive statistics
describe(train)


# create singles by batters variable
train$singles <- train$base_hits - (train$doubles
                               + train$triples + train$homeruns)

# check caught_stealing and stolen_bases for collinearity
plot(train$caught_stealing, train$stolen_bases)


# delete caught_stealing, hit_by_pitch variables
train <- train[-10]
train <- train[-9]

# summary
summary(train)

# reorder columns
train <- train[c(1,2,15,3,4,5,6,7,8,9,10,11,12,13,14)]
summary(train)

## NEW DF ##
ntrain <- train

# descriptive statistics
describe(ntrain)

# correlation matrix (before imputing)
corntrain <- cor(ntrain, use = "complete.obs")
corrplot(corntrain, method = "number")
corntrain

## NEW DF ##
ntrain2 <- ntrain

## IMPUTE MISSING VALUES ##

# analyze missing data impute values for missing data
library(mice)
md.pattern(ntrain2)

# generate histogram and count of missing data
library(VIM)
aggr_plot <- aggr(ntrain2, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

# impute missing data
tempData <- mice(ntrain2,m=9, maxit=50, meth='pmm', seed=500)

# bring imputed data into dataset
ctrain <- complete(tempData,1)
summary(ctrain)

# view imputed data in dataset
# stripplot(tempData, pch = 20, cex = 1.2)

# descriptive statistics (imputed data)
describe(ctrain)

# correlation matrix (after imputing)
corctrain <- cor(ctrain, use = "complete.obs")
corrplot(corctrain, method = "number")
corctrain

# boxplot of outliers
par(mfrow=c(1,1)) # reset plot grid
ggplot(melt(ctrain), aes(x=variable, y=value, colour=variable)) + 
  geom_boxplot() +
  ggtitle("Boxplots")

# summary
summary(ctrain)

# examine data for outliers, convert to NA

itrain <- ctrain

# descriptive statistics (pre-outlier cleaning)
describe(itrain)

## hits allowed
qqnorm(itrain$hits_allowed)
hist(itrain$hits_allowed)
plot(itrain$hits_allowed)
outlier_values <- boxplot.stats(itrain$hits_allowed)$out  # outlier values.
sort(outlier_values)
itrain$hits_allowed <- as.integer(recode(itrain$hits_allowed,"outlier_values='NA'"))
hist(itrain$hits_allowed)
qqnorm(itrain$hits_allowed)
skewness(na.omit(itrain$hits_allowed))
kurtosis(na.omit(itrain$hits_allowed))

## errors
qqnorm(itrain$errors)
hist(itrain$errors)
plot(itrain$errors)
outlier_values <- boxplot.stats(itrain$errors)$out  # outlier values.
sort(outlier_values)
itrain$errors <- as.integer(recode(itrain$errors,"outlier_values='NA'"))
hist(itrain$errors)
plot(itrain$errors)
qqnorm(itrain$errors)
skewness(na.omit(ntrain$errors))
kurtosis(na.omit(ntrain$errors))

## stolen_bases
qqnorm(itrain$stolen_bases)
hist(itrain$stolen_bases)
plot(itrain$stolen_bases)
outlier_values <- boxplot.stats(itrain$stolen_bases)$out  # outlier values.
sort(outlier_values)
itrain$stolen_bases <- as.integer(recode(itrain$stolen_bases,"outlier_values='NA'"))
hist(itrain$stolen_bases)
plot(itrain$stolen_bases)
qqnorm(itrain$stolen_bases)
skewness(na.omit(itrain$stolen_bases))
kurtosis(na.omit(itrain$stolen_bases))

## p_strikeouts
qqnorm(itrain$p_strikeouts)
hist(itrain$p_strikeouts)
plot(itrain$p_strikeouts)
outlier_values <- boxplot.stats(itrain$p_strikeouts)$out  # outlier values.
sort(outlier_values)
itrain$p_strikeouts <- as.integer(recode(itrain$p_strikeouts,"outlier_values='NA'"))
hist(itrain$p_strikeouts)
plot(itrain$p_strikeouts)
qqnorm(itrain$p_strikeouts)
skewness(na.omit(itrain$p_strikeouts))
kurtosis(na.omit(itrain$p_strikeouts))

## walks_allowed
qqnorm(itrain$walks_allowed)
hist(itrain$walks_allowed)
plot(itrain$walks_allowed)
outlier_values <- boxplot.stats(itrain$walks_allowed)$out  # outlier values.
sort(outlier_values)
itrain$walks_allowed <- as.integer(recode(itrain$walks_allowed,"outlier_values='NA'"))
hist(itrain$walks_allowed)
plot(itrain$walks_allowed)
qqnorm(itrain$walks_allowed)
skewness(na.omit(itrain$walks_allowed))
kurtosis(na.omit(itrain$walks_allowed))

## base_hits
qqnorm(itrain$base_hits)
hist(itrain$base_hits)
plot(itrain$base_hits)
outlier_values <- boxplot.stats(itrain$base_hits)$out  # outlier values.
sort(outlier_values)
itrain$base_hits <- as.integer(recode(itrain$base_hits,"outlier_values='NA'"))
hist(itrain$base_hits)
plot(itrain$base_hits)
qqnorm(itrain$base_hits)
skewness(na.omit(itrain$base_hits))
kurtosis(na.omit(itrain$base_hits))

## singles
qqnorm(itrain$singles)
hist(itrain$singles)
plot(itrain$singles)
outlier_values <- boxplot.stats(itrain$singles)$out  # outlier values.
sort(outlier_values)
itrain$singles <- as.integer(recode(itrain$singles,"outlier_values='NA'"))
hist(itrain$singles)
plot(itrain$singles)
qqnorm(itrain$singles)
skewness(na.omit(itrain$singles))
kurtosis(na.omit(itrain$singles))

summary(itrain)
describe(itrain)

# check boxplots
par(mfrow=c(1,1)) # reset plot grid
ggplot(melt(itrain), aes(x=variable, y=value, colour=variable)) + 
  geom_boxplot() +
  ggtitle("Boxplots")

## NEW DF ##
itrain2 <- itrain

# impute missing data (after outlier removal)
tempData <- mice(itrain2,m=7, maxit=50, meth='pmm', seed=500)

# bring imputed data into dataset
ftrain <- complete(tempData,1)

summary(ftrain)
describe(ftrain)

# view correlation matrix
M <- cor(ftrain)
corrplot(M, method = "number")

write.csv(ftrain, file = "final_train.csv",row.names=FALSE)
