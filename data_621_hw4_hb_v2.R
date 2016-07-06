library(plyr)
library(dplyr)
library(readr)
# library(moments)
library(ggplot2)
library(reshape2)
# library(car)
# library(psych)
# library(PerformanceAnalytics)
# library(GGally)
# library(polycor)

# read training data
raw <- read_csv("insurance_training_data.csv")
str(raw)
summary(raw)
head(raw)

train <- raw[-1]
head(train)
tail(train)
str(train)
summary(train)

## DATA CLEANING ##

# convert categorial and count variables to factor
train$TARGET_FLAG <- factor(train$TARGET_FLAG, levels = 0:1, 
                            labels = c("no crash", "crash"))
train$CAR_TYPE <- factor(train$CAR_TYPE)
train$CAR_USE <- factor(train$CAR_USE)
train$EDUCATION <- factor(train$EDUCATION)
train$JOB <- factor(train$JOB)
train$MSTATUS <- factor(train$MSTATUS)
train$PARENT1 <- factor(train$PARENT1)
train$RED_CAR <- factor(train$RED_CAR)
train$REVOKED <- factor(train$REVOKED)
train$SEX <- factor(train$SEX)
train$URBANICITY <- factor(train$URBANICITY)

# convert currency amounts to numeric
clean <- function(ttt){
  as.numeric( gsub('[^a-zA-Z0-9.]', '', ttt))
  }

train$INCOME <- sapply(train$INCOME, clean)
train$HOME_VAL <- sapply(train$HOME_VAL, clean)
train$BLUEBOOK <- sapply(train$BLUEBOOK, clean)
train$OLDCLAIM <- sapply(train$OLDCLAIM, clean)


# rename variable to allow for undo
ctrain <- train

str(ctrain)
summary(ctrain)


## DATA EXPLORATION ##

# boxplots to check distributions
# par(mfrow=c(1,1)) # reset plot grid
ggplot(melt(ctrain), aes(x=TARGET_FLAG, y=value, colour=variable)) + 
  facet_wrap(~variable,scales = "free") + 
  geom_boxplot() +
  ggtitle("Boxplots")


# plot histograms of data
d <- melt(ctrain)
ggplot(d,aes(x = value)) +
  facet_wrap(~variable,scales = "free") +
  geom_histogram()



# generate correlation matrix, identify highly-correlated pairs
library(polycor)
het.mat <- hetcor(data.frame(ctrain))$cor

# http://stackoverflow.com/questions/7074246/show-correlations-as-an-ordered-list-not-as-a-large-matrix

library(knitr)
corrmat <- het.mat
corrmat[lower.tri(corrmat,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
corrmat=as.data.frame(as.table(corrmat))  #Turn into a 3-column table
corrmat=na.omit(corrmat)  #Get rid of the junk we flagged above
corrmat=corrmat[order(-abs(corrmat$Freq)),]    #Sort by highest correlation (whether +ve or -ve)
corrmat_high <- corrmat[which(abs(corrmat$Freq) >= 0.5),]
kable(corrmat_high[, -1])

# 461         SEX    RED_CAR -0.9467632
# 26  TARGET_FLAG TARGET_AMT  0.8593218
# 233     PARENT1    MSTATUS  0.8483938
# 436         SEX   CAR_TYPE  0.6384408
# 363         JOB    CAR_USE -0.6344225
# 207      INCOME   HOME_VAL  0.5778014
# 234    HOME_VAL    MSTATUS -0.5268970
# 468    CAR_TYPE    RED_CAR -0.5059560

# plot highly-correlated pairs
par(mfrow=c(2,4))
plot(ctrain$SEX, ctrain$RED_CAR, xlab = "sex", ylab = "red car") # MEN BUY NEARLY ALL OF THE RED CARS?!
plot(ctrain$PARENT1, ctrain$MSTATUS, xlab = "single parent", ylab = "marital status")
plot(ctrain$SEX, ctrain$CAR_TYPE, xlab = "sex", ylab = "car type") 
plot(ctrain$JOB, ctrain$CAR_USE, xlab = "job", ylab = "car use")
plot(ctrain$INCOME, ctrain$HOME_VAL, xlab = "income", ylab = "home value")
plot(ctrain$HOME_VAL, ctrain$MSTATUS, xlab = "home value", ylab = "marital status")
plot(ctrain$CAR_TYPE, ctrain$RED_CAR, xlab = "car type", ylab = "red car")
plot(ctrain$OLDCLAIM, ctrain$CLM_FREQ, xlab = "old claims", ylab = "claim frequency")

# INCOME vs. HOME_VAL shows high correlation, but significant zero values for home value;
# we should transform HOME_VAL to a binary: homeowner, yes vs. no (zero = no)

par(mfrow=c(1,1)) # reset plot grid
# view relationship between response and selected predictors
plot(ctrain$TARGET_FLAG, ctrain$KIDSDRIV)
plot(ctrain$TARGET_FLAG, ctrain$CLM_FREQ)



attach(ctrain)

# crosstabs
library(MASS)
xtabs.1 <- xtabs(~ TARGET_FLAG + SEX)
xtabs.2 <- xtabs(~ TARGET_FLAG + CLM_FREQ)
xtabs.3 <- xtabs(~ TARGET_FLAG + JOB)
xtabs.4 <- xtabs(~ TARGET_FLAG + CAR_TYPE + SEX)
xtabs.5 <- xtabs(~ TARGET_FLAG + REVOKED)
xtabs.6 <- xtabs(~ TARGET_FLAG + URBANICITY)
xtabs.7 <- xtabs(~ TARGET_FLAG + EDUCATION + SEX)
xtabs.8 <- xtabs(~ TARGET_FLAG + MSTATUS)


