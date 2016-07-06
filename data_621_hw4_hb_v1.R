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

# convert factor variables

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


ctrain <- train

summary(ctrain)


## DATA EXPLORATION ##

# boxplots to check distributions
# par(mfrow=c(1,1)) # reset plot grid
ggplot(melt(ctrain), aes(x=TARGET_FLAG, y=value, colour=variable)) + 
  facet_wrap(~variable,scales = "free") + 
  geom_boxplot() +
  ggtitle("Boxplots")


# examine relationship between OLDCLAIM and CLM_FREQ
fit.claim <- lm(OLDCLAIM ~ CLM_FREQ, data = ctrain)
plot(ctrain$CLM_FREQ, ctrain$OLDCLAIM)
summary(fit.claim)

# 
# # plot histograms of data
# d <- melt(ctrain)
# ggplot(d,aes(x = value)) + 
#   facet_wrap(~variable,scales = "free") + 
#   geom_histogram()
# 
# # correlation matrix
# library(polycor)
# het.mat <- hetcor(data.frame(ctrain))$cor
# 
# 
# # crosstabs
# library(MASS)
# ctrain.xtabs <- xtabs(~ RealizationOfRecipient + AnimacyOfRec + AnimacyOfTheme)

