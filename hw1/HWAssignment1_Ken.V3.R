install.packages("devtools")
devtools::install_github("hadley/tidyr")

library(dplyr)
library(tidyr)
require(dplyr)
library(ggplot2)
require(ggplot2)
library(ggthemes)
require(ggthemes)
require(lubridate)
require(stringr)
library(reshape2)

# To clear a Global Environemnt
rm(list =ls())

# Read in the data files 
file_name <- "~/Downloads/moneyball-evaluation-data.csv"
eval_data <- read.csv(file_name, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

file_name <- "~/Downloads/moneyball-training-data.csv"
training_data <- read.csv(file_name, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

file_name <- "~/Downloads/Data/final_train.csv"

tdc <- read.csv(file_name, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

summary(tdc)

#We will leverage backwards method for regression
#Note that we did not include basehits since it is derivative
attach(tdc)
m1 <- lm(wins~singles+doubles+triples+homeruns+walks+b_strikeouts+stolen_bases+
           hits_allowed+homeruns_allowed+walks_allowed+p_strikeouts+errors+
           double_plays)
summary(m1)

#remove homeruns_allowed
m2 <- lm(wins~singles+doubles+triples+homeruns+walks+b_strikeouts+stolen_bases+
           hits_allowed+walks_allowed+p_strikeouts+errors+
           double_plays)
summary(m2)

#Model is better based on R-square, remove doubles
m3 <- lm(wins~singles+triples+homeruns+walks+b_strikeouts+stolen_bases+
           hits_allowed+walks_allowed+p_strikeouts+errors+
           double_plays)
summary(m3)

#No change in adjusted R-squared, F-stat is 137.9 on 11 and 2264 DF vs. 126.4 on 12 and 2263 DF
#p-value is the same as well
#Unlikely any further changes will improve the model, but we will try
#remove walks_allowed
m4 <- lm(wins~singles+triples+homeruns+walks+b_strikeouts+stolen_bases+
           hits_allowed+p_strikeouts+errors+
           double_plays)
summary(m4)

#In this case the adjusted R-square reduced, p-value remained, we should keep m3
