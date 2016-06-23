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

training_data[is.na(training_data)] <- 0
#Not sure why we have two files, will focus on training_data since that seems to be what is mentioned in the assignment
summary(training_data)

#1 output value (TARGET_WINS), 15 variables, 1 ignored (INDEX)

#Let's look at the distribution of the output variable, TARGET_WINS and run some scatters
attach(training_data)
boxplot(TARGET_WINS)
plot(TEAM_BATTING_H,TARGET_WINS)
plot(TEAM_BATTING_2B,TARGET_WINS)
plot(TEAM_BATTING_3B,TARGET_WINS)
plot(TEAM_BATTING_HR,TARGET_WINS) #Most variance, least clustering
plot(TEAM_BATTING_BB,TARGET_WINS) #2nd least clustering
plot(TEAM_BATTING_SO,TARGET_WINS) #less clustering
plot(TEAM_BASERUN_SB,TARGET_WINS)
plot(TEAM_BASERUN_CS,TARGET_WINS)
plot(TEAM_FIELDING_E,TARGET_WINS)
plot(TEAM_FIELDING_DP,TARGET_WINS) #less clustering
plot(TEAM_PITCHING_BB,TARGET_WINS)
plot(TEAM_PITCHING_H,TARGET_WINS)
plot(TEAM_PITCHING_HR,TARGET_WINS) #less clustering
plot(TEAM_PITCHING_SO,TARGET_WINS)

#Note that when we model, we should either exclude TEAM_BATTING_H or the doubles, triples, HR variables because effectively _H is a subset

#Do not seem to be missing variables nor any need to be fixed...

#Part 3 Models
m_batting = lm(TARGET_WINS~TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+TEAM_BATTING_BB+TEAM_BATTING_SO)
summary(m_batting)

m_baserun = lm(TARGET_WINS~TEAM_BASERUN_SB+TEAM_BASERUN_CS)
summary(m_baserun)

m_fielding = lm(TARGET_WINS~TEAM_FIELDING_E+TEAM_FIELDING_DP)
summary(m_fielding)

m_pitching = lm(TARGET_WINS~TEAM_PITCHING_BB+TEAM_PITCHING_H+TEAM_PITCHING_HR+TEAM_PITCHING_SO)
summary(m_pitching)

#batting has the highest R-squared followed by pitching

m_all = lm(TARGET_WINS~TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+TEAM_BATTING_BB+TEAM_BATTING_SO+TEAM_BASERUN_SB+TEAM_BASERUN_CS+TEAM_FIELDING_E+TEAM_FIELDING_DP+TEAM_PITCHING_BB+TEAM_PITCHING_H+TEAM_PITCHING_HR+TEAM_PITCHING_SO)
summary(m_all)


#Backwards method, remove all where p-value is > 0.05
m_refined = lm(TARGET_WINS~TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_BB+TEAM_BASERUN_SB+TEAM_BASERUN_CS+TEAM_FIELDING_E+TEAM_FIELDING_DP+TEAM_PITCHING_H)
summary(m_refined)

#No HRs, also note that 2B is negative...

m_all_less_1 = lm(TARGET_WINS~TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+TEAM_BATTING_BB+TEAM_BATTING_SO+TEAM_BASERUN_SB+TEAM_BASERUN_CS+TEAM_FIELDING_E+TEAM_FIELDING_DP+TEAM_PITCHING_H+TEAM_PITCHING_SO)
summary(m_all_less_1)

#
m_new <- lm(TARGET_WINS~TEAM_BATTING_H+TEAM_BATTING_BB+TEAM_PITCHING_H+
              TEAM_PITCHING_BB+TEAM_FIELDING_E)
summary(m_new)

#Identify singles but adding a column
training_data <- mutate(training_data,TEAM_BATTING_1B = TEAM_BATTING_H - TEAM_BATTING_2B - TEAM_BATTING_3B - TEAM_BATTING_HR)
attach(training_data)
summary(training_data)

m_improved <- lm(TARGET_WINS~
                   TEAM_BATTING_1B+TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+
                   TEAM_PITCHING_H+TEAM_PITCHING_HR+TEAM_PITCHING_BB+
                   TEAM_FIELDING_E)
summary(m_improved)

#Notice that SO for both BATTING AND PITCHING have the exact same NA value
index <- !is.na(TEAM_BATTING_SO)
training_data_trip <- training_data[index,]
attach(training_data_trip)
summary(training_data_trip)
m2_improved <- lm(TARGET_WINS~
                   TEAM_BATTING_1B+TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+TEAM_BATTING_SO+
                   TEAM_PITCHING_H+TEAM_PITCHING_HR+TEAM_PITCHING_BB+TEAM_PITCHING_SO+
                   TEAM_FIELDING_E)
summary(m2_improved)

#Add a column to delineate between HR pitches and all other hits
training_data_trim <- mutate(training_data_trip,TEAM_PITCHING_HnHR = TEAM_PITCHING_H - TEAM_PITCHING_HR)
attach(training_data_trim)
m3_improved <- lm(TARGET_WINS~
                    TEAM_BATTING_1B+TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+TEAM_BATTING_SO+
                    TEAM_PITCHING_HnHR+TEAM_PITCHING_BB+TEAM_PITCHING_SO+TEAM_PITCHING_HR+
                    TEAM_FIELDING_E)
summary(m3_improved)

#Remove pitching HRs
m4_improved <- lm(TARGET_WINS~
                    TEAM_BATTING_1B+TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+TEAM_BATTING_SO+
                    TEAM_PITCHING_HnHR+TEAM_PITCHING_BB+TEAM_PITCHING_SO+
                    TEAM_FIELDING_E)
summary(m4_improved)

#remove pitching SOs
m5 <- lm(TARGET_WINS~
           TEAM_BATTING_1B+TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+TEAM_BATTING_SO+
           TEAM_PITCHING_HnHR+TEAM_PITCHING_BB+
           TEAM_FIELDING_E)
summary(m5)

#stick with m4
