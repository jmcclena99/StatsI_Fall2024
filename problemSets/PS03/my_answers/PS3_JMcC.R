#####################
#
# Jamie McClenaghan's submission for Quantitative Methods I
#
#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

#loading ggplot2 for graphs
library(ggplot2)
library(broom)
library(texreg)

model1 <- lm(voteshare ~ difflog, data = inc.sub) #Running regression - voteshare as outcome variable, difflog as exploratory variable 

summary(model1) #Summary of results for regression

ggplot(inc.sub, aes(x = voteshare, y = difflog)) +
  geom_point(alpha = 0.225) + #Adjusting density of observations to a lower value to avoid overplotting
  geom_smooth(method = "lm", se = TRUE) + #Adding linear regression and including bands for standard errors
  labs(x = "Incumbent vote share", y = "Incumbent/Challenger campaign spending difference") + #Added labels to clearly highlight X and Y axis variables
  theme_minimal() #Added minimal theme for aesthetic purposes

residuals_model1 <- model1$residuals #Added regression model residuals to new object
residuals_model1

##############################################

model2 <- lm(presvote ~ difflog, data = inc.sub) #Running regression - presvote as outcome variable, difflog as exploratory variable 

summary(model2) #Summary of results for regression

ggplot(inc.sub, aes(x = presvote, y = difflog)) +
  geom_point(alpha = 0.225) + #Adjusting density of observations to a lower value to avoid overplotting
  geom_smooth(method = "lm", se = TRUE) + #Adding linear regression and including bands for standard errors
  labs(x = "Incumbent presidential vote share", y = "Incumbent/Challenger campaign spending difference") + #Added labels to clearly highlight X and Y axis variables
  theme_minimal() #Added minimal theme for aesthetic purposes

residuals_model2 <- model2$residuals #Added regression model residuals to new object
residuals_model2

##############################################

model3 <- lm(voteshare ~ presvote, data = inc.sub) #Running regression - presvote as outcome variable, difflog as exploratory variable 

summary(model3) #Summary of results for regression

ggplot(inc.sub, aes(x = voteshare, y = presvote)) +
  geom_point(alpha = 0.225) + #Adjusting density of observations to a lower value to avoid overplotting
  geom_smooth(method = "lm", se = TRUE) + #Adding linear regression and including bands for standard errors
  labs(x = "Incumbent vote share", y = "Incumbent presidential vote share") + #Added labels to clearly highlight X and Y axis variables
  theme_minimal() #Added minimal theme for aesthetic purposes

residuals_model3 <- model3$residuals #Added regression model residuals to new object
residuals_model3

##############################################

df <- data.frame(Model1Residuals = residuals_model1, Model2Residuals = residuals_model2) #Created data frame to more easily run regressions

model4 <- lm(Model1Residuals ~ Model2Residuals, data = df) #Running regression - presvote as outcome variable, difflog as exploratory variable 

summary(model4) #Summary of results for regression

ggplot(df, aes(x = Model1Residuals, y = Model2Residuals)) +
  geom_point(alpha = 0.225) + #Adjusting density of observations to a lower value to avoid overplotting
  geom_smooth(method = "lm", se = TRUE) + #Adding linear regression and including bands for standard errors
  labs(x = "Incumbent vote share", y = "Incumbent presidential vote share") + #Added labels to clearly highlight X and Y axis variables
  theme_minimal() #Added minimal theme for aesthetic purposes

##############################################

model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub) #Running regression - presvote as outcome variable, difflog as exploratory variable 
summary(model5)






