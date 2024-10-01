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
library(ggplot2)

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

t_score <- qt(0.95, df=length(y)-1)
lower_90_t <- mean(y)-(t_score)*(sd(y)/sqrt(length(y)))
upper_90_t <- mean(y)+(t_score)*(sd(y)/sqrt(length(y)))

lower_90_t
mean(y)
upper_90_t

#Doing the signifigance test now. 

t.test(y, mu = 100, alternative = "greater")

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)


pdf(file="plot1.pdf") +
  plot(expenditure$Y, expenditure$X1,
       xlab="Per capita personal income in state",
       ylab="Per capita expenditure on shelters/housing assistance in state") +
  dev.off()

pdf(file="plot1.pdf") +
plot(expenditure$Y, expenditure$X1,
  xlab="Per capita personal income in state",
  ylab="Per capita expenditure on shelters/housing assistance in state") +
  dev.off()

pdf(file="plot2.pdf") +
plot(expenditure$Y, expenditure$X2,
     xlab="Number of residents per 100,000 that are 'financially insecure' in state",
     ylab="Per capita expenditure on shelters/housing assistance in state") +
  dev.off()

pdf(file="plot3.pdf") +
plot(expenditure$Y, expenditure$X3,
     xlab="Number of people per thousand residing in urban areas in state",
     ylab="Per capita expenditure on shelters/housing assistance in state") +
  dev.off()

expenditure$Region <- factor(expenditure$Region, levels = c(1, 2, 3, 4), labels = c("Northeast", "North Central", "South", "West"))

ggplot(data = expenditure, aes(x = Region, y = Y)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Expenditure levels by region", x = "Region",
  y = "Per capita expenditure on shelters/housing assistance in state") 
ggsave(filename = "expenditure_levels_by_region.pdf")    

pdf(file="plot1_2.pdf") +
  plot(expenditure$Y, expenditure$X1,
       xlab="Per capita personal income in state",
       ylab="Per capita expenditure on shelters/housing assistance in state") +
  dev.off()

ggplot(data = expenditure, aes(x = X1, y = Y, shape = Region, color = Region)) +
  geom_point() +
  labs(title = "Expenditure levels by region", x = "Per capita personal income in state",
       y = "Per capita expenditure on shelters/housing assistance in state") 
ggsave(filename = "expenditure_level_andincome_by_region.pdf")    


