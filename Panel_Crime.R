##############################
##### Importing Libraries ####
##############################
library(plm)
library(tidyverse)
library(stargazer)
library(ggplot2)

################################
####### Data Exploration #######
################################

# Remove all objects
rm(list = ls())

# Load the Crime data set of the plm package
data(Crime)

# Check balanced panel data
is.pbalanced(Crime)

# Extract needed variables
mydata <- Crime[ ,c("county","year", "crmrte", "polpc", "region")]

# A glimpse of the data
head(mydata,5)
glimpse(mydata)

# Manipulate the year column
mydata$year <- as.numeric(paste(19, mydata$year, sep=""))
glimpse(mydata)
head(mydata)

################################
##### Data Visualization #######
################################

# Scatter plot
mydata %>% 
  ggplot(aes(x = polpc, y = log(crmrte), color = region)) +
  geom_point() +
  facet_wrap(~region) +
  xlab("Police per capita") +
  ylab("Log(Crime Rate)") +
  labs(color = "Region", title = "Logarithm of Crime Rate in Different Regions") +
  theme_bw()

# Box plot
mydata %>% 
  ggplot(aes(x = polpc, y = log(crmrte), color = as.factor(region))) +
  geom_boxplot() +
  facet_wrap(~as.factor(region)) +
  xlab("Police per capita") +
  ylab("Log(Crime Rate)") +
  labs(color = "Region", title = "Dispersion of Crime Rate in Different Regions") +
  theme_bw()

################################
###### Panel Data Models #######
################################

# Convert to panel data
mydata <- pdata.frame(mydata, index=c("county", "year"))
class(mydata)

# Renaming columns 
Y <- cbind("crime_rate"=mydata$crmrte)
X <- cbind("police_percapita"=mydata$polpc)

# Summary of descriptive statistics
summary(Y)
summary(X)

# Pooled OLS estimator
pooled <- lm(log(Y)~X, data = mydata)
result <- stargazer(pooled, type = "text", out="table1.txt")

pooled <- plm(log(Y)~X, data = mydata, model = "pooling")
result <- stargazer(pooled, type = "text", out="table1.txt")

# First Difference estimator
first_difference <- plm(log(Y)~X, data = mydata, model = "fd")
result <- stargazer(first_difference, type = "text", out="table1.txt")

# Fixed Effects estimator
fixed_effects <- plm(log(Y)~X, data = mydata, model = "within")
result <- stargazer(fixed_effects, type = "text", out="table1.txt")

# Random Effects estimator
random_effects <- plm(log(Y)~X, data = mydata, model = "random")
result <- stargazer(random_effects, type = "text", out="table1.txt")

# Lagrangian Multiplier test for RE vs OLS
plmtest(pooled)

# Lagrangian Multiplier test for FE vs OLS
pFtest(fixed_effects, pooled)

# Hausman test for FE vs RE
phtest(random_effects, fixed_effects)

# FE Model (Include a lag)
fixed_effects_lag <- plm(log(Y)~lag(log(Y)) + X, data = mydata, model = "within")
result <- stargazer(fixed_effects_lag, type = "text", out="table1.txt")
