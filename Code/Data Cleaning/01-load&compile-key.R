#############################################################################
# Load & compile data                                                       #
#############################################################################

# Install and load packages we'll need

#install.packages("tidyverse","lubridate")
library(tidyverse)
library(lubridate)

# Read the csv data file using readr

esm <- read_csv("data/esm_short.csv", na = c("<not-shown>", "<no-response>")) 

# Explore the structure of the data

# first glimpse 
head(esm, n=5)
#View(esm)

# Data structure and format 
# are all the variable types what they should be?
str(esm)

# Are the column names what you want them to be? 

# make all column names lower case
esm <- esm %>%
  rename_with(tolower, -PID) # drop PID 

# Remove unnecessary/uninformative variables

esm <- esm%>%
  select(.,-contains("instructions"))

# Reformat variables

# need to make sure dates are indicated correctly 
esm<-esm%>%
  mutate(across(ends_with("_ts"), ~ as_datetime(.,format="%m/%d/%y %H:%M")))

head(select(esm,contains("_ts")))


#for some questions we might want to reverse code items
esm <- esm %>%
  mutate(social_cold = 8 - social_warmth)  # reverse 7 pt likert scale
         

# Combine data with a baseline age measure 

baseline<-read_csv("data/baseline.csv")

esm<- esm%>%
  left_join(.,baseline)


# Potential other steps: 
# any other variables you might want to remove?
# any other variables you could reverse code? 









