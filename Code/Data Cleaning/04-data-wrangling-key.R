#############################################################################
# Some data wrangling                                                       #
#############################################################################
# Install and load packages we'll need
library(lubridate)
library(psych)

# Make lagged effects 
esm_clean <- esm_clean %>% 
  arrange(PID, started_ts) %>%   # Order the dataframe by PID and start time
  group_by(PID) %>%
  mutate(happy_lag = lag(happy))


head(select(esm_clean,c(PID,started_ts,happy,happy_lag)),10)

# make sure night isn't affected

esm_clean<-esm_clean %>% 
  arrange(PID, started_ts) %>%   
  mutate(date = as.Date(started_ts)) %>%
  group_by(PID, date) %>%
  mutate(happy_lag = lag(happy))%>%
  ungroup()

head(select(esm_clean,c(PID,started_ts,happy,happy_lag)),10)


# Potential additional steps: Make composite variables (e.g., positive affect)
