#############################################################################
# Check your technology                                                     #
#############################################################################

# Install and load packages we need
library(tidyverse)

# Check that number of prompts you expected were sent 
df_date_pp <- esm %>% 
  mutate(date = as.Date(scheduled_ts)) %>%    
  group_by(PID,date) %>%
  summarise(n_sent = n())# Number of observation sent for each date

# Investigate days with too many or too few prompts

filter(df_date_pp,n_sent!= 7)
View(filter(esm,PID=="100"))
View(filter(esm,PID=="102"))

# Exclude any time points that were not sent at the correct time 
# note that this is a fixed scheduled design and this would require more manual 
# coding for random prompts

esm_clean <- esm %>%
  mutate(date = as_date(scheduled_ts)) %>% # add date column 
  group_by(PID, date) %>% 
  summarise(first_hour = hour(min(scheduled_ts)), .groups = "drop_last") %>%
  # finding hour first prompt was sent each day 
  count(PID, first_hour, name = "n_days") %>%
  slice_max(n_days, n = 1, with_ties = FALSE) %>% # modal first prompt (assuming
  # this was the intended first prompt time)
  right_join(esm, by = "PID") %>%
  mutate(date = as_date(scheduled_ts)) %>%
  rowwise() %>%
  mutate(
    expected_times = list({
      start_time <- as_datetime(paste(date, sprintf("%02d:00:00", first_hour)))
      start_time + hours(seq(0, 12, by = 2))
    })
  ) %>%
  filter(any(abs(difftime(scheduled_ts, expected_times, units = "mins")) == 0)) %>%
  ungroup()

# Are there any prompts where the timing variables do not make sense? 
esm_clean %>%
  mutate(sent_after_started = scheduled_ts > started_ts,
         completed_after_expired = completed_ts > expired_ts) %>%
  filter(sent_after_started| completed_after_expired)%>%
  select(PID,scheduled_ts,completed_ts,expired_ts,participant_tz)
# time zone issue that we will take care of

# Are there any duplicates in your data?
esm_clean %>%
  group_by(PID, scheduled_ts) %>%
  filter(n() > 1) %>%
  ungroup()

# Now that all prompts are correct, add prompt numbers
esm_clean <- esm_clean %>% 
  arrange(PID, scheduled_ts) %>%   # Order the dataframe by PID and scheduled time
  group_by(PID) %>%
  mutate(obs= seq_along(scheduled_ts))%>%
  ungroup()

# Exclude any data collected in  different time zone

esm_clean<-esm_clean %>%
  filter(participant_tz == "America/Chicago"| participant_tz=="UTC")

# Do all the values make sense? 

# check ranges
esm_clean %>%
  select(excited:social_cold) %>%
  select(-contains("_rt")) %>%
  summarise(across(where(is.numeric),
                   list(min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE)))) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(min|max)$"
  ) %>%
  mutate(range = max - min)




