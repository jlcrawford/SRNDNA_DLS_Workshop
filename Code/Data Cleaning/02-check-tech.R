#############################################################################
# Check your technology                                                     #
#############################################################################

# Install and load packages we need


# Check that number of prompts you expected were sent 


# Investigate days with too many or too few prompts


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


# Are there any duplicates in your data?


# Now that all prompts are correct, add prompt numbers


# Exclude any data collected in  different time zone


# Do all the values make sense? 

# check ranges





