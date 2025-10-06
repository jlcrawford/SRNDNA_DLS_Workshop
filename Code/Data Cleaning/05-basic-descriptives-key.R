#############################################################################
# Basic Descriptives                                                        #
#############################################################################

# Install and load packages we'll need
#install.packages("psych")
library(psych)

descriptives<-statsBy(select(esm_clean, "excited", 
               "happy", "relaxed",  "angry", 
               "nervous",  "sad",  "social", 
               "social_dominance", "social_warmth", 
               "PID"),group="PID")

descriptives$mean
describe(descriptives$mean,na.rm=T)
describe(descriptives$sd,na.rm=T)
descriptives$ICC1

# Plotting your variables (e.g., over time)

esm %>% 
  filter(PID==sample(unique(PID),size=5,replace=FALSE))%>%
  gather(var, value, happy,social_warmth) %>%
  ggplot(aes(x=obs, y=value, color=var)) +
  geom_line() +
  geom_point() +
  facet_grid(PID~var)

# Additional practice: Plot other variables in dataset



