# Script to plot US murder rates by state
library(dslabs)
library(tidyverse)
library(maps)
library(viridis)

# Check structure of murder data
head(murders)

#Side note - the %>% pipe I learnt now has a   R alternative!
#Read more here - https://www.tidyverse.org/blog/2023/04/base-vs-magrittr-pipe/
# As far as I can see |> (base R) is simpler, and main diff is that you
# can't use "." to direct what you're piping:
#e.g. 
murders %>% head(.) #works
#but
murders |> head(.) # doesn't work

#it'smylife - let's use the fancier pipe as standard for increased functionality

# Calculate overall average murders per person population
r <- murders %>%
  summarize(rate = sum(total)/sum(population)) |> 
  pull(rate)

# Do the book version as sense check (rate per million):
r2 <- murders %>%
  summarize(rate = sum(total)/sum(population)*10^6) %>%
  pull(rate)


##GRAPH BELOW SAVED IN OUTPUT--------------------------------------------------
# Now add in log10 for axes (and intercept):
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = -0.05, color="grey60") +
  geom_abline(slope = log10(r2), intercept = 1,
              lty = 2, color = "black", linewidth=0.8) +
  scale_x_log10() +
  scale_y_log10()+
  theme_minimal() +
  labs(title = "US gun murders in 2010") +
  xlab("Log10(State population [millions])") +
  ylab("Log10(Total gun murders)")
#NOTE - intercept must be 1 as log10(0) = 1

#Calculate murder rate by region
murd_reg <- murders %>%
  group_by(region) %>%
  mutate(regional_murder_rate = sum(total)/sum(population)*10^6)

r_reg <- unique(murd_reg$regional_murder_rate)

##GRAPH BELOW SAVED IN OUTPUT--------------------------------------------------
#THIS GRAPH makes average rate lines for each region
murders %>%
  ggplot(aes(population/10^6, total, label=abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1, color="grey60") +
  theme_minimal()  +
  geom_smooth(method = "lm", 
              mapping = aes(color=region)) + # need to remove ribbons to plot on single graph
  facet_grid(cols = vars(region), margins=TRUE) + # split by region
  labs(title = "US gun murders in 2010") +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Log10(State population [millions])") +
  ylab("Log10(Total gun murders)")

## Nice, now to try and make a chloropleth map showing gun murder rate
# Retrieve the states map data and merge with crime data
#Make dataset of USA states
states_map <- map_data("state") # note that state names are in lower case

#Let's calculate murder rate for each state
murders$rate <- murders$total/murders$population
murders$ratebymil <- murders$total/murders$population*10^6

#we will need to use state names to match murder rate to state names in map data
murders$lowcasestate <- tolower(murders$state)

#Need the same col name to join by that variable, so make additional column
#in states map (region means different things in states_map and murders)
states_map$lowcasestate <- tolower(states_map$region)

#Now add the murders data to the state map data, using lower case state name
#to specify which rows murder data is entered on
murders_map <- left_join(states_map, murders, by = "lowcasestate")

# Map with murder rate per million people
ggplot(murders_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = ratebymil), color = "white")+
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(fill = "Gun murders \nper million people",
       title = "US gun murders in 2010") +
  xlab("Longitude") +
  ylab("Latitude")

#Reviewer said "dark bits are bad right?" so make scale darker with more murder:

##GRAPH BELOW SAVED IN OUTPUT--------------------------------------------------
# Map with murder rate per million people
ggplot(murders_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = ratebymil), color = "white")+
  #scale_fill_viridis_c(option = "C") +
  scale_fill_gradient(low="white",high=colors()[553]) +
  theme_minimal() +
  labs(fill = "Gun murders \nper million people",
       title = "US gun murders in 2010") +
  xlab("Longitude") +
  ylab("Latitude")
