# Script to plot US murder rates by state
library(dslabs)
library(tidyverse)
library(maps)
library(viridis)

# Check structure of murder data
head(murders)

# Basic plot via piped data (default first argument of aes is x, second is y)
murders %>%
  ggplot() + geom_point(aes(population/10^6, total))

# Add labels using abbrev. colum, and colour by region
murders |> ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_x = 1.5)

#Side note - the %>% pipe I learnt now has a   R alternative!
#Read more here - https://www.tidyverse.org/blog/2023/04/base-vs-magrittr-pipe/
# As far as I can see |> (base R) is simpler, and main diff is that you
# can't use "." to direct what you're piping:
#e.g. 
murders %>% head(.) #works
#but
murders |> head(.) # doesn't work

#it'smylife - let's use the fancier pipe as standard for old times sake

#Let's log the axes to space the data better. Adjust jitter on labels and
#don't pre-transform the population
murders %>% ggplot(aes(population, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  scale_x_log10() +
  scale_y_log10()

# Now minimal version
murders %>% ggplot(aes(population, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal()

# Add titles
murders %>% ggplot(aes(population, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() + 
  labs(title = "US gun murders in 2010")

# Calculate overall average murders per person population
r <- murders |> 
  summarize(rate = sum(total)/sum(population)) |> 
  pull(rate)

# #Add murder rate as line
# murders |> ggplot(aes(population, total, label = abb)) +
#   geom_point(aes(color = region), size = 3) +
#   geom_text(nudge_y = 0.1) +
#   geom_abline(slope=log(r), lty=2, color="black") +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_minimal() + 
#   labs(title = "US gun murders in 2010")
# #WHERE'S MY LINE -line not visible

# Do the book version as sense check:
r2 <- murders %>%
  summarize(rate = sum(total)/sum(population)*10^6) %>%
  pull(rate)

murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_x = 1.5) +
  geom_abline(intercept = log10(r2), lty = 2, color = "darkgrey") +
  theme_minimal() + 

##The above (from the book) does not give what we want - gives a line with 
#gradient 1 and intercept near 0 (1.48). 

#So switch - use log10(r) as the slope, and intercept as 0
# (no people = no deaths, hopefully ;) 
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_x = 1.5) +
  geom_abline(slope = r2, intercept = 0, lty = 2, color = "darkgrey") +
  theme_minimal() 

##GRAPH BELOW SAVED IN OUTPUT--------------------------------------------------
# NICE - got there. Now add in log10 for axes (and intercept):
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

#OK so let's go back to my slightly altered graph
#We're using r not r2 - these are the same but r2 had pop/10^6


#TURN THE BELOW TO STACK EXCHANGE QUESTIONS------------------------------------
#Add murder rate as line
# murders %>%
#   ggplot(aes(population, total, label = abb)) +
#   geom_point(aes(color = region), size = 3) +
#   geom_text(nudge_y = 0.1) +
#   theme_minimal()  +
#   labs(title = "US gun murders in 2010") +
#   xlim(0, NA) +
#   ylim(0, NA) +
#   geom_abline(slope=r, intercept=0, lty=2, color="black") 
# #FIXED - key thing was making sure origin is plotted using xlim
# #however this also removes the log scale
# 
# #Also tried putting axes on log scale and changing intercept to 
# #small (non-zero) number 0.0001, but this didn't work
# murders %>%
#   ggplot(aes(population, total, label = abb)) +
#   geom_point(aes(color = region), size = 3) +
#   geom_text(nudge_y = 0.1) +
#   theme_minimal()  +
#   geom_abline(slope=r, intercept=0.0001, lty=2, color="black") +
#   labs(title = "US gun murders in 2010") +
#   scale_x_log10() +
#   scale_y_log10() 
# 
# # The below works, by transforming the co-ordinates instead!!!
# #Seems to relate to this (unresolved) 
# # bug https://github.com/tidyverse/ggplot2/issues/46
# murders %>%
#   ggplot(aes(population, total, label = abb)) +
#   geom_point(aes(color = region), size = 3) +
#   geom_text(nudge_y = 0.1) +
#   theme_minimal()  +
#   geom_abline(slope=r, intercept=0.0001, lty=2, color="black") +
#   labs(title = "US gun murders in 2010") +
#   coord_trans(y="log10", x="log10")


# Final version - let's avoid the bug and transform the data instead

#Calculate murder rate per m

murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  theme_minimal()  +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(slope=log10(r2), intercept=1, lty=2, color="black") +
  labs(title = "US gun murders in 2010") +
  xlab("State population (millions)") +
  ylab("Total gun murders")


#Calculate murder rate by region
murd_reg <- murders %>%
  group_by(region) %>%
  mutate(regional_murder_rate = sum(total)/sum(population)*10^6)

r_reg <- unique(murd_reg$regional_murder_rate)


#THIS GRAPH makes average rate lines for each region
#Could update with hex colours to make colours match regions
#Not log
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  theme_minimal()  +
  geom_abline(slope=r_reg, intercept=c(1,1,1,1), lty=2, 
              color=c("green", "purple", "red", "blue")) +
  labs(title = "US gun murders in 2010") +
  xlab("State population (millions)") +
  ylab("Total gun murders")

#THIS GRAPH makes average rate lines for each region
#Need to update with hex colours to make colours match regions
#Log
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  theme_minimal()  +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(slope=log10(r_reg), intercept=c(1,1,1,1), lty=2, 
              color=c("green", "purple", "red", "blue")) +
  labs(title = "US gun murders in 2010") +
  xlab("State population (millions)") +
  ylab("Total gun murders")

#THIS GRAPH makes average rate lines for each region
#Now try to do it using "lm" and geom smooth
#Not log
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.5) +
  theme_minimal()  +
  geom_smooth(method = "lm", 
              mapping = aes(population/10^6, total, color=region,),
              se=FALSE) + # need to remove ribbons to plot on single graph
  # facet_grid(cols = ) + need to make it split by region
  labs(title = "US gun murders in 2010") +
  xlab("State population (millions)") +
  ylab("Total gun murders")

##GRAPH BELOW SAVED IN OUTPUT--------------------------------------------------
#THIS GRAPH makes average rate lines for each region
#Now try to do it using "lm" and geom smooth AND with facet grid
# log
murders %>%
  ggplot(aes(population/10^6, total, label=abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1, color="grey60") +
  theme_minimal()  +
  geom_smooth(method = "lm", 
              mapping = aes(color=region)) + # need to remove ribbons to plot on single graph
  facet_grid(cols = vars(region), margins=TRUE) + # need to make it split by region
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
