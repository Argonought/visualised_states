# Script to plot US murder rates by state
library(dslabs)
library(tidyverse)

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
murders |> ggplot(aes(population, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  scale_x_log10() +
  scale_y_log10()

# Now minimal version
murders |> ggplot(aes(population, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal()

# Add titles
# Now minimal version
murders |> ggplot(aes(population, total, label = abb)) +
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
  geom_abline(intercept = log10(r2), lty = 2, color = "darkgrey")
##This does not give what we want - gives a line with gradient 1
#and intercept near 0 (1.48)

#So switch - use log10(r) as the slope, and intercept as 0
# (no people = no deaths, hopefully ;) 
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_x = 1.5) +
  geom_abline(slope = r2, intercept = 0, lty = 2, color = "darkgrey")

# NICE - got there. Now add in log10 for axes (and intercept):
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_x = 0.05) +
  geom_abline(slope = log10(r2), intercept = 1,
              lty = 2, color = "darkgrey", linewidth=1.5) +
  scale_x_log10() +
  scale_y_log10()+
  theme_minimal()
#NOTE - intercept must be 1 as log10(0) = 1

#OK so let's go back to my slightly altered graph
#We're using r not r2 - these are the same but r2 had pop/10^6

#Add murder rate as line
murders %>%
  ggplot(aes(population, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  theme_minimal()  +
  labs(title = "US gun murders in 2010") +
  xlim(0, NA) +
  ylim(0, NA) +
  geom_abline(slope=r, intercept=0, lty=2, color="black") 
#FIXED - key thing was making sure origin is plotted using xlim
#however this also removes the log scale

#Also tried putting axes on log scale and changing intercept to 
#small (non-zero) number 0.0001, but this didn't work
murders %>%
  ggplot(aes(population, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  theme_minimal()  +
  geom_abline(slope=r, intercept=0.0001, lty=2, color="black") +
  labs(title = "US gun murders in 2010") +
  scale_x_log10() +
  scale_y_log10() 

# The below works, by transforming the co-ordinates instead!!!
#Seems to relate to this (unresolved) 
# bug https://github.com/tidyverse/ggplot2/issues/46
murders %>%
  ggplot(aes(population, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  theme_minimal()  +
  geom_abline(slope=r, intercept=0.0001, lty=2, color="black") +
  labs(title = "US gun murders in 2010") +
  coord_trans(y="log10", x="log10")


# Final version - let's learn from the bug and transform the data instead

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

murders %>%
  ggplot(aes(population, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  theme_minimal()  +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(slope=r, intercept=1, lty=2, color="black") +
  labs(title = "US gun murders in 2010") +
  xlab("State population (millions)") +
  ylab("Total gun murders")


#Calculate murder rate by region
murd_reg <- murders %>%
  group_by(region) %>%
  mutate(regional_murder_rate = sum(total)/sum(population)*10^6)

r_reg <- unique(murd_reg$regional_murder_rate)


#THIS GRAPH makes average rate lines for each region
#Need to update with hex colours to make colours match regions
#Not log
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  theme_minimal()  +
  geom_smooth(method = "lm") +
  geom_abline(slope=r_reg, intercept=c(1,1,1,1), lty=2, 
              # color=c("green", "purple", "red", "blue")) +
              color=c(2, 4, 1, 3)) +
  labs(title = "US gun murders in 2010") +
  xlab("State population (millions)") +
  ylab("Total gun murders")

#THIS GRAPH makes average rate lines for each region
#Need to update with hex colours to make colours match regions
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  theme_minimal()  +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(slope=log10(r_reg), intercept=c(1,1,1,1), lty=2, 
              # color=c("green", "purple", "red", "blue")) +
              color=c(2, 4, 1, 3)) +
  labs(title = "US gun murders in 2010") +
  xlab("State population (millions)") +
  ylab("Total gun murders")

#THIS GRAPH makes average rate lines for each region
#Now try to do it using "lm" and geom smooth
#Not log
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  theme_minimal()  +
  geom_smooth(method = "lm", 
              mapping = aes(population/10^6, total, color=region),
              ) + # need to remove ribbons to plot on single graph
  # facet_grid(cols = ) + need to make it split by region
  labs(title = "US gun murders in 2010") +
  xlab("State population (millions)") +
  ylab("Total gun murders")
