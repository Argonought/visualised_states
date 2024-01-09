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

#Add murder rate as line
#CURRENT ISSUE - abline not appearing on graph - probably due to messing with logs
murders |> ggplot(aes(population, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(nudge_y = 0.1) +
  geom_abline(intercept=10, slope=30.01, lty=2, color="black") +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() + 
  labs(title = "US gun murders in 2010")
