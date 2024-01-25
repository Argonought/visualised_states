# Visualised_states
R scripts for visualising data about gun murder data from the USA, inspiration from Introduction to Data Science by Rafael Irizarry.

Code to produce the graphs below can be found in ./Plot_US_rates.R

# Combined gun murders graph
![What is this](USA_combined_gunmurders.png)

# Gun murders graph faceted by region
![What is this](USA_gunmurders_lm_facetgrid.png)

# Gun murders chloropleth map
![What is this](USA_gunmurder_map_final.png)

## Lessons learned
1) R now has a native pipe function
   #e.g. 
murders %>% head(.) #works
but
murders |> head(.) # doesn't work
2) Split code into scripts, each with a single function, and returning an output, so you can combine better in future
