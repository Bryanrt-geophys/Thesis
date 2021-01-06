# install.packages(c("zoo", "peakPick"))
# install.packages("tidyr")
# install.packages("forcats")
# install.packages("tidyverse")

library(forcats)
library(tidyverse)
library(dplyr)
library(zoo)
library(peakPick)
library(tidyr)
library(purrr)

# Data
transect <- readr::read_csv("https://raw.githubusercontent.com/Bryanrt-geophys/Thesis/Thesis/r_code/transect.csv")
transect <- transect[-1]

# Pivot to long format
long_transect <- transect %>%
  pivot_longer(-Y,
               names_to = "timestep",
               names_prefix="Z_",
               values_to = "elevation") %>%
  mutate(elevation = if_else(abs(elevation) < 10, 0, elevation)) # say that <0 is = 0

long_transect$timestep = fct_inorder(long_transect$timestep)


# used to find mountain-side of basin - possibly replace variables with data.frames

find_first_zero <- function(Y, elevation_vector, n){
  
  below_zero <- elevation_vector <= 0  
  
  r <- rle(below_zero)
  
  tbl <- tibble(values = r$values, lengths = r$lengths) %>%
    filter(values, lengths >= n) %>%
    slice(n())
  
  i <- which(r$lengths == tbl$lengths)
  
  cumsums <- cumsum(r$lengths)
  
  Y[elevation_vector %in% elevation_vector[(cumsums[i-1]+1):cumsums[i]]]
  
}



# adds the smoothed curves to the long data, aids in visualization of process
long_transect <- long_transect %>%
  group_by(timestep) %>%
  mutate(elevation_smooth = smooth.spline(elevation, spar=.5)$y)

# can only use what is being piped into summarize 
basin_geometry <- long_transect %>% 
  group_by(timestep) %>%
  mutate(mountain = find_mountain(elevation_smooth, elevation),
         valley = find_valley(elevation_smooth, elevation),
         i_lowest_point = which.min(elevation),
         Z_lowest_point = elevation[i_lowest_point],
         Y_lowest_point = Y[i_lowest_point])

basin_geometry %>%
  group_by(timestep) %>%
  summarize(
    basin = find_first_zero(Y = Y, elevation_vector = elevation, n = 20)
  ) %>%
  mutate(
    min_Y = min(basin, na.rm = TRUE),
    max_Y = max(basin, na.rm = TRUE)
  )

basin_geometry %>%
  group_by(timestep) %>%
  summarize(
    basin = find_first_zero(Y = Y, elevation_vector = elevation, n = 20)
  ) %>%
  nest(basin = -timestep) %>%
  mutate(
    min_Y = map(basin, ~ min(.x$basin, na.rm = TRUE)),
    max_Y = map(basin, ~ max(.x$basin, na.rm = TRUE))
  ) %>%
  unnest(cols = c("min_Y", "max_Y")) %>%
  ungroup()
