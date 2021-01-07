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
  # this attempt to smooth out random noise may no longer be necessary 
  mutate(elevation = if_else(abs(elevation) < 10, 0, elevation)) # say that <0 is = 0

long_transect$timestep = fct_inorder(long_transect$timestep)


# used to find all values below zero and pick out runs longer than n and return the Y coordinates
find_formost_basin <- function(Y, elevation_vector, n){
  
  # values used to test sections of function:
  # elevation_vector = long_transect$elevation[which(long_transect$timestep == 56)]
  # Y = long_transect$Y[which(long_transect$timestep == 56)]
  # n = 50
  
  below_zero <- elevation_vector <= 0  
  
  r <- rle(below_zero)
  
  tbl <- tibble(values = r$values, lengths = r$lengths) %>%
    filter(values, lengths >= n) %>%
    slice(n())
  
  i <- which(r$lengths == tbl$lengths)
  
  cumsums <- cumsum(r$lengths)
  
  Y[(cumsums[i-1]+1):cumsums[i]]
  
  # Y[elevation_vector %in% elevation_vector[(cumsums[i-1]+1):cumsums[i]]]
  # issue seems to be at this chunk - looks like this matches values found
  # in elevation_vector[cumsums:cumsums] with values in elevation_vector
  # this is problematic as there are repeat values
  # instead, get Y values between i and i-1
  
}

basin_geometry <- long_transect %>%
  group_by(timestep) %>%
  summarize(
    basin = find_formost_basin(Y = Y, elevation_vector = elevation, n = 20)
  ) %>%
  nest(basin = -timestep) %>%
  mutate(
    min_Y = map(basin, ~ min(.x$basin, na.rm = TRUE)),
    max_Y = map(basin, ~ max(.x$basin, na.rm = TRUE))
  ) %>%
  unnest(cols = c("min_Y", "max_Y")) %>%
  ungroup()

ggplot(long_transect) +
  geom_line(aes(x = -Y, y = elevation, color = timestep)) +
  geom_point(data = basin_geometry,
             mapping = aes(x = -min_Y, y = 1, color = timestep),
             shape = 1, size = 2, show.legend = FALSE) +
  geom_point(data = basin_geometry,
             mapping = aes(x = -max_Y, y = 1, color = timestep),
             shape = 2, size = 2, show.legend = FALSE)
