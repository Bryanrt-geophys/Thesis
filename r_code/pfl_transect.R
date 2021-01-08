library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(gganimate)
theme_set(theme_bw())

# creates empty dataframe
transect <- data.frame(matrix(NA, ncol = 1))

temp <- data.frame(matrix(NA, ncol = 1))

time = c(1:72)
j = 0

for (i in time) {
  if (i < 2) {
    # itterates through all created .pfl files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "Sediment_Transport", "Increased_l_fluv_sedim", sprintf("NS_00%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    # filters pfl and puts it in a new variable including the timestep
    transect <- transect %>% cbind(pfl$V1) %>% cbind(pfl$V2) %>% cbind(pfl$V5)
  }
  if (i >= 2 && i < 10) {
    # itterates through all created .pfl files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "Sediment_Transport", "Increased_l_fluv_sedim", sprintf("NS_00%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    # filters pfl and puts it in a new variable including the timestep
    transect <- transect %>% cbind(pfl$V5)
  }
  if (i >= 10 && i <13) {
    # itterates through all created .pfl files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "Sediment_Transport", "Increased_l_fluv_sedim", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    # filters pfl and puts it in a new variable including the timestep
    transect <- transect %>% cbind(pfl$V5) 
  }  
  if (i >= 13 && i < 23) {
    # itterates through all created .pfl files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "Sediment_Transport", "Increased_l_fluv_sedim", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    # filters pfl and puts it in a new variable including the timestep
    transect <- transect %>% cbind(pfl$V6) 
  }
  if (i >= 23 && i < 33) {
    # itterates through all created .pfl files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "Sediment_Transport", "Increased_l_fluv_sedim", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    # filters pfl and puts it in a new variable including the timestep
    transect <- transect %>% cbind(pfl$V7) 
  }
  if (i >= 33 && i < 43) {
    # itterates through all created .pfl files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "Sediment_Transport", "Increased_l_fluv_sedim", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    # filters pfl and puts it in a new variable including the timestep
    transect <- transect %>% cbind(pfl$V8) 
  }
  if (i >= 43 && i < 53) {
    # itterates through all created .pfl files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "Sediment_Transport", "Increased_l_fluv_sedim", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    # filters pfl and puts it in a new variable including the timestep
    transect <- transect %>% cbind(pfl$V9) 
  }
  if (i >= 53 && i < 63) {
    # itterates through all created .pfl files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "Sediment_Transport", "Increased_l_fluv_sedim", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    # filters pfl and puts it in a new variable including the timestep
    transect <- transect %>% cbind(pfl$V10) 
  }
  if (i >= 63 && i < 73) {
    # itterates through all created .pfl files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "Sediment_Transport", "Increased_l_fluv_sedim", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    # filters pfl and puts it in a new variable including the timestep
    transect <- transect %>% cbind(pfl$V11) 
  }
}

# removes unused first colomn -- janky coding
transect <- transect[,-c(1,2)]

# renames columns to be unique
transect <- rename_with(transect, ~ paste0("colname", 1:length(transect)))

# Pivot to long format
long_transect <- transect %>%
  pivot_longer(-colname1,
               names_to = "timestep",
               names_prefix="colname_",
               values_to = "elevation") %>%
  mutate(elevation = if_else(abs(elevation) < 10, 0, elevation)) # say that <0 is =0

long_transect$timestep = fct_inorder(long_transect$timestep)

# Helper function, finds the first non-negative after the minimum
find_first_non_zero <- function(elevation_vector){
  
  start_at <- which.min(elevation_vector) # the position of the minimum
  
  if(elevation_vector[start_at] > 0){
    warning("The lowest point is positive! Returning NA.")
    return(NA)
  }
  
  subset_positions <- seq(start_at, length(elevation_vector))
  
  pos <- first(which(elevation_vector[subset_positions] >= 0))
  pos + start_at
}

# now, for each timestamp, compute index, Z and Y of interesting points
points_transect <- long_transect %>%
  group_by(timestep) %>%
  summarize(i_lowest_point = which.min(elevation),
            i_sea_level = find_first_non_zero(elevation),
            Z_lowest_point = elevation[i_lowest_point],
            Z_sea_level = elevation[i_sea_level],
            Y_lowest_point = colname1[i_lowest_point],
            Y_sea_level = colname1[i_sea_level]) %>%
  mutate(basin_width = Y_lowest_point - Y_sea_level) %>%
  mutate(My = c(1:(ncol(transect)-1))) 
# %>%
#   mutate(sub1 = subsidence$depth) %>%
#   mutate(sub2 = subsidence2$depth) %>%
#   mutate(sub3 = subsidence3$depth) %>%
#   mutate(sub4 = subsidence4$depth) %>%
#   mutate(sub5 = subsidence5$depth) %>%
#   mutate(sub6 = subsidence6$depth)
#> `summarise()` ungrouping output (override with `.groups` argument)


# We can plot it all together
ggplot(long_transect) +
  geom_line(aes(y=elevation, x=-colname1, color = timestep)) +
  geom_point(data = points_transect,
             mapping = aes(x=-Y_lowest_point, y=Z_lowest_point, color = timestep),
             shape = 1, size = 2, show.legend = FALSE) +
  geom_point(data = points_transect,
             mapping = aes(x=-Y_sea_level, y=Z_sea_level, color = timestep),
             shape = 2, size = 2, show.legend = FALSE) +
  facet_wrap(~timestep) +
  guides(color = FALSE)

p_width <- ggplot(points_transect, aes(x = My)) +
  geom_line(aes(y = basin_width))
#   geom_vline(aes(xintercept = 12)) +
#   geom_vline(aes(xintercept = 22)) +
#   geom_vline(aes(xintercept = 33)) +
#   geom_vline(aes(xintercept = 42)) +
#   geom_vline(aes(xintercept = 52)) +
#   geom_vline(aes(xintercept = 62)) 

# ggplot(points_transect, aes(x = My)) +
#   geom_line(aes(y = basin_width * 5, color = "width")) +
#   geom_line(aes(y = sub1, color = "sub1")) +
#   geom_line(aes(y = sub2, color = "sub2")) +
#   geom_line(aes(y = sub3, color = "sub3")) +
#   geom_line(aes(y = sub4, color = "sub4")) +
#   geom_line(aes(y = sub5, color = "sbu5")) +
#   geom_line(aes(y = sub6, color = "sub6")) +
#   geom_vline(aes(xintercept = 12)) +
#   geom_vline(aes(xintercept = 22)) +
#   geom_vline(aes(xintercept = 33)) +
#   geom_vline(aes(xintercept = 42)) +
#   geom_vline(aes(xintercept = 52)) +
#   geom_vline(aes(xintercept = 62))
