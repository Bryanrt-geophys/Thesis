#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("gridExtra")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(gganimate)
theme_set(theme_bw())

#number of timesteps/files being evaluated 
time = c(0:72)

#well locations
{
  x_well1 = 250
  y_well1 = 748
  
  x_well2 = 250
  y_well2 = 688
  
  x_well3 = 250
  y_well3 = 618
  
  x_well4 = 250
  y_well4 = 538
  
  x_well5 = 250
  y_well5 = 448
  
  x_well6 = 250
  y_well6 = 348
}

#empty transect dataframe
{
transect = data.frame(X = numeric(), 
                    Y = numeric(), 
                    w = numeric(), 
                    topo = numeric(), 
                    i = numeric())
}

#empty well dataframes
{
  well1 = data.frame(X = numeric(), 
                     Y = numeric(), 
                     w = numeric(), 
                     topo = numeric(), 
                     i = numeric(),
                     model = numeric())
  
  well2 = data.frame(X = numeric(), 
                     Y = numeric(), 
                     w = numeric(), 
                     topo = numeric(), 
                     i = numeric(),
                     model = numeric())
  
  well3 = data.frame(X = numeric(), 
                     Y = numeric(), 
                     w = numeric(), 
                     topo = numeric(), 
                     i = numeric(),
                     model = numeric())
  
  well4 = data.frame(X = numeric(), 
                     Y = numeric(), 
                     w = numeric(), 
                     topo = numeric(), 
                     i = numeric(),
                     model = numeric())
  
  well5 = data.frame(X = numeric(), 
                     Y = numeric(), 
                     w = numeric(), 
                     topo = numeric(), 
                     i = numeric(),
                     model = numeric())
  
  well6 = data.frame(X = numeric(), 
                     Y = numeric(), 
                     w = numeric(), 
                     topo = numeric(), 
                     i = numeric(),
                     model = numeric())
}

#loops through timesteps evaluating over a transect
for (i in time) {
  if (i < 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_00%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
  
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)

    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well), ]
    #new_row <- new_row[new_row$Y == sprintf("%s", y_well),]
    new_row <- cbind(new_row, i)

    transect <- rbind(transect, new_row)
  }

  if (i >= 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_0%s.xyzt", i))
    sprintf("NS_0%s.xyzt", i)
    
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well), ]
    #new_row <- new_row[new_row$Y == sprintf("%s", y_well),]
    new_row <- cbind(new_row, i)
    
    transect <- rbind(transect, new_row)
  }
}

#loops through timesteps evaluating at well1
for (i in time) {
  
  if (i < 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_00%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
    
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well1), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well1), ]
    new_row <- new_row[new_row$Y == sprintf("%s", y_well1),]
    new_row <- cbind(new_row, i)
    
    well1<- rbind(well1, new_row)
  }
  
  if (i >= 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_0%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well1), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well1), ]
    new_row <- new_row[new_row$Y == sprintf("%s", y_well1),]
    new_row <- cbind(new_row, i)
    
    well1 <- rbind(well1, new_row)
  }
}

#loops through timesteps evaluating at well2
for (i in time) {
  
  if (i < 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_00%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
    
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well2), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well2), ]
    new_row <- new_row[new_row$Y == sprintf("%s", y_well2),]
    new_row <- cbind(new_row, i)
    
    well2 <- rbind(well2, new_row)
  }
  
  if (i >= 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_0%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well2), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well2), ]
    new_row <- new_row[new_row$Y == sprintf("%s", y_well2),]
    new_row <- cbind(new_row, i)
    
    well2 <- rbind(well2, new_row)
  }
}

#loops through timesteps evaluating at well3
for (i in time) {
  
  if (i < 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_00%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
    
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well3), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well3), ]
    new_row <- new_row[new_row$Y == sprintf("%s", y_well3),]
    new_row <- cbind(new_row, i)
    
    well3 <- rbind(well3, new_row)
  }
  
  if (i >= 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_0%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well3), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well3), ]
    new_row <- new_row[new_row$Y == sprintf("%s", y_well3),]
    new_row <- cbind(new_row, i)
    
    well3 <- rbind(well3, new_row)
  }
}

#loops through timesteps evaluating at well4
for (i in time) {
  
  if (i < 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_00%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
    
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well4), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well4), ]
    new_row <- new_row[new_row$Y == sprintf("%s", y_well4),]
    new_row <- cbind(new_row, i)
    
    well4 <- rbind(well4, new_row)
  }
  
  if (i >= 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_0%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well4), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well4), ]
    new_row <- new_row[new_row$Y == sprintf("%s", y_well4),]
    new_row <- cbind(new_row, i)
    
    well4 <- rbind(well4, new_row)
  }
}

#loops through timesteps evaluating at well5
for (i in time) {
  
  if (i < 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_00%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
    
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well5), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well5), ]
    new_row <- new_row[new_row$Y == sprintf("%s", y_well5),]
    new_row <- cbind(new_row, i)
    
    well5 <- rbind(well5, new_row)
  }
  
  if (i >= 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_0%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well5), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well5), ]
    new_row <- new_row[new_row$Y == sprintf("%s", y_well5),]
    new_row <- cbind(new_row, i)
    
    well5 <- rbind(well5, new_row)
  }
}

#loops through timesteps evaluating at well6
for (i in time) {
  
  if (i < 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_00%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
    
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well6), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well6), ]
    new_row <- new_row[new_row$Y == sprintf("%s", y_well6),]
    new_row <- cbind(new_row, i)
    
    well6 <- rbind(well6, new_row)
  }
  
  if (i >= 10) {
    #itterates through all created .xyzt files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "2Thrust_VaryingDistance","thick_to_thin", sprintf("NS_0%s.xyzt", i))
    sprintf("NS_00%s.xyzt", i)
    #opens the respective .xyzt file 
    xyzt <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    #renaming the colomns
    xyzt <- xyzt %>% rename(X = V1, Y = V2, w = V3, topo = V4)
    
    #filters xyzt and puts it in a new variable including the timestep
    new_row <- xyzt[xyzt$X == sprintf("%s", x_well6), ]
    #new_row <- xyzt[xyzt$Y == sprintf("%s", y_well6), ]
    new_row <- new_row[new_row$Y == sprintf("%s", y_well6),]
    new_row <- cbind(new_row, i)
    
    well6 <- rbind(well6, new_row)
  }
}

#attempts to account for individual block uplift. Deflection seems to be calculated
#from the base of the model and therefore ignores individual block uplift
{
  for (v in well1$i) {
    if (v > 13) {
      well1$w[which(well1$i == v)] <- -well1$topo[which(well1$i == v)]
    }
  }
  
  for (v in well2$i) {
    if (v > 23) {
      well2$w[which(well2$i == v)] = -well2$topo[which(well2$i == v)]
    }
  }
  
  for (v in well3$i) {
    if (v > 33) {
      well3$w[which(well3$i == v)] <- -well3$topo[which(well3$i == v)]
    }
  }
  
  for (v in well4$i) {
    if (v >43) {
      well4$w[which(well4$i == v)] <- -well4$topo[which(well4$i == v)]
    }
  }
  
  for (v in well5$i) {
    if (v > 53) {
      well5$w[which(well5$i == v)] <- -well5$topo[which(well5$i == v)]
    }
  }
}

#creates plots
{
p1 <- ggplot(transect, aes(x = -Y)) +
  geom_line(aes(y = topo, color = i)) +
  geom_vline(xintercept = -y_well1, color = "red") +
  geom_vline(xintercept = -y_well2, color = "orange") +
  geom_vline(xintercept = -y_well3, color = "green") +
  geom_vline(xintercept = -y_well4, color = "turquoise") +
  geom_vline(xintercept = -y_well5, color = "blue") +
  geom_vline(xintercept = -y_well6, color = "purple") +
  theme_bw() + 
  labs(x = "Y coordinate (km)", y = "Topography (m)") 

p2 <- ggplot(transect, aes(x = -Y)) +
  geom_line(aes(y = -w, color = i)) +
  geom_vline(xintercept = -y_well1, color = "red") +
  geom_vline(xintercept = -y_well2, color = "orange") +
  geom_vline(xintercept = -y_well3, color = "green") +
  geom_vline(xintercept = -y_well4, color = "turquoise") +
  geom_vline(xintercept = -y_well5, color = "blue") +
  geom_vline(xintercept = -y_well6, color = "purple") +
  theme_bw() + 
  labs(x = "Y coordinate (km)", y = "Deflection (m)") 

p3 <- ggplot(, aes(x = well1$i)) +
  geom_line(aes(y = -well1$w, color = sprintf("hypothetical well 1 \n X:%s Y:%s", x_well1, y_well1))) +
  geom_line(aes(y = -well2$w, color = sprintf("hypothetical well 2 \n X:%s Y:%s", x_well2, y_well2))) +
  geom_line(aes(y = -well3$w, color = sprintf("hypothetical well 3 \n X:%s Y:%s", x_well3, y_well3))) +
  geom_line(aes(y = -well4$w, color = sprintf("hypothetical well 4 \n X:%s Y:%s", x_well4, y_well4))) +
  geom_line(aes(y = -well5$w, color = sprintf("hypothetical well 5 \n X:%s Y:%s", x_well5, y_well5))) +
  geom_line(aes(y = -well6$w, color = sprintf("hypothetical well 6 \n X:%s Y:%s", x_well6, y_well6))) +
  theme_bw() + 
  labs(x = "Timestep (My)", y = "Deflection (m)") + 
  theme(legend.title = element_blank())

p4 <- ggplot(transect, aes(x = -Y)) +
  geom_point(aes(y = topo, color = i)) +
  theme_bw() + 
  labs(x = "Y coordinate (km)", y = "Topography (m)") + 
  transition_time(i) + 
  labs(title = sprintf("Timestep: {%s}", transect$i)) +
  shadow_wake(wake_length = 0.1, alpha = FALSE) 

p5 <- ggplot(transect, aes(x = -Y)) +
  geom_point(aes(y = -w, color = i)) +
  theme_bw() + 
  labs(x = "Y coordinate (km)", y = "Deflection (m)") + 
  transition_time(i) + 
  labs(title = "Timestep: {i}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)
}

# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}  # Multiple plot function

multiplot(p1, p2, p3, cols = 2) # Plots all in two columns
