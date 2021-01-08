# Wrangling .pfl documents to examin deflection, prepare basinmod files, &
# basin geoemetries

#function used to bind together dataframes of different sizes
rbind.all.columns <- function(x, y) {

  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))

  x[, c(as.character(y.diff))] <- NA

  y[, c(as.character(x.diff))] <- NA

  return(rbind(x, y))
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
}

library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(gganimate)
theme_set(theme_bw())


peak_steps1 <- c(12)
peak_steps2 <- c(22)
peak_steps3 <- c(32)
peak_steps4 <- c(42)
peak_steps5 <- c(52)
peak_steps6 <- c(62)

# finds the location of peak subsidence
  for (i in peak_steps1) {
    path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_0%s.pfl", i))

    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)

    # filters pfl and puts it in a new variable including the timestep
    peak_sub1 <- pfl[which(pfl$V5 == min(pfl$V5)),]$V2
  }
  for (i in peak_steps2) {
    path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)

    # filters pfl and puts it in a new variable including the timestep
    peak_sub2 <- pfl[which(pfl$V6 == min(pfl$V6)),]$V2
  }
  for (i in peak_steps3) {
    path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)

    # filters pfl and puts it in a new variable including the timestep
    peak_sub3 <- pfl[which(pfl$V7 == min(pfl$V7)),]$V2
  }
  for (i in peak_steps4) {
    path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)

    # filters pfl and puts it in a new variable including the timestep
    peak_sub4 <- pfl[which(pfl$V8 == min(pfl$V8)),]$V2
  }
  for (i in peak_steps5) {
    path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)

    # filters pfl and puts it in a new variable including the timestep
    peak_sub5 <- pfl[which(pfl$V9 == min(pfl$V9)),]$V2
  }
  for (i in peak_steps6) {
    path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)

    # filters pfl and puts it in a new variable including the timestep
    peak_sub6 <- pfl[which(pfl$V10 == min(pfl$V10)),]$V2
  }

# number of timesteps/files being evaluated 
time = c(1:72)

# well1 analysis
{
  # creates empty dataframe
  transect <- data.frame(matrix(NA, ncol = 28))
  basinmod1 <- data.frame(matrix(NA, ncol = 2))
  
  # rename to match pfl columns
  {transect <- rename(transect, 
                      V1 = X1,
                      V2 = X2,
                      V3 = X3,
                      V4 = X4,
                      V5 = X5,
                      V6 = X6,
                      V7 = X7,
                      V8 = X8,
                      V9 = X9,
                      V10 = X10,
                      V11 = X11,
                      V12 = X12,
                      V13 = X13,
                      V14 = X14,
                      V15 = X15,
                      V16 = X16,
                      V17 = X17,
                      V18 = X18,
                      V19 = X19,
                      V20 = X20,
                      V21 = X21,
                      V22 = X22,
                      V23 = X23,
                      V24 = X24,
                      V25 = X25,
                      V26 = X26,
                      V27 = X27,
                      i = X28)      
  }
  
  # rename basinmod1 columns
  {  
  basinmod1 <- rename(basinmod1,
                      Ma22 = X1,
                      Ma12 = X2)
}  
  
  # puts all of the filtered pfl values into a data frame
for (i in time) {
  if (i < 10) {
    # itterates through all created .pfl files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_00%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    # filters pfl and puts it in a new variable including the timestep
    new_row <- pfl[which(pfl$V2 == peak_sub1),]
    new_row <- cbind(new_row, i)
    rownames(new_row) <- i
    
    transect <- rbind.all.columns(transect, new_row)
  }
  
  if (i >= 10 && i <= 22) {
    # itterates through all created .pfl files in the directory path
    path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_0%s.pfl", i))
    
    # opens the respective .pfl file 
    pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    # filters pfl and puts it in a new variable including the timestep
    new_row <- pfl[which(pfl$V2 == peak_sub1),]
    new_row <- cbind(new_row, i)
    rownames(new_row) <- i
    
    transect <- rbind.all.columns(transect, new_row)
  }
}

timestep = c(0)
depth = c(0)
subsidence = data.frame(timestep, depth)

# subisdence curve generated from the pfl file. Compare this to the sub curve made through Basinmod
for (i in time) {
  if (i <= peak_steps1) {
    subsidence[i, 2] <- rbind(transect$V5[which(transect$i == i)])
    subsidence[i, 1] <- cbind(i)
  }
  if (i <= peak_steps2 && i > peak_steps1) {
    subsidence[i, 2] <- rbind(transect$V6[which(transect$i == i)])
    subsidence[i, 1] <- cbind(i)
  }
  if (i <= peak_steps3 && i > peak_steps2) {
    subsidence[i, 2] <- rbind(transect$V7[which(transect$i == i)])
    subsidence[i, 1] <- cbind(i)
  }
  if (i <= peak_steps4 && i > peak_steps3) {
    subsidence[i, 2] <- rbind(transect$V8[which(transect$i == i)])
    subsidence[i, 1] <- cbind(i)
  }
  if (i <= peak_steps5 && i > peak_steps4) {
    subsidence[i, 2] <- rbind(transect$V9[which(transect$i == i)])
    subsidence[i, 1] <- cbind(i)
  }
  if (i <= peak_steps6 && i > peak_steps5) {
    subsidence[i, 2] <- rbind(transect$V10[which(transect$i == i)])
    subsidence[i, 1] <- cbind(i)
  }
  if (i > peak_steps6) {
    subsidence[i, 2] <- rbind(transect$V11[which(transect$i == i)])
    subsidence[i, 1] <- cbind(i)
  }
}

for (i in c(5:26)) {
  basinmod1[i,1] <- rbind(transect[22, i + 1])
  basinmod1[i,2] <- rbind(transect[12, i + 1])
}

basinmod1 <-basinmod1[-c(1,2,3,4),]
}

# # well2 analysis
# {
#   # creates empty dataframe
#   transect <- data.frame(matrix(NA, ncol = 28))
#   
#   basinmod2 <- data.frame(matrix(NA, ncol = 2))
#   
#   # rename to match pfl columns
#   {transect <- rename(transect, 
#                       V1 = X1,
#                       V2 = X2,
#                       V3 = X3,
#                       V4 = X4,
#                       V5 = X5,
#                       V6 = X6,
#                       V7 = X7,
#                       V8 = X8,
#                       V9 = X9,
#                       V10 = X10,
#                       V11 = X11,
#                       V12 = X12,
#                       V13 = X13,
#                       V14 = X14,
#                       V15 = X15,
#                       V16 = X16,
#                       V17 = X17,
#                       V18 = X18,
#                       V19 = X19,
#                       V20 = X20,
#                       V21 = X21,
#                       V22 = X22,
#                       V23 = X23,
#                       V24 = X24,
#                       V25 = X25,
#                       V26 = X26,
#                       V27 = X27,
#                       i = X28)      
#   }
#   
#   # rename basinmod2 columns
#   {  
#     basinmod2 <- rename(basinmod2,
#                         Ma22 = X1,
#                         Ma12 = X2)
#   }  
#   
#    # puts all of the filtered pfl values into a data frame
#   for (i in time) {
#     if (i < 10) {
#       # itterates through all created .pfl files in the directory path
#       path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_00%s.pfl", i))
#       
#       # opens the respective .pfl file 
#       pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
#       
#       # filters pfl and puts it in a new variable including the timestep
#       new_row <- pfl[which(pfl$V2 == peak_sub2),]
#       new_row <- cbind(new_row, i)
#       rownames(new_row) <- i
#       
#       transect <- rbind.all.columns(transect, new_row)
#     }
#     
#     if (i >= 10) {
#       # itterates through all created .pfl files in the directory path
#       path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_0%s.pfl", i))
#       
#       # opens the respective .pfl file 
#       pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
#       
#       # filters pfl and puts it in a new variable including the timestep
#       new_row <- pfl[which(pfl$V2 == peak_sub2),]
#       new_row <- cbind(new_row, i)
#       rownames(new_row) <- i
#       
#       transect <- rbind.all.columns(transect, new_row)
#     }
#   }
#   
#   timestep = c(0)
#   depth = c(0)
#   subsidence2 = data.frame(timestep, depth)
#   
#   # subisdence curve generated from the pfl file. Compare this to the sub curve made through Basinmod
#   for (i in time) {
#     if (i <= peak_steps1) {
#       print(transect$V5[which(transect$i == i)])
#       subsidence2[i, 2] <- rbind(transect$V5[which(transect$i == i)])
#       subsidence2[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps2 && i > peak_steps1) {
#       print(transect$V6[which(transect$i == i)])
#       subsidence2[i, 2] <- rbind(transect$V6[which(transect$i == i)])
#       subsidence2[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps3 && i > peak_steps2) {
#       print(transect$V7[which(transect$i == i)])
#       subsidence2[i, 2] <- rbind(transect$V7[which(transect$i == i)])
#       subsidence2[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps4 && i > peak_steps3) {
#       print(transect$V8[which(transect$i == i)])
#       subsidence2[i, 2] <- rbind(transect$V8[which(transect$i == i)])
#       subsidence2[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps5 && i > peak_steps4) {
#       print(transect$V9[which(transect$i == i)])
#       subsidence2[i, 2] <- rbind(transect$V9[which(transect$i == i)])
#       subsidence2[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps6 && i > peak_steps5) {
#       print(transect$V10[which(transect$i == i)])
#       subsidence2[i, 2] <- rbind(transect$V10[which(transect$i == i)])
#       subsidence2[i, 1] <- cbind(i)
#     }
#     if (i > peak_steps6) {
#       print(transect$V11[which(transect$i == i)])
#       subsidence2[i, 2] <- rbind(transect$V11[which(transect$i == i)])
#       subsidence2[i, 1] <- cbind(i)
#     }
#   }
#   
#   for (i in c(6:38)) {
#     basinmod2[i,1] <- rbind(transect[32, i + 1])
#     basinmod2[i,2] <- rbind(transect[22, i + 1])
#     
#   }
#   
#   basinmod2 <-basinmod2[-c(1,2,3,4,5,27),]
# }
# 
# # well3 analysis
# {
#   # creates empty dataframe
#   transect <- data.frame(matrix(NA, ncol = 28))
#   
#   basinmod3 <- data.frame(matrix(NA, ncol = 2))
#   
#   # rename to match pfl columns
#   {transect <- rename(transect, 
#                       V1 = X1,
#                       V2 = X2,
#                       V3 = X3,
#                       V4 = X4,
#                       V5 = X5,
#                       V6 = X6,
#                       V7 = X7,
#                       V8 = X8,
#                       V9 = X9,
#                       V10 = X10,
#                       V11 = X11,
#                       V12 = X12,
#                       V13 = X13,
#                       V14 = X14,
#                       V15 = X15,
#                       V16 = X16,
#                       V17 = X17,
#                       V18 = X18,
#                       V19 = X19,
#                       V20 = X20,
#                       V21 = X21,
#                       V22 = X22,
#                       V23 = X23,
#                       V24 = X24,
#                       V25 = X25,
#                       V26 = X26,
#                       V27 = X27,
#                       i = X28)      
#   }
#   
#   # rename basinmod3 columns
#   {  
#     basinmod3 <- rename(basinmod3,
#                         Ma22 = X1,
#                         Ma12 = X2)
#   }  
#   
#   # puts all of the filtered pfl values into a data frame
#   for (i in time) {
#     if (i < 10) {
#       # itterates through all created .pfl files in the directory path
#       path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_00%s.pfl", i))
#       
#       # opens the respective .pfl file 
#       pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
#       
#       # filters pfl and puts it in a new variable including the timestep
#       new_row <- pfl[which(pfl$V2 == peak_sub3),]
#       new_row <- cbind(new_row, i)
#       rownames(new_row) <- i
#       
#       transect <- rbind.all.columns(transect, new_row)
#     }
#     
#     if (i >= 10) {
#       # itterates through all created .pfl files in the directory path
#       path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_0%s.pfl", i))
#       
#       # opens the respective .pfl file 
#       pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
#       
#       # filters pfl and puts it in a new variable including the timestep
#       new_row <- pfl[which(pfl$V2 == peak_sub3),]
#       new_row <- cbind(new_row, i)
#       rownames(new_row) <- i
#       
#       transect <- rbind.all.columns(transect, new_row)
#     }
#     
#   }
#   
#   timestep = c(0)
#   depth = c(0)
#   subsidence3 = data.frame(timestep, depth)
#   
#   # subisdence curve generated from the pfl file. Compare this to the sub curve made through Basinmod
#   for (i in time) {
#     if (i <= peak_steps1) {
#       print(transect$V5[which(transect$i == i)])
#       subsidence3[i, 2] <- rbind(transect$V5[which(transect$i == i)])
#       subsidence3[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps2 && i > peak_steps1) {
#       print(transect$V6[which(transect$i == i)])
#       subsidence3[i, 2] <- rbind(transect$V6[which(transect$i == i)])
#       subsidence3[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps3 && i > peak_steps2) {
#       print(transect$V7[which(transect$i == i)])
#       subsidence3[i, 2] <- rbind(transect$V7[which(transect$i == i)])
#       subsidence3[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps4 && i > peak_steps3) {
#       print(transect$V8[which(transect$i == i)])
#       subsidence3[i, 2] <- rbind(transect$V8[which(transect$i == i)])
#       subsidence3[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps5 && i > peak_steps4) {
#       print(transect$V9[which(transect$i == i)])
#       subsidence3[i, 2] <- rbind(transect$V9[which(transect$i == i)])
#       subsidence3[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps6 && i > peak_steps5) {
#       print(transect$V10[which(transect$i == i)])
#       subsidence3[i, 2] <- rbind(transect$V10[which(transect$i == i)])
#       subsidence3[i, 1] <- cbind(i)
#     }
#     if (i > peak_steps6) {
#       print(transect$V11[which(transect$i == i)])
#       subsidence3[i, 2] <- rbind(transect$V11[which(transect$i == i)])
#       subsidence3[i, 1] <- cbind(i)
#     }
#   }
#   
#   for (i in c(7:49)) {
#     basinmod3[i,1] <- rbind(transect[42, i + 1])
#     basinmod3[i,2] <- rbind(transect[32, i + 1])
#   }
#   
#   basinmod3 <-basinmod3[-c(1,2,3,4,5,6,27),]
# }
# 
# # well4 analysis
# {
#   # creates empty dataframe
#   transect <- data.frame(matrix(NA, ncol = 28))
#   
#   basinmod4 <- data.frame(matrix(NA, ncol = 2))
#   
#   # rename to match pfl columns
#   {transect <- rename(transect, 
#                       V1 = X1,
#                       V2 = X2,
#                       V3 = X3,
#                       V4 = X4,
#                       V5 = X5,
#                       V6 = X6,
#                       V7 = X7,
#                       V8 = X8,
#                       V9 = X9,
#                       V10 = X10,
#                       V11 = X11,
#                       V12 = X12,
#                       V13 = X13,
#                       V14 = X14,
#                       V15 = X15,
#                       V16 = X16,
#                       V17 = X17,
#                       V18 = X18,
#                       V19 = X19,
#                       V20 = X20,
#                       V21 = X21,
#                       V22 = X22,
#                       V23 = X23,
#                       V24 = X24,
#                       V25 = X25,
#                       V26 = X26,
#                       V27 = X27,
#                       i = X28)      
#   }
#   
#   # rename basinmod4 columns
#   {  
#     basinmod4 <- rename(basinmod4,
#                         Ma22 = X1,
#                         Ma12 = X2)
#   }  
#   
#   # puts all of the filtered pfl values into a data frame
#   for (i in time) {
#     if (i < 10) {
#       # itterates through all created .pfl files in the directory path
#       path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_00%s.pfl", i))
#       
#       # opens the respective .pfl file 
#       pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
#       
#       # filters pfl and puts it in a new variable including the timestep
#       new_row <- pfl[which(pfl$V2 == peak_sub4),]
#       new_row <- cbind(new_row, i)
#       rownames(new_row) <- i
#       
#       transect <- rbind.all.columns(transect, new_row)
#     }
#     
#     if (i >= 10) {
#       # itterates through all created .pfl files in the directory path
#       path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_0%s.pfl", i))
#       
#       # opens the respective .pfl file 
#       pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
#       
#       # filters pfl and puts it in a new variable including the timestep
#       new_row <- pfl[which(pfl$V2 == peak_sub4),]
#       new_row <- cbind(new_row, i)
#       rownames(new_row) <- i
#       
#       transect <- rbind.all.columns(transect, new_row)
#     }
#   }
#   
#   timestep = c(0)
#   depth = c(0)
#   subsidence4 = data.frame(timestep, depth)
#   
#   # subisdence curve generated from the pfl file. Compare this to the sub curve made through Basinmod
#   for (i in time) {
#     if (i <= peak_steps1) {
#       print(transect$V5[which(transect$i == i)])
#       subsidence4[i, 2] <- rbind(transect$V5[which(transect$i == i)])
#       subsidence4[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps2 && i > peak_steps1) {
#       print(transect$V6[which(transect$i == i)])
#       subsidence4[i, 2] <- rbind(transect$V6[which(transect$i == i)])
#       subsidence4[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps3 && i > peak_steps2) {
#       print(transect$V7[which(transect$i == i)])
#       subsidence4[i, 2] <- rbind(transect$V7[which(transect$i == i)])
#       subsidence4[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps4 && i > peak_steps3) {
#       print(transect$V8[which(transect$i == i)])
#       subsidence4[i, 2] <- rbind(transect$V8[which(transect$i == i)])
#       subsidence4[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps5 && i > peak_steps4) {
#       print(transect$V9[which(transect$i == i)])
#       subsidence4[i, 2] <- rbind(transect$V9[which(transect$i == i)])
#       subsidence4[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps6 && i > peak_steps5) {
#       print(transect$V10[which(transect$i == i)])
#       subsidence4[i, 2] <- rbind(transect$V10[which(transect$i == i)])
#       subsidence4[i, 1] <- cbind(i)
#     }
#     if (i > peak_steps6) {
#       print(transect$V11[which(transect$i == i)])
#       subsidence4[i, 2] <- rbind(transect$V11[which(transect$i == i)])
#       subsidence4[i, 1] <- cbind(i)
#     }
#   }
#   
#   for (i in c(8:60)) {
#     basinmod4[i,1] <- rbind(transect[52, i + 1])
#     basinmod4[i,2] <- rbind(transect[42, i + 1])
#   }
#   
#   basinmod4 <-basinmod4[-c(1,2,3,4,5,6,7,27),]
# }
# 
# # well5 analysis
# {
#   # creates empty dataframe
#   transect <- data.frame(matrix(NA, ncol = 28))
#   
#   basinmod5 <- data.frame(matrix(NA, ncol = 2))
#   
#   # rename to match pfl columns
#   {transect <- rename(transect, 
#                       V1 = X1,
#                       V2 = X2,
#                       V3 = X3,
#                       V4 = X4,
#                       V5 = X5,
#                       V6 = X6,
#                       V7 = X7,
#                       V8 = X8,
#                       V9 = X9,
#                       V10 = X10,
#                       V11 = X11,
#                       V12 = X12,
#                       V13 = X13,
#                       V14 = X14,
#                       V15 = X15,
#                       V16 = X16,
#                       V17 = X17,
#                       V18 = X18,
#                       V19 = X19,
#                       V20 = X20,
#                       V21 = X21,
#                       V22 = X22,
#                       V23 = X23,
#                       V24 = X24,
#                       V25 = X25,
#                       V26 = X26,
#                       V27 = X27,
#                       i = X28)      
#   }
#   
#   # rename basinmod5 columns
#   {  
#     basinmod5 <- rename(basinmod5,
#                         Ma22 = X1,
#                         Ma12 = X2)
#   }  
#   
#   # puts all of the filtered pfl values into a data frame
#   for (i in time) {
#     if (i < 10) {
#       # itterates through all created .pfl files in the directory path
#       path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_00%s.pfl", i))
#       
#       # opens the respective .pfl file 
#       pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
#       
#       # filters pfl and puts it in a new variable including the timestep
#       new_row <- pfl[which(pfl$V2 == peak_sub5),]
#       new_row <- cbind(new_row, i)
#       rownames(new_row) <- i
#       
#       transect <- rbind.all.columns(transect, new_row)
#     }
#     
#     if (i >= 10) {
#       # itterates through all created .pfl files in the directory path
#       path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_0%s.pfl", i))
#       
#       # opens the respective .pfl file 
#       pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
#       
#       # filters pfl and puts it in a new variable including the timestep
#       new_row <- pfl[which(pfl$V2 == peak_sub5),]
#       new_row <- cbind(new_row, i)
#       rownames(new_row) <- i
#       
#       transect <- rbind.all.columns(transect, new_row)
#     }
#   }
#   
#   timestep = c(0)
#   depth = c(0)
#   subsidence5 = data.frame(timestep, depth)
#   
#   # subisdence curve generated from the pfl file. Compare this to the sub curve made through Basinmod
#   for (i in time) {
#     if (i <= peak_steps1) {
#       print(transect$V5[which(transect$i == i)])
#       subsidence5[i, 2] <- rbind(transect$V5[which(transect$i == i)])
#       subsidence5[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps2 && i > peak_steps1) {
#       print(transect$V6[which(transect$i == i)])
#       subsidence5[i, 2] <- rbind(transect$V6[which(transect$i == i)])
#       subsidence5[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps3 && i > peak_steps2) {
#       print(transect$V7[which(transect$i == i)])
#       subsidence5[i, 2] <- rbind(transect$V7[which(transect$i == i)])
#       subsidence5[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps4 && i > peak_steps3) {
#       print(transect$V8[which(transect$i == i)])
#       subsidence5[i, 2] <- rbind(transect$V8[which(transect$i == i)])
#       subsidence5[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps5 && i > peak_steps4) {
#       print(transect$V9[which(transect$i == i)])
#       subsidence5[i, 2] <- rbind(transect$V9[which(transect$i == i)])
#       subsidence5[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps6 && i > peak_steps5) {
#       print(transect$V10[which(transect$i == i)])
#       subsidence5[i, 2] <- rbind(transect$V10[which(transect$i == i)])
#       subsidence5[i, 1] <- cbind(i)
#     }
#     if (i > peak_steps6) {
#       print(transect$V11[which(transect$i == i)])
#       subsidence5[i, 2] <- rbind(transect$V11[which(transect$i == i)])
#       subsidence5[i, 1] <- cbind(i)
#     }
#   }
#   
#   for (i in c(9:69)) {
#     basinmod5[i,1] <- rbind(transect[62, i + 1])
#     basinmod5[i,2] <- rbind(transect[52, i + 1])
#   }
#   
#   basinmod5 <-basinmod5[-c(1,2,3,4,5,6,7,8,27),]
# }
# 
# # well6 analysis
# {
#   # creates empty dataframe
#   transect <- data.frame(matrix(NA, ncol = 28))
#   
#   basinmod6 <- data.frame(matrix(NA, ncol = 2))
#   
#   # rename to match pfl columns
#   {transect <- rename(transect, 
#                       V1 = X1,
#                       V2 = X2,
#                       V3 = X3,
#                       V4 = X4,
#                       V5 = X5,
#                       V6 = X6,
#                       V7 = X7,
#                       V8 = X8,
#                       V9 = X9,
#                       V10 = X10,
#                       V11 = X11,
#                       V12 = X12,
#                       V13 = X13,
#                       V14 = X14,
#                       V15 = X15,
#                       V16 = X16,
#                       V17 = X17,
#                       V18 = X18,
#                       V19 = X19,
#                       V20 = X20,
#                       V21 = X21,
#                       V22 = X22,
#                       V23 = X23,
#                       V24 = X24,
#                       V25 = X25,
#                       V26 = X26,
#                       V27 = X27,
#                       i = X28)      
#   }
#   
#   # rename basinmod5 columns
#   {  
#     basinmod6 <- rename(basinmod6,
#                         Ma22 = X1,
#                         Ma12 = X2)
#   }  
#   
#   # puts all of the filtered pfl values into a data frame
#   for (i in time) {
#     if (i < 10) {
#       # itterates through all created .pfl files in the directory path
#       path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_00%s.pfl", i))
#       
#       # opens the respective .pfl file 
#       pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
#       
#       # filters pfl and puts it in a new variable including the timestep
#       new_row <- pfl[which(pfl$V2 == peak_sub6),]
#       new_row <- cbind(new_row, i)
#       rownames(new_row) <- i
#       
#       transect <- rbind.all.columns(transect, new_row)
#     }
#     
#     if (i >= 10) {
#       # itterates through all created .pfl files in the directory path
#       path <- file.path("~","Bryan", "Tisc_Models", "1Thrust_VaryingTVA", "3_test_T4_V5_A18", "2D3", sprintf("NS_0%s.pfl", i))
#       
#       # opens the respective .pfl file 
#       pfl <- read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
#       
#       # filters pfl and puts it in a new variable including the timestep
#       new_row <- pfl[which(pfl$V2 == peak_sub6),]
#       new_row <- cbind(new_row, i)
#       rownames(new_row) <- i
#       
#       transect <- rbind.all.columns(transect, new_row)
#     }
#   }
#   
#   timestep = c(0)
#   depth = c(0)
#   subsidence6 = data.frame(timestep, depth)
#   
#   # subisdence curve generated from the pfl file. Compare this to the sub curve made through Basinmod
#   for (i in time) {
#     if (i <= peak_steps1) {
#       print(transect$V5[which(transect$i == i)])
#       subsidence6[i, 2] <- rbind(transect$V5[which(transect$i == i)])
#       subsidence6[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps2 && i > peak_steps1) {
#       print(transect$V6[which(transect$i == i)])
#       subsidence6[i, 2] <- rbind(transect$V6[which(transect$i == i)])
#       subsidence6[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps3 && i > peak_steps2) {
#       print(transect$V7[which(transect$i == i)])
#       subsidence6[i, 2] <- rbind(transect$V7[which(transect$i == i)])
#       subsidence6[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps4 && i > peak_steps3) {
#       print(transect$V8[which(transect$i == i)])
#       subsidence6[i, 2] <- rbind(transect$V8[which(transect$i == i)])
#       subsidence6[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps5 && i > peak_steps4) {
#       print(transect$V9[which(transect$i == i)])
#       subsidence6[i, 2] <- rbind(transect$V9[which(transect$i == i)])
#       subsidence6[i, 1] <- cbind(i)
#     }
#     if (i <= peak_steps6 && i > peak_steps5) {
#       print(transect$V10[which(transect$i == i)])
#       subsidence6[i, 2] <- rbind(transect$V10[which(transect$i == i)])
#       subsidence6[i, 1] <- cbind(i)
#     }
#     if (i > peak_steps6) {
#       print(transect$V11[which(transect$i == i)])
#       subsidence6[i, 2] <- rbind(transect$V11[which(transect$i == i)])
#       subsidence6[i, 1] <- cbind(i)
#     }
#   }
#   
#   for (i in c(10:80)) {
#     basinmod6[i,1] <- rbind(transect[72, i + 1])
#     basinmod6[i,2] <- rbind(transect[62, i + 1])
#   }
#   
#   basinmod6 <- basinmod6[-c(1,2,3,4,5,6,7,8,9,27),]
# }

# plots geohistory curves
p_sub <- ggplot(,aes(x = subsidence$timestep)) +
  geom_line(aes(y = subsidence$depth, color = sprintf("hypothetical well 1 \n X:%s Y:%s", transect$V1[9], peak_sub1))) +
  # geom_line(aes(y = subsidence2$depth, color = sprintf("hypothetical well 2 \n X:%s Y:%s", transect$V1[9], peak_sub2))) +
  # geom_line(aes(y = subsidence3$depth, color = sprintf("hypothetical well 3 \n X:%s Y:%s", transect$V1[9], peak_sub3))) +
  # geom_line(aes(y = subsidence4$depth, color = sprintf("hypothetical well 4 \n X:%s Y:%s", transect$V1[9], peak_sub4))) +
  # geom_line(aes(y = subsidence5$depth, color = sprintf("hypothetical well 5 \n X:%s Y:%s", transect$V1[9], peak_sub5))) +
  # geom_line(aes(y = subsidence6$depth, color = sprintf("hypothetical well 6 \n X:%s Y:%s", transect$V1[9], peak_sub6))) +
  theme_bw() +
  labs(x = "Timestep (My)", y = "Subsidence (m)") + 
  theme(legend.title = element_blank()) +
  guides(color = FALSE)
  # guides(color = FALSE) +
  # geom_vline(aes(xintercept = 12)) +
  # geom_vline(aes(xintercept = 22)) +
  # geom_vline(aes(xintercept = 33)) +
  # geom_vline(aes(xintercept = 42)) +
  # geom_vline(aes(xintercept = 52)) +
  # geom_vline(aes(xintercept = 62)) 

ggplot(,aes(x = -pfl$V2)) +
   # geom_line(aes(y = pfl$V4)) +
   geom_line(aes(y = pfl$V5)) +
   geom_line(aes(y = pfl$V6)) +
   geom_line(aes(y = pfl$V7)) +
   geom_line(aes(y = pfl$V8)) +
   geom_line(aes(y = pfl$V9)) +
   geom_line(aes(y = pfl$V10)) +
   geom_line(aes(y = pfl$V16, color = "12 My")) +
   geom_line(aes(y = pfl$V27, color = "22 My")) +
  # geom_line(aes(y = pfl$V38, color = "32 My")) +
  # geom_line(aes(y = pfl$V49, color = "42 My")) +
  # geom_line(aes(y = pfl$V60, color = "52 My")) +
  # geom_line(aes(y = pfl$V71, color = "62 My")) +
  # geom_line(aes(y = pfl$V82, color = "72 My")) +
  theme_bw()


