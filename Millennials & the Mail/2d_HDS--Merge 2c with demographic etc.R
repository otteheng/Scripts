# -> Gerhard O
# -> 12/28/2020
# -> Data sets: 2c_HDS, 2a_HDS

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(readxl)
library(data.table)

# Initial Data Sets
raw_data_path_2c <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2c_HDS--Merge 2b files"
raw_data_path_2a <- 'C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2a_HDS'
misc_files <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Script\\Misc"

# Clean Data
clean_data_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2d_HDS--Merge 2c with demographic etc"

#------------------------------------------------------------------------#
#             Load in Data sets with Demographic, etc. variables needed  #
#                           for merge with Volume data                   #
#------------------------------------------------------------------------#

# Import

  # Mail Volume composite data
  mail.vol <- read.csv(file= paste(raw_data_path_2c, "2c_mail volume.csv", 
                                   sep = '\\'))  
  
  # Adult roster
  rects <- fread(paste(raw_data_path_2a, "2a_rects_final.csv", sep = '\\'),
                 select = c(1:24, 27:29, 31, 242:250, 252:253, 265:286, 
                            292:295))
  
  # Capitalize column names and rename
  names(rects) <- toupper(colnames(rects))
  rects <- rects %>%
    rename("SAMPLE.NUMBER" = "SAMPLE NUMBER") %>%
    rename("SURVEY.YEAR" = "SURVEY YEAR" )

#------------------------------------------------------------------------#
#                             Merge Data Sets                            #
#                       Volume Data + Adult Roster                       #
#------------------------------------------------------------------------#
  
  # Use merge function with _merge
  source(paste(misc_files, "full_join_track.R", sep = "\\"))
  
  merged <- mail.vol %>% full_join_track(rects, 
                                             by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                                             .merge = T)
  # Merge output--100% matched mail.vol
  # 0 Rows ONLY from left data frame
  # 64062 Rows ONLY from right data frame
  # 107628 Rows matched
  
  # Only keep matched rows
  merged2 <- subset(merged, .merge == "matched")
  
  # Replace spaces with dots for easier call back
  names(merged2) <- make.names(names(merged2), unique = T)
  
df.new.name <- '2d_volume-rects.csv'  
write_csv(merged2, path = paste(clean_data_path, df.new.name, sep="\\"),
          append=FALSE, col_names=TRUE)
  




