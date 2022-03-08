# -> Gerhard O
# -> 12/3/2022
# -> Data sets: 1c_HDS, 1a_HDS

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(readxl)
library(data.table)
library(zoo)

# Set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Initial Data Sets
raw_data_path_1c <- "./Mail Floor/Code/Clean Data/Updated HDS/1c--Merge 1b files"
raw_data_path_1a <- "./_Raw Data/Household Diary Survey/Unzipped files (CSV)/2000-2020"
raw_data_path_2a <- "./_Raw Data/Household Diary Survey/Unzipped files (CSV)/2000-2019"
misc_files <- "./_Scripts"

# Clean Data
clean_data_path <- "./Mail Floor/Code/Clean Data/Updated HDS/1d--Merge 1c with Demographic vars"

#------------------------------------------------------------------------#
#     Step 1: Load in Data sets with Demographic, etc. variables needed  #
#                       for merge with Volume data                       #
#                                                                        #
#------------------------------------------------------------------------#

#1.1: Import

    #A. Mail Volume composite data
    mail.vol <- read.csv(file= paste(raw_data_path_1c, "1c_mail volume.csv", 
                                     sep = '/'))  

    #B. Adult roster
    rects.2019 <- fread(paste(raw_data_path_2a, "2a_rects_final.csv", sep = '/'),
                   select = c(1:24, 27:29, 31, 242:250, 252:253, 265:286, 
                              292:295))
    rects.2020 <- fread(paste(raw_data_path_1a, "1a_rects_final.csv", sep = '/'),
                        select = c(1:24, 27:29, 31, 242:250, 252:253, 265:286, 
                                   292:295))
    rects.2020.all <- read.csv(paste(raw_data_path_1a, "1a_rects_final.csv", sep = '/'),
                               nrows = 2)
    
#1.2: Since 2000-2019 and 2000-2020 don't match I'll conduct some tests
    
    #A. Compare column names from both data sets
    c.2019 <- sort(colnames(rects.2019))
    c.2020 <- sort(colnames(rects.2020))
    c.names <- data.frame(c.2019, c.2020)
    
    #B. Test to see which variables don't match between 2000-2019 data and 2000-2020 data
    test.2020 <- fread(paste(raw_data_path_1a, "1a_rects_final.csv", sep = '/'), select = c.2019)
    c.test.2020 <- sort(c(colnames(test.2020), rep(NA, 4)), na.last = T)
    c.names.1 <- data.frame(c.2019, c.2020, c.test.2020)
    
    #C. Look at all 2000-2020 columns
    c.2020.all <- data.frame(colnames(rects.2020.all))
    
        ##1. Change of name (I think)
        #    Annual Household Income Level = Household.Income
        #    Census Region = ???
        #    HOWNER. Homeowner Yes or No = ???
        #    State/County FIPS Code = X.CTFIP..COUNTY.FIPS.CODE

#1.2: Light clean
    
    #A. Capitalize column names and rename (2000-2019)
    names(rects.2019) <- toupper(colnames(rects.2019))
    rects.2019 <- rects.2019 %>%
      rename("SAMPLE.NUMBER" = "SAMPLE NUMBER") %>%
      rename("SURVEY.YEAR" = "SURVEY YEAR" )
    
    #B. Capitalize column names and rename (2000-rects.2020)
    names(rects.2020) <- toupper(colnames(rects.2020))
    rects.2020 <- rects.2020 %>%
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





