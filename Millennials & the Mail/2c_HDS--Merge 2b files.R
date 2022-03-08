# -> Gerhard O
# -> 12/23/2020 EDIT: 2/2/2021 including mail/packages sent
# -> Data sets: 2b_HDS

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(readxl)

# Initial Data Sets
raw_data_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2b_HDS--Build mail volume by ID year"
misc_files <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Script\\Misc"

# Clean Data
clean_data_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2c_HDS--Merge 2b files"

#------------------------------------------------------------------------#
#                       Merge Aggregate HDS data together                #
#                               Data: 2b_HDS                             #
#------------------------------------------------------------------------#

    # Import data sets
    file.name.csv <- dir(raw_data_path, pattern=".csv")
    csvs_2b = lapply(paste(raw_data_path, file.name.csv, sep = '\\'), read.csv)
    
    # Use merge function that includes _merge
    source(paste(misc_files, "full_join_track.R", sep = "\\"))
    
    #--------------> Merge Data Sets <--------------#
    # Merge Standard Marketing Mail (BRMR) & First Class Mail Received (FCMR)
    merged <- csvs_2b[[1]] %>% full_join_track(csvs_2b[[2]], 
                              by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                              .merge = T) %>% 
      rename(.merge1_2 = .merge) %>%
      
      # Merge in Non-profit Mail Received (NPMR)
      full_join_track(csvs_2b[[4]], 
                      by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                      .merge = T) %>% 
      rename(.merge1_2_4 = .merge) %>%
      
      # Merge in Periodicals Received (PEMR)
      full_join_track(csvs_2b[[5]], 
                      by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                      .merge = T) %>% 
      rename(.merge1_2_4_5 = .merge) %>%
      
      # Merge in Packages Received (PKGR)
      full_join_track(csvs_2b[[6]], 
                      by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                      .merge = T) %>% 
      rename(.merge1_2_4_5_6 = .merge) %>%
      
      # Merge in Unaddressed Mail Received (UAMR)
      full_join_track(csvs_2b[[8]], 
                      by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                      .merge = T) %>% 
      rename(.merge1_2_4_5_6_8 = .merge) %>%
      
      # Merge in First Class Mail Sent (FCMS)
      full_join_track(csvs_2b[[3]], 
                      by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                      .merge = T) %>% 
      rename(.merge1_2_4_5_6_8_3 = .merge) %>%
      
      # Merge in Packages Sent (PGGS)
      full_join_track(csvs_2b[[7]], 
                      by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                      .merge = T) %>% 
      rename(.merge1_2_4_5_6_8_3_7 = .merge) 

    #------------------------------------------------------------------------#
    #                           Convert NAs to Zero                          #
    #------------------------------------------------------------------------#
    
    # This indicates that ID didn't receive X type of mail
    merged[is.na(merged)] <- 0

#------------------------------------------------------------------------#
#                             Export to CSV                              #
#------------------------------------------------------------------------#

# Include merge indicator    
df.new.name <- "2c_mail volume (_merge).csv"
write_csv(merged, path = paste(clean_data_path, df.new.name, sep="\\"),
          append=FALSE, col_names=TRUE)

# Remove merge indicator  
merged2 <- merged %>% select(-contains(".merge"))
df.new.name <- "2c_mail volume.csv"
write_csv(merged2, path = paste(clean_data_path, df.new.name, sep="\\"),
          append=FALSE, col_names=TRUE)