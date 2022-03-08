# -> Gerhard O
# -> 12/08/2020
# -> Data sets: 1a_Census data

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(readxl)
library(gmodels)

# Initial Data Sets
raw_data_path <- 'C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\Census\\1a_Census_Marital'
cross_walk_path <- 'C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\_Misc'
misc_files <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Script\\Misc"

# Clean Data
clean_data_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\Census\\1c_Census_Marital-Crosswalk"

#------------------------------------------------------------------------#
#                           Conduct Data Checks                          #
#                      Census: 2018; Crosswalk: 2018                     #
#------------------------------------------------------------------------#

# Load in Census Marital files
census.file <- '1a_ACSDT5Y2018.B12002_data_with_overlays_2020-11-18T125040.csv'
census18 <- read.csv(paste(raw_data_path, census.file, sep = '\\'), 
                     colClasses=c("ZCTA"="character"))

#---> 
#---> Census Data Checks
#--->

    # Check that ZCTA is character
    str(census18$ZCTA)
    
    # Number of unique ZCTAs
    length(census18$ZCTA) # 33120
    length(unique(census18$ZCTA)) # 33120
    
    # Length of ZCTAs (all 5 characters)
    unique(nchar(census18$ZCTA))

# Load in Crosswalk 2018 files
crosswalk.file <- 'zip_to_zcta_2018.xlsx'
crosswalk18 <- read_excel(paste(cross_walk_path, crosswalk.file, sep = '\\'))

#---> 
#---> Crosswalk Data Checks
#--->

    # Check that ZCTA is character
    str(crosswalk18$ZCTA)
    
    # Number of unique ZCTAs
    length(crosswalk18$ZCTA) # 41160
    length(unique(crosswalk18$ZCTA)) # 33138
    
    # Length of ZCTAs (all 5 characters)
    unique(nchar(crosswalk18$ZCTA))
    
    # Tab of crosswalk types
    unique(crosswalk18$zip_join_type)
    table(crosswalk18$zip_join_type)


    #------------------------------------------------------------------#
    #                        Merge in Crosswalk                        #
    #                Census: 2018; Crosswalk: 2018                     #
    #------------------------------------------------------------------#

    # Use merge function that includes _merge
    source(paste(misc_files, "full_join_track.R", sep = "\\"))
    
    # Merge values together using function above
    merge_cen18_cross18 <- full_join_track(census18, crosswalk18, .merge = T)
    
    # Look at merged values (outputs automatically, reproducing output)
    table(merge_cen18_cross18$.merge)
      #  Joining, by = "ZCTA"
      #  6 Rows ONLY from left data frame
      #  33 Rows ONLY from right data frame
      #  41127 Rows matched

    # Convert to tibble
    merge_cen18_cross18 <- as_tibble(merge_cen18_cross18)
    
    # Look at the Census ZCTAs that did not merge
    # Only ZCTA = 95314, contained any 
    View(filter(merge_cen18_cross18, .merge == 'left_only')) 
    
    # Move columns
    merge_cen18_cross18 <- merge_cen18_cross18 %>% 
      relocate(c(ZIP_CODE, PO_NAME, STATE, .merge ), .after = ZCTA )
    
    

#------------------------------------------------------------------------#
#                           Conduct Data Checks                          #
#                      Census: 2017; Crosswalk: 2018                     #
#------------------------------------------------------------------------#  
    
# Load in Census Marital files
census.file <- '1a_ACSDT5Y2017.B12002_data_with_overlays_2020-11-18T125040.csv'
census17 <- read.csv(paste(raw_data_path, census.file, sep = '\\'), 
                     colClasses=c("ZCTA"="character"))
    
#---> 
#---> Census Data Checks
#--->
    
    # Check that ZCTA is character
    str(census17$ZCTA)
    
    # Number of unique ZCTAs
    length(census17$ZCTA) # 33120
    length(unique(census17$ZCTA)) # 33120
    
    # Length of ZCTAs (all 5 characters)
    unique(nchar(census17$ZCTA))
    


    #------------------------------------------------------------------#
    #                        Merge in Crosswalk                        #
    #                Census: 2017; Crosswalk: 2018                     #
    #------------------------------------------------------------------#

    # Was unable to find Crosswalk for 2017. Will test to see how well Crosswalk
    # for 2018 matches the Census data in 2017.     
    
    # Merge values together using function above
    merge_cen17_cross18 <- full_join_track(census17, crosswalk18, .merge = T)
    
    # Look at merged values (outputs automatically, reproducing output)
    table(merge_cen17_cross18$.merge)
      #  Joining, by = "ZCTA"
      #  6 Rows ONLY from left data frame
      #  33 Rows ONLY from right data frame
      #  41127 Rows matched
    
    # Convert to tibble
    merge_cen17_cross18 <- as_tibble(merge_cen17_cross18)
    
    # Look at the Census ZCTAs that did not merge
    View(filter(merge_cen17_cross18, .merge == 'left_only'))
    
    # Move columns
    merge_cen17_cross18 <- merge_cen17_cross18 %>% 
      relocate(c(ZIP_CODE, PO_NAME, STATE, .merge ), .after = ZCTA )
    
    
    
    
#------------------------------------------------------------------------#
#                           Conduct Data Checks                          #
#                      Census: 2016; Crosswalk: 2016                     #
#------------------------------------------------------------------------#  

# Load in Census Marital files
census.file <- '1a_ACSDT5Y2016.B12002_data_with_overlays_2020-11-18T125040.csv'
census16 <- read.csv(paste(raw_data_path, census.file, sep = '\\'), 
                     colClasses=c("ZCTA"="character"))
    
#---> 
#---> Census Data Checks
#--->
    
    # Check that ZCTA is character
    str(census16$ZCTA)
    
    # Number of unique ZCTAs
    length(census16$ZCTA) # 33120
    length(unique(census16$ZCTA)) # 33120
    
    # Length of ZCTAs (all 5 characters)
    unique(nchar(census16$ZCTA))
    
    # Load in Crosswalk 2016 files
    crosswalk.file <- 'zip_to_zcta_2016.xlsx'
    crosswalk16 <- read_excel(paste(cross_walk_path, crosswalk.file, sep = '\\'))
    
#---> 
#---> Crosswalk Data Checks
#--->
    
    # Rename Var
    names(crosswalk16)[names(crosswalk16) == 'ZCTA_USE'] <- 'ZCTA'
    
    # Check that ZCTA is character
    str(crosswalk16$ZCTA)
    
    # Number of unique ZCTAs
    length(crosswalk16$ZCTA) # 41250
    length(unique(crosswalk16$ZCTA)) # 33144
    
    # Length of ZCTAs (all 5 characters)
    unique(nchar(crosswalk16$ZCTA))
    
    # Tab of crosswalk types
    names(crosswalk16)[names(crosswalk16) == 'ZIPType'] <- 'zip_join_type'
    unique(crosswalk16$zip_join_type)
    table(crosswalk16$zip_join_type)    
    
    #------------------------------------------------------------------#
    #                        Merge in Crosswalk                        #
    #                Census: 2016; Crosswalk: 2016                     #
    #------------------------------------------------------------------#
    
    # Merge values together using function above
    merge_cen16_cross16 <- full_join_track(census16, crosswalk16, .merge = T)
    
    # Look at merged values (outputs automatically, reproducing output)
    table(merge_cen16_cross16$.merge)
    #  Joining, by = "ZCTA"
    # 0 Rows ONLY from left data frame
    # 33 Rows ONLY from right data frame
    # 41217 Rows matched
    
    # Convert to tibble
    merge_cen16_cross16 <- as_tibble(merge_cen16_cross16)
    
    # Move columns
    merge_cen16_cross16 <- merge_cen16_cross16 %>% 
      relocate(c(ZIP, CityName, StateAbbr, .merge ), .after = ZCTA )      
    
    

#------------------------------------------------------------------------#
#                           Conduct Data Checks                          #
#                      Census: 2015; Crosswalk: 2015                     #
#------------------------------------------------------------------------#  
    
# Load in Census Marital files
census.file <- '1a_ACSDT5Y2015.B12002_data_with_overlays_2020-11-18T125040.csv'
census15 <- read.csv(paste(raw_data_path, census.file, sep = '\\'), 
                     colClasses=c("ZCTA"="character"))

#---> 
#---> Census Data Checks
#--->
    
    # Check that ZCTA is character
    str(census15$ZCTA)
    
    # Number of unique ZCTAs
    length(census15$ZCTA) # 33120
    length(unique(census15$ZCTA)) # 33120
    
    # Length of ZCTAs (all 5 characters)
    unique(nchar(census15$ZCTA))
    
    # Load in Crosswalk 2015 files
    crosswalk.file <- 'zip_to_zcta_2015.xlsx'
    crosswalk15 <- read_excel(paste(cross_walk_path, crosswalk.file, sep = '\\'))
    
#---> 
#---> Crosswalk Data Checks
#--->
    
    # Rename Var
    names(crosswalk15)[names(crosswalk15) == 'ZCTA_USE'] <- 'ZCTA'
    
    # Check that ZCTA is character
    str(crosswalk15$ZCTA)
    
    # Number of unique ZCTAs
    length(crosswalk15$ZCTA) # 41250
    length(unique(crosswalk15$ZCTA)) # 33144
    
    # Length of ZCTAs (all 5 characters)
    unique(nchar(crosswalk15$ZCTA))
    
    # Tab of crosswalk types
    names(crosswalk15)[names(crosswalk15) == 'ZIPType'] <- 'zip_join_type'
    unique(crosswalk15$zip_join_type)
    table(crosswalk15$zip_join_type)    
    
    #------------------------------------------------------------------#
    #                        Merge in Crosswalk                        #
    #                Census: 2015; Crosswalk: 2015                     #
    #------------------------------------------------------------------#
    
    # Merge values together using function above
    merge_cen15_cross15 <- full_join_track(census15, crosswalk15, .merge = T)
    
    # Look at merged values (outputs automatically, reproducing output)
    table(merge_cen15_cross15$.merge)
    #  Joining, by = "ZCTA"
    # 0 Rows ONLY from left data frame
    # 33 Rows ONLY from right data frame
    # 41237 Rows matched
    
    # Convert to tibble
    merge_cen15_cross15 <- as_tibble(merge_cen15_cross15)
    
    # Move columns
    merge_cen15_cross15 <- merge_cen15_cross15 %>% 
      relocate(c(ZIP, PO_NAME, STATE, .merge ), .after = ZCTA )  
    

#------------------------------------------------------------------------#
#                           Conduct Data Checks                          #
#                      Census: 2014; Crosswalk: 2015                     #
#------------------------------------------------------------------------#  

# Load in Census Marital files
census.file <- '1a_ACSDT5Y2014.B12002_data_with_overlays_2020-11-18T125040.csv'
census14 <- read.csv(paste(raw_data_path, census.file, sep = '\\'), 
                     colClasses=c("ZCTA"="character"))

#---> 
#---> Census Data Checks
#--->
    
    # Check that ZCTA is character
    str(census14$ZCTA)
    
    # Number of unique ZCTAs
    length(census14$ZCTA) # 33120
    length(unique(census14$ZCTA)) # 33120
    
    # Length of ZCTAs (all 5 characters)
    unique(nchar(census14$ZCTA))
    
    #------------------------------------------------------------------#
    #                        Merge in Crosswalk                        #
    #                Census: 2014; Crosswalk: 2015                     #
    #------------------------------------------------------------------#
    
    # Merge values together using function above
    merge_cen14_cross15 <- full_join_track(census14, crosswalk15, .merge = T)
    
    # Look at merged values (outputs automatically, reproducing output)
    table(merge_cen14_cross15$.merge)
    #  Joining, by = "ZCTA"
    # 0 Rows ONLY from left data frame
    # 33 Rows ONLY from right data frame
    # 41237 Rows matched
    
    # Convert to tibble
    merge_cen14_cross15 <- as_tibble(merge_cen14_cross15)
    
    # Move columns
    merge_cen14_cross15 <- merge_cen14_cross15 %>% 
      relocate(c(ZIP, PO_NAME, STATE, .merge ), .after = ZCTA )  
    
    
#------------------------------------------------------------------------#
#                           Conduct Data Checks                          #
#                      Census: 2013; Crosswalk: 2015                     #
#------------------------------------------------------------------------#  
    
# Load in Census Marital files
census.file <- '1a_ACSDT5Y2013.B12002_data_with_overlays_2020-11-18T125040.csv'
census13 <- read.csv(paste(raw_data_path, census.file, sep = '\\'), 
                     colClasses=c("ZCTA"="character"))

#---> 
#---> Census Data Checks
#--->
    
    # Check that ZCTA is character
    str(census13$ZCTA)
    
    # Number of unique ZCTAs
    length(census13$ZCTA) # 33120
    length(unique(census13$ZCTA)) # 33120
    
    # Length of ZCTAs (all 5 characters)
    unique(nchar(census13$ZCTA))
    
    #------------------------------------------------------------------#
    #                        Merge in Crosswalk                        #
    #                Census: 2013; Crosswalk: 2015                     #
    #------------------------------------------------------------------#
    
    # Merge values together using function above
    merge_cen13_cross15 <- full_join_track(census13, crosswalk15, .merge = T)
    
    # Look at merged values (outputs automatically, reproducing output)
    table(merge_cen13_cross15$.merge)
    #  Joining, by = "ZCTA"
    # 0 Rows ONLY from left data frame
    # 33 Rows ONLY from right data frame
    # 41237 Rows matched
    
    # Convert to tibble
    merge_cen13_cross15 <- as_tibble(merge_cen13_cross15)
    
    # Move columns
    merge_cen13_cross15 <- merge_cen13_cross15 %>% 
      relocate(c(ZIP, PO_NAME, STATE, .merge ), .after = ZCTA )  
    
    
#------------------------------------------------------------------------#
#                           Conduct Data Checks                          #
#                      Census: 2012; Crosswalk: 2015                     #
#------------------------------------------------------------------------#  

# Load in Census Marital files
census.file <- '1a_ACSDT5Y2012.B12002_data_with_overlays_2020-11-18T125040.csv'
census12 <- read.csv(paste(raw_data_path, census.file, sep = '\\'), 
                     colClasses=c("ZCTA"="character"))

#---> 
#---> Census Data Checks
#--->

    # Check that ZCTA is character
    str(census12$ZCTA)
    
    # Number of unique ZCTAs
    length(census12$ZCTA) # 33120
    length(unique(census12$ZCTA)) # 33120
    
    # Length of ZCTAs (all 5 characters)
    unique(nchar(census12$ZCTA))
    
    #------------------------------------------------------------------#
    #                        Merge in Crosswalk                        #
    #                Census: 2012; Crosswalk: 2015                     #
    #------------------------------------------------------------------#
    
    # Merge values together using function above
    merge_cen12_cross15 <- full_join_track(census12, crosswalk15, .merge = T)
    
    # Look at merged values (outputs automatically, reproducing output)
    table(merge_cen12_cross15$.merge)
    #  Joining, by = "ZCTA"
    # 0 Rows ONLY from left data frame
    # 33 Rows ONLY from right data frame
    # 41237 Rows matched
    
    # Convert to tibble
    merge_cen12_cross15 <- as_tibble(merge_cen12_cross15)
    
    # Move columns
    merge_cen12_cross15 <- merge_cen12_cross15 %>% 
      relocate(c(ZIP, PO_NAME, STATE, .merge ), .after = ZCTA )  
    
    
#------------------------------------------------------------------------#
#                           Conduct Data Checks                          #
#                      Census: 2011; Crosswalk: 2015                     #
#------------------------------------------------------------------------#  

# Load in Census Marital files
census.file <- '1a_ACSDT5Y2011.B12002_data_with_overlays_2020-11-18T125040.csv'
census11 <- read.csv(paste(raw_data_path, census.file, sep = '\\'), 
                     colClasses=c("ZCTA"="character"))

#---> 
#---> Census Data Checks
#--->
    
    # Check that ZCTA is character
    str(census11$ZCTA)
    
    # Number of unique ZCTAs
    length(census11$ZCTA) # 33120
    length(unique(census11$ZCTA)) # 33120
    
    # Length of ZCTAs (all 5 characters)
    unique(nchar(census11$ZCTA))
    
    #------------------------------------------------------------------#
    #                        Merge in Crosswalk                        #
    #                Census: 2012; Crosswalk: 2015                     #
    #------------------------------------------------------------------#
    
    # Merge values together using function above
    merge_cen11_cross15 <- full_join_track(census11, crosswalk15, .merge = T)
    
    # Look at merged values (outputs automatically, reproducing output)
    table(merge_cen11_cross15$.merge)
    #  Joining, by = "ZCTA"
    # 0 Rows ONLY from left data frame
    # 33 Rows ONLY from right data frame
    # 41237 Rows matched
    
    # Convert to tibble
    merge_cen11_cross15 <- as_tibble(merge_cen11_cross15)
    
    # Move columns
    merge_cen11_cross15 <- merge_cen11_cross15 %>% 
      relocate(c(ZIP, PO_NAME, STATE, .merge ), .after = ZCTA )   
    
    
#------------------------------------------------------------------------#
#                             Export as CSV                              #
#------------------------------------------------------------------------#  
    
dfs_merged <- grep("merge_",names(.GlobalEnv),value=TRUE)

dfs_merged_ls <- do.call("list",mget(dfs_merged))   

lapply(seq_along(dfs_merged_ls),
       function(i) write.table(
         dfs_merged_ls[[i]], paste(clean_data_path, paste0("1c_", 
                                      paste0(names(dfs_merged_ls)[i], ".csv")), 
                                     sep = "\\"),
                               row.names = FALSE, sep = ","))




