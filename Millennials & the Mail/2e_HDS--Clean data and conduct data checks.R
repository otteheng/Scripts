# -> Gerhard O
# -> 12/29/2020
# -> Data sets: 2d_HDS

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(readxl)
library(data.table)
library(janitor)

# Initial Data Sets
raw_data_path_2d <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2d_HDS--Merge 2c with demographic etc"
fips_path <- "S:\\Projects\\Millennials & Mail\\Code\\_Misc Docs"

# Clean Data
clean_data_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2e_HDS--Cleaned data"

#------------------------------------------------------------------------#
#                   Label Variables for easier reading                   #
#                                                                        #
#------------------------------------------------------------------------#

# Import
vol.rects <- read.csv(file= paste(raw_data_path_2d, "2d_volume-rects.csv", 
                                 sep = '\\'))  

# Per an email with NuStats I will only work with Respondent data
vol.rects2 <- vol.rects[, c(1:10, 34, 36, 37:42, 44:46, 48:57, 70:71)]
      
#--->
#---> Labels: Respondent
#--->  
      
      # Respondent: Age Cohort
      vol.rects2$RESPONDENT..AGE.COHORT = 
        factor(vol.rects2$RESPONDENT..AGE.COHORT,
               levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 38, 98, 99),
               labels = c("DK/RF", "18-24", "18-24", "25-34", "35-44", 
                          "45-54", "55-64", "65-69", "70-74", "75+", 
                          "70+", "DK", "RF"), ordered = TRUE)
      
      # Respondent: Educational Attainment 
      vol.rects2$RESPONDENT..EDUCATIONAL.ATTAINMENT = 
        factor(vol.rects2$RESPONDENT..EDUCATIONAL.ATTAINMENT, 
               levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
               labels = c("DK/RF", "8th grade or less", "Some high school", 
                          "High school graduate", "Some college", 
                          "Technical school graduate", "College graduate", 
                          "Post-graduate work", "DK", "RF"), ordered = TRUE)
      
      # Respondent: Employment Status
      vol.rects2$RESPONDENT..EMPLOYMENT.STATUS..PAST.12.MONTHS. = 
        factor(vol.rects2$RESPONDENT..EMPLOYMENT.STATUS..PAST.12.MONTHS., 
               levels = c(-5, -1, 1, 2, 8, 9, 98, 99),
               labels = c("QUESTION NOT IN SURVEY", "QUESTION SKIPPED",
                          "Yes", "No", "DK", "RF", "DK", "RF"))
      
      # Respondent: Gender           
      vol.rects2$RESPONDENT..GENDER = 
        factor(vol.rects2$RESPONDENT..GENDER, 
               levels = c(-5, 0, 1, 2, 8, 9),
               labels = c("QUESTION NOT IN SURVEY", "?", "Male", "Female", "DF", 
                          "RF"))
      
      # Respondent: Hispanic Origin
      vol.rects2$RESPONDENT..HISPANIC.ORIGIN =
        factor(vol.rects2$RESPONDENT..HISPANIC.ORIGIN, 
               levels = c(-5, -1, 1, 2, 8, 9, 98, 99),
               labels = c("QUESTION NOT IN SURVEY", "QUESTION SKIPPED", 
                          "Yes", "No", "DK", "RF", "DF", "RF"))
      
      # Respondent: Labor Force Status
      vol.rects2$RESPONDENT..LABOR.FORCE.STATUS = 
        factor(vol.rects2$RESPONDENT..LABOR.FORCE.STATUS, 
               levels = c(-1, 1, 2, 3, 4, 5, 8, 9),
               labels = c("DK/RF", "Employed full-time", "Employed part-time",
                          "Retired", "Not employed", "Self employed", "DK", 
                          "RF"))
      
      # Respondent: Marital Status
      vol.rects2$RESPONDENT..MARITAL.STATUS = 
        factor(vol.rects2$RESPONDENT..MARITAL.STATUS,
               levels = c(-1, 1, 2, 3, 4, 5, 6, 8, 9),
               labels = c("DK/RF", "Married", "Living as married", 
                          "Single, never been married", "Divorced", "Separated",
                          "Widowed", "DK", "RF"))
      
      # Respondent: Labor Force Status
      vol.rects2$RESPONDENT..LABOR.FORCE.STATUS = 
        factor(vol.rects2$RESPONDENT..LABOR.FORCE.STATUS,
               levels = c(-1, 1, 2, 3, 4, 5, 6, 8, 9),
               labels = c("QUESTION SKIPPED", "A student", "A homemaker", 
                          "Disabled", "Temporarily laid off", "Retired", 
                          "Other", "DK", "RF"))
      
      # Respondent: Occupation
      vol.rects2$RESPONDENT..OCCUPATION.CLASSIFICATION = 
        factor(vol.rects2$RESPONDENT..OCCUPATION.CLASSIFICATION,
               levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 97, 99),
               labels = c("QUESTION SKIPPED", "Professional or managerial", 
                          "Sales, office, administrative", "Craftsmen/foremen, mechanic",
                          "Service", "Farming, fishing, or forestry", 
                          "Construction", "Production, transportation", 
                          "Other/DK/RF", "Other/DK/RF"))
      
      # Respondent: Race/Ethnicity
      vol.rects2$RESPONDENT..RACE.ETHNICITY = 
        factor(vol.rects2$RESPONDENT..RACE.ETHNICITY,
               levels = c(-1, 1, 2, 3, 4, 5, 7, 8, 9, 30),
               labels = c("DK/RF", "White", "Black", "Asian", 
                          "American Indian or Alaska Native", 
                          "Native Hawaiian or Other Paciric Islander", 
                          "Other", "DK", "RF", "Hispanic"))
      
      # Home Ownership Status
      vol.rects2$Q87..HOME.OWNERSHIP.STATUS = 
        factor(vol.rects2$Q87..HOME.OWNERSHIP.STATUS,
               levels = c(1, 2, 8, 9),
               labels = c("Own", "Rent", "DK", "RF"))
      
      # Home Type
      vol.rects2$Q89..HOME.TYPE = 
        factor(vol.rects2$Q89..HOME.TYPE,
               levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
               labels = c("Single-family detached house", "Apartment/condominium",
                          "Mobile home", "Group Quarters", "Hotel", 
                          "Duplex or townhouse", "Rooming House", "DK", "RF"))
            
      # Income Level
      vol.rects2$ANNUAL.HOUSEHOLD.INCOME.LEVEL =
        factor(vol.rects2$ANNUAL.HOUSEHOLD.INCOME.LEVEL,
               levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 31, 98, 99),
               labels = c("Under $7,000", "$7,000 - $9,999", "$10,000 - $14,999", 
                          "$15,000 - $19,999","$20,000 - $24,999", 
                          "$25,000 - $34,999", "$35,000 - $49,999",
                          "$50,000 - $64,999", "$65,000 - $79,999", 
                          "$80,000 - $99,999", "$100,000 - $119,999", 
                          "$120,000 - $149,999", "$150,000 or more", 
                          "$100,000 or more", "DK", "RF"), ordered = TRUE)
          # Recode 100,000+ incomes
          vol.rects2 <- vol.rects2 %>% mutate(ANNUAL.HOUSEHOLD.INCOME.LEVEL = 
                                recode(ANNUAL.HOUSEHOLD.INCOME.LEVEL,
                                       "$100,000 - $119,999" = "$100,000 or more",
                                       "$120,000 - $149,999" = "$100,000 or more",
                                       "$150,000 or more" = "$100,000 or more"))

      
#------------------------------------------------------------------------#
#                             Drop Missing Values                        #
#------------------------------------------------------------------------#

    # Drop from categorical variables
      vol.rects2[vol.rects2 == 'RF'] <- NA
      vol.rects2[vol.rects2 == 'DK'] <- NA 
      vol.rects2[vol.rects2 == 'DK/RF'] <- NA 
      vol.rects2[vol.rects2 == 'Other/DK/RF'] <- NA  
      vol.rects2[vol.rects2 == 'QUESTION SKIPPED'] <- NA  
      vol.rects2[vol.rects2 == 'QUESTION NOT IN SURVEY'] <- NA  
      vol.rects2[vol.rects2 == '?'] <- NA  

    # Drop from continuous variables
      vol.rects2$Q60..NUMBER.OF.PERSONS.IN.HOUSEHOLD[vol.rects2$Q60..NUMBER.OF.PERSONS.IN.HOUSEHOLD > 40 | 
                                                     vol.rects2$Q60..NUMBER.OF.PERSONS.IN.HOUSEHOLD < 0] <- NA
      vol.rects2$Q61..NUMBER.OF.CHILDREN.IN.HOUSEHOLD.AGE.0.TO.5[vol.rects2$Q61..NUMBER.OF.CHILDREN.IN.HOUSEHOLD.AGE.0.TO.5 > 40 | 
                                                       vol.rects2$Q61..NUMBER.OF.CHILDREN.IN.HOUSEHOLD.AGE.0.TO.5 < 0] <- NA
      vol.rects2$Q61..NUMBER.OF.CHILDREN.IN.HOUSEHOLD.AGE.0.TO.5[vol.rects2$Q62..NUMBER.OF.CHILDREN.IN.HOUSEHOLD.AGE.6.TO.12 > 40 | 
                                                                   vol.rects2$Q62..NUMBER.OF.CHILDREN.IN.HOUSEHOLD.AGE.6.TO.12 < 0] <- NA
      vol.rects2$Q61..NUMBER.OF.CHILDREN.IN.HOUSEHOLD.AGE.0.TO.5[vol.rects2$Q63..NUMBER.OF.CHILDREN.IN.HOUSEHOLD.AGE.13.TO.17 > 40 | 
                                                                   vol.rects2$Q63..NUMBER.OF.CHILDREN.IN.HOUSEHOLD.AGE.13.TO.17 < 0] <- NA

#------------------------------------------------------------------------#
#                         Create New Variables                           #
#------------------------------------------------------------------------#

      # Sum number of children vars into one    
      vol.rects2 <- vol.rects2 %>% mutate(NUMBER.OF.CHILDREN.UNDER.18 =
                 rowSums(select(.,Q61..NUMBER.OF.CHILDREN.IN.HOUSEHOLD.AGE.0.TO.5:Q63..NUMBER.OF.CHILDREN.IN.HOUSEHOLD.AGE.13.TO.17)))
      
      # Create Mail Volume Measure
      vol.rects2 <- vol.rects2 %>% mutate(MAIL.VOLUME.SUM = 
                 rowSums(select(.,c("MAIL.VOLUME.BRMR_PRST", "MAIL.VOLUME.FCMR",
                                    "MAIL.VOLUME.NPMR_PRST"))))

      # Merge in Full County and State names for FIPS code
      fips <- read_excel(paste(fips_path, "FIPS Crosswalk.xlsx", sep = '\\'))  
      vol.rects3 <- left_join(vol.rects2, fips, by = "STATE.COUNTY.FIPS.CODE")
      
          # Split into County and State
          vol.rects3 <- vol.rects3 %>%
            separate(FULL.NAME.COUNTY.STATE, c("COUNTY", "STATE.ABB"), ",")
          
          # Trim white space in State Abbreviations
          vol.rects3$STATE.ABB <- gsub(" ", "", vol.rects3$STATE.ABB)
          
          # Merge in Census regional divisions
          census.regions <- read_excel(paste(fips_path, "US Census Regions and Divisions.xlsx", 
                                       sep = '\\'))
          vol.rects4 <- left_join(vol.rects3, census.regions, by = "STATE.ABB")
      
      # Make census regions into factors
      vol.rects4$STATE.ABB = factor(vol.rects4$STATE.ABB)
      vol.rects4$REGION = factor(vol.rects4$REGION)
      vol.rects4$DIVISION = factor(vol.rects4$DIVISION)
            
#------------------------------------------------------------------------#
#                         Create Dummy Variables                         #
#------------------------------------------------------------------------#
      
      # Age variables 
      vol.rects4$MILLENNIAL.DUMMY <- 
        ifelse(vol.rects$HEAD.OF.HOUSEHOLD..AGE.COHORT == 
                 "18-24" & (vol.rects$SURVEY.YEAR >= 2005 &
                            vol.rects$SURVEY.YEAR <= 2014), 1,0)
      
#------------------------------------------------------------------------#
#             Rename variable for easier read in regression              #
#------------------------------------------------------------------------#      

      # Rename
      vol.new.names <- vol.rects4 %>% 
        rename(ZIP.CODE = ZIP.CODE..HOUSEHOLD., 
               NUM.HOUSEHOLD = Q60..NUMBER.OF.PERSONS.IN.HOUSEHOLD,
               NUM.18PLUS = Q65..NUMBER.OF.ADULTS.AGED.18.YEARS.AND.OLDER.IN.HOUSEHOLD,
               INC.LEVEL = ANNUAL.HOUSEHOLD.INCOME.LEVEL,
               HOME.OWNER = Q87..HOME.OWNERSHIP.STATUS,
               HOME.OWNER.Y.N = HOWNER..HOMEOWNER.YES.OR.NO,
               HOME.TYPE = Q89..HOME.TYPE,
               AGE.COHORT = RESPONDENT..AGE.COHORT,
               GENDER = RESPONDENT..GENDER,
               MARITAL.STATUS = RESPONDENT..MARITAL.STATUS,
               LABOR.FORCE.STATUS = RESPONDENT..LABOR.FORCE.STATUS,
               OCCUPATION.CLASS = RESPONDENT..OCCUPATION.CLASSIFICATION,
               EDUCATION = RESPONDENT..EDUCATIONAL.ATTAINMENT,
               HISPANIC = RESPONDENT..HISPANIC.ORIGIN,
               RACE = RESPONDENT..RACE.ETHNICITY,
               NUM.UNDER18 = NUMBER.OF.CHILDREN.UNDER.18)

# Save to rda file
df.new.name <- '2e_volume-rects-new_names.rda'   
save(vol.new.names, file = paste(clean_data_path, df.new.name, sep="\\"))      

# Save to CSV            
df.new.name <- '2e_volume-rects-new_names.csv'       
write_csv(vol.new.names, path = paste(clean_data_path, df.new.name, sep="\\"),
          append=FALSE, col_names=TRUE)
      