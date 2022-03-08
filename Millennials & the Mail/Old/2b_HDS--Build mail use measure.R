# -> Gerhard O
# -> 12/15/2020
# -> Data sets: 2a_HDS

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(readxl)

# Initial Data Sets
raw_data_path <- 'C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2a_HDS'
raw_brmr <- '2a_brmr_prst.csv'

# Clean Data
clean_data_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2b_HDS-Mail use measure"

#------------------------------------------------------------------------#
#                       Load Household Diary Study Data                  #
#                                 Data: BRMR                             #
#------------------------------------------------------------------------#

# Load in HDS (BRMR) data
brmr <- read.csv(paste(raw_data_path, raw_brmr, sep = '\\'))

    # Create a vector of columns that are ALL NA
    na_vector <- character(0)
    for (i in 1:ncol(brmr)) {
      if (all(is.na(brmr[i])) == TRUE) {
        print(colnames(brmr[i]))
        na_vector[i] <- colnames(brmr[i])
      }
    }
    na_vector <- na_vector[!is.na(na_vector)] # 27 columns in total
    
    # Remove columns with all NAs
    ncol(brmr) # 563
    not_all_na <- function(x) any(!is.na(x))
    brmr <- brmr %>% select_if(not_all_na)
    ncol(brmr) # 563-27=536    
    
    # Keep columns of interest
    brmr2 <- brmr[, c(1:7, 20, 25:37, 53:59, 100:103, 158, 159, 163:166, 205, 
                      206, 207, 215, 238, 239, 248, 249, 267, 268, 272, 274, 275, 
                      281, 290:311, 325:327)]


#------------------------------------------------------------------------#
#                       Make unique at the ID, Year Level                #
#                                 Data: BRMR2                            #
#------------------------------------------------------------------------#

# Reorder and move columns with "Number of..."
cols_number.of <- brmr2 %>% dplyr:: select(contains(c("Number.of"))) %>%
      colnames() 
brmr2 <- brmr2 %>% relocate(cols_number.of, .after = SURVEY.YEAR)

#--->
#---> Check unique values  
#--->

    # How many unique IDs do we have?
    length(unique(brmr2$SAMPLE.NUMBER)) # 104967
    
    # Number of unique rows overall
    brmr <- "" # Save memory
    length(unique(brmr2)) # 77

    # Unique values ID and Date
    length(unique(c(brmr2$SAMPLE.NUMBER, brmr2$SURVEY.YEAR) )) # 104987
    length(unique(c(brmr2$SAMPLE.NUMBER, brmr2$SURVEY.YEAR,
                    brmr2$X200A..Number.of.packages.received.in.the.last.3.months) )) # 105056
    length(unique(c(brmr2$SAMPLE.NUMBER, brmr2$SURVEY.YEAR,
                    brmr2$Q27C..Number.of.statements.received.in.the.mail) )) # 105033

    # Number of unique IDs by year
    brmr2 %>%
      group_by(SURVEY.YEAR) %>%
      summarise(n_distinct(SAMPLE.NUMBER))
    
    # Number of unique values for number of packages in last 3 months vars by year
    brmr2 %>%
      group_by(SURVEY.YEAR) %>%
      summarise(n_distinct(X200A..Number.of.packages.received.in.the.last.3.months))

    # Number of unique values for number of packages in last 3 months vars by year
    brmr2 %>%
      group_by(SURVEY.YEAR) %>%
      summarise(n_distinct(Q200..Number.of.packages.received.in.the.last.month))

    # Number of unique values for number of packages in last 3 months vars by year
    brmr2 %>%
      group_by(SURVEY.YEAR) %>%
      summarise(n_distinct(Q27..Number.of.bills.received.in.the.mail))

 
        
# Remove duplicates: Want data set unique at the id, year level
    
    # Turn into tibble 
    brmr2_tb <- as_tibble(brmr2)
    brmr3_tb <- brmr2_tb %>% distinct(brmr2$SAMPLE.NUMBER, brmr2$SURVEY.YEAR,
                                      brmr2$X200A..Number.of.packages.received.in.the.last.3.months,
                                      .keep_all = TRUE)
    
    
    # Sort by vector name [z] then [x]
    View( brmr3_tb[with(brmr3_tb, order(SAMPLE.NUMBER, SURVEY.YEAR)),]   )
    
    

#------------------------------------------------------------------------#
#                         Create Labels for Variables                    #
#                                                                        #
#------------------------------------------------------------------------#

    
# Head of Household Gender
    brmr3_tb$HEAD.OF.HOUSEHOLD..GENDER = factor(brmr3_tb$HEAD.OF.HOUSEHOLD..GENDER,
                       levels = c(1, 2, 8, 9),
                       labels = c("Male", "Female", "DK", "RF"))
    
    # Crosstab of Gender and Year
    t <- with(brmr3_tb, table(SURVEY.YEAR , HEAD.OF.HOUSEHOLD..GENDER))
    t <- prop.table(t, margin = 1)
    t  # Consistently 75-80% are Men
    
# Head of Household Age Cohort
    brmr3_tb$HEAD.OF.HOUSEHOLD..Age.Cohort = 
      factor(brmr3_tb$HEAD.OF.HOUSEHOLD..Age.Cohort,
                      levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 38, 98, 99),
                      labels = c("DK/RF", "18-24", "18-24", "25-34", "35-44", 
                                 "45-54", "55-64", "65-69", "70-74", "75+", 
                                 "70+", "DK", "RF"))
    
    # Crosstab of Age Cohort and Year
    t <- with(brmr3_tb, table(SURVEY.YEAR , HEAD.OF.HOUSEHOLD..Age.Cohort))
    t <- prop.table(t, margin = 1)
    t  # Consistently 75-80% are Men
    
    # Write to .txt
    write.table(t, file = paste(clean_data_path, "2b_HDS--Tables (Age Cohort).csv",
                   sep = '\\'), sep = ",", quote = FALSE, row.names = T)

# Head of Household Age Cohort
    brmr3_tb$HEAD.OF.HOUSEHOLD..Age.Cohort = 
      factor(brmr3_tb$HEAD.OF.HOUSEHOLD..Age.Cohort,
             levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 38, 98, 99),
             labels = c("DK/RF", "18-24", "18-24", "25-34", "35-44", 
                        "45-54", "55-64", "65-69", "70-74", "75+", 
                        "70+", "DK", "RF"))

    
#--->
#---> Median/Averages
#--->      

    # Median Age by year
    brmr3_tb %>%
      group_by(SURVEY.YEAR) %>%
      summarise(mean(Q29..Number.of.Bills.Paid.by.Household.per.Month))           