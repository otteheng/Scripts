# -> Gerhard O
# -> 10/28/2021
# -> Data sets: Mintel
# -> Clean data provided by Comperemedia an advertising firm

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
source('./library_GSO.R')

# Data Paths
raw_data_path <- "./_Raw Data/Mail by Sector"
clean_data_path <- "./Mail Floor/Code/Clean Data/4a"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

#1.1: Pull in data provided by Mintel (Comperemedia)

    #A. Mail volume by sector (Sept 2020-2021)
    # mintel_raw <- read.xlsx(paste(raw_data_path, "Mintel_September 2020-September 2021 Mail Volume and Spend by Sector.xlsx", 
    #                               sep = "/"), sheet = "Data", startRow = 12)
    
    #B. Mail volume by sector (Sept 2019-2021) 
    mintel_raw <- read.xlsx(paste(raw_data_path, "Mintel_September 2019-September 2021 Mail Volume and Spend by Sector.xlsx", 
                                  sep = "/"), sheet = "Data", startRow = 12)

#------------------------------------------------------------------------#
#                           Step 2: Clean data                           #
#                                                                        #
#------------------------------------------------------------------------#
    
#2.1: Explore data
    
    #A. Data types
    str(mintel_raw) #imported date as integer
    
    #B. Tables
    table(mintel_raw$Mailing.Type) # Includes statement mailing
    table(mintel_raw$Industry)
    
#2.2: Change data
    
    #A. Convert date (Excel stores dates as number of days from some origin day)
    mintel <- mintel_raw
    mintel <- mintel %>%
      mutate(Date = convertToDate(Month)) %>%
      dplyr::select(-Month)
    
    #B. Create variable just for statement mailing
    statement <- mintel %>%
      filter(Mailing.Type=="Statement Mailing")
    
    
#------------------------------------------------------------------------#
#                             Step 3: Export                             #
#                                                                        #
#------------------------------------------------------------------------#    
    
#3.1: Export
    
    #A. Full data set
    df.new.name <- '4a_mail.by.sector--all data_2019-2021.csv'       
    write_csv(mintel, path = paste(clean_data_path, df.new.name, sep="/"),
              append=FALSE, col_names=TRUE)
    
    #B. With just statement data
    df.new.name <- '4a_mail.by.sector--statement mailing_2019-2021.csv'       
    write_csv(statement, path = paste(clean_data_path, df.new.name, sep="/"),
              append=FALSE, col_names=TRUE)
    
    
    
    