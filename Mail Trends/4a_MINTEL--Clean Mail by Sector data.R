# -> Gerhard O
# -> 10/28/2021
#       Update: 2.2.2022   
# -> Data sets: Mintel
# -> Clean data provided by Comperemedia an advertising firm

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
source('./library_GSO.R')

# Data Paths
raw_data_path <- "./_Raw Data/Mail by Sector"
historic_data_path <- "./Mail Floor/Code/Clean Data/4a"
clean_data_path <- "./Mail Trends/Sources/2022-02/Clean Data"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

#1.1: Pull in data provided by Mintel (Comperemedia)

    #A. Mail volume by sector (Sept 2020-2021)
    # mintel_raw <- read.xlsx(paste(raw_data_path, "Mintel_September 2020-September 2021 Mail Volume and Spend by Sector.xlsx", 
    #                               sep = "/"), sheet = "Data", startRow = 12)
    
    # #B. Mail volume by sector (Sept 2019-2021) 
    # mintel_raw <- read.xlsx(paste(raw_data_path, "Mintel_September 2019-September 2021 Mail Volume and Spend by Sector.xlsx",
    #                               sep = "/"), sheet = "Data", startRow = 12)
    
    #C. Mail volume by sector (Oct 2021) 
    mintel_raw.oct <- read.xlsx(paste(raw_data_path, "October 2021 Mail Volume + Spend.xlsx",
                                  sep = "/"), sheet = "Data", startRow = 12)
    
    #D. Mail volume by sector (Nov 2021) 
    mintel_raw.nov <- read.xlsx(paste(raw_data_path, "November 2021 Mail Volume + Spend.xlsx",
                                  sep = "/"), sheet = "Data", startRow = 12)
    
    #E. Mail volume by sector (Dec 2021) 
    mintel_raw.dec <- read.xlsx(paste(raw_data_path, "December 2021 Mail Volume + Spend.xlsx",
                                      sep = "/"), sheet = "Data", startRow = 12)

#------------------------------------------------------------------------#
#                           Step 2: Clean data                           #
#                                                                        #
#------------------------------------------------------------------------#
    
#2.1: Explore data
    
    #A. Put data frames into list in order to more easily loop over them
    df.list <- list(mintel_raw.oct, mintel_raw.nov, mintel_raw.dec)
    
    #A. Data types
    lapply(df.list, function(x) str(x)) #imported date as integer
    
    #B. Tables
    lapply(df.list, function(x) table(x$Mailing.Type)) # Includes statement mailing
    lapply(df.list, function(x) table(x$Industry)) 
    
#2.2: Change data
    
    #A. New list
    mintel.oct <- mintel_raw.oct
    mintel.nov <- mintel_raw.nov
    mintel.dec <- mintel_raw.dec
    df.list <- list(mintel.oct, mintel.nov, mintel.dec)
    
    #B. Convert date (Excel stores dates as number of days from some origin day)
    df.list = lapply(df.list, function(x) x <- x %>%
                  mutate(Date = convertToDate(Month)) %>% 
                  filter(Date==max(Date)) %>%
                  dplyr::select(-Month))
    
    #C. Append data
    mintel.all <- do.call(rbind, df.list)
    
#------------------------------------------------------------------------#
#                 Step 3: Append to data I cleaned previously            #
#                         Old data: Sept 2019-Sept 2021                  #
#------------------------------------------------------------------------# 

#3.1: Load in old data
        
    #A. Import
    mintel_hist <- read_csv(paste(historic_data_path, "4a_mail.by.sector--all data_2019-2021.csv",
                                      sep = "/"))
    
#3.2: Append current data
    
    #A. Append
    mintel.all <- rbind(mintel_hist, mintel.all)
    
        ##1. Make sure all dates are there
        min(mintel.all$Date)
        max(mintel.all$Date)
        
    #B. Create data frame just for statement mailing 
    mintel.statement <- mintel.all %>%
      filter(Mailing.Type=="Statement Mailing")
    
        ##1. Make sure all dates are there
        min(mintel.statement$Date)
        max(mintel.statement$Date)
        
#------------------------------------------------------------------------#
#                             Step 4: Export                             #
#                                                                        #
#------------------------------------------------------------------------#    
    
#3.1: Export
    
    #A. Full data set
    df.new.name <- '4a_mail.by.sector--all data_SEPT2019-DEC2021.csv'       
    write_csv(mintel.all, path = paste(clean_data_path, df.new.name, sep="/"),
              append=FALSE, col_names=TRUE)
    
    #B. With just statement data
    df.new.name <- '4a_mail.by.sector--statement mailing_SEPT2019-DEC2021.csv'       
    write_csv(mintel.statement, path = paste(clean_data_path, df.new.name, sep="/"),
              append=FALSE, col_names=TRUE)
    
    
    
    