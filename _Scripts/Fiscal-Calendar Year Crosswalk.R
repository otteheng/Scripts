#------------------------------------------------------------------------#
#               Code to Build Fiscal to Calendar Year Crosswalk          #
#------------------------------------------------------------------------#

library(zoo)
library(dplyr)
library(lubridate)

export_path <- "fill in with path"

#1.1: Build out range of years you want to use

    #A. Start and End dates
    start_date <- as.Date("2000-01-01")
    end_date <- as.Date("2021-07-26")

    #B. Data frame
    ym <- seq(as.yearmon(start_date), as.yearmon(end_date), 1/12)
    df <- data.frame(start = pmax(start_date, as.Date(ym)),
               end = pmin(end_date, as.Date(ym, frac = 1)),
               MONTH = month.name[cycle(ym)],
               CALENDAR.YEAR = as.integer(ym),
               stringsAsFactors = FALSE)
    
#1.2: Add variables
    
    #A. Add month as numeric variable
    
        ##1. Function to convert month name to numeric
        mo2num <- function(x) match(tolower(x), tolower(month.name))
        
        ##2. Add month as numeric
        df <- df %>%
            mutate(MONTH.NUM = mo2num(MONTH))
        
    #B. Add quarterly values
    df <- df %>%
        mutate(FISCAL.QUARTER = case_when(
            MONTH.NUM==10 | MONTH.NUM==11 | MONTH.NUM==12 ~ 1,
            MONTH.NUM==1 | MONTH.NUM==2 | MONTH.NUM==3 ~ 2,
            MONTH.NUM==4 | MONTH.NUM==5 | MONTH.NUM==6 ~ 3,
            MONTH.NUM==7 | MONTH.NUM==8 | MONTH.NUM==9 ~ 4
        ),
        CALENDAR.QUARTER = case_when(
            MONTH.NUM==10 | MONTH.NUM==11 | MONTH.NUM==12 ~ 4,
            MONTH.NUM==1 | MONTH.NUM==2 | MONTH.NUM==3 ~ 1,
            MONTH.NUM==4 | MONTH.NUM==5 | MONTH.NUM==6 ~ 2,
            MONTH.NUM==7 | MONTH.NUM==8 | MONTH.NUM==9 ~ 3
        ))
    
    #C. Add fiscal year
    # https://stackoverflow.com/questions/33877714/assigning-dates-to-fiscal-year
    df <- df %>%
        mutate(FISCAL.YEAR = as.integer(as.yearmon(start) - 9/12 + 1))
    
#2.1: Export (Optional)
    
    #A. CSV
    # write.csv(df, paste(export_path, "Fiscal-Calendar Year Crosswalk_2000-2021.csv", 
    #                                    sep = "\\"), row.names = FALSE) 
    
    
    
    
    