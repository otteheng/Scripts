# -> Gerhard O
# -> 10/18/2021
#     Update: 1/21/2022
# -> Data sets: FY2000 Q1 - FY2020 Q4

# ITEMS TO CHANGE WITH EACH NEW QUARTERLY WSJ
#   1. Names of bea_raw, bea_index_raw, wsj_raw to include correct month
#   2. Under step 2-> 2.1-> C -> 2, make sure that the average, median, and percentiles
#       are only for future quarters. Will throw error if current quarter is included.

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
source('./library_GSO.R')

# Raw Data
raw_excel_path <- "./WSJ/4-Code and Analysis/Excel Analysis"
raw_1a_path <- "./WSJ/4-Code and Analysis/Clean Data/1a"

# New Data
new_data_path <- "./WSJ/4-Code and Analysis/Clean Data/2b_Unemployment"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

unemp_raw <- read_xlsx(paste(raw_excel_path, "Unemployment Charts July 12.xlsx",
                                  sep = "/"), skip = 4, col_names = T, n_max = 65, 
                            sheet = "UnEmp Rate July") 

wsj_raw <- head(read.csv(paste(raw_1a_path, "1a_WSJ--Survey Data January 2022.csv",
                               sep = "/")),-1) # Remove row with average

#------------------------------------------------------------------------#
#                           Step 2: Light Clean                          #
#                                                                        #
#------------------------------------------------------------------------#

#2.1: Remove unnecessary columns

    #A. Remove Dec 2021, June 2021, etc. 
    unemp <- unemp_raw %>%
      dplyr::select(`Calendar Year...1`:`75th Percentile`)
    
#2.2: Column changes
    
    #A. Make "WSJ Estimate" columns blank. Will fill in with updated WSJ Survey
    unemp$Median <- NA
    unemp$`25th Percentile` <- NA
    unemp$`75th Percentile` <- NA
    
    #B. Make column names syntax correct
    names(unemp) <- names(unemp) %>%
      make.names()


#------------------------------------------------------------------------#
#                             Step 3: Recreate                           #
#                     UnEmp Rate October using BLS website data          #
#                                 Column M                               #
#------------------------------------------------------------------------#

#3.1: Pull in unemployment info from BLS site: https://www.bls.gov/cps/news.htm
    
    #A. Unemployment rates (%)
    # July 2021 = 5.4
    # August 2021 = 5.2
    # September 2021 = 4.8
    # October 2021 = 4.6
    # November 2021 = 4.2
    # December 2021 = 3.9
    
#3.2: Append to data frame
    
    #A. Add unemployment rates
    unemp <- unemp %>%
      mutate(BLS = case_when(
        Period...10 == "M07" & Calendar.Year...8==2021 ~ 5.4,
        Period...10 == "M08" & Calendar.Year...8==2021 ~ 5.2,
        Period...10 == "M09" & Calendar.Year...8==2021 ~ 4.8,
        Period...10 == "M10" & Calendar.Year...8==2021 ~ 4.6,
        Period...10 == "M11" & Calendar.Year...8==2021 ~ 4.3,
        Period...10 == "M12" & Calendar.Year...8==2021 ~ 3.9,
        TRUE ~ as.numeric(BLS)
      ))

#------------------------------------------------------------------------#
#                             Step 3: Recreate                           #
#                     UnEmp Rate October using BLS website data          #
#                                 Column N-P                             #
#------------------------------------------------------------------------#

#3.1: Pull data from WSJ Survey    
wsj.1 <- wsj_raw
    
    #A. Pull Unemployment rate columns
    wsj.1 <- wsj.1 %>%
      dplyr::select("WSJ.Economic.Survey.January.2022.Name", 
                    starts_with("Unemployment.Rate"))
    
    #B. Collapse into Average, Median, 25th and 75th percentile
    wsj.1 <- wsj.1 %>%
        summarise(median.2022_M06 = median(Unemployment.Rate.June.2022, na.rm = T),
                  median.2022_M12 = median(Unemployment.Rate.Dec.2022, na.rm = T),
                  median.2023_M06 = median(Unemployment.Rate.June.2023, na.rm = T),
                  median.2023_M12 = median(Unemployment.Rate.Dec.2023, na.rm = T),
                  median.2024_M06 = median(Unemployment.Rate.June.2024, na.rm = T),
                  median.2024_M12 = median(Unemployment.Rate.Dec.2024, na.rm = T),
                  
                  perc25.2022_M06 = quantile(Unemployment.Rate.June.2022, na.rm = T,
                                           c(0.25)),
                  perc25.2022_M12 = quantile(Unemployment.Rate.Dec.2022, na.rm = T,
                                           c(0.25)),
                  perc25.2023_M06 = quantile(Unemployment.Rate.June.2023, na.rm = T,
                                           c(0.25)),
                  perc25.2023_M12 = quantile(Unemployment.Rate.Dec.2023, na.rm = T,
                                           c(0.25)),
                  perc25.2024_M06 = quantile(Unemployment.Rate.June.2024, na.rm = T,
                                           c(0.25)),
                  perc25.2024_M12 = quantile(Unemployment.Rate.Dec.2024, na.rm = T,
                                           c(0.25)),
                  
                  perc75.2022_M06 = quantile(Unemployment.Rate.June.2022, na.rm = T,
                                           c(0.75)),
                  perc75.2022_M12 = quantile(Unemployment.Rate.Dec.2022, na.rm = T,
                                           c(0.75)),
                  perc75.2023_M06 = quantile(Unemployment.Rate.June.2023, na.rm = T,
                                           c(0.75)),
                  perc75.2023_M12 = quantile(Unemployment.Rate.Dec.2023, na.rm = T,
                                           c(0.75)),
                  perc75.2024_M06 = quantile(Unemployment.Rate.June.2024, na.rm = T,
                                           c(0.75)),
                  perc75.2024_M12 = quantile(Unemployment.Rate.Dec.2024, na.rm = T,
                                           c(0.75)))
    
    #C. Transform data for merge
    
        ##1. Transpose
        wsj.1 <- as.data.frame(t(wsj.1))
        
        ##2. Make index into column
        wsj.1 <- wsj.1 %>% 
          rownames_to_column("formula")   
                
        ##3. Separate column into formula and year/month
        wsj.1 <- wsj.1 %>%
          separate(formula, c("Calendar.Year...8", "Period...10"), sep = "_")
        wsj.1 <- wsj.1 %>%
          separate(Calendar.Year...8, c("formula", "Calendar.Year...8"), sep = "\\.")
        
        ##4. Different data frame for average, median, and 25th, 75th percentile
        wsj.1.med <- wsj.1 %>% 
          filter(formula=="median") %>%
          dplyr::rename(Median = V1) %>%
          dplyr::select(-formula)
        wsj.1.perc.25 <- wsj.1 %>%
          filter(formula=="perc25") %>%
          dplyr::rename(X25th.Percentile = V1) %>%
          dplyr::select(-formula)
        wsj.1.perc.75 <- wsj.1 %>%
          filter(formula=="perc75") %>%
          dplyr::rename(X75th.Percentile = V1) %>%
          dplyr::select(-formula)
        
        ##5. Change column type to match unemployment
        wsj.1.med$Calendar.Year...8 <- as.numeric(wsj.1.med$Calendar.Year...8)
        wsj.1.perc.25$Calendar.Year...8 <- as.numeric(wsj.1.perc.25$Calendar.Year...8)
        wsj.1.perc.75$Calendar.Year...8 <- as.numeric(wsj.1.perc.75$Calendar.Year...8)
        
#3.2: Merge with unemployment 
        
    #A. Median
    unemp <- left_join(unemp, wsj.1.med, by = c("Calendar.Year...8", "Period...10"))
    
    #B. 25th Percentile
    unemp <- left_join(unemp, wsj.1.perc.25, by = c("Calendar.Year...8", "Period...10"))
    
    #C. 75th Percentile
    unemp <- left_join(unemp, wsj.1.perc.75, by = c("Calendar.Year...8", "Period...10"))
    
#3.2: Clean up merged columns
    
    #A. Select new columns
    unemp <- unemp %>%
      dplyr::select(!ends_with(".x"))
    
#------------------------------------------------------------------------#
#                             Step 4: Export                             #
#                                                                        #
#------------------------------------------------------------------------#    

#4.1: Export    
        
    #A. Export
    write.xlsx(unemp, paste(new_data_path, "2b_UnEmp Rate--Unemployment Sheet_January 22.xlsx",
                                 sep = "/"), sheetName = "UnEmp Rate January 22",
               zoom = 90, startRow = 6, startCol = 3, colNames = FALSE, overwrite = FALSE)    
    
#**Note: Copy over the column headers. Easier than coding it.  
#*
#*   Will need to copy the sheet to file with charts on it   
    
    
    
    
    