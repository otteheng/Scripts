# -> Gerhard O
# -> 10/19/2021
#     Update: 1/21/2022

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
source('./library_GSO.R')

# Raw Data
raw_1a_path <- "./WSJ/4-Code and Analysis/Clean Data/1a"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

wsj_raw <- head(read.csv(paste(raw_1a_path, "1a_WSJ--Survey Data January 2022.csv",
                               sep = "/")),-1) # Remove row with average


#------------------------------------------------------------------------#
#                             Step 3: Calculate                          #
#                   Maximum difference between WSJ Monthly Projections   #
#------------------------------------------------------------------------#

#3.1: Pull data from WSJ Survey    
wsj.1 <- wsj_raw
    
    #A. Pull quarterly GDP annualized columns
    wsj.1 <- wsj.1 %>%
      dplyr::select("WSJ.Economic.Survey.January.2022.Name", 
                    starts_with("GDP"))
    
    #B. Collapse into 25th and 75th percentile
    wsj.1 <- wsj.1 %>%
      summarise(perc25.2021_Q4 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Fourth.Quarter.2021, na.rm = T,
                                           c(0.25)),
                perc25.2022_Q1 = quantile(GDP..Quarterly..Annualized.Growth.Rate..First.Quarter.2022, na.rm = T,
                                          c(0.25)),
                perc25.2022_Q2 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Second.Quarter.2022, na.rm = T,
                                          c(0.25)),
                perc25.2022_Q3 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Third.Quarter.2022, na.rm = T,
                                          c(0.25)),
                perc25.2022_Q4 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Fourth.Quarter.2022, na.rm = T,
                                          c(0.25)),
                perc75.2021_Q4 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Fourth.Quarter.2021, na.rm = T,
                                           c(0.75)),
                perc75.2022_Q1 = quantile(GDP..Quarterly..Annualized.Growth.Rate..First.Quarter.2022, na.rm = T,
                                          c(0.75)),
                perc75.2022_Q2 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Second.Quarter.2022, na.rm = T,
                                          c(0.75)),
                perc75.2022_Q3 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Third.Quarter.2022, na.rm = T,
                                          c(0.75)),
                perc75.2022_Q4 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Fourth.Quarter.2022, na.rm = T,
                                          c(0.75))) %>%
      mutate(max.diff.2021_Q4 = perc75.2021_Q4 - perc25.2021_Q4,
             max.diff.2022_Q1 = perc75.2022_Q1 - perc25.2022_Q1,
             max.diff.2022_Q2 = perc75.2022_Q2 - perc25.2022_Q2,
             max.diff.2022_Q3 = perc75.2022_Q3 - perc25.2022_Q3,
             max.diff.2022_Q4 = perc75.2022_Q4 - perc25.2022_Q4) %>%
      dplyr::select(starts_with("max")) %>%
      mutate(max.diff.all = max(c(max.diff.2021_Q4, max.diff.2022_Q1, max.diff.2022_Q2,
                                  max.diff.2022_Q3, max.diff.2022_Q4)))
    
    
    
    