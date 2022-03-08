# -> Gerhard O
# -> 11/10/2021
# -> Data sets: Cleaned mail by sector data (MINTEL)

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
source('./library_GSO.R')
library(forcats)

# Data Paths
clean_data_path <- "./Mail Floor/Code/Clean Data/4a"
graph_path <- "./Mail Floor/Code/Tables & Figures/4b"


#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

#1.1: Pull in data provided by Mintel (Comperemedia)

    #A. Advertising Mail
    all_raw <- read_csv(paste(clean_data_path, "4a_mail.by.sector--all data.csv",
                                    sep = "/"))

#------------------------------------------------------------------------#
#                             Step 2: Clean                              #
#                                                                        #
#------------------------------------------------------------------------# 
    
#2.1: Explore data
    
    #A. Mail types
    t.2.1.A <- as.data.frame(table(all_raw$Mailing.Type))
    
    #B. List of items to exclude
    # Follow Up Mailing (Retention)
    # Statement Mailing
    # Renewal Mailing (Retention)
    
#2.2: Filter out non-advertising mail types
    
    #A. Only ads
    adv <- all_raw %>%
      filter(Mailing.Type!="Follow Up Mailing (Retention)" &
             Mailing.Type!="Statement Mailing" &
             Mailing.Type!="Renewal Mailing (Retention)")
    
    #B. Create new variable(s)
    adv$Mailing.Type2 <- "Advertising"
    
#2.3: Aggregate data
    
    #A. Volume and Spend by month
    adv.agg <- adv %>%
      group_by(Date) %>%
      summarise(volume = sum(Estimated.Mail.Volume),
                spend = sum(Spend))
    
    #B. Volume and Spend by month and industry
    adv.agg.ind <- adv %>%
      group_by(Date, Industry) %>%
      summarise(volume = sum(Estimated.Mail.Volume),
                spend = sum(Spend)) %>%
      arrange(Date, volume)
    
        ##1. Create factor levels explicitly (https://stackoverflow.com/questions/69814998/ordering-a-stacked-bar-graph-by-second-variable-changing-over-time)
        adv.agg.ind <- 
          adv.agg.ind %>% 
          arrange(Date) %>%
          mutate(year_mo = fct_inorder(as.character(Date)))
        
        ##2. Split the new data by month and create different factor levels
        ls_adv.agg.ind <- 
          adv.agg.ind %>%
          split(., .$year_mo) %>%
          map(function(x) {x$Industry <- fct_reorder(x$Industry, x$volume); x})
        
        ##3. Make your geom_col list (geom_col is equivalent to geom_bar(stat= "identity")
        ls_adv.agg.ind_col <- map(ls_adv.agg.ind, function(x){
          geom_col(data = x, mapping = aes(x=as.Date(year_mo), y=volume, fill = Industry),
                   width = 15)
        })
        
    #C. Volume by company (top 5 each month)
    adv.agg.comp <- adv %>%
      group_by(Date, Primary.Company) %>%
      summarise(volume = sum(Estimated.Mail.Volume)) %>%
      arrange(Date, -volume) %>%
      group_by(Date) %>%
      slice(1:5)

#------------------------------------------------------------------------#
#                             Step 3: Graph                              #
#                                                                        #
#------------------------------------------------------------------------#     
    
    
  
    
    
    
    
    
    
    
    
    
      