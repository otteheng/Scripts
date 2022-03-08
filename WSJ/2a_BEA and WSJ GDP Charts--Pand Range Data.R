# -> Gerhard O
# -> 9/14/2021
#     Update: 1/21/2022

# ITEMS TO CHANGE WITH EACH NEW QUARTERLY WSJ
#   1. Names of bea_raw, bea_index_raw, wsj_raw to include correct month
#   2. Update CURRENT quarter if need in: step 2->2.1->B->1
#                                         step 2->2.3->A->1
#                                         step 3->3.3->A
#                                         step 4->4.3->A
#                                         step 5->5.3->A
#   3. Under: step 2->2.1->C->1, change the first selected variable name to 
#       match the name of WSJ column with survey names. 
#   4a. Under: step 2->2.1->C->2, make sure that the average, median, and percentiles
#       are only for future quarters. Will throw error if current quarter is included.
#   4b. Additionally, under step 2->2.1->C->2, make sure to include ALL FUTURE
#       quarters. WSJ updates their projections periodically. 

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
source('./library_GSO.R')

# Raw Data
raw_excel_path <- "./WSJ/4-Code and Analysis/Excel Analysis"
raw_1a_path <- "./WSJ/4-Code and Analysis/Clean Data/1a"

# New Data
new_data_path <- "./WSJ/4-Code and Analysis/Clean Data/2a_BEA and WSJ GDP"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

pand_range_raw <- read_xlsx(paste(new_data_path, "2a_Pand Range--BEA and WSJ GDP_October 21.xlsx",
                           sep = "/"), skip = 5, col_names = T, n_max = 19, 
                        sheet = "Pand Range Data July 21") 

bea_raw <- read.csv(paste(raw_1a_path, "1a_BEA--GDP_2006Q1-2021Q3 (Jan 22).csv", 
                          sep = "/"))

bea_index_raw <- read.csv(paste(raw_1a_path, "1a_BEA--GDP Indexes_2006Q1-2021Q3 (Jan 22).csv", 
                                sep = "/"))

wsj_raw <- head(read.csv(paste(raw_1a_path, "1a_WSJ--Survey Data January 2022.csv",
                           sep = "/")),-1) # Remove row with average

#------------------------------------------------------------------------#
#                             Step 2: Recreate                           #
#                 Pand Range Data Sheet Using BEA and WSJ Data           #
#                               COLUMNS B-F                              #
#------------------------------------------------------------------------#

#2.1: Add "Annualized Change from Previous Quarter Average" (COLUMN C)

    #A. Create new ~blank data set
    pand_range <- pand_range_raw %>% dplyr::select(`Simple Change from Preceding Quarter`)
    
    #B. Clean up "Annualized Change from Previous Quarter Average" from BEA
    bea <- bea_raw
    
        ##1. Only keep data from 2017Q4 to 2021Q3 (most recent)
        bea <- bea %>% dplyr::select(Items, X2017.Q4:X2021.Q3)
        
        ##2. Only need GDP
        bea <- bea[1,]
        
        ##3. Transpose
        bea <- as.data.frame(t(bea))
        
        ##4. Make index into column
        bea <- bea %>% row_to_names(row_number = 1) %>%
          rownames_to_column("Simple Change from Preceding Quarter")
        
        ##5. Clean up year/quarter column
        bea <- bea %>%
          mutate(`Simple Change from Preceding Quarter` = 
                   sub("X", "", `Simple Change from Preceding Quarter`)) %>%
          mutate(`Simple Change from Preceding Quarter` = 
                   sub("\\.", " ", `Simple Change from Preceding Quarter`))
        
        ##6. Convert to numeric
        bea$`Annualized Change from Previous Quarter - Average` <- 
          as.numeric(bea$`Gross domestic product`)
        bea <- bea %>%
          dplyr::select(`Simple Change from Preceding Quarter`,
                        `Annualized Change from Previous Quarter - Average`)
        # bea <- bea %>%
        #   mutate(test = percent(`Gross domestic product`,accuracy=0.1, scale = 1))
        
    #C. Clean and Append predictions from WSJ survey to BEA. 
    #   MAKE SURE ALL PREDICTED QUARTERS ARE INCLUDED in step 2->2.1->C->2
    wsj.1 <- wsj_raw
        
        ##1. Pull quarterly GDP annualized columns
        wsj.1 <- wsj.1 %>%
          dplyr::select("WSJ.Economic.Survey.January.2022.Name", 
                        contains("Annualized.Growth.Rate"))
        
        ##2. Collapse into Average, Median, 25th and 75th percentile
        wsj.1 <- wsj.1 %>%
          summarise(average_2021.Q4 = mean(GDP..Quarterly..Annualized.Growth.Rate..Fourth.Quarter.2021, na.rm = T),
                    average_2022.Q1 = mean(GDP..Quarterly..Annualized.Growth.Rate..First.Quarter.2022, na.rm = T),
                    average_2022.Q2 = mean(GDP..Quarterly..Annualized.Growth.Rate..Second.Quarter.2022, na.rm = T),
                    average_2022.Q3 = mean(GDP..Quarterly..Annualized.Growth.Rate..Third.Quarter.2022, na.rm = T),
                    average_2022.Q4 = mean(GDP..Quarterly..Annualized.Growth.Rate..Fourth.Quarter.2022, na.rm = T),
                    
                    median_2021.Q4 = median(GDP..Quarterly..Annualized.Growth.Rate..Fourth.Quarter.2021, na.rm = T),
                    median_2022.Q1 = median(GDP..Quarterly..Annualized.Growth.Rate..First.Quarter.2022, na.rm = T),
                    median_2022.Q2 = median(GDP..Quarterly..Annualized.Growth.Rate..Second.Quarter.2022, na.rm = T),
                    median_2022.Q3 = median(GDP..Quarterly..Annualized.Growth.Rate..Third.Quarter.2022, na.rm = T),
                    median_2022.Q4 = median(GDP..Quarterly..Annualized.Growth.Rate..Fourth.Quarter.2022, na.rm = T),
                    
                    perc.25_2021.Q4 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Fourth.Quarter.2021, 
                                               c(0.25), na.rm = T),
                    perc.25_2022.Q1 = quantile(GDP..Quarterly..Annualized.Growth.Rate..First.Quarter.2022, 
                                               c(0.25), na.rm = T),
                    perc.25_2022.Q2 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Second.Quarter.2022, 
                                               c(0.25), na.rm = T),
                    perc.25_2022.Q3 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Third.Quarter.2022, 
                                               c(0.25), na.rm = T),
                    perc.25_2022.Q4 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Fourth.Quarter.2022, 
                                               c(0.25), na.rm = T),
                    
                    perc.75_2021.Q4 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Fourth.Quarter.2021, 
                                               c(0.75), na.rm = T),
                    perc.75_2022.Q1 = quantile(GDP..Quarterly..Annualized.Growth.Rate..First.Quarter.2022, 
                                               c(0.75), na.rm = T),
                    perc.75_2022.Q2 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Second.Quarter.2022, 
                                               c(0.75), na.rm = T),
                    perc.75_2022.Q3 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Third.Quarter.2022, 
                                               c(0.75), na.rm = T),
                    perc.75_2022.Q4 = quantile(GDP..Quarterly..Annualized.Growth.Rate..Fourth.Quarter.2022, 
                                               c(0.75), na.rm = T))
        
        
        ##3. Transpose
        wsj.1 <- as.data.frame(t(wsj.1))
        
        ##4. Make index into column
        wsj.1 <- wsj.1 %>% 
          rownames_to_column("formula")    
        
        ##5. Separate column into formula and year/quarter
        wsj.1 <- wsj.1 %>%
          separate(formula, c("formula", "Simple Change from Preceding Quarter"),
                   sep = "_")
        
        ##6. Different data frame for average, median, and 25th, 75th percentile
        wsj.1.avg <- wsj.1 %>% 
          filter(formula=="average") %>% 
          mutate(`Simple Change from Preceding Quarter` = 
                   sub("\\.", " ", `Simple Change from Preceding Quarter`))
        
        ##7. Trim down average to prepare for appending with BEA
        wsj.2.avg <- wsj.1.avg %>%
          rename("Annualized Change from Previous Quarter - Average" = "V1") %>%
          dplyr::select(!formula)
        
        ##8. Append WSJ Survey predictions to BEA data
        bea.wsj_avg <- rbind(bea, wsj.2.avg) 
        
    #D. Merge with pand_range
    pand_range <- right_join(pand_range, bea.wsj_avg, by = "Simple Change from Preceding Quarter")
    
        ##1. Round "Annualized Change from Previous Quarter - Average" to match excel
        pand_range <- pand_range %>%
          mutate(`Annualized Change from Previous Quarter - Average` = 
                  round(`Annualized Change from Previous Quarter - Average`, digits = 1) )
    
    
#2.2: Add "Simple GDP Average" (COLUMN D)
#     Excel formula::(=(1+C13)^(1/4)-1))
    
    #A. Add column using Renee's formula
    pand_range <- pand_range %>%
      mutate(`Simple GDP - Average` = (1+(`Annualized Change from Previous Quarter - Average`/100) )^(1/4)-1 ) %>%
      mutate(`Simple GDP - Average` = round(`Simple GDP - Average`*100, 
                                            digits = 1))
    
#2.3: Add "Real GDP Index Index True Average" (COLUMN E)
    
    #A. Clean up "Real GDP Index True Average" from BEA Index
    bea_index <- bea_index_raw

        ##1. Only keep data from 2017Q4 to 2021Q2 (most recent)
        bea_index <- bea_index %>% dplyr::select(Items, X2017.Q4:X2021.Q3)
        
        ##2. Only need GDP Index
        bea_index <- bea_index[1,]
        
        ##3. Transpose
        bea_index <- as.data.frame(t(bea_index))
        
        ##4. Make index into column
        bea_index <- bea_index %>% row_to_names(row_number = 1) %>%
          rownames_to_column("Simple Change from Preceding Quarter")
        
        ##5. Clean up year/quarter column
        bea_index <- bea_index %>%
          mutate(`Simple Change from Preceding Quarter` = 
                   sub("X", "", `Simple Change from Preceding Quarter`)) %>%
          mutate(`Simple Change from Preceding Quarter` = 
                   sub("\\.", " ", `Simple Change from Preceding Quarter`))
        
        ##6. Convert to numeric
        bea_index$`Real GDP Index Index True Average` <- 
          as.numeric(bea_index$`Gross domestic product`)
        bea_index <- bea_index %>%
          dplyr::select(`Simple Change from Preceding Quarter`,
                        `Real GDP Index Index True Average`)

    #C. Merge left with pand_range
    pand_range <- left_join(pand_range, bea_index, by = "Simple Change from Preceding Quarter")
    
    #D. Fill in predicted values that are currently NA b/c they were not in BEA Index
    #   Excel formula::(=E22*(1+$D23))
    for (i in 1:nrow(pand_range)) {
      if (is.na(pand_range$`Real GDP Index Index True Average`[i]==T)) {
        lag.var <- pand_range$`Real GDP Index Index True Average`[i-1]
        pand_range$`Real GDP Index Index True Average`[i] <- 
          lag.var*(1+(pand_range$`Simple GDP - Average`[i]/100) )
      }
    }
    
    #E. Round
    pand_range <- pand_range %>%
      mutate(`Real GDP Index Index True Average` = round(`Real GDP Index Index True Average`, 
                                                 digits = 3))

    
#2.4: Add "Index 2019 Q4=100 Average" (COLUMN F)
#     Excel formula::(=E15/$E$15*100)
        
    #A. Add column using formula
    pand_range <- pand_range %>%
      mutate(`Index 2019 Q4=100 Average` = pand_range$`Real GDP Index Index True Average`/
               as.numeric(pand_range[pand_range$`Simple Change from Preceding Quarter`=="2019 Q4", 
                          c("Real GDP Index Index True Average")]) ) %>%
      mutate(`Index 2019 Q4=100 Average` = round(`Index 2019 Q4=100 Average`*100, 
                                            digits = 1))
    
#2.5: Add blank column (COLUMN G)
    
    #A. Blank column
    pand_range$"Blank.2" <- ""


#------------------------------------------------------------------------#
#                             Step 3: Recreate                           #
#                 Pand Range Data Sheet Using BEA and WSJ Data           #
#                               COLUMNS H-K                              #
#------------------------------------------------------------------------#   
    
#3.1: Add "Annualized Change from Previous Quarter - Median" (COLUMN H)
    
    #A. Add from WSJ Survey
        
        ##1. Different data frame for average, median, and 25th, 75th percentile
        wsj.1.median <- wsj.1 %>% 
          filter(formula=="median") %>% 
          mutate(`Simple Change from Preceding Quarter` = 
                   sub("\\.", " ", `Simple Change from Preceding Quarter`))
        
        ##2. Trim down average to prepare for appending with BEA
        wsj.2.median <- wsj.1.median %>%
          rename("Annualized Change from Previous Quarter - Median" = "V1") %>%
          dplyr::select(!formula) %>%
          mutate(`Annualized Change from Previous Quarter - Median` = 
                   round(`Annualized Change from Previous Quarter - Median`, 
                         digits = 1))
        
    #B. Merge
    pand_range <- left_join(pand_range, wsj.2.median, by = "Simple Change from Preceding Quarter")
        
#3.2: Add "Simple GDP - Median" (COLUMN I)
#     Excel Formula::(=(1+H21)^(1/4)-1))
    
    #A. Add column using Renee's formula
    pand_range <- pand_range %>%
      mutate(`Simple GDP - Median` = 
               (1+(`Annualized Change from Previous Quarter - Median`/100) )^(1/4)-1 ) %>%
      mutate(`Simple GDP - Median` = round(`Simple GDP - Median`*100, 
                                            digits = 1))
    
#3.3: Add "Real GDP Index Median" (COLUMN J)
#     Excel Formula::(=J20*(1+I21))
    
    #A. Use latest KNOWN Year/Quarter "Real GDP Index Index True Average" as base
    pand_range <- pand_range %>%
      mutate(`Real GDP Index Median` = 
               case_when(`Simple Change from Preceding Quarter`=="2021 Q3" ~ 
                           `Real GDP Index Index True Average`))
    
    #B. Fill in remaining values with Excel formula::=J20*(1+I21)
    for (i in 2:nrow(pand_range)) {
      print(i)
      if (is.na(pand_range$`Real GDP Index Median`[i-1])==F ) {
        print("TRUEE")
        lag.var <- pand_range$`Real GDP Index Median`[i-1]
        pand_range$`Real GDP Index Median`[i] <- 
          lag.var*(1+(pand_range$`Simple GDP - Median`[i]/100) )
      }
    }

    #C. Round
    pand_range <- pand_range %>%
      mutate(`Real GDP Index Median` = round(`Real GDP Index Median`, 
                                                         digits = 3))
    
#3.4: Add "Median" (COLUMN K)
#     Excel formula::(=J20/$E$15*100)
    
    #A. Add median using formula
    pand_range <- pand_range %>%
      mutate(Median = pand_range$`Real GDP Index Median`/
               as.numeric(pand_range[pand_range$`Simple Change from Preceding Quarter`=="2019 Q4", 
                                     c("Real GDP Index Index True Average")]) ) %>%
      mutate(Median = round(Median*100, 1))
    
#3.5: Add blank column (COLUMN L)
    
    #A. Blank column
    pand_range$"Blank.3" <- ""
    
    
#------------------------------------------------------------------------#
#                             Step 4: Recreate                           #
#                 Pand Range Data Sheet Using BEA and WSJ Data           #
#                               COLUMNS M-P                              #
#------------------------------------------------------------------------# 
    
#4.1: Add "Annualized Change from Previous Quarter - 25th Percentile" (COLUMN M)
    
    #A. Add from WSJ Survey
    
        ##1. Different data frame for average, median, and 25th, 75th percentile
        wsj.1.perc.25 <- wsj.1 %>% 
          filter(formula=="perc.25") %>% 
          mutate(`Simple Change from Preceding Quarter` = 
                   sub("\\.", " ", `Simple Change from Preceding Quarter`))
        
        ##2. Trim down average to prepare for appending with BEA
        wsj.2.perc.25 <- wsj.1.perc.25 %>%
          rename("Annualized Change from Previous Quarter - 25th Percentile" = "V1") %>%
          dplyr::select(!formula) %>%
          mutate(`Annualized Change from Previous Quarter - 25th Percentile` = 
                   round(`Annualized Change from Previous Quarter - 25th Percentile`, 
                         digits = 1)) 
        
    #B. Merge
    pand_range <- left_join(pand_range, wsj.2.perc.25, by = "Simple Change from Preceding Quarter")
    
#4.2: Add "Simple GDP - Median" (COLUMN N)
#     Excel Formula::(=(1+H21)^(1/4)-1))
    
    #A. Add column using Renee's formula
    pand_range <- pand_range %>%
      mutate(`Simple GDP - 25th Percentile` = 
               (1+(`Annualized Change from Previous Quarter - 25th Percentile`/100) )^(1/4)-1 ) %>%
      mutate(`Simple GDP - 25th Percentile` = round(`Simple GDP - 25th Percentile`*100, 
                                           digits = 1))

#4.3: Add "Real GDP Index 25th Percentile" (COLUMN O) 
#     Excel Formula::(=O20*(1+N21))
    
    #A. Use latest KNOWN Year/Quarter "Real GDP Index Index True Average" as base
    pand_range <- pand_range %>%
      mutate(`Real GDP Index 25th Percentile` = 
               case_when(`Simple Change from Preceding Quarter`=="2021 Q3" ~ 
                           `Real GDP Index Index True Average`))
    
    #B. Fill in remaining values with Excel formula::=J20*(1+I21)
    for (i in 2:nrow(pand_range)) {
      print(i)
      if (is.na(pand_range$`Real GDP Index 25th Percentile`[i-1])==F ) {
        print("TRUEE")
        lag.var <- pand_range$`Real GDP Index 25th Percentile`[i-1]
        pand_range$`Real GDP Index 25th Percentile`[i] <- 
          lag.var*(1+(pand_range$`Simple GDP - 25th Percentile`[i]/100) )
      }
    }
    
    #C. Round
    pand_range <- pand_range %>%
      mutate(`Real GDP Index 25th Percentile` = round(`Real GDP Index 25th Percentile`, 
                                             digits = 3))    
    
#4.4: Add "25th Percentile" (COLUMN P)
#     Excel Formula::(=O20/$E$15*100)
    
    #A. Add 25th Percentile using formula
    pand_range <- pand_range %>%
      mutate(`25th Percentile` = pand_range$`Real GDP Index 25th Percentile`/
               as.numeric(pand_range[pand_range$`Simple Change from Preceding Quarter`=="2019 Q4", 
                                     c("Real GDP Index Index True Average")]) ) %>%
      mutate(`25th Percentile` = round(`25th Percentile`*100, 1))
    
#4.5: Add blank column (COLUMN L)
    
    #A. Blank column
    pand_range$"Blank.4" <- ""
    
    
#------------------------------------------------------------------------#
#                             Step 5: Recreate                           #
#                 Pand Range Data Sheet Using BEA and WSJ Data           #
#                               COLUMNS R-U                              #
#------------------------------------------------------------------------# 
    
#5.1: Add "Annualized Change from Previous Quarter - 25th Percentile" (COLUMN M)
    
    #A. Add from WSJ Survey
    
    ##1. Different data frame for average, median, and 25th, 75th percentile
    wsj.1.perc.75 <- wsj.1 %>% 
      filter(formula=="perc.75") %>% 
      mutate(`Simple Change from Preceding Quarter` = 
               sub("\\.", " ", `Simple Change from Preceding Quarter`))
    
    ##2. Trim down average to prepare for appending with BEA
    wsj.2.perc.75 <- wsj.1.perc.75 %>%
      rename("Annualized Change from Previous Quarter - 75th Percentile" = "V1") %>%
      dplyr::select(!formula) %>%
      mutate(`Annualized Change from Previous Quarter - 75th Percentile` = 
               round(`Annualized Change from Previous Quarter - 75th Percentile`, 
                     digits = 1)) 
    
    #B. Merge
    pand_range <- left_join(pand_range, wsj.2.perc.75, by = "Simple Change from Preceding Quarter")
    
#5.2: Add "Simple GDP - Median" (COLUMN N)
#     Excel Formula::(=(1+H21)^(1/4)-1))
    
    #A. Add column using Renee's formula
    pand_range <- pand_range %>%
      mutate(`Simple GDP - 75th Percentile` = 
               (1+(`Annualized Change from Previous Quarter - 75th Percentile`/100) )^(1/4)-1 ) %>%
      mutate(`Simple GDP - 75th Percentile` = round(`Simple GDP - 75th Percentile`*100, 
                                                    digits = 1))
    
#5.3: Add "Real GDP Index 75th Percentile" (COLUMN O) 
#     Excel Formula::(=O20*(1+N21))
    
    #A. Use latest KNOWN Year/Quarter "Real GDP Index Index True Average" as base
    pand_range <- pand_range %>%
      mutate(`Real GDP Index 75th Percentile` = 
               case_when(`Simple Change from Preceding Quarter`=="2021 Q3" ~ 
                           `Real GDP Index Index True Average`))
    
    #B. Fill in remaining values with Excel formula::=J20*(1+I21)
    for (i in 2:nrow(pand_range)) {
      print(i)
      if (is.na(pand_range$`Real GDP Index 75th Percentile`[i-1])==F ) {
        print("TRUEE")
        lag.var <- pand_range$`Real GDP Index 75th Percentile`[i-1]
        pand_range$`Real GDP Index 75th Percentile`[i] <- 
          lag.var*(1+(pand_range$`Simple GDP - 75th Percentile`[i]/100) )
      }
    }
    
    #C. Round
    pand_range <- pand_range %>%
      mutate(`Real GDP Index 75th Percentile` = round(`Real GDP Index 75th Percentile`, 
                                                      digits = 3))    
    
#5.4: Add "75th Percentile" (COLUMN P)
#     Excel Formula::(=O20/$E$15*100)
    
    #A. Add 75th Percentile using formula
    pand_range <- pand_range %>%
      mutate(`75th Percentile` = pand_range$`Real GDP Index 75th Percentile`/
               as.numeric(pand_range[pand_range$`Simple Change from Preceding Quarter`=="2019 Q4", 
                                     c("Real GDP Index Index True Average")]) ) %>%
      mutate(`75th Percentile` = round(`75th Percentile`*100, 1))    
    
    
#------------------------------------------------------------------------#
#                             Step 6: Export                             #
#                                                                        #
#------------------------------------------------------------------------#     
    
#6.1: Make Cosmetic Changes before export
    
    #A. Add percent symbol to COLUMNS C, D, H, I, M, N
    pand_range <- pand_range %>%
      mutate(`Annualized Change from Previous Quarter - Average` = 
               percent(`Annualized Change from Previous Quarter - Average`,
                       accuracy=0.1, scale = 1),
             `Simple GDP - Average` = percent(`Simple GDP - Average`,accuracy=0.1, 
                                             scale = 1),
             `Annualized Change from Previous Quarter - Median` = 
               percent(`Annualized Change from Previous Quarter - Median`,
                       accuracy=0.1, scale = 1),
             `Simple GDP - Median` = percent(`Simple GDP - Median`,accuracy=0.1, 
                                             scale = 1),
             `Annualized Change from Previous Quarter - 25th Percentile` = 
               percent(`Annualized Change from Previous Quarter - 25th Percentile`,
                       accuracy=0.1, scale = 1),
             `Simple GDP - 25th Percentile` = percent(`Simple GDP - 25th Percentile`,accuracy=0.1, 
                                             scale = 1),
             `Annualized Change from Previous Quarter - 75th Percentile` = 
               percent(`Annualized Change from Previous Quarter - 75th Percentile`,
                       accuracy=0.1, scale = 1),
             `Simple GDP - 75th Percentile` = percent(`Simple GDP - 75th Percentile`,accuracy=0.1, 
                                                      scale = 1))
    
    #B. Add blank column as the first column
    pand_range <- cbind(Blank.1 = "", pand_range)
    
# #6.2: Change formatting to excel formatting (IGNORE FOR NOW)
# #https://stackoverflow.com/questions/48063019/formatting-percentages-in-r-package-openxlsx
#     
#     #A. Create an Excel workbook object and add a worksheet
#     wb = createWorkbook()
#     sht = addWorksheet(wb, "Pand Range Data July")
#     
#     #B. Create a percent style
#     pct = createStyle(numFmt="PERCENTAGE")
#     
#     #C. Add data to the worksheet we just created
#     writeData(wb, sht, pand_range)
#     
#     #D. Add the percent style to the desired cells
#     addStyle(wb, sht, style=pct, cols=c(3,4), rows=2:(nrow(pand_range)+1), 
#              gridExpand=TRUE)
#     
#     saveWorkbook(wb, paste(new_data_path, "test.xlsx", 
#                            sep = "\\"))
        
#6.3: Export as an Excel
    
    #A. Export
    write.xlsx(pand_range, paste(new_data_path, "2a_Pand Range--BEA and WSJ GDP_January 22.xlsx",
                                 sep = "/"), sheetName = "Pand Range Data January 22",
               zoom = 90, startRow = 6, colNames = TRUE, overwrite = TRUE)
        
                            
                            
                            