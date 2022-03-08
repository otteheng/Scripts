#------------------------------------------------------------------------#
#                           Miscellaneous Code                           #
#------------------------------------------------------------------------#
# Initial Data Sets
raw_data_path_2d <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2d_HDS--Merge 2c with demographic etc"

# Import
vol.rects <- read.csv(file= paste(raw_data_path_2d, "2d_volume-rects.csv", 
                                  sep = '\\'))  

# Recreating the numbers from footnote 2 of the OIG report "Millennials & the Mail"
# "Among adults aged 18 to 34, the amount of mail received per week fell from 17 
# mailpieces in 2001 to 10 mailpieces in 2017"

# First-Class Mail and all Commercial and Nonprofit Marketing Mail received      
# •	FCMR •	BRMR •	NPMR
test <- vol.rects[, c(1:8,48)]
test <- test %>% filter(SURVEY.YEAR==2001 | 
                        SURVEY.YEAR==2017) 
test <- test %>% mutate(sum_1 = 
                 rowSums(select(.,c("MAIL.VOLUME.BRMR_PRST", "MAIL.VOLUME.FCMR",
                                   "MAIL.VOLUME.NPMR_PRST"))))
test$RESPONDENT..AGE.COHORT = 
  factor(test$RESPONDENT..AGE.COHORT,
         levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 38, 98, 99),
         labels = c("DK/RF", "18-24", "18-24", "25-34", "35-44", 
                    "45-54", "55-64", "65-69", "70-74", "75+", 
                    "70+", "DK", "RF"), ordered = TRUE)
test <- test %>% mutate(RESPONDENT..AGE.COHORT = 
                          recode(RESPONDENT..AGE.COHORT,
                                 "18-24" = "18-34",
                                 "25-34" = "18-34"))
test <-test[!(test$RESPONDENT..AGE.COHORT=="RF" | 
                test$RESPONDENT..AGE.COHORT=="DK"),]

# Mean
aggregate(x = test$sum_1, by = list(test$RESPONDENT..AGE.COHORT,
                                    test$SURVEY.YEAR), FUN = "mean") 

# Median: This gets the right answer!
aggregate(x = test$sum_1, by = list(test$RESPONDENT..AGE.COHORT,
                                    test$SURVEY.YEAR), FUN = "median") 