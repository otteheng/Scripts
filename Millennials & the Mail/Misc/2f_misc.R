#------------------------------------------------------------------------#
#                           Miscellaneous Code                           #
#------------------------------------------------------------------------#

library(gmodels)

# Initial Data Sets
raw_data_path_2e <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2e_HDS--Cleaned data"

# Load volume data (RDA)
load(file = paste(raw_data_path_2e, "2e_volume-rects-new_names.rda", sep = '\\'))  

#------------------------------------------------------------------------#
#                             Cross Tabs                                 #
#------------------------------------------------------------------------#

# Create two data sets of just 2005 and 2014
t2005 <- vol.new.names %>% filter(SURVEY.YEAR==2005)
t2014 <- vol.new.names %>% filter(SURVEY.YEAR==2014)

# Cross tab of age(s) by year
CrossTable(t2005$AGE.COHORT, t2005$SURVEY.YEAR, format="SAS")
CrossTable(t2014$AGE.COHORT, t2014$SURVEY.YEAR, format="SAS")


#------------------------------------------------------------------------#
#                   Count Number People who received zero mail           #
#                               in Each Year                             #
#------------------------------------------------------------------------#

vol.new.names %>% 
  group_by(SURVEY.YEAR) %>%
  summarise(mail.sum = sum(MAIL.VOLUME.SUM==0),
            brmr.sum = sum(MAIL.VOLUME.BRMR_PRST==0),
            fcmr.sum = sum(MAIL.VOLUME.FCMR==0),
            npmr.sum = sum(MAIL.VOLUME.NPMR_PRST==0),
            pemr.sum = sum(MAIL.VOLUME.PEMR==0),
            uamr.sum = sum(MAIL.VOLUME.UAMR==0)) 

