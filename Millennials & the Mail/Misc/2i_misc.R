#------------------------------------------------------------------------#
#                           Miscellaneous Code                           #
#------------------------------------------------------------------------#


# Percent of total data belonging to each element within variable
# https://stackoverflow.com/questions/62684309/error-in-sumcount-invalid-type-closure-of-argument
vol.new.names %>%
  add_count() %>% # create a column with name 'n'
  group_by(INC.LEVEL) %>% # grouped by two columns
  summarise(per = str_c(round(100 * n()/first(n), 2), '%')) 

# Percent belonging to each Age cohort overall
vol.new.names %>%
  add_count() %>% # create a column with name 'n'
  group_by(AGE.COHORT) %>% # grouped by two columns
  summarise(per = str_c(round(100 * n()/first(n), 2), '%')) 

# Percent belonging to each Age cohort overall
test.age2 <- vol.new.names %>%
  add_count(SURVEY.YEAR) %>% # create a column with name 'n'
  group_by(AGE.COHORT, SURVEY.YEAR) %>% # grouped by two columns
  summarise(per = str_c(round(100 * n()/first(n), 2), '%')) 

test1 <- vol.new.names %>%
  add_count(SURVEY.YEAR) %>% # create a column with name 'n'
  group_by(INC.LEVEL, SURVEY.YEAR) %>% # grouped by two columns
  summarise(per = str_c(round(100 * n()/first(n), 2), '%'),
            countT = n()) 

# Income level count by year and married
test2 <- vol.new.names %>%
  add_count(SURVEY.YEAR) %>% # create a column with name 'n'
  filter(MARITAL.STATUS == "Married") %>%
  group_by(INC.LEVEL, SURVEY.YEAR, MARITAL.STATUS) %>% # grouped by two columns
  summarise(per = str_c(round(100 * n()/first(n), 2), '%'),
            countT = n()) 

# Household size count by year and married
test3 <- vol.new.names %>%
  add_count(SURVEY.YEAR) %>% # create a column with name 'n'
  filter(MARITAL.STATUS == "Married") %>%
  group_by(NUM.HOUSEHOLD, SURVEY.YEAR, MARITAL.STATUS) %>% # grouped by two columns
  summarise(per = str_c(round(100 * n()/first(n), 2), '%'),
            countT = n()) 

# Household size count by year, married, and 18-24
test4 <- vol.new.names %>%
  add_count(SURVEY.YEAR) %>% # create a column with name 'n'
  filter(MARITAL.STATUS == "Married" & AGE.COHORT=="18-24") %>%
  group_by(NUM.HOUSEHOLD, SURVEY.YEAR, MARITAL.STATUS) %>% # grouped by two columns
  summarise(per = str_c(round(100 * n()/first(n), 2), '%'),
            countT = n()) 

# Race count by year 
test5 <- vol.new.names %>%
  add_count(SURVEY.YEAR) %>% # create a column with name 'n'
  group_by(RACE, SURVEY.YEAR) %>% # grouped by two columns
  summarise(per = str_c(round(100 * n()/first(n), 2), '%'),
            countT = n()) 
