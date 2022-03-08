# -> Gerhard O
# -> 2/16/2021
# -> Data sets: 2e_HDS

# Load Packages in Library
source('C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Script\\_Package-Library.R')

# Initial Data Sets
raw_data_path_2e <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2e_HDS--Cleaned data"

# Clean Data
graph_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Tables & Figures\\2i_HDS"

#------------------------------------------------------------------------#
#                         Descriptive Statistics                         #
#                           And Tables/Figures                           #
#------------------------------------------------------------------------#

# Load volume data (RDA)
load(file = paste(raw_data_path_2e, "2e_volume-rects-new_names.rda", sep = '\\'))  

#------------------------------------------------------------------------#
#                   Data Frames of Rate of Change Mail Use Over Time     #
#                               by Mail Use                              #
#------------------------------------------------------------------------#   


#3.1: Set up Data for Mail Volume

    #A. Set up Mean Data

        ##1. By Cohort
        df.age1 <- vol.new.names[, c("SURVEY.YEAR", "MAIL.VOLUME.SUM", 
                                     "AGE.COHORT")]
        df.age1 <- df.age1 %>%
            filter(AGE.COHORT=="18-24" | AGE.COHORT=="25-34" | 
                       AGE.COHORT=="35-44" | AGE.COHORT=="45-54") 
        mean.data <- aggregate(x = df.age1$MAIL.VOLUME.SUM, 
                                  by = list(df.age1$SURVEY.YEAR, df.age1$AGE.COHORT),
                                  FUN = 'mean')
        colnames(mean.data) <- c("Year", "Age.Cohort", "Mean")
        
        ##2. By Gender
        df.gender.age <- vol.new.names[, c("SURVEY.YEAR", "GENDER", 
                                           "MAIL.VOLUME.SUM", "AGE.COHORT")]
        df.gender.age <- df.gender.age %>%
            filter(AGE.COHORT=="18-24" | AGE.COHORT=="25-34" | 
                       AGE.COHORT=="35-44" | AGE.COHORT=="45-54") %>%
            filter(GENDER=="Male" | GENDER=="Female")
        gender.age.mean <- aggregate(x = df.gender.age$MAIL.VOLUME.SUM, 
                                  by = list(df.gender.age$SURVEY.YEAR, 
                                            df.gender.age$AGE.COHORT,
                                            df.gender.age$GENDER),
                                  FUN = 'mean')
        colnames(gender.age.mean) <- c("Year", "Age.Cohort","Gender", "Mean")
        gender.age.mean$Mean <- round(gender.age.mean$Mean, 2)
        
        ##3. By Region
        df.region.age <- vol.new.names[, c("SURVEY.YEAR", "AGE.COHORT", 
                                           "MAIL.VOLUME.SUM", "REGION")]
        df.region.age <- df.region.age %>%
            filter(AGE.COHORT=="18-24" | AGE.COHORT=="25-34" | 
                   AGE.COHORT=="35-44" | AGE.COHORT=="45-54") %>%
            drop_na(REGION) 
        region.age.mean <- aggregate(x = df.region.age$MAIL.VOLUME.SUM, 
                                  by = list(df.region.age$SURVEY.YEAR, 
                                            df.region.age$AGE.COHORT,
                                            df.region.age$REGION),
                                  FUN = 'mean')
        colnames(region.age.mean) <- c("Year", "Age.Cohort","Region", "Mean")
        region.age.mean$Mean <- round(region.age.mean$Mean, 2)
        
        ##4. By Children
        df.child.age <- vol.new.names[, c("SURVEY.YEAR", "AGE.COHORT", 
                                           "MAIL.VOLUME.SUM", "NUM.UNDER18")]
        df.child.age <- df.child.age %>%
            filter(AGE.COHORT=="18-24" | AGE.COHORT=="25-34" | 
                   AGE.COHORT=="35-44" | AGE.COHORT=="45-54") %>%
            mutate(CHILDREN = ifelse(NUM.UNDER18 > 0, "Children At Home", 
                                     "No Children At Home"))
        child.age.mean <- aggregate(x = df.child.age$MAIL.VOLUME.SUM, 
                                  by = list(df.child.age$SURVEY.YEAR, 
                                            df.child.age$AGE.COHORT,
                                            df.child.age$CHILDREN),
                                  FUN = 'mean')
        colnames(child.age.mean) <- c("Year", "Age.Cohort","Children", "Mean")
        child.age.mean$Mean <- round(child.age.mean$Mean, 2)
        
        ##5. Marital Status (Married and Single)
        df.married.age <- vol.new.names[, c("SURVEY.YEAR", "AGE.COHORT", 
                                          "MAIL.VOLUME.SUM", "MARITAL.STATUS")]
        df.married.age1 <- df.married.age %>% # Include only Married and Single
            filter(AGE.COHORT=="18-24" | AGE.COHORT=="25-34" | 
                   AGE.COHORT=="35-44" | AGE.COHORT=="45-54") %>%
            filter(MARITAL.STATUS=="Married" | 
                   MARITAL.STATUS=="Single, never been married")
        married.age.mean1 <- aggregate(x = df.married.age1$MAIL.VOLUME.SUM, 
                                  by = list(df.married.age1$SURVEY.YEAR, 
                                           df.married.age1$AGE.COHORT,
                                           df.married.age1$MARITAL.STATUS),
                                  FUN = 'mean')
        colnames(married.age.mean1) <- c("Year", "Age.Cohort","Marital.Status", "Mean")
        married.age.mean1$Mean <- round(married.age.mean1$Mean, 2)
        
        ##6. Marital Status (More categories)
        df.married.age2 <- df.married.age %>% 
            filter(AGE.COHORT=="18-24" | AGE.COHORT=="25-34" | 
                   AGE.COHORT=="35-44" | AGE.COHORT=="45-54") %>%
            mutate(Marital.Status = case_when(
                MARITAL.STATUS=="Living as married" ~ "Married",
                MARITAL.STATUS=="Separated" ~ "Divorced",
                MARITAL.STATUS=="Widowed" | is.na(MARITAL.STATUS) ~ "Other",
                TRUE ~ as.character(MARITAL.STATUS) )) %>%
            dplyr::select(SURVEY.YEAR, AGE.COHORT, MAIL.VOLUME.SUM, Marital.Status)

        
            ###a. Creating a count of marriage categories by year and cohort
            married.count <- vol.new.names %>% 
                dplyr::select(MARITAL.STATUS, SURVEY.YEAR, AGE.COHORT) %>% 
                filter(AGE.COHORT=="18-24" | AGE.COHORT=="25-34" | 
                       AGE.COHORT=="35-44" | AGE.COHORT=="45-54") %>% 
                mutate(Marital.Status = case_when(
                    MARITAL.STATUS=="Living as married" ~ "Married",
                    MARITAL.STATUS=="Separated" ~ "Divorced",
                    MARITAL.STATUS=="Widowed" | is.na(MARITAL.STATUS) ~ "Other",
                    TRUE ~ as.character(MARITAL.STATUS) )) %>%
                group_by(SURVEY.YEAR, AGE.COHORT, Marital.Status) %>% count()
            
            colnames(married.count) <- c("Year", "Age.Cohort",
                                         "Marital.Status", "Count")
            
            ###b. Aggregate and find means
            married.age.mean2 <- aggregate(x = df.married.age2$MAIL.VOLUME.SUM, 
                                  by = list(df.married.age2$SURVEY.YEAR, 
                                            df.married.age2$AGE.COHORT,
                                            df.married.age2$Marital.Status),
                                  FUN = 'mean')
            colnames(married.age.mean2) <- c("Year", "Age.Cohort",
                                             "Marital.Status", "Mean")
            married.age.mean2$Mean <- round(married.age.mean2$Mean, 2)
            
            ###c. Merge Count and Remove averages with low counts to avoid
            #       spurious averages
            married.age.mean3 <- merge(married.age.mean2, married.count, 
                                  by = c("Year", "Age.Cohort", "Marital.Status"))
            married.age.mean4 <- married.age.mean3 %>% 
                filter( (Count >= 20 & Age.Cohort=="18-24") | 
                        (Count >= 18 & Age.Cohort=="25-34") | 
                        (Count >= 30 & Age.Cohort=="35-44") |
                        (Count >= 30 & Age.Cohort=="45-54")   )
            
        ##7. Married by different categories
        df.married.age.vars <- vol.new.names[, c("SURVEY.YEAR", "AGE.COHORT", 
                                            "MAIL.VOLUME.SUM", "MARITAL.STATUS",
                                            "GENDER", "INC.LEVEL", "HOME.OWNER",
                                            "NUM.HOUSEHOLD", "NUM.UNDER18")]
        df.married.only <- df.married.age.vars %>% # Include only Married and Single
            filter(AGE.COHORT=="18-24" | AGE.COHORT=="25-34" | 
                       AGE.COHORT=="35-44" | AGE.COHORT=="45-54") %>%
            filter(MARITAL.STATUS=="Married") %>%
            mutate(NUM.HOUSEHOLD.GROUPED = case_when(
                NUM.HOUSEHOLD<=2 ~ "2", NUM.HOUSEHOLD==3 ~ "3",
                NUM.HOUSEHOLD==4 ~ "4", NUM.HOUSEHOLD==5 ~ "5",
                NUM.HOUSEHOLD>5  ~ "6 or more"
            )) %>%
            mutate(CHILDREN = ifelse(NUM.UNDER18 > 0, "Children At Home", 
                                     "No Children At Home"))
        
            ###a. Aggregate and find means (Gender)
            married.age.gend.mean <- aggregate(x = df.married.only$MAIL.VOLUME.SUM, 
                                  by = list(df.married.only$SURVEY.YEAR, 
                                         df.married.only$AGE.COHORT,
                                         df.married.only$MARITAL.STATUS,
                                         df.married.only$GENDER),
                                  FUN = 'mean')
            colnames(married.age.gend.mean) <- c("Year", "Age.Cohort",
                                                 "Marital.Status", "Gender", 
                                                 "Mean")
            married.age.gend.mean$Mean <- round(married.age.gend.mean$Mean, 2)
            
            ###b. Aggregate and find means (Income)
            married.age.inc.mean <- aggregate(x = df.married.only$MAIL.VOLUME.SUM, 
                                  by = list(df.married.only$SURVEY.YEAR, 
                                            df.married.only$AGE.COHORT,
                                            df.married.only$MARITAL.STATUS,
                                            df.married.only$INC.LEVEL),
                                  FUN = 'mean')
            colnames(married.age.inc.mean) <- c("Year", "Age.Cohort",
                                                "Marital.Status", "Income.Level", 
                                                "Mean")
            married.age.inc.mean$Mean <- round(married.age.inc.mean$Mean, 2)
            
            ###c. Aggregate and find means (Household Size)
            married.age.household.mean <- aggregate(x = df.married.only$MAIL.VOLUME.SUM, 
                                  by = list(df.married.only$SURVEY.YEAR, 
                                            df.married.only$AGE.COHORT,
                                            df.married.only$MARITAL.STATUS,
                                            df.married.only$NUM.HOUSEHOLD.GROUPED),
                                  FUN = 'mean')
            colnames(married.age.household.mean) <- c("Year", "Age.Cohort",
                                                      "Marital.Status", "Household.Size", 
                                                      "Mean")
            married.age.household.mean$Mean <- round(married.age.household.mean$Mean, 2)
            
            ###d. Aggregate and find means (Children)
            married.age.children.mean <- aggregate(x = df.married.only$MAIL.VOLUME.SUM, 
                                 by = list(df.married.only$SURVEY.YEAR, 
                                          df.married.only$AGE.COHORT,
                                          df.married.only$MARITAL.STATUS,
                                          df.married.only$CHILDREN),
                                 FUN = 'mean')
            colnames(married.age.children.mean) <- c("Year", "Age.Cohort",
                                                     "Marital.Status", "Children", 
                                                     "Mean")
            married.age.children.mean$Mean <- round(married.age.children.mean$Mean, 2)
            
        ##8. By Income
        df.inc.age <- vol.new.names[, c("SURVEY.YEAR", "AGE.COHORT", 
                                          "MAIL.VOLUME.SUM", "INC.LEVEL")]
        df.inc.age <- df.inc.age %>%
            filter(AGE.COHORT=="18-24" | AGE.COHORT=="25-34" | 
                       AGE.COHORT=="35-44" | AGE.COHORT=="45-54") 
        inc.age.mean <- aggregate(x = df.inc.age$MAIL.VOLUME.SUM, 
                                    by = list(df.inc.age$SURVEY.YEAR, 
                                              df.inc.age$AGE.COHORT,
                                              df.inc.age$INC.LEVEL),
                                    FUN = 'mean')
        colnames(inc.age.mean) <- c("Year", "Age.Cohort","Income.Level", "Mean")
        inc.age.mean$Mean <- round(inc.age.mean$Mean, 2)
        
        ##9. By Race
        df.race.age <- vol.new.names[, c("SURVEY.YEAR", "AGE.COHORT", 
                                        "MAIL.VOLUME.SUM", "RACE")]
        df.race.age <- df.race.age %>%
            filter(AGE.COHORT=="18-24" | AGE.COHORT=="25-34" | 
                       AGE.COHORT=="35-44" | AGE.COHORT=="45-54") %>%
            mutate(RACE2 = case_when(
                RACE=="Hispanic" ~ "Other",
                RACE=="American Indian or Alaska Native" ~ "Other",
                RACE=="Native Hawaiian or Other Paciric Islander" ~ "Other",
                TRUE ~ as.character(RACE)
            )) %>%
            filter(!is.na(RACE2))
        race.age.mean <- aggregate(x = df.race.age$MAIL.VOLUME.SUM, 
                                  by = list(df.race.age$SURVEY.YEAR, 
                                            df.race.age$AGE.COHORT,
                                            df.race.age$RACE2),
                                  FUN = 'mean')
        colnames(race.age.mean) <- c("Year", "Age.Cohort","Race", "Mean")
        race.age.mean$Mean <- round(race.age.mean$Mean, 2)
            
    #B. Set up Median Data
    median.data <- aggregate(x = df.age1$MAIL.VOLUME.SUM, 
                             by = list(df.age1$SURVEY.YEAR, df.age1$AGE.COHORT),
                             FUN = 'median')
    colnames(median.data) <- c("Year", "Age.Cohort", "Median")

#3.2: Aggregate 
    
    #A. Merge Mean and Median
    df.agg <- merge(mean.data, median.data, by = c("Year", "Age.Cohort"))
    colnames(df.agg) <- c("Year", "Age.Cohort", "Mean", "Median")
    df.agg$Mean <- round(df.agg$Mean, digits = 2)
    df.agg$Age.Cohort <- factor(df.agg$Age.Cohort, # Change order of variable for graph
                                levels = rev(levels(df.agg$Age.Cohort))) 
    
    #B. Sort by Age Cohort and Year
    df.agg <- df.agg[with(df.agg, order(Age.Cohort, Year)),]
    gender.age.mean <- 
        gender.age.mean[with(gender.age.mean, order(Age.Cohort, Gender, Year)),]

#3.4: Rate of Change Data Frames
    
    #A. Percent Change
    
        ##1. Age Cohort
        df.perc <- PercChange(df.agg, Var = 'Mean', 
                                     type = 'percent', 
                                     NewVar = 'Mean.Percent.Change',
                                     GroupVar = 'Age.Cohort')
        df.perc$Mean.Percent.Change <- 
            round(df.perc$Mean.Percent.Change, digits = 2)
        df.perc <- PercChange(df.perc, Var = 'Median', 
                                   type = 'percent', 
                                   NewVar = 'Median.Percent.Change',
                                   GroupVar = 'Age.Cohort')
        df.perc$Median.Percent.Change <- 
            round(df.perc$Median.Percent.Change, digits = 2)
        
        ##2. Age Cohort and Gender
        df.perc.gender.age <- gender.age.mean %>%
            group_by(Gender, Age.Cohort, .add = T) %>%
            mutate(percent.change = round(((Mean - lag(Mean)) / lag(Mean))*100,2))
    
    #B. Drop NA
    df.perc <- df.perc %>% drop_na()
    
#3.5: Reduce down to generations and reshape 
    
    #A. Filter
    df.gen <- df.perc %>%
        filter( (Age.Cohort=='18-24' &  Year %in% (2005:2014))  | # Millennial
                (Age.Cohort=='25-34' & (Year %in% (2000:2005) |   # Gen X
                                        Year %in% (2015:2019))) | # Millennial
                (Age.Cohort=='35-44' &  Year %in% (2009:2015))  | # Gen X
                (Age.Cohort=='45-54' &  Year %in% (2000:2009))  ) # Boomer
    
            ##1. Reshape Wide
            re.df.gen <- reshape(df.gen, v.names=c("Mean", "Median", 
                            "Mean.Percent.Change", "Median.Percent.Change"), 
                     timevar="Age.Cohort", idvar=c("Year"), direction="wide") 
            
            ##2. Divide Percent Change 25-34 into two columns
            re.df.gen <- re.df.gen %>%
                mutate(`Mean.Percent.Change1.25-34` = 
                    ifelse(Year < 2006, `Mean.Percent.Change.25-34`, NA)) %>%
                mutate(`Mean.Percent.Change2.25-34` = 
                    ifelse(Year > 2014, `Mean.Percent.Change.25-34`, NA))
            re.df.gen$`Mean.Percent.Change.25-34` <- NULL
            
            ##2. Filter only percent change Mean variables
            cnames.perc.ch <- colnames(re.df.gen[, grep(pattern="Mean.Percent.Change", colnames(re.df.gen))])
            re.df.gen1 <- re.df.gen[, c("Year", cnames.perc.ch)]
            
            ##3. Reshape Long to include NAs
            re.df.gen1 <- melt(re.df.gen1, id = c("Year")) 
            
            ##4. Change name
            re.df.gen1 <- re.df.gen1 %>%
                mutate(`Generations by Cohort` = case_when(
                    variable=="Mean.Percent.Change.45-54" ~ "Boomer: 45-54",
                    variable=="Mean.Percent.Change.35-44" ~ "Gen X: 35-44",
                    variable=="Mean.Percent.Change1.25-34" ~ "Gen X: 25-34",
                    variable=="Mean.Percent.Change2.25-34" ~ "Millennial: 25-34",
                    variable=="Mean.Percent.Change.18-24" ~ "Millennial: 18-24"
                )) %>%
                mutate(Generations = case_when(
                    variable=="Mean.Percent.Change.45-54" ~ "Boomer",
                    variable=="Mean.Percent.Change.35-44" ~ "Gen X",
                    variable=="Mean.Percent.Change1.25-34" ~ "Gen X",
                    variable=="Mean.Percent.Change2.25-34" ~ "Millennial",
                    variable=="Mean.Percent.Change.18-24" ~ "Millennial"
                ))
            
            ##5. Omit NAs
            re.df.gen2 <- na.omit(re.df.gen1)
            
#3.5: Rate of Change From a Given Year
#     https://datascience.stackexchange.com/questions/73993/r-rates-of-change-from-an-initial-value         
    
    #A. Percent Change from the year 2000
    df.perc.2000 <- df.agg %>%
        group_by(Age.Cohort, .add = T) %>%
        mutate(percent.change = round(((Mean - lag(Mean)) / lag(Mean))*100,2),
               First = head(Mean, 1),
               BaselineChange = 
                   case_when(Mean != First ~ (Mean - First) * 100 / First,
                            TRUE ~ 1 * NA)) 
        
        ##1. Round to 2 decimal points 
        df.perc.2000$BaselineChange <- round(df.perc.2000$BaselineChange, 2)
    
        ##2. Replace NAs with zero
        df.perc.2000 <- df.perc.2000 %>% 
            mutate(BaselineChange = replace_na(BaselineChange, 0))
    
    #B. Percent Change from the year 2009
    df.perc.2009 <- df.agg %>%
        filter(Year %in% (2009:2019)) %>%
        group_by(Age.Cohort, .add = T) %>%
        mutate(percent.change = round(((Mean - lag(Mean)) / lag(Mean))*100,2),
               First = head(Mean, 1),
               BaselineChange = 
                   case_when(Mean != First ~ (Mean - First) * 100 / First,
                             TRUE ~ 1 * NA)) 

        ##1. Round to 2 decimal points 
        df.perc.2009$BaselineChange <- round(df.perc.2009$BaselineChange, 2)
        
        ##2. Replace NAs with zero
        df.perc.2009 <- df.perc.2009 %>% 
            mutate(BaselineChange = replace_na(BaselineChange, 0))        
    
#------------------------------------------------------------------------#
#                                   Graphs                               #
#                      Rate of Change by Generations and Cohorts         #       
#------------------------------------------------------------------------#     
                
#4.1: % Change Mail Use Graph
    
    #A. Graph
    graph.mean.chg <- df.perc %>%
        ggplot( aes(x=Year, y=Mean.Percent.Change, group=Age.Cohort, 
                    color=Age.Cohort)) +
        scale_x_continuous(limits = c(2001, 2019) , breaks = (seq(2001, 2019, 2))) +
        scale_y_continuous(limits = c(-50,40), breaks = (seq(-40, 40, 10))) +
        geom_hline(yintercept = 0, color = "#1C2833") +
        geom_line(size = 1) +
        theme_minimal() +
        ggtitle("Mean % Change Mail Volume by Age Cohort") +
        labs(color = "Age Cohort") +
        ylab("% Change Mail Volume") + 
        scale_color_manual(values=c("#01a2d9","#76c0c1", "#6794a7", 
                                    "#014d64")) + 
        theme(plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = 
                  element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7)) +
        geom_signif(annotation="18-24 = Millennial", y_position=-47, xmin=2005, 
                    xmax=2014, tip_length = c(0.04, 0.04), color = "#014d64", 
                    textsize = 3) + 
        geom_signif(annotation="25-34 = Millennial", y_position=-47, xmin=2015, 
                    xmax=2019, tip_length = c(0.04, 0.04), color = "#6794a7", 
                    textsize = 3) +
        geom_signif(annotation="25-34 = Gen X", y_position=-40, xmin=2001, 
                    xmax=2005, tip_length = c(0.04, 0.04), color = "#6794a7", 
                    textsize = 3) +
        geom_signif(annotation="35-44 = Gen X", y_position=-40, xmin=2009, 
                    xmax=2015, tip_length = c(0.04, 0.04), color = "#76c0c1", 
                    textsize = 3) +
        geom_signif(annotation="45-54 = Boomer", y_position=-35, xmin=2001, 
                    xmax=2009, tip_length = c(0.04, 0.04), color = "#01a2d9", 
                    textsize = 3)
    
    #B. Save Graph
    ggsave(paste(graph_path, "4_1--Percent Change Mean Mail Volume by Cohort.png", 
                 sep="\\"), graph.mean.chg, width=6, height=4.5)

#4.2: % Change Mail Use Graph (Filter by Generation by Cohort)
    
    #A. Graph
    graph.mean.chg.gen.cohort <- re.df.gen1 %>%
        ggplot( aes(x=Year, y=value, group=`Generations by Cohort`, 
                    color=`Generations by Cohort`)) +
        scale_x_continuous(limits = c(2001, 2019) , breaks = (seq(2001, 2019, 2))) +
        scale_y_continuous(limits = c(-35,40), breaks = (seq(-40, 40, 10))) +
        geom_hline(yintercept = 0, color = "#1C2833") +
        geom_line(size = 1) +
        theme_minimal() +
        ggtitle("Mean % Change Mail Volume by \n Generation and Age Cohort") +
        ylab("% Change Mail Volume") + 
        scale_color_manual(values=c("#01a2d9","#76c0c1", "#6794a7", 
                                    "#014d64", "#efe8d1")) + 
        theme(plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = 
                  element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7))
    
    #B. Save Graph
    ggsave(paste(graph_path, "4_2--Mean Percent Change Mail Volume by Generation and Age Cohort.png", 
                 sep="\\"), graph.mean.chg.gen.cohort, width=6, height=4.5)


#4.3: % Change Mail Use Graph (Filter by Generation by Cohort)
    
    #A. Graph
    graph.mean.chg.gen <- re.df.gen2 %>%
        ggplot( aes(x=Year, y=value, group=Generations, 
                    color=Generations)) +
        scale_x_continuous(limits = c(2001, 2019) , breaks = (seq(2001, 2019, 2))) +
        scale_y_continuous(limits = c(-35,40), breaks = (seq(-30, 40, 10))) +
        geom_hline(yintercept = 0, color = "#1C2833") +
        geom_line(size = 1) +
        theme_minimal() +
        ggtitle("Mean % Change Mail Volume by \n Full Generation Cohorts") +
        ylab("% Change Mail Volume") + 
        scale_color_manual(values=c("#01a2d9","#76c0c1", "#6794a7")) + 
        theme(plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = 
                  element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7))    
    
    #B. Save Graph
    ggsave(paste(graph_path, "4_3--Mean Percent Change Mail Volume by Generation.png", 
                 sep="\\"), graph.mean.chg.gen, width=6, height=4.5)
    
    
#4.4: Mean % Change Mail Volume From the Year 2000
    
    #A. Graph
    graph.mean.2000 <- df.perc.2000 %>%
        ggplot( aes(x=Year, y=BaselineChange, group=Age.Cohort, 
                    color=Age.Cohort)) +
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(-52,55), breaks = (seq(-50, 60, 10))) +
        geom_hline(yintercept = 0, color = "#1C2833") +
        geom_line(size = 1) +
        theme_minimal() +
        ggtitle(label = "Change in Average Mail Volume From the Year 2000",
                subtitle = "Average of Respondents Weekly Totals") +
        ylab("%\u0394 in Average Weekly Mail Volume") + 
        labs(color = "Age Cohort") +
        scale_color_manual(values=c("#01a2d9","#76c0c1", "#6794a7", 
                                    "#014d64")) + 
        theme(text = element_text(family = "Georgia"),
              plot.title = element_text(color="black", size=12, hjust = .5),
              plot.subtitle = element_text(size = 8, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = 
                  element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              legend.position = c(0.9, 0.8)) +
        geom_signif(annotation="18-24 = Millennial", y_position=-47, xmin=2005, 
                    xmax=2014, tip_length = c(0.04, 0.04), color = "#014d64", 
                    textsize = 3) + 
        geom_signif(annotation="25-34 = Millennial", y_position=-47, xmin=2015, 
                    xmax=2019, tip_length = c(0.04, 0.04), color = "#6794a7", 
                    textsize = 3) +
        geom_signif(annotation="25-34 = Gen X", y_position=-37, xmin=2001, 
                    xmax=2005, tip_length = c(0.04, 0.04), color = "#6794a7", 
                    textsize = 3) +
        geom_signif(annotation="35-44 = Gen X", y_position=-37, xmin=2009, 
                    xmax=2015, tip_length = c(0.04, 0.04), color = "#76c0c1", 
                    textsize = 3) +
        geom_signif(annotation="45-54 = Boomer", y_position=-30, xmin=2001, 
                    xmax=2009, tip_length = c(0.04, 0.04), color = "#01a2d9", 
                    textsize = 3)
    
    #B. Save Graph
    ggsave(paste(graph_path, "4_4--Mean Percent Change Mail Volume From the Year 2000.png", 
                 sep="\\"), graph.mean.2000, width=6, height=4.5)
    
    #C. Export Data from Graph
    df.new.name <- '4_4 Data.xlsx'       
    # write_csv(df.perc.2000, path = paste(graph_path, df.new.name, sep="\\"),
    #           append=FALSE, col_names=TRUE)
    write.xlsx(df.perc.2000, paste(graph_path, df.new.name, sep="\\"))

#4.5: Mean % Change Mail Volume From the Year 2009
    
    #A. Graph
    graph.mean.2009 <- df.perc.2009 %>%
        ggplot( aes(x=Year, y=BaselineChange, group=Age.Cohort, 
                    color=Age.Cohort)) +
        scale_x_continuous(limits = c(2009, 2019) , breaks = (seq(2009, 2019, 1))) +
        scale_y_continuous(limits = c(-52,10), breaks = (seq(-50, 10, 10))) +
        geom_hline(yintercept = 0, color = "#1C2833") +
        geom_line(size = 1) +
        theme_minimal() +
        ggtitle("Mean % Change Mail Volume \n From the Year 2009") +
        ylab("% Change Mail Volume") + 
        labs(color = "Age Cohort") +
        scale_color_manual(values=c("#01a2d9","#76c0c1", "#6794a7", 
                                    "#014d64")) + 
        theme(text = element_text(family = "Georgia"),
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = 
                  element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              legend.position = c(0.9, 0.8)) +
        geom_signif(annotation="18-24 = Millennial", y_position=-47, xmin=2009, 
                    xmax=2014, tip_length = c(0.04, 0.04), color = "#014d64", 
                    textsize = 3) + 
        geom_signif(annotation="25-34 = Millennial", y_position=-47, xmin=2015, 
                    xmax=2019, tip_length = c(0.04, 0.04), color = "#6794a7", 
                    textsize = 3) +
        geom_signif(annotation="35-44 = Gen X", y_position=-40, xmin=2009, 
                    xmax=2015, tip_length = c(0.04, 0.04), color = "#76c0c1", 
                    textsize = 3) 
    
    #B. Save Graph
    ggsave(paste(graph_path, "4_5--Mean Percent Change Mail Volume From the Year 2009.png", 
                 sep="\\"), graph.mean.2009, width=6, height=4.5)

    #C. Export Data from Graph
    df.new.name <- '4_5 Data.xlsx'       
    # write_csv(df.perc.2000, path = paste(graph_path, df.new.name, sep="\\"),
    #           append=FALSE, col_names=TRUE)
    write.xlsx(df.perc.2009, paste(graph_path, df.new.name, sep="\\"))        

        
#------------------------------------------------------------------------#
#                               Graphs: Box Plots                        #
#                    Median, 25th, 75th percentile, Outliers             #       
#------------------------------------------------------------------------#  

#5.1.1: Box Plot of Mail Volume by Age Cohort 
    
    #A. Grouped Box Plot
    boxplot.m.vol.outliers <- df.age1 %>% 
        
        # Even numbered years + 2019
        filter(SURVEY.YEAR %% 2 == 0 | SURVEY.YEAR==2019) %>%
        
        # Convert year to a string
        mutate(SURVEY.YEAR = as.character(SURVEY.YEAR)) %>%
        
        # Box Plot
        ggplot(aes(x=SURVEY.YEAR, y=MAIL.VOLUME.SUM, fill=AGE.COHORT)) + 
        scale_y_continuous(breaks = seq(0, 160, by=20), limits=c(0,160)) +
        geom_boxplot(alpha = .8) +
        theme_minimal() +
        ggtitle("Mail Volume by Age Cohort") +
        ylab("Mail Volume") + 
        scale_fill_manual(values=c("#014d64", "#6794a7", "#76c0c1", "#01a2d9" )) +
        theme(plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = 
                  element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7))
    
    #B. Save Graph
    ggsave(paste(graph_path, "5_1_1--Box Plots of Mail Volume by Age Cohort.png", 
                 sep="\\"), boxplot.m.vol.outliers, width=8, height=4.5)        
    
#5.1.2: Box Plot of Mail Volume by Age Cohort (No Outliers)
    
    #A. Grouped Box Plot
    boxplot.m.vol.no.outliers <- df.age1 %>% 
        
        # Even numbered years + 2019
        filter(SURVEY.YEAR %% 2 == 0 | SURVEY.YEAR==2019) %>%
        
        # Convert year to a string
        mutate(SURVEY.YEAR = as.character(SURVEY.YEAR)) %>%
        
        # Box Plot
        ggplot(aes(x=SURVEY.YEAR, y=MAIL.VOLUME.SUM, fill=AGE.COHORT)) + 
        scale_y_continuous(breaks = seq(0, 70, by=10)) +
        geom_boxplot(alpha = .8, outlier.shape = NA) +
        theme_minimal() +
        ggtitle("Mail Volume by Age Cohort") +
        ylab("Mail Volume") + 
        scale_fill_manual(values=c("#014d64", "#6794a7", "#76c0c1", "#01a2d9" )) +
        theme(plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = 
                  element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7)) +
        coord_cartesian(ylim = c(0, 70))
    
    #B. Save Graph
    ggsave(paste(graph_path, "5_1_2--Box Plots of Mail Volume by Age Cohort.png", 
                 sep="\\"), boxplot.m.vol.no.outliers, width=8, height=4.5)        
        

#------------------------------------------------------------------------#
#                           Graphs: Line Graphs                          #
#    Independent Variables: Gender, Region, Children, Marital Status     #       
#------------------------------------------------------------------------#          
                
#6.1: Average Mail Received by Gender and Age Cohort
    
    #A. Graph
    graph.gender.age.mean <- gender.age.mean %>%
        ggplot( aes(x=Year, y=Mean, group=Gender, color=Gender)) +
        geom_rect(data = subset(gender.age.mean, Age.Cohort == "18-24"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2005, xmax = 2014), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_rect(data = subset(gender.age.mean, Age.Cohort == "25-34"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2015, xmax = 2019), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_line(size = 1) +
        scale_x_continuous(limits = c(1999, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by=5)) +
        ggtitle("Average Mail Received by Gender and Age Cohort") +
        ylab("Mean Volume Received") + 
        labs(color = "Gender<sup>1</sup>",
             caption = '<sup>1</sup>*Highlighted area indicates years cohorts are considered Millennials*') +
        scale_color_manual(values=c("#e3120b", "#336666")) + 
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = element_text(size = 7),
              legend.position = c(0.93, 0.2),
              strip.background = element_rect(linetype="solid"),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))

    #B. Save Graph
    ggsave(paste(graph_path, "6_1--Average Mail Received by Gender and Age Cohort .png", 
                 sep="\\"), graph.gender.age.mean, width=8, height=4.5) 
    
#6.2: Percent Change in Mail Received by Gender and Age Cohort
    
    #A. Graph
    graph.perc.gender.age.mean <- df.perc.gender.age %>%
        ggplot( aes(x=Year, y=percent.change, group=Gender, color=Gender)) +
        geom_hline(yintercept = 0, color = "#1C2833") +
        geom_line(size = 1) +
        scale_x_continuous(limits = c(1999, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(-35, 61), breaks = seq(-35, 60, by=10)) +
        ggtitle("Percent Change by Gender and Age Cohort") +
        ylab("% Change in Mail Received") + 
        scale_color_manual(values=c("#01a2d9","#76c0c1")) + 
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid"))        

    #B. Save Graph
    ggsave(paste(graph_path, "6_2--Percent Change of Mail Received by Gender and Age Cohort .png", 
                 sep="\\"), graph.perc.gender.age.mean, width=8, height=4.5) 
    
#6.3: Average Mail Received by Region and Age Cohort
    
    #A. Graph
    graph.region.age.mean <- region.age.mean %>%
        ggplot( aes(x=Year, y=Mean, group=Age.Cohort, color=Age.Cohort)) +
        geom_line(size = 1) +
        scale_x_continuous(limits = c(1999, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by=5)) +
        ggtitle("Average Mail Received by Region and Age Cohort") +
        ylab("Mean Volume Received") + 
        scale_color_manual(values=c("#014d64", "#6794a7", "#76c0c1", "#01a2d9")) + 
        facet_rep_grid(. ~ Region, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid"))          

    #B. Save Graph
    ggsave(paste(graph_path, "6_3--Average Mail Received by Region and Age Cohort.png", 
                 sep="\\"), graph.region.age.mean, width=8, height=4.5) 
    
#6.4.1: Average Mail Received by Children and Age Cohort
    
    #A. Graph
    graph.child.age.mean <- child.age.mean %>%
        ggplot( aes(x=Year, y=Mean, group=Age.Cohort, color=Age.Cohort)) +
        geom_line(size = 1) +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by=5)) +
        ggtitle("Average Mail Received by those \n With and Without Children by Age Cohort") +
        ylab("Mean Volume Received") + 
        labs(color = "Age Cohort") +
        scale_color_manual(values=c("#014d64", "#6794a7", "#76c0c1", "#01a2d9")) + 
        facet_rep_grid(. ~ Children, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid"))          
    
    #B. Save Graph
    ggsave(paste(graph_path, "6_4_1--Average Mail Received by those With and Without Children by Age Cohort.png", 
                 sep="\\"), graph.child.age.mean, width=8, height=4.5) 
    
#6.4.2: Average Mail Received by Children and Age Cohort
    
    #A. Graph
    graph.child.age.mean2 <- child.age.mean %>%
        ggplot( aes(x=Year, y=Mean, group=Children, color=Children)) +
        geom_rect(data = subset(child.age.mean, Age.Cohort == "18-24"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2005, xmax = 2014), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_rect(data = subset(child.age.mean, Age.Cohort == "25-34"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2015, xmax = 2019), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_line(size = 1) +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(5, 35), breaks = seq(5, 35, by=5)) +
        ggtitle("Average Mail Received by those With and Without Children by Age Cohort") +
        ylab("Mean Volume Received") + 
        labs(color = "Legend<sup>1</sup>",
             caption = '<sup>1</sup>*Highlighted area indicates years cohorts are considered Millennials*') +
        scale_color_manual(labels = c("Children\nAt Home", "No Children\nAt Home"),
                           values=c("#014d64", "#6794a7")) + 
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = element_text(size = 7),
              legend.position = c(0.92, 0.2),
              panel.grid.minor.x = element_line(color = NA),
              panel.grid.minor.y = element_line(color = NA),
              strip.background = element_rect(linetype="solid"),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))          
    
    #B. Save Graph
    ggsave(paste(graph_path, "6_4_2--Average Mail Received by those With and Without Children by Age Cohort.png", 
                 sep="\\"), graph.child.age.mean2, width=8, height=4.5) 
    
#6.5.1: Average Mail Received by Marital Status and Age Cohort
    
    #A. Graph
    graph.marriage.age.mean1 <- married.age.mean1 %>%
        ggplot( aes(x=Year, y=Mean, group=Age.Cohort, color=Age.Cohort)) +
        geom_line(size = 1) +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(5, 35), breaks = seq(5, 35, by=5)) +
        ggtitle("Average Mail Received by Marital Status and Age Cohort") +
        ylab("Mean Volume Received") + 
        labs(color = "Age Cohort") +
        scale_color_manual(values=c("#014d64", "#6794a7", "#76c0c1", "#01a2d9")) + 
        facet_rep_grid(. ~ Marital.Status, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA))           
    
    #B. Save Graph
    ggsave(paste(graph_path, "6_5_1--Average Mail Received by Marital Status and Age Cohort.png", 
                 sep="\\"), graph.marriage.age.mean1, width=8, height=4.5)         
    
#6.5.2: Average Mail Received by Marital Status and Age Cohort
    
    #A. Graph
    graph.marriage.age.mean2 <- married.age.mean1 %>%
        ggplot( aes(x=Year, y=Mean, group=Marital.Status, color=Marital.Status)) +
        geom_rect(data = subset(married.age.mean1, Age.Cohort == "18-24"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2005, xmax = 2014), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_rect(data = subset(married.age.mean1, Age.Cohort == "25-34"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2015, xmax = 2019), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_line(size = 1) +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(5, 35), breaks = seq(5, 35, by=5)) +
        ggtitle("Average Mail Received by Marital Status and Age Cohort") +
        ylab("Mean Volume Received") + 
        labs(color = "Marital Status<sup>1</sup>",
             caption = '<sup>1</sup>*Highlighted area indicates years cohorts are considered Millennials*') +
        scale_color_manual(labels = c("Married", "Single,\nNever\nMarried"), 
                           values=c("#e3120b", "#336666")) + 
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))           
    
    #B. Save Graph
    ggsave(paste(graph_path, "6_5_2--Average Mail Received by Marital Status and Age Cohort.png", 
                 sep="\\"), graph.marriage.age.mean2, width=8, height=4.5)          

#6.5.3: Average Mail Received by Marital Status (More Groups) and Age Cohort
    
    #A. Reorder Items
    married.age.mean4$Marital.Status <- factor(married.age.mean4$Marital.Status, 
                                levels = c("Married", "Single, never been married",
                                           "Divorced", "Other"))
    married.age.mean4 <- married.age.mean4[with(married.age.mean4, order(Marital.Status, Age.Cohort, Year)),]

    #B. Graph
    graph.marriage.age.mean3 <- married.age.mean4 %>%
        ggplot( aes(x=Year, y=Mean, group=Marital.Status, color=Marital.Status)) +
        geom_rect(data = subset(married.age.mean4, Age.Cohort == "18-24"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2005, xmax = 2014), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_rect(data = subset(married.age.mean4, Age.Cohort == "25-34"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2015, xmax = 2019), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_line(size = 1) +
        guides(fill=guide_legend(ncol=2)) +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by=5)) +
        ggtitle("Average Mail Received by Marital Status and Age Cohort") +
        ylab("Mean Volume Received") + 
        labs(color = "Age Cohort<sup>1</sup>", 
             caption = 'Note: Removed "Other" and "Divorced" from Cohorts with small sample sizes<br><sup>1</sup>*Highlighted area indicates years cohorts are considered Millennials*') +
        scale_color_manual(labels = c("Married", "Single,\nNever\nMarried",
                                      "Divorced", "Other"),
                           values=c("#e3120b", "#336666", "#8abbd0", "#efe8d1")) + 
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        guides(col = guide_legend(ncol = 2)) +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8, hjust = .5),
              legend.text = element_text(size = 7),
              legend.position = c(0.9, 0.2),
              legend.key.size = unit(4, 'mm'),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
    
    #B. Save Graph
    ggsave(paste(graph_path, "6_5_3--Average Mail Received by Marital Status (expanded) and Age Cohort.png", 
                 sep="\\"), graph.marriage.age.mean3, width=8, height=4.5)  
    
#6.5.4: Average Mail Received by Marital Status (More Groups) and Age Cohort (diff bars)
    
    #A. Reorder Items
    married.age.mean4$Marital.Status <- factor(married.age.mean4$Marital.Status, 
                                               levels = c("Married", "Single, never been married",
                                                          "Divorced", "Other"))
    married.age.mean4 <- married.age.mean4[with(married.age.mean4, order(Marital.Status, Age.Cohort, Year)),]
    
    #B. Reshape Data to include difference between means (w/ Children-w/o Children)
    married.age.mean5 <- married.age.mean4[, c("Year", "Age.Cohort", "Marital.Status", "Mean")]
    married.age.mean.wide <- reshape(married.age.mean5, 
                v.names=c("Mean"), 
                timevar="Marital.Status", idvar=c("Year", "Age.Cohort"), 
                direction="wide") 
    
        ##1. Add variable of difference between married and single
        married.age.mean.wide <- married.age.mean.wide %>%
            mutate(Diff = Mean.Married - `Mean.Single, never been married`)
    
    #C. Graph
    graph.marriage.age.mean4 <- married.age.mean.wide %>%
        ggplot(aes(x=Year)) +
        geom_rect(data = subset(married.age.mean.wide, Age.Cohort == "18-24"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2005, xmax = 2014), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_rect(data = subset(married.age.mean.wide, Age.Cohort == "25-34"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2015, xmax = 2019), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_line(aes(y=Mean.Married, color = "Married"), size = 1 ) +
        geom_line(aes(y=`Mean.Single, never been married`, color = "Single,\nNever\nMarried"), size = 1) +
        geom_col(aes(y=Diff)) +
        scale_color_manual(values = c(
            'Married' = "#336666",
            'Single,\nNever\nMarried' = "#8abbd0")) +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(-6, 38), breaks = seq(-5, 35, by=5)) +
        ggtitle("Average Mail Received by Marital Status and Age Cohort") +
        ylab("Mean Volume Received") + 
        labs(color = 'Legend<sup>1</sup>',
             caption = 'Note: Bars represent difference between Married and Single respondents<br><sup>1</sup>*Highlighted area indicates years cohorts are considered Millennials*') +
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7,
                                                      color = "#7B7D7D"))    
    
    #B. Save Graph
    ggsave(paste(graph_path, "6_5_4--Average Mail Received by Marital Status and Age Cohort (Diff bars).png", 
                 sep="\\"), graph.marriage.age.mean4, width=8, height=4.5)   
    
#6.6.1: Average Mail Received if Married by Gender
    
    #A. Graph (Line)
    graph.marriaged.age.gend1 <- married.age.gend.mean %>%
        ggplot( aes(x=Year, y=Mean, group=Gender, color=Gender)) +
        geom_rect(data = subset(married.age.gend.mean, Age.Cohort == "18-24"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2005, xmax = 2014), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_rect(data = subset(married.age.gend.mean, Age.Cohort == "25-34"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2015, xmax = 2019), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_line(size = 1) +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by=5)) +
        ggtitle("Average Mail Received if Married by Gender") +
        ylab("Mean Volume Received") + 
        labs(color = "Gender<sup>1</sup>", 
             caption = '<sup>1</sup>*Highlighted area indicates years cohorts are considered Millennials*') +
        scale_color_manual(values=c("#e3120b", "#336666")) + 
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7)) 
    
    #B. Save Graph
    ggsave(paste(graph_path, "6_6_1--Average Mail Received if Married by Gender (Line).png", 
                 sep="\\"), graph.marriaged.age.gend1, width=8, height=4.5) 
    
    #C. Graph (Bar)
    graph.marriaged.age.gend2 <- married.age.gend.mean %>%
        
        # Odd Numbered Years
        filter(Year %% 2 == 1) %>%
        
        ggplot( aes(x=Year, y=Mean, fill=Gender)) +
        geom_bar(width = 1.5, position=position_dodge(.9), stat="identity") +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by=5)) +
        ggtitle("Average Mail Received if Married by Gender") +
        ylab("Mean Volume Received") + 
        scale_fill_manual(values=c("#e3120b", "#336666")) + 
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA)) 
    
    #D. Save Graph
    ggsave(paste(graph_path, "6_6_1--Average Mail Received if Married by Gender (Bar).png", 
                 sep="\\"), graph.marriaged.age.gend2, width=8, height=4.5) 
    
#6.6.2: Average Mail Received if Married by Children
    
    #A. Reshape Data to include difference between means (w/ Children-w/o Children)
    married.age.children.mean.wide <- reshape(married.age.children.mean, 
                                              v.names=c("Mean"), 
                                              timevar="Children", idvar=c("Year", "Age.Cohort"), 
                                              direction="wide") 
    
        ##1. Add difference between those with and without children
        married.age.children.mean.wide <- married.age.children.mean.wide %>%
            mutate(Diff = `Mean.Children At Home` - `Mean.No Children At Home`)
    
    #B. Graph 
    graph.married.age.child <- married.age.children.mean.wide %>%
        ggplot(aes(x=Year)) +
        geom_rect(data = subset(married.age.children.mean.wide, Age.Cohort == "18-24"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2005, xmax = 2014), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_rect(data = subset(married.age.children.mean.wide, Age.Cohort == "25-34"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2015, xmax = 2019), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_line(aes(y=`Mean.Children At Home`, color = "Children\nAt Home"), size = 1 ) +
        geom_line(aes(y=`Mean.No Children At Home`, color = "No Children\nAt Home"), size = 1) +
        geom_col(aes(y=Diff)) +
        scale_color_manual(values = c(
            'Children\nAt Home' = "#336666",
            'No Children\nAt Home' = "#8abbd0")) +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(-6, 38), breaks = seq(-5, 35, by=5)) +
        ggtitle("Average Mail Received if Married by\n Whether or Not Respondents Had Children at Home") +
        ylab("Mean Volume Received") + 
        labs(color = 'Legend<sup>1</sup>',
             caption = 'Note: Bars represent difference between respondents with and without children<br><sup>1</sup>*Highlighted area indicates years cohorts are considered Millennials*') +
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7,
                                                      color = "#7B7D7D"))         

    #C. Save Graph
    ggsave(paste(graph_path, "6_6_2--Average Mail Received if Married by Whether or Not Respondents Had Children at Home.png", 
                 sep="\\"), graph.married.age.child, width=8, height=4.5)  
    
#6.6.3: Average Mail Received if Married by Household Size
    
    #A. Graph (Line)
    graph.married.age.house <- married.age.household.mean %>%
        ggplot( aes(x=Year, y=Mean, group=Household.Size, color=Household.Size)) +
        geom_rect(data = subset(married.age.household.mean, Age.Cohort == "18-24"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2005, xmax = 2014), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_rect(data = subset(married.age.household.mean, Age.Cohort == "25-34"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2015, xmax = 2019), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_line(size = 1) +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(0, 42), breaks = seq(0, 40, by=5)) +
        ggtitle("Average Mail Received if Married by Household Size") +
        ylab("Mean Volume Received") + 
        labs(color = "Household Size<sup>1</sup>", 
             caption = '<sup>1</sup>*Highlighted area indicates years cohorts are considered Millennials*') +
        scale_color_manual(values=c("#e3120b", "#336666", "#8abbd0",
                                    "#acc8d4", "#efe8d1")) + 
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))         
    
    #B. Save Graph
    ggsave(paste(graph_path, "6_6_3--Average Mail Received if Married by Whether or Not Respondents Had Children at Home.png", 
                 sep="\\"), graph.married.age.house, width=8, height=4.5) 
    
#6.6.4: Average Mail Received if Married by Income
    
    #A. Graph (Line)
    graph.married.age.income <- married.age.inc.mean %>%
        ggplot( aes(x=Year, y=Mean, group=Income.Level, color=Income.Level)) +
        geom_rect(data = subset(married.age.inc.mean, Age.Cohort == "18-24"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2005, xmax = 2014), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_rect(data = subset(married.age.inc.mean, Age.Cohort == "25-34"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2015, xmax = 2019), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_line(size = 1) +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by=5)) +
        ggtitle("Average Mail Received if Married by Income") +
        ylab("Mean Volume Received") + 
        labs(color = "Household Income<sup>1</sup>", 
             caption = '<sup>1</sup>*Highlighted area indicates years cohorts are considered Millennials*') +
        scale_color_manual(values=c("#CCEEF9", "#B2E5F5", "#8CD8F1", 
                                    "#59C7EB", "#1DB3E3", "#00A0DA", 
                                    "#008ECE" )) + 
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
    
    #B. Save Graph
    ggsave(paste(graph_path, "6_6_4--Average Mail Received if Married by Income.png", 
                 sep="\\"), graph.married.age.income, width=8, height=4.5) 

#6.7.1: Percent of Total Mail Received by Income
    
    #A. Generate as a percent
    inc.age.mean2 <- inc.age.mean %>%
        
        # Even numbered years + 2019
        filter(Year %% 2 == 1) %>%
        
        # Create Percent as a total of age cohort and year
        group_by(Age.Cohort, Year, .add=TRUE) %>% # grouped by two columns
        mutate(total = sum(Mean)) %>%
        mutate(Percent = round(100 * Mean/first(total), 2))
        
    #B. Graph
    graph.age.income.perc <- inc.age.mean2 %>%
        ggplot( aes(x=Year, y=Percent, fill=Income.Level)) +
        geom_bar(position="stack", stat="identity", width = 1.5) +
        scale_x_continuous(limits = c(2000, 2020) , 
                           breaks = (seq(2001, 2019, 2))) +
        scale_y_continuous(labels = function(x) paste0(x * 1, '%')) +
        ggtitle("Percent of Total Volume Received by Income and Generational Cohort") +
        ylab("% of Total Volume") + 
        scale_fill_manual(values=c("#01a2d9", "#adadad", "#8abbd0", "#efe8d1", "#FDDC21",
                                   "#91b8bd", "#014d64")) + 
        facet_grid(. ~ Age.Cohort) + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))   
    
    #B. Save Graph
    ggsave(paste(graph_path, "6_7_1--Percent of Total Volume Received by Income and Generational Cohort.png", 
                 sep="\\"), graph.age.income.perc, width=8, height=4.5) 
    
#6.7.2: Total Mail Received by Income
    
    #A. Graph
    graph.age.income <- inc.age.mean %>%
        ggplot( aes(x=Year, y=Mean, fill=Income.Level)) +
        geom_rect(data = subset(inc.age.mean, Age.Cohort == "18-24"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2005, xmax = 2014), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_rect(data = subset(inc.age.mean, Age.Cohort == "25-34"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2015, xmax = 2019), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_bar(position="stack", stat="identity", width = .5) +
        scale_x_continuous(limits = c(2000, 2020) , 
                           breaks = (seq(2001, 2019, 2))) +
        ggtitle("Total Volume Received by Income and Generational Cohort") +
        labs(color = "Income Level<sup>1</sup>", 
             caption = '<sup>1</sup>*Highlighted area indicates years cohorts are considered Millennials*') +
        ylab("Total Weekly Volume") + 
        scale_fill_manual(values=c("#01a2d9", "#adadad", "#8abbd0", "#efe8d1", "#FDDC21",
                                   "#91b8bd", "#014d64")) + 
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))  
    
    #B. Save Graph
    ggsave(paste(graph_path, "6_7_2--Total Volume Received by Income and Generational Cohort.png", 
                 sep="\\"), graph.age.income, width=8, height=4.5) 

#6.8.1: Average Mail Received by Race (with bars)
    
    #A. Reshape Data to include difference between means (White-Black)
    race.age.mean.wide <- reshape(race.age.mean, 
                                  v.names=c("Mean"), 
                                  timevar="Race", idvar=c("Year", "Age.Cohort"), 
                                  direction="wide") 
    race.age.mean.wide <- race.age.mean.wide %>%
        mutate(Diff = Mean.White - Mean.Black)
    
    #B. Graph with Bars
    graph.age.race.bars <- race.age.mean.wide %>%
        ggplot(aes(x=Year)) +
        geom_rect(data = subset(race.age.mean.wide, Age.Cohort == "18-24"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2005, xmax = 2014), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_rect(data = subset(race.age.mean.wide, Age.Cohort == "25-34"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2015, xmax = 2019), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_col(aes(y=Diff)) +
        geom_line(aes(y=Mean.White, color = "White"), size = 1 ) +
        geom_line(aes(y=Mean.Black, color = "Black"), size = 1) +
        geom_line(aes(y=Mean.Asian, color = "Asian"), size = 1) +
        geom_line(aes(y=Mean.Other, color = "Other<sup>2</sup>"), size = 1) +
        scale_color_manual(values = c(
            'White' = "#014d64", 
            'Black' = "#adadad",
            'Asian' = "#01a2d9",
            'Other<sup>2</sup>' = "#8abbd0")) +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(-11, 40), breaks = seq(-10, 40, by=5)) +
        ggtitle("Average Mail Received by Race") +
        ylab("Mean Volume Received") + 
        labs(color = 'Race<sup>1</sup>',
             caption = 'Note: Bars represent difference between White and Black respondents.<br><sup>1</sup>*Highlighted area indicates years cohorts are considered Millennials*<br><sup>2</sup>*Other includes: Hispanic, Native Americans, and those with 2 or more races*') +
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = ggtext::element_markdown(size = 7),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7,
                                                      color = "#7B7D7D"))  
    
    #C. Save Graph
    ggsave(paste(graph_path, "6_8_1--Average Mail Received by Race (bars).png", 
                 sep="\\"), graph.age.race.bars, width=8, height=4.5) 
    
    #E. Percent Change from the max value in a year by race
    df.perc.race.wide.max <- race.age.mean.wide %>%
        group_by(Age.Cohort, .add = T) %>%
        mutate(percent.change = round(((Mean.White - lag(Mean.White)) / lag(Mean.White))*100,2),
               First = head(Mean.White, 1),
               White.BaselineChange = 
                   case_when(Mean.White != First ~ (Mean.White - First) * 100 / First,
                             TRUE ~ 1 * NA),
               Maximum = max(Mean.White),
               Max.White.BaselineChange = 
                   case_when(Mean.White != Maximum ~ (Mean.White - Maximum) * 100 / Maximum,
                             TRUE ~ 1 * NA)) %>%
        mutate(percent.change = round(((Mean.Black - lag(Mean.Black)) / lag(Mean.Black))*100,2),
               First = head(Mean.Black, 1),
               Black.BaselineChange = 
                   case_when(Mean.Black != First ~ (Mean.Black - First) * 100 / First,
                             TRUE ~ 1 * NA),
               Maximum = max(Mean.Black),
               Max.Black.BaselineChange = 
                   case_when(Mean.Black != Maximum ~ (Mean.Black - Maximum) * 100 / Maximum,
                             TRUE ~ 1 * NA)) 
    
        ##1. Round to 2 decimal points 
        df.perc.race.wide.max$White.BaselineChange <- round(df.perc.race.wide.max$White.BaselineChange, 2)
        df.perc.race.wide.max$Max.White.BaselineChange <- round(df.perc.race.wide.max$Max.White.BaselineChange, 2)
        df.perc.race.wide.max$Black.BaselineChange <- round(df.perc.race.wide.max$Black.BaselineChange, 2)
        df.perc.race.wide.max$Max.Black.BaselineChange <- round(df.perc.race.wide.max$Max.Black.BaselineChange, 2)
        
        ##2. Replace NAs with zero
        df.perc.race.wide.max <- df.perc.race.wide.max %>% 
            mutate(White.BaselineChange = replace_na(White.BaselineChange, 0))
        df.perc.race.wide.max <- df.perc.race.wide.max %>% 
            mutate(Max.White.BaselineChange = replace_na(Max.White.BaselineChange, 0))
        df.perc.race.wide.max <- df.perc.race.wide.max %>% 
            mutate(Black.BaselineChange = replace_na(Black.BaselineChange, 0))
        df.perc.race.wide.max <- df.perc.race.wide.max %>% 
            mutate(Max.Black.BaselineChange = replace_na(Max.Black.BaselineChange, 0))
        
        View(df.perc.race.wide.max %>% 
                 dplyr::select(Year, Age.Cohort, Mean.White, Mean.Black,
                               Max.White.BaselineChange, Max.Black.BaselineChange,
                               White.BaselineChange, Black.BaselineChange) %>%
                 filter(Year==2019))

#6.8.2: Average Mail Received by Race (no bars)
    
    #A. Graph (No bars marking difference between White and Black)
    graph.age.race <- race.age.mean %>%
        ggplot( aes(x=Year, y=Mean, group=Race, color=Race)) +
        geom_rect(data = subset(race.age.mean, Age.Cohort == "18-24"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2005, xmax = 2014), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_rect(data = subset(race.age.mean, Age.Cohort == "25-34"), 
                  aes(ymin = -Inf, ymax = Inf, xmin = 2015, xmax = 2019), 
                  alpha = 0.1, fill = '#F2F3F4', color = NA) +
        geom_line(size = 1) +
        scale_x_continuous(limits = c(2000, 2020) , breaks = (seq(2000, 2020, 5))) +
        scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by=5)) +
        ggtitle("Average Mail Received by Race") +
        ylab("Mean Volume Received") + 
        labs(color = "Race<sup>1</sup>", 
             caption = 'Note: "Other" includes: Hispanic, Native Americans, and those with 2 or more races<br><sup>1</sup>*Highlighted area indicates years cohorts are considered Millennials*') +
        scale_color_manual(values=c("#01a2d9", "#adadad", "#8abbd0", "#014d64" )) + 
        facet_rep_grid(. ~ Age.Cohort, repeat.tick.labels = 'left') + 
        coord_capped_cart(bottom='both', left='both') +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_text(color="black", size=12, hjust = .5),
              axis.title.x = element_blank(), axis.title.y = element_text(size = 9),
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = ggtext::element_markdown(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))     
    
    #B. Save Graph
    ggsave(paste(graph_path, "6_8_2--Average Mail Received by Race.png", 
                 sep="\\"), graph.age.race, width=8, height=4.5)   
    
    #C. Percent Change from the year 2000 by race
    df.perc.race.2000 <- race.age.mean %>%
        group_by(Age.Cohort, Race, .add = T) %>%
        mutate(percent.change = round(((Mean - lag(Mean)) / lag(Mean))*100,2),
               First = head(Mean, 1),
               BaselineChange = 
                   case_when(Mean != First ~ (Mean - First) * 100 / First,
                             TRUE ~ 1 * NA)) 
    
        ##1. Round to 2 decimal points 
        df.perc.race.2000$BaselineChange <- round(df.perc.race.2000$BaselineChange, 2)
        
        ##2. Replace NAs with zero
        df.perc.race.2000 <- df.perc.race.2000 %>% 
            mutate(BaselineChange = replace_na(BaselineChange, 0))
        
    #D. Percent Change from the year 2009
    df.perc.race.2009 <- race.age.mean %>%
        filter(Year %in% (2009:2019)) %>%
        group_by(Age.Cohort, Race, .add = T) %>%
        mutate(percent.change = round(((Mean - lag(Mean)) / lag(Mean))*100,2),
               First = head(Mean, 1),
               BaselineChange = 
                   case_when(Mean != First ~ (Mean - First) * 100 / First,
                             TRUE ~ 1 * NA)) 
        
        ##1. Round to 2 decimal points 
        df.perc.race.2009$BaselineChange <- round(df.perc.race.2009$BaselineChange, 2)
        
        ##2. Replace NAs with zero
        df.perc.race.2009 <- df.perc.race.2009 %>% 
            mutate(BaselineChange = replace_na(BaselineChange, 0))    
    
    #E. Percent Change from the max value in a year by race
    df.perc.race.max <- race.age.mean %>%
        group_by(Age.Cohort, Race, .add = T) %>%
        mutate(percent.change = round(((Mean - lag(Mean)) / lag(Mean))*100,2),
               First = head(Mean, 1),
               BaselineChange = 
                   case_when(Mean != First ~ (Mean - First) * 100 / First,
                             TRUE ~ 1 * NA),
               Maximum = max(Mean),
               Max.BaselineChange = 
                   case_when(Mean != Maximum ~ (Mean - Maximum) * 100 / Maximum,
                             TRUE ~ 1 * NA)) 
        
        ##1. Round to 2 decimal points 
        df.perc.race.max$BaselineChange <- round(df.perc.race.max$BaselineChange, 2)
        df.perc.race.max$Max.BaselineChange <- round(df.perc.race.max$Max.BaselineChange, 2)
        
        ##2. Replace NAs with zero
        df.perc.race.max <- df.perc.race.max %>% 
            mutate(BaselineChange = replace_na(BaselineChange, 0))
        df.perc.race.max <- df.perc.race.max %>% 
            mutate(Max.BaselineChange = replace_na(Max.BaselineChange, 0))