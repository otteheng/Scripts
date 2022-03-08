# -> Gerhard O
# -> 1/19/2021
# -> Data sets: 2e_HDS

# Load Packages in Library
source('C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Script\\_Package-Library.R')

# Initial Data Sets
raw_data_path_2e <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2e_HDS--Cleaned data"

# Clean Data
graph_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Tables & Figures\\2g_HDS"

#------------------------------------------------------------------------#
#                               Run Regressions                          #
#               Testing Hypothesis from Meeting with Peter 1/19/2021     #
#------------------------------------------------------------------------#

# Load volume data (RDA)
load(file = paste(raw_data_path_2e, "2e_volume-rects-new_names.rda", sep = '\\')) 

#------------------------------------------------------------------------#
#                      Create Separate DFs for Years                     #
#------------------------------------------------------------------------#

#1.1: 2005 & 2014

    #A: Year 2005
    same05 <- vol.new.names %>% filter(SURVEY.YEAR==2005, 
                                          AGE.COHORT=="18-24" | 
                                          AGE.COHORT=="25-34" |
                                          AGE.COHORT=="45-54")

    #B: Year 2005
    same14 <- vol.new.names %>% filter(SURVEY.YEAR==2014, 
                                          AGE.COHORT=="18-24" | 
                                          AGE.COHORT=="25-34" |
                                          AGE.COHORT=="45-54")
    
#------------------------------------------------------------------------#
#                             Dummy Variables                            #
#------------------------------------------------------------------------#    

# 2.1: Dummy Vars for 2005
    
    #A. Age variables 
    same05$DUMMY.18.24 <- ifelse(same05$AGE.COHORT == "18-24" , 1,0)  
    same05$DUMMY.25.34 <- ifelse(same05$AGE.COHORT == "25-34" , 1,0)  
    same05$DUMMY.45.54 <- ifelse(same05$AGE.COHORT == "45-54" , 1,0) 
    
    #B. Children
    same05$DUMMY.CHILD <- ifelse(same05$NUM.UNDER18 > 0, 1,0)
    
    #C. Married
    same05$DUMMY.MARRIED <- ifelse(same05$MARITAL.STATUS == "Married", 1,0)
    
    #D. Household
    same05$DUMMY.HOUSE  <- ifelse(same05$HOME.OWNER == "Own", 1,0)
    
    #E. Gender
    same05$DUMMY.MALE <- ifelse(same05$GENDER == "Male", 1, 0)
    same05$DUMMY.FEMALE <- ifelse(same05$GENDER == "Female", 1, 0)
    
    #F. Race
    same05$DUMMY.BLACK <- ifelse(same05$RACE == "Black", 1, 0)
    same05$DUMMY.WHITE <- ifelse(same05$RACE == "White", 1, 0)
    same05$DUMMY.OTHER <- ifelse(same05$RACE=="Native Hawaiian or Other Paciric Islander" | 
                                 same05$RACE=="American Indian or Alaska Native" | 
                                 same05$RACE=="Asian" | same05$RACE=="Other" | 
                                 same05$RACE=="Hispanic", 1, 0)
    
# 2.2: Dummy Vars for 2014
    
    #A. Age variables 
    same14$DUMMY.18.24 <- ifelse(same14$AGE.COHORT == "18-24" , 1,0)  
    same14$DUMMY.25.34 <- ifelse(same14$AGE.COHORT == "25-34" , 1,0)  
    same14$DUMMY.45.54 <- ifelse(same14$AGE.COHORT == "45-54" , 1,0) 
    
    #B. Children
    same14$DUMMY.CHILD <- ifelse(same14$NUM.UNDER18 > 0, 1,0)
    
    #C. Married
    same14$DUMMY.MARRIED <- ifelse(same14$MARITAL.STATUS == "Married", 1,0)
    
    #D. Household
    same14$DUMMY.HOUSE  <- ifelse(same14$HOME.OWNER == "Own", 1,0)
    
    #E. Gender
    same14$DUMMY.MALE <- ifelse(same14$GENDER == "Male", 1, 0)
    same14$DUMMY.FEMALE <- ifelse(same14$GENDER == "Female", 1, 0)
    
    #F. Race
    same14$DUMMY.BLACK <- ifelse(same14$RACE == "Black", 1, 0)
    same14$DUMMY.WHITE <- ifelse(same14$RACE == "White", 1, 0)
    same14$DUMMY.OTHER <- ifelse(same14$RACE=="Native Hawaiian or Other Paciric Islander" | 
                                   same14$RACE=="American Indian or Alaska Native" | 
                                   same14$RACE=="Asian" | same14$RACE=="Other" | 
                                   same14$RACE=="Hispanic", 1, 0)

#------------------------------------------------------------------------#
#                             Variable Changes                           #
#------------------------------------------------------------------------#  
    
#3.1: Restructure variables
    
    #A. Education 
    same05 <- same05 %>%
      mutate(EDUC.RESTRUC = case_when(
        EDUCATION=="8th grade or less" | EDUCATION == "Some high school" |
        EDUCATION=="High school graduate" ~ "HS or Less", 
        EDUCATION=="Some college" | EDUCATION=="Technical school graduate" ~ 
          "Some College",
        EDUCATION=="College graduate" | EDUCATION== "Post-graduate work" ~ 
          "BS or Higher"
      )) %>%
      
    #B. Race
      mutate(RACE.RESTRUC = case_when(
        RACE=="Native Hawaiian or Other Paciric Islander" | 
          RACE=="American Indian or Alaska Native" | RACE=="Asian" | 
          RACE=="Other" | RACE=="Hispanic" ~ "Other",
        RACE=="White" ~ "White", 
        RACE=="Black" ~ "Black"
      ))
    same05$EDUC.RESTRUC <- as.factor(same05$EDUC.RESTRUC)
    same05$RACE.RESTRUC <- as.factor(same05$RACE.RESTRUC)
    
    #A. Education
    same14 <- same14 %>%
      mutate(EDUC.RESTRUC = case_when(
        EDUCATION=="8th grade or less" | EDUCATION == "Some high school" |
        EDUCATION=="High school graduate" ~ "HS or Less", 
        EDUCATION=="Some college" | EDUCATION=="Technical school graduate" ~ 
          "Some College",
        EDUCATION=="College graduate" | EDUCATION== "Post-graduate work" ~ 
          "BS or Higher"
      )) %>%
      
      #B. Race
      mutate(RACE.RESTRUC = case_when(
        RACE=="Native Hawaiian or Other Paciric Islander" | 
          RACE=="American Indian or Alaska Native" | RACE=="Asian" | 
          RACE=="Other" | RACE=="Hispanic" ~ "Other",
        RACE=="White" ~ "White", 
        RACE=="Black" ~ "Black"
      ))
    same14$EDUC.RESTRUC <- as.factor(same14$EDUC.RESTRUC)
    same14$RACE.RESTRUC <- as.factor(same14$RACE.RESTRUC)
    
#3.2: Refactor
    
    #A. Race
    same05$RACE.RESTRUC <- relevel(same05$RACE.RESTRUC, "White")
    same14$RACE.RESTRUC <- relevel(same14$RACE.RESTRUC, "White")
    
    #B. Education
    same05$EDUC.RESTRUC <- relevel(same05$EDUC.RESTRUC, "HS or Less")
    same14$EDUC.RESTRUC <- relevel(same14$EDUC.RESTRUC, "HS or Less")
    
#3.3: Data Check
    
    #A. Education
    
        ##1. Data set 2005
        table(same05$EDUCATION) # Over sample on "BS or Higher." Check for weights
        table(same05$EDUC.RESTRUC) # case_when worked. Numbers add up. 
        
        ##2. Data set 2014
        table(same14$EDUCATION) # Over sample even greater on "BS or Higher"
        table(same14$EDUC.RESTRUC) # case_when worked. Numbers add up. 
        
    #B. Race
        
        ##1. Data set 2005
        table(same05$RACE)
        table(same05$RACE.RESTRUC)
        
        ##2. Data set 2014
        table(same14$RACE)
        table(same14$RACE.RESTRUC)
    
    
#------------------------------------------------------------------------#
#                         Regressions (Unweighted)                       #
#------------------------------------------------------------------------#      
   
#4.1: OLS w/o interaction terms
    
    #A. Reg1
    reg1.05 <- lm(data = same05, log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34 + DUMMY.45.54 + DUMMY.CHILD + DUMMY.MARRIED + 
                    DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC)
    summary(reg1.05)
    
    #B. Reg2
    reg1.14 <- lm(data = same14, log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34 + DUMMY.45.54 + DUMMY.CHILD + DUMMY.MARRIED + 
                    DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC )
    summary(reg1.14)
    
    #C. Reg3 (Add First class mail sent and Packages sent)
    reg1.sent.05 <- lm(data = same05, log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34 + DUMMY.45.54 + DUMMY.CHILD + DUMMY.MARRIED + 
                    DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC +
                    MAIL.VOLUME.FCMS + MAIL.VOLUME.PKGS)
    summary(reg1.sent.05)
    
    #D. Reg4 (Add First class mail sent and Packages sent)
    reg1.sent.14 <- lm(data = same14, log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34 + DUMMY.45.54 + DUMMY.CHILD + DUMMY.MARRIED + 
                    DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC +
                    MAIL.VOLUME.FCMS + MAIL.VOLUME.PKGS)
    summary(reg1.sent.14)
    
#4.2: OLS with interaction terms
    
    #A. Reg1
    reg2.05 <- lm(data = same05, log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34*DUMMY.CHILD + DUMMY.45.54*DUMMY.CHILD + 
                    DUMMY.25.34*DUMMY.MARRIED + DUMMY.45.54*DUMMY.MARRIED +
                    DUMMY.25.34*DUMMY.HOUSE + DUMMY.45.54*DUMMY.HOUSE + 
                    DUMMY.25.34*DUMMY.MALE + DUMMY.45.54*DUMMY.MALE + 
                    DUMMY.25.34*REGION + DUMMY.45.54*REGION +
                    DUMMY.25.34*RACE.RESTRUC + DUMMY.45.54*RACE.RESTRUC + 
                    DUMMY.25.34*EDUC.RESTRUC + DUMMY.45.54*EDUC.RESTRUC)
    summary(reg2.05)
    
    #B. Reg2
    reg2.14 <- lm(data = same14, log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34*DUMMY.CHILD + DUMMY.45.54*DUMMY.CHILD + 
                    DUMMY.25.34*DUMMY.MARRIED + DUMMY.45.54*DUMMY.MARRIED +
                    DUMMY.25.34*DUMMY.HOUSE + DUMMY.45.54*DUMMY.HOUSE + 
                    DUMMY.25.34*DUMMY.MALE + DUMMY.45.54*DUMMY.MALE + 
                    DUMMY.25.34*REGION + DUMMY.45.54*REGION +
                    DUMMY.25.34*RACE.RESTRUC + DUMMY.45.54*RACE.RESTRUC + 
                    DUMMY.25.34*EDUC.RESTRUC + DUMMY.45.54*EDUC.RESTRUC)
    summary(reg2.14)  
    
    #C. Reg3 (Add First class mail sent and Packages sent)
    reg2.sent.05 <- lm(data = same05, log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34*DUMMY.CHILD + DUMMY.45.54*DUMMY.CHILD + 
                    DUMMY.25.34*DUMMY.MARRIED + DUMMY.45.54*DUMMY.MARRIED +
                    DUMMY.25.34*DUMMY.HOUSE + DUMMY.45.54*DUMMY.HOUSE + 
                    DUMMY.25.34*DUMMY.MALE + DUMMY.45.54*DUMMY.MALE + 
                    DUMMY.25.34*REGION + DUMMY.45.54*REGION +
                    DUMMY.25.34*RACE.RESTRUC + DUMMY.45.54*RACE.RESTRUC + 
                    DUMMY.25.34*EDUC.RESTRUC + DUMMY.45.54*EDUC.RESTRUC + 
                    DUMMY.25.34*MAIL.VOLUME.FCMS + DUMMY.45.54*MAIL.VOLUME.FCMS +
                    DUMMY.25.34*MAIL.VOLUME.PKGS + DUMMY.45.54*MAIL.VOLUME.PKGS)
    summary(reg2.sent.05)
    
    #D. Reg4 (Add First class mail sent and Packages sent)
    reg2.sent.14 <- lm(data = same14, log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34*DUMMY.CHILD + DUMMY.45.54*DUMMY.CHILD + 
                    DUMMY.25.34*DUMMY.MARRIED + DUMMY.45.54*DUMMY.MARRIED +
                    DUMMY.25.34*DUMMY.HOUSE + DUMMY.45.54*DUMMY.HOUSE + 
                    DUMMY.25.34*DUMMY.MALE + DUMMY.45.54*DUMMY.MALE + 
                    DUMMY.25.34*REGION + DUMMY.45.54*REGION +
                    DUMMY.25.34*RACE.RESTRUC + DUMMY.45.54*RACE.RESTRUC + 
                    DUMMY.25.34*EDUC.RESTRUC + DUMMY.45.54*EDUC.RESTRUC +
                    DUMMY.25.34*MAIL.VOLUME.FCMS + DUMMY.45.54*MAIL.VOLUME.FCMS +
                    DUMMY.25.34*MAIL.VOLUME.PKGS + DUMMY.45.54*MAIL.VOLUME.PKGS)
    summary(reg2.sent.14)

#------------------------------------------------------------------------#
#                         Regressions (Weighted)                         #
#------------------------------------------------------------------------#  
    
#5.1: OLS w/o interaction terms (Weighted = Respondent)
    
    #A. Reg1
    reg1.w.r.05 <- lm(data = same05, weights = same05$RECRUITMENT.WEIGHT,
                    log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34 + DUMMY.45.54 + DUMMY.CHILD + DUMMY.MARRIED + 
                    DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC)
    summary(reg1.w.r.05)
    
    #B. Reg2
    reg1.w.r.14 <- lm(data = same14, weights = same14$RECRUITMENT.WEIGHT, 
                    log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34 + DUMMY.45.54 + DUMMY.CHILD + DUMMY.MARRIED + 
                    DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC )
    summary(reg1.w.r.14)  
    
    #C. Reg3 (Add First class mail sent and Packages sent)
    reg1.w.r.sent.05 <- lm(data = same05, weights = same05$RECRUITMENT.WEIGHT,
                         log1p(MAIL.VOLUME.SUM) ~ 
                         DUMMY.25.34 + DUMMY.45.54 + DUMMY.CHILD + DUMMY.MARRIED + 
                         DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC +
                         MAIL.VOLUME.FCMS + MAIL.VOLUME.PKGS)
    summary(reg1.w.r.sent.05)
    
    #D. Reg4 (Add First class mail sent and Packages sent)
    reg1.w.r.sent.14 <- lm(data = same14, weights = same14$RECRUITMENT.WEIGHT,
                         log1p(MAIL.VOLUME.SUM) ~ 
                         DUMMY.25.34 + DUMMY.45.54 + DUMMY.CHILD + DUMMY.MARRIED + 
                         DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC +
                         MAIL.VOLUME.FCMS + MAIL.VOLUME.PKGS)
    summary(reg1.w.r.sent.14)

#5.2: OLS with interaction terms (Weighted = Respondent)
    
    #A. Reg1
    reg2.w.r.05 <- lm(data = same05, weights = same05$RECRUITMENT.WEIGHT,
                  log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34*DUMMY.CHILD + DUMMY.45.54*DUMMY.CHILD + 
                    DUMMY.25.34*DUMMY.MARRIED + DUMMY.45.54*DUMMY.MARRIED +
                    DUMMY.25.34*DUMMY.HOUSE + DUMMY.45.54*DUMMY.HOUSE + 
                    DUMMY.25.34*DUMMY.MALE + DUMMY.45.54*DUMMY.MALE + 
                    DUMMY.25.34*REGION + DUMMY.45.54*REGION +
                    DUMMY.25.34*RACE.RESTRUC + DUMMY.45.54*RACE.RESTRUC + 
                    DUMMY.25.34*EDUC.RESTRUC + DUMMY.45.54*EDUC.RESTRUC)
    summary(reg2.w.r.05)
    
    #B. Reg2
    reg2.w.r.14 <- lm(data = same14, weights = same14$RECRUITMENT.WEIGHT, 
                  log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34*DUMMY.CHILD + DUMMY.45.54*DUMMY.CHILD + 
                    DUMMY.25.34*DUMMY.MARRIED + DUMMY.45.54*DUMMY.MARRIED +
                    DUMMY.25.34*DUMMY.HOUSE + DUMMY.45.54*DUMMY.HOUSE + 
                    DUMMY.25.34*DUMMY.MALE + DUMMY.45.54*DUMMY.MALE + 
                    DUMMY.25.34*REGION + DUMMY.45.54*REGION +
                    DUMMY.25.34*RACE.RESTRUC + DUMMY.45.54*RACE.RESTRUC + 
                    DUMMY.25.34*EDUC.RESTRUC + DUMMY.45.54*EDUC.RESTRUC)
    summary(reg2.w.r.14)   
    
    #C. Reg3 (Add First class mail sent and Packages sent)
    reg2.w.r.sent.05 <- lm(data = same05, weights = same05$RECRUITMENT.WEIGHT,
                      log1p(MAIL.VOLUME.SUM) ~ 
                        DUMMY.25.34*DUMMY.CHILD + DUMMY.45.54*DUMMY.CHILD + 
                        DUMMY.25.34*DUMMY.MARRIED + DUMMY.45.54*DUMMY.MARRIED +
                        DUMMY.25.34*DUMMY.HOUSE + DUMMY.45.54*DUMMY.HOUSE + 
                        DUMMY.25.34*DUMMY.MALE + DUMMY.45.54*DUMMY.MALE + 
                        DUMMY.25.34*REGION + DUMMY.45.54*REGION +
                        DUMMY.25.34*RACE.RESTRUC + DUMMY.45.54*RACE.RESTRUC + 
                        DUMMY.25.34*EDUC.RESTRUC + DUMMY.45.54*EDUC.RESTRUC +
                        DUMMY.25.34*MAIL.VOLUME.FCMS + DUMMY.45.54*MAIL.VOLUME.FCMS +
                        DUMMY.25.34*MAIL.VOLUME.PKGS + DUMMY.45.54*MAIL.VOLUME.PKGS)
    summary(reg2.w.r.sent.05)
    
    #D. Reg4 (Add First class mail sent and Packages sent)
    reg2.w.r.sent.14 <- lm(data = same14, weights = same14$RECRUITMENT.WEIGHT, 
                      log1p(MAIL.VOLUME.SUM) ~ 
                        DUMMY.25.34*DUMMY.CHILD + DUMMY.45.54*DUMMY.CHILD + 
                        DUMMY.25.34*DUMMY.MARRIED + DUMMY.45.54*DUMMY.MARRIED +
                        DUMMY.25.34*DUMMY.HOUSE + DUMMY.45.54*DUMMY.HOUSE + 
                        DUMMY.25.34*DUMMY.MALE + DUMMY.45.54*DUMMY.MALE + 
                        DUMMY.25.34*REGION + DUMMY.45.54*REGION +
                        DUMMY.25.34*RACE.RESTRUC + DUMMY.45.54*RACE.RESTRUC + 
                        DUMMY.25.34*EDUC.RESTRUC + DUMMY.45.54*EDUC.RESTRUC +
                        DUMMY.25.34*MAIL.VOLUME.FCMS + DUMMY.45.54*MAIL.VOLUME.FCMS +
                        DUMMY.25.34*MAIL.VOLUME.PKGS + DUMMY.45.54*MAIL.VOLUME.PKGS)
    summary(reg2.w.r.sent.14)  
    
#------------------------------------------------------------------------#
#                        Export Tables/Figures                           #
#                                                                        #
#------------------------------------------------------------------------#     
    
#6.1: Summary Stats
    
    #A. Example
    stargazer(attitude, type = 'html', digits = 2, 
              out = paste(graph_path, 'example.doc', sep = '\\'))
    
    #B. 2005
    stargazer(same05[, c("DUMMY.18.24", "DUMMY.25.34", "DUMMY.45.54", "DUMMY.CHILD",
                         "DUMMY.MARRIED", "DUMMY.HOUSE", "DUMMY.MALE",
                         "DUMMY.BLACK", "DUMMY.WHITE", "DUMMY.OTHER",
                         "MAIL.VOLUME.FCMS", "MAIL.VOLUME.PKGS")], 
              type = 'html', digits = 2, title = "Table 1.1: Summary Stats 2005",
              out = paste(graph_path, 'Sum Stats 1.1 (2005).doc', sep = '\\'))
    
    #C. 2014
    stargazer(same14[, c("DUMMY.18.24", "DUMMY.25.34", "DUMMY.45.54", "DUMMY.CHILD",
                         "DUMMY.MARRIED", "DUMMY.HOUSE", "DUMMY.MALE",
                         "DUMMY.BLACK", "DUMMY.WHITE", "DUMMY.OTHER",
                         "MAIL.VOLUME.FCMS", "MAIL.VOLUME.PKGS")], 
              type = 'html', digits = 2, title = "Table 1.1: Summary Stats 2014",
              out = paste(graph_path, 'Sum Stats 1.2 (2014).doc', sep = '\\'))
    

#6.2: Residuals Graph
    
    #A. Normality of Residuals
    
        ##1. Change Margins
        par(mar=c(1,1,1,1))
        
        ##2. Graphs
        qqPlot(reg1.05, main="QQ Plot: 2005 (no interaction, unweighted)") # qq plot for studentized resid
        qqPlot(reg1.14, main="QQ Plot: 2014 (no interaction, unweighted)")
        qqPlot(reg2.05, main="QQ Plot: 2005 (Interaction, unweighted)")
        qqPlot(reg2.14, main="QQ Plot: 2014 (Interaction, unweighted)")
    
    #B. Distribution of studentized residuals
        
        ##1. 2005 (no interation, unweighted) 
        sresid <- studres(reg1.05)
        hist(sresid, freq=FALSE,
             main="Dist Resid 2005 (no interation, unweighted)")
        xfit<-seq(min(sresid),max(sresid),length=40)
        yfit<-dnorm(xfit)
        lines(xfit, yfit)
    
        ##2. 2014 (no interation, unweighted) 
        sresid <- studres(reg1.14)
        hist(sresid, freq=FALSE,
             main="Dist Resid 2014 (no interation, unweighted)")
        xfit<-seq(min(sresid),max(sresid),length=40)
        yfit<-dnorm(xfit)
        lines(xfit, yfit)
        
        ##3. 2005 (Interaction, unweighted) 
        sresid <- studres(reg2.05)
        hist(sresid, freq=FALSE,
             main="Dist Resid 2005 (Interaction, unweighted) ")
        xfit<-seq(min(sresid),max(sresid),length=40)
        yfit<-dnorm(xfit)
        lines(xfit, yfit)
        
        ##4. 2014 (Interaction, unweighted) 
        sresid <- studres(reg2.14)
        hist(sresid, freq=FALSE,
             main="Dist Resid 2014 (Interaction, unweighted)")
        xfit<-seq(min(sresid),max(sresid),length=40)
        yfit<-dnorm(xfit)
        lines(xfit, yfit)
        
#6.3: Regression Tables
    
    #A. OLS with interaction tables
    stargazer(reg1.05, reg1.14, reg2.05, reg2.14, type = 'html', 
              title = 'Table 3: Regression Results (Unweighted)', 
              column.labels = c("2005", "2014", "2005", "2014"), 
              dep.var.labels=c("Log of Total Mail Volume"),
              omit.stat=c("LL","ser"), no.space = TRUE, 
              covariate.labels = c("Cohort 25-34", "Cohort 45-54", 
                        "Children", "Married", "Home Owner",
                        "Male", "Region: Northeast", "Region: South",
                        "Region: West", "Race: Black", "Race: Other",
                        "BS or Higher", "Some College", 
                        "Cohort 25-34*Children", "Cohort 45-54*Children",
                        "Cohort 25-34*Married", "Cohort 45-54*Married",
                        "Cohort 25-34*Home Owner", "Cohort 45-54*Home Owner",
                        "Cohort 25-34*Male", "Cohort 45-54*Male",
                        "Cohort 25-34*Northeast", "Cohort 25-34*South",
                        "Cohort 25-34*West", "Cohort 45-54*Northeast",
                        "Cohort 45-54*South", "Cohort 45-54*West",
                        "Cohort 25-34*Black", "Cohort 25-34*Other",
                        "Cohort 45-54*Black", "Cohort 45-54*Other",
                        "Cohort 25-34*BS or Higher", "Cohort 25-34*Some College",
                        "Cohort 45-54*BS or Higher", "Cohort 45-54*Some College"
                                    ),
              out = paste(graph_path, 'Table 3 (Reg)(labeled).doc', sep = '\\') )
    
    #B. OLS with interaction tables (weighted: respondent)
    stargazer(reg1.w.r.05, reg1.w.r.14, reg2.w.r.05, reg2.w.r.14, type = 'html', 
              title = 'Table 4: Regression Results (Weight:Respondent)', 
              column.labels = c("2005", "2014", "2005", "2014"), 
              dep.var.labels=c("Log of Total Mail Volume"),
              omit.stat=c("LL","ser"), no.space = TRUE, 
              covariate.labels = c("Cohort 25-34", "Cohort 45-54", 
                       "Children", "Married", "Home Owner",
                       "Male", "Region: Northeast", "Region: South",
                       "Region: West", "Race: Black", "Race: Other",
                       "BS or Higher", "Some College", 
                       "Cohort 25-34*Children", "Cohort 45-54*Children",
                       "Cohort 25-34*Married", "Cohort 45-54*Married",
                       "Cohort 25-34*Home Owner", "Cohort 45-54*Home Owner",
                       "Cohort 25-34*Male", "Cohort 45-54*Male",
                       "Cohort 25-34*Northeast", "Cohort 25-34*South",
                       "Cohort 25-34*West", "Cohort 45-54*Northeast",
                       "Cohort 45-54*South", "Cohort 45-54*West",
                       "Cohort 25-34*Black", "Cohort 25-34*Other",
                       "Cohort 45-54*Black", "Cohort 45-54*Other",
                       "Cohort 25-34*BS or Higher", "Cohort 25-34*Some College",
                       "Cohort 45-54*BS or Higher", "Cohort 45-54*Some College"
              ),
              out = paste(graph_path, 'Table 4 (Reg)(labeled)(weighted-Respond).doc', sep = '\\') )
    
    #C. OLS with interaction tables 
    #   (Includes First Class Mail Sent and Packages Sent)
    stargazer(reg1.sent.05, reg1.sent.14, reg2.sent.05, reg2.sent.14, type = 'html', 
              title = 'Table 5: Regression Results (Unweighted) (Mail Sent)', 
              column.labels = c("2005", "2014", "2005", "2014"), 
              dep.var.labels=c("Log of Total Mail Volume"),
              omit.stat=c("LL","ser"), no.space = TRUE, 
              covariate.labels = c("Cohort 25-34", "Cohort 45-54", 
                       "Children", "Married", "Home Owner",
                       "Male", "Region: Northeast", "Region: South",
                       "Region: West", "Race: Black", "Race: Other",
                       "BS or Higher", "Some College", "FCM Sent",
                       "Packages Sent",
                       "Cohort 25-34*Children", "Cohort 45-54*Children",
                       "Cohort 25-34*Married", "Cohort 45-54*Married",
                       "Cohort 25-34*Home Owner", "Cohort 45-54*Home Owner",
                       "Cohort 25-34*Male", "Cohort 45-54*Male",
                       "Cohort 25-34*Northeast", "Cohort 25-34*South",
                       "Cohort 25-34*West", "Cohort 45-54*Northeast",
                       "Cohort 45-54*South", "Cohort 45-54*West",
                       "Cohort 25-34*Black", "Cohort 25-34*Other",
                       "Cohort 45-54*Black", "Cohort 45-54*Other",
                       "Cohort 25-34*BS or Higher", "Cohort 25-34*Some College",
                       "Cohort 45-54*BS or Higher", "Cohort 45-54*Some College",
                       "Cohort 25-34*FCM Sent", "Cohort 45-54*FCM Sent",
                       "Cohort 25-34*Packages Sent", "Cohort 45-54*Packages Sent"
              ),
              out = paste(graph_path, 'Table 5 (Reg)(labeled)(Mail Sent).doc', sep = '\\') )

    
    #D. OLS with interaction tables (weighted: respondent)
    #   (Includes First Class Mail Sent and Packages Sent)
    stargazer(reg1.w.r.sent.05, reg1.w.r.sent.14, 
              reg2.w.r.sent.05, reg2.w.r.sent.14, type = 'html', 
              title = 'Table 6: Regression Results (Weight:Respondent) (Mail Sent)', 
              column.labels = c("2005", "2014", "2005", "2014"), 
              dep.var.labels=c("Log of Total Mail Volume"),
              omit.stat=c("LL","ser"), no.space = TRUE, 
              covariate.labels = c("Cohort 25-34", "Cohort 45-54", 
                       "Children", "Married", "Home Owner",
                       "Male", "Region: Northeast", "Region: South",
                       "Region: West", "Race: Black", "Race: Other",
                       "BS or Higher", "Some College", "FCM Sent",
                       "Packages Sent",
                       "Cohort 25-34*Children", "Cohort 45-54*Children",
                       "Cohort 25-34*Married", "Cohort 45-54*Married",
                       "Cohort 25-34*Home Owner", "Cohort 45-54*Home Owner",
                       "Cohort 25-34*Male", "Cohort 45-54*Male",
                       "Cohort 25-34*Northeast", "Cohort 25-34*South",
                       "Cohort 25-34*West", "Cohort 45-54*Northeast",
                       "Cohort 45-54*South", "Cohort 45-54*West",
                       "Cohort 25-34*Black", "Cohort 25-34*Other",
                       "Cohort 45-54*Black", "Cohort 45-54*Other",
                       "Cohort 25-34*BS or Higher", "Cohort 25-34*Some College",
                       "Cohort 45-54*BS or Higher", "Cohort 45-54*Some College",
                       "Cohort 25-34*FCM Sent", "Cohort 45-54*FCM Sent",
                       "Cohort 25-34*Packages Sent", "Cohort 45-54*Packages Sent"
              ),
              out = paste(graph_path, 'Table 6 (Reg)(labeled)(weighted-Respond)(Mail Sent).doc', sep = '\\') )
    