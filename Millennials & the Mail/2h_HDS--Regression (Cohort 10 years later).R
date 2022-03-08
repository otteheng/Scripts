# -> Gerhard O
# -> 1/29/2021
# -> Data sets: 2e_HDS

# Load Packages in Library
source('C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Script\\_Package-Library.R')

# Initial Data Sets
raw_data_path_2e <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2e_HDS--Cleaned data"

# Clean Data
graph_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Tables & Figures\\2h_HDS"

#------------------------------------------------------------------------#
#                               Run Regressions                          #
#                       From Meeting with Peter 1/29/2021                #
#------------------------------------------------------------------------#

# Load volume data (RDA)
load(file = paste(raw_data_path_2e, "2e_volume-rects-new_names.rda", sep = '\\')) 


#------------------------------------------------------------------------#
#                      Create Separate DFs for Years                     #
#------------------------------------------------------------------------#

#1.1: 2005 & 2015 (Same Aged but 10 years in the future)
#     *25-34 in 2015 doesn't fit 100% due to different size of 18-24 bracket

    #A: Year 2005
    diff05 <- vol.new.names %>% 
    filter(SURVEY.YEAR==2005 & (AGE.COHORT=="45-54" |
                                AGE.COHORT=="25-34" |
                                AGE.COHORT=="18-24"))  

    #B: Year 2015
    diff15 <- vol.new.names %>% 
    filter(SURVEY.YEAR==2015 & (AGE.COHORT=="55-64" | 
                                AGE.COHORT=="35-44" |
                                AGE.COHORT=="25-34")) 
    
#------------------------------------------------------------------------#
#                             Dummy Variables                            #
#------------------------------------------------------------------------#    
    
# 2.1: Dummy Vars for 2005
    
    #A. Age variables 
    diff05$DUMMY.18.24 <- ifelse(diff05$AGE.COHORT == "18-24" , 1,0)  
    diff05$DUMMY.25.34 <- ifelse(diff05$AGE.COHORT == "25-34" , 1,0)  
    diff05$DUMMY.45.54 <- ifelse(diff05$AGE.COHORT == "45-54" , 1,0) 
    
    #B. Children
    diff05$DUMMY.CHILD <- ifelse(diff05$NUM.UNDER18 > 0, 1,0)
    
    #C. Married
    diff05$DUMMY.MARRIED <- ifelse(diff05$MARITAL.STATUS == "Married", 1,0)
    
    #D. Household
    diff05$DUMMY.HOUSE  <- ifelse(diff05$HOME.OWNER == "Own", 1,0)
    
    #E. Gender
    diff05$DUMMY.MALE <- ifelse(diff05$GENDER == "Male", 1, 0)
    diff05$DUMMY.FEMALE <- ifelse(diff05$GENDER == "Female", 1, 0)
    
    #F. Race
    diff05$DUMMY.BLACK <- ifelse(diff05$RACE == "Black", 1, 0)
    diff05$DUMMY.WHITE <- ifelse(diff05$RACE == "White", 1, 0)
    diff05$DUMMY.OTHER <- ifelse(diff05$RACE=="Native Hawaiian or Other Paciric Islander" | 
                                   diff05$RACE=="American Indian or Alaska Native" | 
                                   diff05$RACE=="Asian" | diff05$RACE=="Other" | 
                                   diff05$RACE=="Hispanic", 1, 0)    

# 2.2: Dummy Vars for 2015
    
    #A. Age variables 
    diff15$DUMMY.55.64 <- ifelse(diff15$AGE.COHORT == "55-64" , 1,0)  
    diff15$DUMMY.35.44 <- ifelse(diff15$AGE.COHORT == "35-44" , 1,0)  
    diff15$DUMMY.25.34 <- ifelse(diff15$AGE.COHORT == "25-34" , 1,0) 
    
    #B. Children
    diff15$DUMMY.CHILD <- ifelse(diff15$NUM.UNDER18 > 0, 1,0)
    
    #C. Married
    diff15$DUMMY.MARRIED <- ifelse(diff15$MARITAL.STATUS == "Married", 1,0)
    
    #D. Household
    diff15$DUMMY.HOUSE  <- ifelse(diff15$HOME.OWNER == "Own", 1,0)
    
    #E. Gender
    diff15$DUMMY.MALE <- ifelse(diff15$GENDER == "Male", 1, 0)
    diff15$DUMMY.FEMALE <- ifelse(diff15$GENDER == "Female", 1, 0)
    
    #F. Race
    diff15$DUMMY.BLACK <- ifelse(diff15$RACE == "Black", 1, 0)
    diff15$DUMMY.WHITE <- ifelse(diff15$RACE == "White", 1, 0)
    diff15$DUMMY.OTHER <- ifelse(diff15$RACE=="Native Hawaiian or Other Paciric Islander" | 
                                   diff15$RACE=="American Indian or Alaska Native" | 
                                   diff15$RACE=="Asian" | diff15$RACE=="Other" | 
                                   diff15$RACE=="Hispanic", 1, 0)    
    
#------------------------------------------------------------------------#
#                             Variable Changes                           #
#------------------------------------------------------------------------#  
    
#3.1: Restructure variables
    
    #A. Education 
    diff05 <- diff05 %>%
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
    diff05$EDUC.RESTRUC <- as.factor(diff05$EDUC.RESTRUC)
    diff05$RACE.RESTRUC <- as.factor(diff05$RACE.RESTRUC)
    
    #A. Education
    diff15 <- diff15 %>%
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
    diff15$EDUC.RESTRUC <- as.factor(diff15$EDUC.RESTRUC)
    diff15$RACE.RESTRUC <- as.factor(diff15$RACE.RESTRUC)
    
#3.2: Refactor
    
    #A. Race
    diff05$RACE.RESTRUC <- relevel(diff05$RACE.RESTRUC, "White")
    diff15$RACE.RESTRUC <- relevel(diff15$RACE.RESTRUC, "White")
    
    #B. Education
    diff05$EDUC.RESTRUC <- relevel(diff05$EDUC.RESTRUC, "HS or Less")
    diff15$EDUC.RESTRUC <- relevel(diff15$EDUC.RESTRUC, "HS or Less")
    
#3.3: Data Check
    
    #A. Education
    
        ##1. Data set 2005
        table(diff05$EDUCATION) # Over sample on "BS or Higher." Check for weights
        table(diff05$EDUC.RESTRUC) # case_when worked. Numbers add up. 
        
        ##2. Data set 2014
        table(diff15$EDUCATION) # Over sample even greater on "BS or Higher"
        table(diff15$EDUC.RESTRUC) # case_when worked. Numbers add up. 
    
    #B. Race
    
        ##1. Data set 2005
        table(diff05$RACE)
        table(diff05$RACE.RESTRUC)
        
        ##2. Data set 2014
        table(diff15$RACE)
        table(diff15$RACE.RESTRUC)
        
#------------------------------------------------------------------------#
#                         Regressions (Unweighted)                       #
#------------------------------------------------------------------------#      

#4.1: OLS w/o interaction terms
    
    #A. Reg1
    reg1.05 <- lm(data = diff05, log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34 + DUMMY.45.54 + DUMMY.CHILD + DUMMY.MARRIED + 
                    DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC )
    summary(reg1.05)
    
    #B. Reg2
    reg1.15 <- lm(data = diff15, log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.35.44 + DUMMY.55.64 + DUMMY.CHILD + DUMMY.MARRIED + 
                    DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC )
    summary(reg1.15)
    
    #C. Reg3 (Add First class mail sent and Packages sent)
    reg1.sent.05 <- lm(data = diff05, log1p(MAIL.VOLUME.SUM) ~ 
                         DUMMY.25.34 + DUMMY.45.54 + DUMMY.CHILD + DUMMY.MARRIED + 
                         DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC +
                         MAIL.VOLUME.FCMS + MAIL.VOLUME.PKGS)
    summary(reg1.sent.05)
    
    #D. Reg4 (Add First class mail sent and Packages sent)
    reg1.sent.15 <- lm(data = diff15, log1p(MAIL.VOLUME.SUM) ~ 
                         DUMMY.35.44 + DUMMY.55.64 + DUMMY.CHILD + DUMMY.MARRIED + 
                         DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC +
                         MAIL.VOLUME.FCMS + MAIL.VOLUME.PKGS)
    summary(reg1.sent.15)
    
#4.2: OLS with interaction terms
    
    #A. Reg1
    reg2.05 <- lm(data = diff05, log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.25.34*DUMMY.CHILD + DUMMY.45.54*DUMMY.CHILD + 
                    DUMMY.25.34*DUMMY.MARRIED + DUMMY.45.54*DUMMY.MARRIED +
                    DUMMY.25.34*DUMMY.HOUSE + DUMMY.45.54*DUMMY.HOUSE + 
                    DUMMY.25.34*DUMMY.MALE + DUMMY.45.54*DUMMY.MALE + 
                    DUMMY.25.34*REGION + DUMMY.45.54*REGION +
                    DUMMY.25.34*RACE.RESTRUC + DUMMY.45.54*RACE.RESTRUC + 
                    DUMMY.25.34*EDUC.RESTRUC + DUMMY.45.54*EDUC.RESTRUC)
    summary(reg2.05)
    
    #B. Reg2
    reg2.15 <- lm(data = diff15, log1p(MAIL.VOLUME.SUM) ~ 
                    DUMMY.35.44*DUMMY.CHILD + DUMMY.55.64*DUMMY.CHILD + 
                    DUMMY.35.44*DUMMY.MARRIED + DUMMY.55.64*DUMMY.MARRIED +
                    DUMMY.35.44*DUMMY.HOUSE + DUMMY.55.64*DUMMY.HOUSE + 
                    DUMMY.35.44*DUMMY.MALE + DUMMY.55.64*DUMMY.MALE + 
                    DUMMY.35.44*REGION + DUMMY.55.64*REGION +
                    DUMMY.35.44*RACE.RESTRUC + DUMMY.55.64*RACE.RESTRUC + 
                    DUMMY.35.44*EDUC.RESTRUC + DUMMY.55.64*EDUC.RESTRUC)
    summary(reg2.15)  
    
    #C. Reg3 (Add First class mail sent and Packages sent)
    reg2.sent.05 <- lm(data = diff05, log1p(MAIL.VOLUME.SUM) ~ 
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
    reg2.sent.15 <- lm(data = diff15, log1p(MAIL.VOLUME.SUM) ~ 
                         DUMMY.35.44*DUMMY.CHILD + DUMMY.55.64*DUMMY.CHILD + 
                         DUMMY.35.44*DUMMY.MARRIED + DUMMY.55.64*DUMMY.MARRIED +
                         DUMMY.35.44*DUMMY.HOUSE + DUMMY.55.64*DUMMY.HOUSE + 
                         DUMMY.35.44*DUMMY.MALE + DUMMY.55.64*DUMMY.MALE + 
                         DUMMY.35.44*REGION + DUMMY.55.64*REGION +
                         DUMMY.35.44*RACE.RESTRUC + DUMMY.55.64*RACE.RESTRUC + 
                         DUMMY.35.44*EDUC.RESTRUC + DUMMY.55.64*EDUC.RESTRUC +
                         DUMMY.35.44*MAIL.VOLUME.FCMS + DUMMY.55.64*MAIL.VOLUME.FCMS +
                         DUMMY.35.44*MAIL.VOLUME.PKGS + DUMMY.55.64*MAIL.VOLUME.PKGS)
    summary(reg2.sent.15)
        
        
#------------------------------------------------------------------------#
#                         Regressions (Weighted)                         #
#------------------------------------------------------------------------#  
    
#5.1: OLS w/o interaction terms (Weighted = Respondent)
    
    #A. Reg1
    reg1.w.r.05 <- lm(data = diff05, weights = diff05$RECRUITMENT.WEIGHT,
                      log1p(MAIL.VOLUME.SUM) ~ 
                        DUMMY.25.34 + DUMMY.45.54 + DUMMY.CHILD + DUMMY.MARRIED + 
                        DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC )
    summary(reg1.w.r.05)
    
    #B. Reg2
    reg1.w.r.15 <- lm(data = diff15, weights = diff15$RECRUITMENT.WEIGHT, 
                      log1p(MAIL.VOLUME.SUM) ~ 
                        DUMMY.35.44 + DUMMY.55.64 + DUMMY.CHILD + DUMMY.MARRIED + 
                        DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC )
    summary(reg1.w.r.15) 
    
    #C. Reg3 (Add First class mail sent and Packages sent)
    reg1.w.r.sent.05 <- lm(data = diff05, weights = diff05$RECRUITMENT.WEIGHT,
                           log1p(MAIL.VOLUME.SUM) ~ 
                             DUMMY.25.34 + DUMMY.45.54 + DUMMY.CHILD + DUMMY.MARRIED + 
                             DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC +
                             MAIL.VOLUME.FCMS + MAIL.VOLUME.PKGS)
    summary(reg1.w.r.sent.05)
    
    #D. Reg4 (Add First class mail sent and Packages sent)
    reg1.w.r.sent.15 <- lm(data = diff15, weights = diff15$RECRUITMENT.WEIGHT,
                           log1p(MAIL.VOLUME.SUM) ~ 
                             DUMMY.35.44 + DUMMY.55.64 + DUMMY.CHILD + DUMMY.MARRIED + 
                             DUMMY.HOUSE + DUMMY.MALE + REGION + RACE.RESTRUC + EDUC.RESTRUC +
                             MAIL.VOLUME.FCMS + MAIL.VOLUME.PKGS)
    summary(reg1.w.r.sent.15)
    
#5.2: OLS with interaction terms (Weighted = Respondent)
    
    #A. Reg1
    reg2.w.r.05 <- lm(data = diff05, weights = diff05$RECRUITMENT.WEIGHT,
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
    reg2.w.r.15 <- lm(data = diff15, weights = diff15$RECRUITMENT.WEIGHT, 
                      log1p(MAIL.VOLUME.SUM) ~ 
                        DUMMY.35.44*DUMMY.CHILD + DUMMY.55.64*DUMMY.CHILD + 
                        DUMMY.35.44*DUMMY.MARRIED + DUMMY.55.64*DUMMY.MARRIED +
                        DUMMY.35.44*DUMMY.HOUSE + DUMMY.55.64*DUMMY.HOUSE + 
                        DUMMY.35.44*DUMMY.MALE + DUMMY.55.64*DUMMY.MALE + 
                        DUMMY.35.44*REGION + DUMMY.55.64*REGION +
                        DUMMY.35.44*RACE.RESTRUC + DUMMY.55.64*RACE.RESTRUC + 
                        DUMMY.35.44*EDUC.RESTRUC + DUMMY.55.64*EDUC.RESTRUC)
    summary(reg2.w.r.15)      
    
    #C. Reg3 (Add First class mail sent and Packages sent)
    reg2.w.r.sent.05 <- lm(data = diff05, weights = diff05$RECRUITMENT.WEIGHT,
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
    reg2.w.r.sent.15 <- lm(data = diff15, weights = diff15$RECRUITMENT.WEIGHT, 
                       log1p(MAIL.VOLUME.SUM) ~ 
                         DUMMY.35.44*DUMMY.CHILD + DUMMY.55.64*DUMMY.CHILD + 
                         DUMMY.35.44*DUMMY.MARRIED + DUMMY.55.64*DUMMY.MARRIED +
                         DUMMY.35.44*DUMMY.HOUSE + DUMMY.55.64*DUMMY.HOUSE + 
                         DUMMY.35.44*DUMMY.MALE + DUMMY.55.64*DUMMY.MALE + 
                         DUMMY.35.44*REGION + DUMMY.55.64*REGION +
                         DUMMY.35.44*RACE.RESTRUC + DUMMY.55.64*RACE.RESTRUC + 
                         DUMMY.35.44*EDUC.RESTRUC + DUMMY.55.64*EDUC.RESTRUC +
                         DUMMY.35.44*MAIL.VOLUME.FCMS + DUMMY.55.64*MAIL.VOLUME.FCMS +
                         DUMMY.35.44*MAIL.VOLUME.PKGS + DUMMY.55.64*MAIL.VOLUME.PKGS)
    summary(reg2.w.r.sent.15)  

    
#------------------------------------------------------------------------#
#                        Export Tables/Figures                           #
#                                                                        #
#------------------------------------------------------------------------#     

#6.1: Summary Stats
    
    #A. Example
    # stargazer(attitude, type = 'html', digits = 2, 
    #           out = paste(graph_path, 'example.doc', sep = '\\'))
    
    #B. 2005
    stargazer(diff05[, c("DUMMY.18.24", "DUMMY.25.34", "DUMMY.45.54", "DUMMY.CHILD",
                         "DUMMY.MARRIED", "DUMMY.HOUSE", "DUMMY.MALE",
                         "DUMMY.BLACK", "DUMMY.WHITE", "DUMMY.OTHER", 
                         "MAIL.VOLUME.FCMS", "MAIL.VOLUME.PKGS")], 
              type = 'html', digits = 2, title = "Table 1.1: Summary Stats 2005",
              out = paste(graph_path, 'Sum Stats 1.1 (2005).doc', sep = '\\'))

    #C. 2015
    stargazer(diff15[, c("DUMMY.25.34", "DUMMY.35.44", "DUMMY.55.64", "DUMMY.CHILD",
                         "DUMMY.MARRIED", "DUMMY.HOUSE", "DUMMY.MALE",
                         "DUMMY.BLACK", "DUMMY.WHITE", "DUMMY.OTHER", 
                         "MAIL.VOLUME.FCMS", "MAIL.VOLUME.PKGS")], 
              type = 'html', digits = 2, title = "Table 1.1: Summary Stats 2015",
              out = paste(graph_path, 'Sum Stats 1.2 (2015).doc', sep = '\\'))

#6.2: Residuals Graph
    
    #A. Normality of Residuals
    
        ##1. Change Margins
        par(mar=c(1,1,1,1))
        
        ##2. Graphs
        qqPlot(reg1.05, main="QQ Plot: 2005 (no interaction, unweighted)") # qq plot for studentized resid
        qqPlot(reg1.15, main="QQ Plot: 2015 (no interaction, unweighted)")
        qqPlot(reg2.05, main="QQ Plot: 2005 (Interaction, unweighted)")
        qqPlot(reg2.15, main="QQ Plot: 2015 (Interaction, unweighted)")
        
    #B. Distribution of studentized residuals
    
        ##1. 2005 (no interaction, unweighted) 
        sresid <- studres(reg1.05)
        hist(sresid, freq=FALSE,
             main="Dist Resid 2005 (no interaction, unweighted)")
        xfit<-seq(min(sresid),max(sresid),length=40)
        yfit<-dnorm(xfit)
        lines(xfit, yfit)
        
        ##2. 2015 (no interaction, unweighted) 
        sresid <- studres(reg1.15)
        hist(sresid, freq=FALSE,
             main="Dist Resid 2015 (no interaction, unweighted)")
        xfit<-seq(min(sresid),max(sresid),length=40)
        yfit<-dnorm(xfit)
        lines(xfit, yfit)
        
        ##3. 2005 (Interaction, unweighted) 
        sresid <- studres(reg2.05)
        hist(sresid, freq=FALSE,
             main="Dist Resid 2005 (Interaction, unweighted)")
        xfit<-seq(min(sresid),max(sresid),length=40)
        yfit<-dnorm(xfit)
        lines(xfit, yfit)
        
        ##4. 2015 (Interaction, unweighted) 
        sresid <- studres(reg2.15)
        hist(sresid, freq=FALSE,
             main="Dist Resid 2015 (Interaction, unweighted)")
        xfit<-seq(min(sresid),max(sresid),length=40)
        yfit<-dnorm(xfit)
        lines(xfit, yfit)  
        
#6.3: Regression Tables
    
        #A. OLS with interaction tables
        stargazer(reg1.05, reg1.15, reg2.05, reg2.15, type = 'html', 
                  title = 'Table 3: Regression Results (Unweighted)', 
                  column.labels = c("2005", "2015", "2005", "2015"), 
                  dep.var.labels=c("Log of Total Mail Volume"),
                  omit.stat=c("LL","ser"), no.space = TRUE, 
                  covariate.labels = c("Cohort 25-34", "Cohort 45-54",
                       "Cohort 35-44", "Cohort 55-64", 
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
                       "Cohort 45-54*BS or Higher", "Cohort 45-54*Some College",
                       
                       "Cohort 35-44*Children", "Cohort 55-64*Children",
                       "Cohort 35-44*Married", "Cohort 55-64*Married",
                       "Cohort 35-44*Home Owner", "Cohort 55-64*Home Owner",
                       "Cohort 35-44*Male", "Cohort 55-64*Male",
                       "Cohort 35-44*Northeast", "Cohort 35-44*South",
                       "Cohort 35-44*West", "Cohort 55-64*Northeast",
                       "Cohort 55-64*South", "Cohort 55-64*West",
                       "Cohort 35-44*Black", "Cohort 35-44*Other",
                       "Cohort 55-64*Black", "Cohort 55-64*Other",
                       "Cohort 35-44*BS or Higher", "Cohort 35-44*Some College",
                       "Cohort 55-64*BS or Higher", "Cohort 55-64*Some College"
                  ),
                  out = paste(graph_path, 'Table 3 (Reg)(labeled).doc', sep = '\\') )
        
        #B. OLS with interaction tables (weighted: respondent)
        stargazer(reg1.w.r.05, reg1.w.r.15, reg2.w.r.05, reg2.w.r.15, type = 'html', 
                  title = 'Table 4: Regression Results (Weight:Respondent)', 
                  column.labels = c("2005", "2015", "2005", "2015"), 
                  dep.var.labels=c("Log of Total Mail Volume"),
                  omit.stat=c("LL","ser"), no.space = TRUE, 
                  covariate.labels = c("Cohort 25-34", "Cohort 45-54",
                     "Cohort 35-44", "Cohort 55-64", 
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
                     "Cohort 45-54*BS or Higher", "Cohort 45-54*Some College",
                     
                     "Cohort 35-44*Children", "Cohort 55-64*Children",
                     "Cohort 35-44*Married", "Cohort 55-64*Married",
                     "Cohort 35-44*Home Owner", "Cohort 55-64*Home Owner",
                     "Cohort 35-44*Male", "Cohort 55-64*Male",
                     "Cohort 35-44*Northeast", "Cohort 35-44*South",
                     "Cohort 35-44*West", "Cohort 55-64*Northeast",
                     "Cohort 55-64*South", "Cohort 55-64*West",
                     "Cohort 35-44*Black", "Cohort 35-44*Other",
                     "Cohort 55-64*Black", "Cohort 55-64*Other",
                     "Cohort 35-44*BS or Higher", "Cohort 35-44*Some College",
                     "Cohort 55-64*BS or Higher", "Cohort 55-64*Some College"
                  ),
                  out = paste(graph_path, 'Table 4 (Reg)(labeled)(weighted-Respond).doc', sep = '\\') )
        
        #C. OLS with interaction tables 
        #   (Includes First Class Mail Sent and Packages Sent)
        stargazer(reg1.sent.05, reg1.sent.15, reg2.sent.05, reg2.sent.15, type = 'html', 
                  title = 'Table 5: Regression Results (Unweighted) (Mail Sent)', 
                  column.labels = c("2005", "2015", "2005", "2015"), 
                  dep.var.labels=c("Log of Total Mail Volume"),
                  omit.stat=c("LL","ser"), no.space = TRUE, 
                  covariate.labels = c("Cohort 25-34", "Cohort 45-54",
                     "Cohort 35-44", "Cohort 55-64",
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
                     "Cohort 25-34*Packages Sent", "Cohort 45-54*Packages Sent",
                     
                     "Cohort 35-44*Children", "Cohort 55-64*Children",
                     "Cohort 35-44*Married", "Cohort 55-64*Married",
                     "Cohort 35-44*Home Owner", "Cohort 55-64*Home Owner",
                     "Cohort 35-44*Male", "Cohort 55-64*Male",
                     "Cohort 35-44*Northeast", "Cohort 35-44*South",
                     "Cohort 35-44*West", "Cohort 55-64*Northeast",
                     "Cohort 55-64*South", "Cohort 55-64*West",
                     "Cohort 35-44*Black", "Cohort 35-44*Other",
                     "Cohort 55-64*Black", "Cohort 55-64*Other",
                     "Cohort 35-44*BS or Higher", "Cohort 35-44*Some College",
                     "Cohort 55-64*BS or Higher", "Cohort 55-64*Some College",
                     "Cohort 35-44*FCM Sent", "Cohort 55-64*FCM Sent",
                     "Cohort 35-44*Packages Sent", "Cohort 55-64*Packages Sent"
                  ),
                  out = paste(graph_path, 'Table 5 (Reg)(labeled)(Mail Sent).doc', sep = '\\') )
        
        
        #D. OLS with interaction tables (weighted: respondent)
        #   (Includes First Class Mail Sent and Packages Sent)
        stargazer(reg1.w.r.sent.05, reg1.w.r.sent.15, 
                  reg2.w.r.sent.05, reg2.w.r.sent.15, type = 'html', 
                  title = 'Table 6: Regression Results (Weight:Respondent) (Mail Sent)', 
                  column.labels = c("2005", "2015", "2005", "2015"), 
                  dep.var.labels=c("Log of Total Mail Volume"),
                  omit.stat=c("LL","ser"), no.space = TRUE, 
                  covariate.labels = c("Cohort 25-34", "Cohort 45-54",
                     "Cohort 35-44", "Cohort 55-64", 
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
                     "Cohort 25-34*Packages Sent", "Cohort 45-54*Packages Sent",
                     
                     "Cohort 35-44*Children", "Cohort 55-64*Children",
                     "Cohort 35-44*Married", "Cohort 55-64*Married",
                     "Cohort 35-44*Home Owner", "Cohort 55-64*Home Owner",
                     "Cohort 35-44*Male", "Cohort 55-64*Male",
                     "Cohort 35-44*Northeast", "Cohort 35-44*South",
                     "Cohort 35-44*West", "Cohort 55-64*Northeast",
                     "Cohort 55-64*South", "Cohort 55-64*West",
                     "Cohort 35-44*Black", "Cohort 35-44*Other",
                     "Cohort 55-64*Black", "Cohort 55-64*Other",
                     "Cohort 35-44*BS or Higher", "Cohort 35-44*Some College",
                     "Cohort 55-64*BS or Higher", "Cohort 55-64*Some College",
                     "Cohort 35-44*FCM Sent", "Cohort 55-64*FCM Sent",
                     "Cohort 35-44*Packages Sent", "Cohort 55-64*Packages Sent"
                  ),
                  out = paste(graph_path, 'Table 6 (Reg)(labeled)(weighted-Respond)(Mail Sent).doc', sep = '\\') )
        