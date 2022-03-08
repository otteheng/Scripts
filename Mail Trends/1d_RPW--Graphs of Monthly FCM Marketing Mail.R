# -> Gerhard O
# -> 11/12/2021
#       Update: 1.31.2022   
# -> Data sets: RPW Monthly
#       Pull in most recent copy from the "Volume Percentiles" project folder
#       For any question ask Colter 
# -> Build figures of FCM and Marketing Mail 

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
source('./library_GSO.R')

# Data Paths
vol_perc_path <- "./Volume Percentiles/4-Code and Analysis/Excel Analysis/RPW Factor Development"
graph_path <- "./Mail Trends/Sources/2022-02"


#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

rpw_raw <- read_xlsx(paste(vol_perc_path, "21-12_RPW Factors Jul. 2019-DEC. 2021_1.24.24_DRAFT.xlsx",
                          sep = "/"), sheet = "Series_VolRVCformat", range = "A5:BX14")

#------------------------------------------------------------------------#
#                             Step 2: Clean                              #
#                                                                        #
#------------------------------------------------------------------------#

#2.1: Restructure data (Long)

    #A. Only FCM and Marketing mail
    rpw <- rpw_raw
    rpw <- rpw %>%
      filter(...1=="FCM Total Domestic" | ...1=="USPS Marketing Mail (excl. Parcels) ***") %>%
      mutate(category = case_when(
        ...1=="FCM Total Domestic" ~ "FCM",
        ...1=="USPS Marketing Mail (excl. Parcels) ***" ~ "MM"
      )) %>%
      dplyr::select(-...1) %>%
      relocate(category, .before = `Oct FY 2016`)
    
    #B. Reshape long
    
        ##1. First Class Mail
        fcm.long <- rpw %>%
          filter(category=="FCM") %>%
          gather("month.fiscal.year", "value")
        
        ##2. Marketing Mail
        mm.long <- rpw %>%
          filter(category=="MM") %>%
          gather("month.fiscal.year", "value")
   
    #C. Make first row column name
    header.true <- function(df) {
      names(df) <- as.character(unlist(df[1,]))
      df[-1,]
    }    
        
        ##1. First Class Mail
        fcm.long <- header.true(fcm.long) 
        
        ##2. Marketing Mail
        mm.long <- header.true(mm.long) 
        
    #D. Merge
    rpw.long <- inner_join(fcm.long, mm.long, by = c("category"))

#2.3: Restructure data (Wide)
    
    #A. Separate category into month and fiscal year
    rpw.long <- rpw.long %>%
      mutate(category = gsub("FY", "", category)) %>%
      separate(category, c("month", "fiscal.year"))
    
    #B. Convert to millions
    rpw.long <- rpw.long %>%
      mutate(FCM = floor(as.numeric(FCM)*1000000),
             MM = floor(as.numeric(MM)*1000000))
    
    #C. Make wide again
    rpw.long.wide <- rpw.long %>%
      pivot_wider(names_from = fiscal.year, values_from = c(FCM, MM))
    
    #D. Factor month
    rpw.long.wide$month <- factor(rpw.long.wide$month, 
                                  levels = c("Oct","Nov", "Dec", "Jan", "Feb", 
                                             "Mar", "Apr", "May", "Jun", "Jul", 
                                             "Aug", "Sep"))
    
    #E. Find min and max of all columns
    
        ##1. First Class Mail
        sort(apply(rpw.long.wide[, c("FCM_2018", "FCM_2019", "FCM_2020", 
                                     "FCM_2021", "FCM_2022")], 2, min, na.rm = T))
        sort(apply(rpw.long.wide[, c("FCM_2018", "FCM_2019", "FCM_2020", 
                                     "FCM_2021", "FCM_2022")], 2, max, na.rm = T))
        
        ##2. Marketing Mail
        sort(apply(rpw.long.wide[, c("MM_2018", "MM_2019", "MM_2020", 
                                     "MM_2021", "MM_2022")], 2, min, na.rm = T))
        sort(apply(rpw.long.wide[, c("MM_2018", "MM_2019", "MM_2020", 
                                     "MM_2021", "MM_2022")], 2, max, na.rm = T))
        
#------------------------------------------------------------------------#
#                             Step 3: Graph                              #
#                           Line Graphs of Volume                        #
#------------------------------------------------------------------------#  

#3.1: Basic graph of volume for FCM by month from FY 2018-2021
    
    #A. Graph    
    graph.3.1 <- rpw.long.wide %>% 
      ggplot(aes(x=month, group = 1)) +
      
        ##1. Line
        geom_line( aes(y=FCM_2018, color = "FY 2018"), size=1.1) +
        geom_line( aes(y=FCM_2019, color = "FY 2019"), size=1.1) +
        geom_line( aes(y=FCM_2020, color = "FY 2020"), size=1.1) +
        geom_line( aes(y=FCM_2021, color = "FY 2021"), size=1.1) +
        geom_line( aes(y=FCM_2022, color = "FY 2022"), size=1.1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "**First-Class Mail Volume**") +
        labs(caption = 'Source: Monthly RPW',
             color = 'Fiscal Year') +
        ylab("Volume (in Millions)")+
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values = c(
          'FY 2018' = "#acc8d4", 
          'FY 2019' = "#336666",
          'FY 2020' = "#dbcc98",
          'FY 2021' = "#e3120b",
          'FY 2022' = "#2E45B8")) +
        scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                           limits = c(3800000000, 5700000000 )) + 
        
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = ggtext::element_markdown(color="black", size=16, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title.x = element_text(size = 12, color = "grey30"), 
              axis.title.y = element_text(color = "grey30", size=11),
              axis.title.y.right = element_text(color = "grey30", size=9),
              axis.text = element_text(size = 12),
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 9),
              legend.position = c(0.9, 0.8),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 9, 
                                                      color = "#7B7D7D")) 
      
    #C. Save Graph
    ggsave(paste(graph_path, "1d_3.1--First Class Mail by Month and Fiscal Year.png", 
                 sep="/"), graph.3.1, scale = 1, width = 9, 
           height = 9, dpi = 300)  


#3.2: Basic graph of volume for MM by month from FY 2018-2021

    #A. Graph    
    graph.3.2 <- rpw.long.wide %>% 
      ggplot(aes(x=month, group = 1)) +
      
        ##1. Line
        geom_line( aes(y=MM_2018, color = "FY 2018"), size=1) +
        geom_line( aes(y=MM_2019, color = "FY 2019"), size=1) +
        geom_line( aes(y=MM_2020, color = "FY 2020"), size=1) +
        geom_line( aes(y=MM_2021, color = "FY 2021"), size=1) +
        geom_line( aes(y=MM_2022, color = "FY 2022"), size=1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "**Marketing Mail Volume**") +
        labs(caption = 'Source: Monthly RPW',
             color = 'Fiscal Year') +
        ylab("Volume (in Millions)")+
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values = c(
          'FY 2018' = "#acc8d4", 
          'FY 2019' = "#336666",
          'FY 2020' = "#dbcc98",
          'FY 2021' = "#e3120b",
          'FY 2022' = "#2E45B8")) +
        scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                           limits = c(3400000000, 9600000000),
                           breaks = seq(3400000000, 9600000000, by=2000000000)) + 
      
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = ggtext::element_markdown(color="black", size=16, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title.x = element_text(size = 12, color = "grey30"), 
              axis.title.y = element_text(color = "grey30", size=11),
              axis.title.y.right = element_text(color = "grey30", size=9),
              axis.text = element_text(size = 12),
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 9),
              legend.position = c(0.9, 0.8),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 9, 
                                                      color = "#7B7D7D")) 
      
    #C. Save Graph
    ggsave(paste(graph_path, "1d_3.2--Marketing Mail by Month and Fiscal Year.png", 
                 sep="/"), graph.3.2, scale = 1, width = 9, 
           height = 9, dpi = 300)  
