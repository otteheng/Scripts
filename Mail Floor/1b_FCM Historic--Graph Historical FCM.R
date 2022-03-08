# -> Gerhard O
# -> 9/23/2021
# -> Data sets: Historical FCM 
# -> Build figures for Historical FCM
# -> Original files found here: C:\Users\bg4cn0\USPS\ARTSI Team Folder - Documents\Projects\Mail Floor\Code\_Old\historical-mail-volume_2021-04-14.R

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
source('./library_GSO.R')

# Data Paths
graph_path <- "./Mail Floor/Code/Tables & Figures/1b"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

#1.1: FCM data put together by Peter
fcm <- read_csv("./_Raw Data/USPS Public/historical-fcm-volume-1926-2020_2021-04-14.csv")

#1.2: Census Population data pulled by Peter
pop <- read_csv("./_Raw Data/Census/census-intercensial-population-1900-2020_2021-05-05.csv")

#------------------------------------------------------------------------#
#                           Step 2: Light Clean                          #
#                                                                        #
#------------------------------------------------------------------------#


#2.1: Historical FCM

    #A. Merge FCM with population
    fcm2 <- merge(fcm, pop)

    #B. Create variable Pieces Per Person
    fcm2$ppp <- round(fcm2$fcm/(fcm2$pop/1000000), 2)
    
    #C. Divide FCM by Pieces per Person to get scale for double y-axis
    fcm2 <- fcm2 %>%
      mutate(yaxis = fcm/ppp)

#------------------------------------------------------------------------#
#                             Step 3: Graph                              #
#                                                                        #
#------------------------------------------------------------------------#

#3.1: Graph with FCM and Pieces per person on the same graph
    
    #A. Constants
    coeff <- .01 # Value used to transform the data
    fcm.Color <- "#69b3a2"
    ppp.Color <- rgb(0.2, 0.6, 0.9, 1)
    
    #B. Graph
    graph.3.1 <- ggplot(fcm2, aes(x=year)) +
      
        ##1. Line and bars
        geom_bar( aes(y=fcm), stat="identity", size=.1, fill=fcm.Color,
                  color="black", alpha=.4) +
        annotate("label", x=1975, y=75000, label = "First Class Mail", size = 3,
                 color = fcm.Color) +
        geom_line( aes(y=ppp/coeff), size=2, color=ppp.Color) +
        annotate("label", x=1940, y=30000, label = "Pieces Per Person", size = 3,
                 color = ppp.Color) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "First Mail Volumes and Pieces per Person",
                subtitle = "Years: 1925-2020") +
        labs(caption = 'Source: about.usps.com and Census Bureau',
             color = 'Age Cohort') +
        xlab("") +
      
        ##3. Scales
        scale_x_continuous(limits = c(1925, 2020) , breaks = (seq(1920, 2020, 10))) +
        scale_y_continuous(name = "First Class Mail (in Billions)", 
                           labels = unit_format(unit = "B", scale = 1e-3, accuracy = 1),
          sec.axis = sec_axis(~.*coeff, name="Pieces Per Person (FCM)")) + 
        
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = ggtext::element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title.x = element_text(size = 9, color = "grey30"), 
              axis.title.y = element_text(color = "grey30", size=9),
              axis.title.y.right = element_text(color = "grey30", size=9),
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              legend.position = c(0.1, 0.3),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 

    #C. Save Graph
    ggsave(paste(graph_path, "1b_3.1--FCM and Pieces per Person (1925-2020).png",
                 sep="/"), graph.3.1, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)
