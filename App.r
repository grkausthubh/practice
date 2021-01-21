#load required packages
library(shiny)
library(dplyr)
library(ggplot2)
#remove existing lists in the working directory
rm(list = ls(all = TRUE))

#create static lists to load the drop downs for various tabs
TrendList <- c('General_Trend','Trend_Day&SubscriberType')
ChartList = c('AvgDuration_StartHour_DayType_Plot','StartHour_Day&SubscriberType_NoOfRides')

#Read the input data. GDA_DATA(original dataset provided). Made few changes to the data and created new columns for anlaysis and saved as GDA
GDA <- read.csv("Data/GDA.csv")
#GDA <- read.csv("C:/Users/mc54378/Desktop/DS/Kausthubh-R/GDA.csv")
#getting unique terminal names sorted in ascending order for terminal analysis selection
TerminalNames <- sort(unique(GDA$StartTerminal))

#create individual datasets using dplyr function for various plots used for analysis
avg_trip_duration = GDA %>%
  select(SubscriberType,DayType,StartHour,Duration) %>%
  filter(Duration<7200) %>%
  group_by(StartHour, SubscriberType, DayType) %>%
  summarise(no_of_rides = n(), avg_duration = mean(Duration))

trips_byday = GDA %>% 
  group_by(DayType,Start_Date, SubscriberType) %>%
  summarize(count = n())

cust_byday  = GDA %>% 
  group_by(DayType,Start_Date,SubscriberType) %>%
  summarize(count = n())

Start = GDA %>% group_by(StartTerminal, Start_Date) %>% summarize(Outbound=n())
End = GDA %>% group_by(EndTerminal,End_Date) %>% summarize(Inbound=n())
Diff = Start %>% inner_join(End,by= c('StartTerminal'='EndTerminal','Start_Date'='End_Date')) %>%
  mutate(Difference = Inbound - Outbound)

StartHr = GDA %>% group_by(StartTerminal, StartHour) %>% summarize(OutboundHr=n())
EndHr = GDA %>% group_by(EndTerminal,EndHour) %>% summarize(InboundHr=n())
DiffHr = StartHr %>% inner_join(EndHr,by= c('StartTerminal'='EndTerminal','StartHour'='EndHour')) %>%
  mutate(DifferenceHr = InboundHr - OutboundHr)

#cluster analysis
clusterAnalysis = GDA %>% group_by(Start_Date) %>% summarize(no_of_rides = n()) %>% 
  mutate(dummy = 0)

clusterOutput = kmeans(clusterAnalysis %>% select(dummy,no_of_rides),2,nstart = 25)

#Add the cluster values to the dataset 
clusterAnalysis$cluster = clusterOutput$cluster

#UI function for Shiny start

ui <- fluidPage(
  
  titlePanel("Bike Sharing Program Analysis"),
  
  shinyUI(navbarPage("My Application",
                    # cretae multiple tabs - each for each type of anlaysis as mentioned in the heading
                     tabPanel("Trend Analysis",
                              radioButtons(inputId = "Trend",
                                          label = "Trend charts",
                                          choices = TrendList),
                              plotOutput(outputId = "Trend_Plot", height = "500px")
                     ),
                     
                     tabPanel("Cluster Analysis",
                              selectInput(inputId = "ClusterCount",
                                          label = "Cluster count",
                                          choices = c(1, 2, 'both'),
                                          selected = 'both'),
                              plotOutput(outputId = "Cluster_Plot", height = "500px")
                     ),
                     
                     tabPanel("Analysis Charts & Recommendations",
                        selectInput(inputId = "ChartToDisplay",
                                    label = "Analysis charts",
                                    choices = ChartList,
                                    selected = 1),
                        plotOutput(outputId = "main_plot", height = "500px")  
                     ),
                    
                     tabPanel("Terminal Analysis - Capacity Planning",
                              selectInput(inputId = "TerminalName",
                                          label = "Terminal ID",
                                          choices = TerminalNames,
                                          selected = 1),
                              
                              div(class="span6",plotOutput(outputId= "Store_Plot")),
                              div(class="span6",plotOutput(outputId= "Store_Plot_2")),
                              div(class="span6",plotOutput(outputId= "Store_Plot_Drill")),
                              div(class="span6",plotOutput(outputId= "Store_Plot_2_Drill"))
                              
                      )
  ))
)
  
#  Define server logic required to draw plots
server <- function(input, output) {
  
  
  #clusters for general trend analysis
  output$Trend_Plot <- renderPlot({
    
      #set chart name from the radio button value
      ggPlotTrendName <- input$Trend
      
        if(ggPlotTrendName == 'General_Trend'){
        #general trend line
         ggplot(trips_byday, aes(as.Date(Start_Date), count)) +
          geom_point(color = 'darkgreen') +
          labs( x = "Date Range", y = "Number of Rides",
          title ="General Trend of rides each day ",
          caption = "The general trend can be seen segregated into two groups consistently for 6 months.") +
          theme(plot.title=element_text(family="Arial", face="bold", size=25, hjust = 0.5),
             plot.caption = element_text(size = 20, hjust=0.5),
          axis.text.x=element_text(angle=90, hjust=1))
        }
        
        else if(ggPlotTrendName == 'Trend_Day&SubscriberType'){
          #general trend line by week day by subscriber type
          ggplot(cust_byday, aes(x = as.Date(Start_Date),count,  color = DayType)) +
            geom_line(size = 1) +
            facet_grid(. ~ SubscriberType) +
            labs( x = "Date Range", y = "Number of Rides",
                  title ="General Trend of rides each day by Day Type by Subscriber Type",
                  caption = "Clear indication that customers use the bikes a lot less than subscribers on weekdays.
                Further analysis required for subscribers to understand what consumer profile they belong to.") +
            theme(plot.title=element_text(family="Arial", face="bold", size=25, hjust = 0.5),
                  plot.caption = element_text(size = 20, hjust=0.5),
                  axis.text.x=element_text(angle=90, hjust=1))
        }
  })
  
  #plots for Cluster Analysis - cluster created - 2
  output$Cluster_Plot <- renderPlot({
    
    cluster_input = input$ClusterCount
    
    if(cluster_input %in% c(1,2)){
      
      ggplot(clusterAnalysis %>% filter(cluster == cluster_input), aes(x = as.Date(Start_Date), y=no_of_rides, color = cluster)) +
        geom_point() +
        geom_smooth(method = "lm") + coord_cartesian(ylim = c(0,1600)) +
        labs( x = "Date Range", y = "Number of Rides",
              title ="Cluster Analysis",
              caption = "Cluster with more number of rides depicts the weekend usage of the bikes, which is a flat line trend.
              Cluster with fewer rides shows the weekday usage, which is steadily increasing over the six month period.") +
        theme(plot.title=element_text(family="Arial", face="bold", size=25, hjust = 0.5),
              plot.caption = element_text(size = 20, hjust=0.5),
              axis.text.x=element_text(angle=90, hjust=1))
    }
    

    else if (cluster_input == 'both'){
      
      ggplot(clusterAnalysis, aes(x = as.Date(Start_Date), y=no_of_rides, color = cluster)) +
        geom_point() +
        geom_smooth(method = "lm")+ coord_cartesian(ylim = c(0,1600)) +
        labs( x = "Date Range", y = "Number of Rides",
              title ="Cluster Analysis",
              caption = "Cluster profiling reveals that the underlying data is segmented on week part (weekend or weekday).
              The stark difference between the weekend and weekday usage can help in taking decisions on bike maintenance program. 
              Weekends are suitable as there are less number of customers/subscribers using the bikes.") +
        theme(plot.title=element_text(family="Arial", face="bold", size=25, hjust = 0.5),
              plot.caption = element_text(size = 20, hjust=0.5),
              axis.text.x=element_text(angle=90, hjust=1))
    }
  })
  
  #drill down plots for further analysis
  output$main_plot <- renderPlot({
    
    ggPlotName <- input$ChartToDisplay

    if(ggPlotName == 'AvgDuration_StartHour_DayType_Plot'){
      ggplot(avg_trip_duration, aes(x= StartHour, y = avg_duration, fill = DayType)) +
        geom_bar(position="dodge", stat = "identity") +
        facet_grid(. ~ SubscriberType) +
        labs( x = "Start Hour", y = "Average duration of rides",
              title ="Average Duration of ride each hour segregated by Day Type",
              caption = "Customers and subscribers who have used bikes for less than 2 hours are considered for this analysis.
              The trend clearly suggests that the customers are using the bikes for longer duration than the subscribers.
              This analysis can be used to change the pricing strategy, to either incentivize customers or 
              to charge customers based on hourly usage.
              
              Assumption: Customers are people who pay for using the bike each day as and when required.") +
        theme(plot.title=element_text(family="Arial", face="bold", size=25, hjust = 0.5),
              plot.caption = element_text(size = 20, hjust = 0.5),
              axis.text.x=element_text(angle=90, hjust=1))
    }
    else if(ggPlotName == 'StartHour_Day&SubscriberType_NoOfRides'){
      ggplot(avg_trip_duration, aes(x= StartHour, y = no_of_rides,fill = DayType)) +
        geom_bar(position="dodge", stat = "identity")+
        facet_grid(. ~ SubscriberType) +
        labs( x = "Start Hour", y = "Number of rides",
              title ="Number of rides each hour segregated by Day Type",
              caption = "The 6 month data has been aggregated at hour level.
              This bimodal plot (for subscribers) is the exact opposite of the Average Duration plot, depicting that 
              the most number of rides are taken by subscribers.
              This trend is consistent for weekdays with a bimodal distribution during the peak hours,
              reaffirming that subscribers are predominantly office-goers.") +
        theme(plot.title=element_text(family="Arial", face="bold", size=25, hjust = 0.5),
              plot.caption = element_text(size = 20, hjust = 0.5),
              axis.text.x=element_text(angle=90, hjust=1))
    }
    
  })

  # plots for terminal analysis - this is for capacity planning for each terminal which can be selected from the drop down
  output$Store_Plot <- renderPlot({
  
    ggplot(data = Diff %>% filter(StartTerminal==input$TerminalName), aes(x = as.Date(Start_Date))) +
      geom_line(aes(y = Inbound), color= 'red') +
      geom_line(aes(y = Outbound), color= 'darkgreen') +
      labs( x = "Start Date", y = "Inbound and Outbound count for selected terminal",
            title ="Terminal Inbound, Outbound count each day",
            caption = "Red - inbound rides each day      Green - outbound rides each day
            This plot can be used as a tool to analyze terminal capacities.
            To ensure optimal usage of bikes, the inbound and outbound ride counts should be equal.
            For example, terminal 70 reveals that inbound ride count is consistently higher than outbound, and growing.
            
            Assumption: The docker capacity is limited and different for each terminal based on the usage.
            ") +
      theme(plot.title=element_text(family="Arial", face="bold", size=25, hjust = 0.5),
            plot.caption = element_text(size = 20, hjust = 0.5),
            axis.text.x=element_text(angle=90, hjust=1))
  })
  
  output$Store_Plot_2 <- renderPlot({
    ggplot(data = Diff %>% filter(StartTerminal==input$TerminalName), aes(as.Date(Start_Date), Difference)) + geom_line(size = 1) +
    theme_light() +
      labs( x = "Start Date", y = "Inbound - Outbound",
            title ="Terminal Inbound, Outbound count each day",
            caption = "If the difference is positive, it means there is more inbound for the particular day than outbound.
            This chart enables Capacity Management across terminals for optimal balance between inbound and outbound.
            ") +
      theme(plot.title=element_text(family="Arial", face="bold", size=25, hjust = 0.5),
            plot.caption = element_text(size = 20, hjust = 0.5),
            axis.text.x=element_text(angle=90, hjust=1))
    
  })
  
  output$Store_Plot_Drill <- renderPlot({
    
      ggplot(data = DiffHr %>% filter(StartTerminal==input$TerminalName), aes(x = StartHour)) +
        geom_line(aes(y = InboundHr), color= 'red') +
        geom_line(aes(y = OutboundHr), color= 'darkgreen') +
        labs( x = "Start Hour", y = "Inbound and Outbound count for selected terminal",
              title ="Terminal Inbound, Outbound count each hour") +
        theme(plot.title=element_text(family="Arial", face="bold", size=25, hjust = 0.5),
              plot.caption = element_text(size = 20, hjust = 0.5),
              axis.text.x=element_text(angle=90, hjust=1))
    
  })
  
  output$Store_Plot_2_Drill <- renderPlot({
    
        ggplot(data = DiffHr %>% filter(StartTerminal==input$TerminalName), aes(StartHour, DifferenceHr)) + geom_line(size = 1) +
        theme_light() +
        labs( x = "Start Date", y = "Inbound and Outbound difference for selected terminal",
              title ="Terminal Inbound, Outbound count each hour") +
        theme(plot.title=element_text(family="Arial", face="bold", size=25, hjust = 0.5),
              plot.caption = element_text(size = 20, hjust = 0.5),
              axis.text.x=element_text(angle=90, hjust=1))
    })

}

shinyApp(ui = ui, server = server)