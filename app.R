#CS 424 -Spring 22- Project - Krishnan CS

#libraries to include
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(dplyr)
library(scales)

# #loading all the data once and storing what's needed in a separate data file
# temp_df = list.files(pattern="*.tsv")
# cta_temp <- lapply(temp_df, read.delim)
# cta <- do.call(rbind, cta_temp)
# 
# #converting the date to ymd format using lubridate
# cta$date <- ymd(mdy(cta$date))
# 
# #selecting subsets of the rides and dates column for UIC Halstead, O'Hare and Addison-North Main
# uic_data <- subset(cta, cta$stationname == 'UIC-Halsted', select = c('date','rides') )
# ohare_data <- subset(cta, cta$stationname == "O\'Hare Airport", select = c('date','rides')  )
# addison_data <- subset(cta, cta$stationname == 'Addison-North Main', select = c('date','rides')  )
# 
# #Creating 3 separate data files for the stations for use henceforth
# write.csv(uic_data,'./data/uic_data.csv')
# write.csv(ohare_data,'./data/ohare_data.csv')
# write.csv(addison_data,'./data/addison_data.csv')

#Reading from stored datafiles
uic_data <- read.csv(file = 'data/uic_data.csv')
ohare_data <- read.csv(file = 'data/ohare_data.csv')
addison_data <- read.csv(file = 'data/addison_data.csv')
options(scipen=999)

#converting date format
uic_data$date <- ymd(uic_data$date)
ohare_data$date <- ymd(ohare_data$date)
addison_data$date <- ymd(addison_data$date)

#creating list of years for menu items
years <- sort(unique(year(uic_data$date)) )



# Define UI 
ui <- dashboardPage(
  
  dashboardHeader(title = 'CS 424 Project-1'),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("Dashboard", tabName="dashboard", selected = TRUE, icon = icon('dashboard', lib = "font-awesome")),
                     menuItem("About", tabName="about",  icon = icon("info")),
                     menuItem("Interesting Dates", tabName="interesting",  icon = icon("calendar", lib = "font-awesome"))
                   )
                   ),
  
  dashboardBody(

    tabItems(
      #Main tab containing the plots
      tabItem(tabName = "dashboard",
              fluidPage(
                column( 
                  width = 6,
                  #Select Station
                  box(title = "Select Station-1", solidHeader = TRUE, status = "primary",
                    width = 12,
                    selectInput(inputId = "station1", label = NULL, c('UIC-Halstead'= 'uic',"O'Hare" = 'ohare', "Addison" = 'addison'), selected='uic', selectize = FALSE),
                    plotOutput("plot1")
                  ),
                  box(
                    title = "Choose Year and Filter", solidHeader = TRUE, status = "primary",
                    width = 12,
                    selectInput(inputId = "year1", label = NULL, years, selected='2021', selectize = FALSE),
                    selectInput(inputId = "timeframe1", label = NULL, c("Date" = "date","Month" = "month","Day" = 'day' ), selectize = FALSE),
                    plotOutput("plot2")
                  ),
                  box( 
                    title = "Table", solidHeader = TRUE, status = "primary",
                    width = 12,
                    dataTableOutput("tab1")
                  )
                ),
                column( 
                  width = 6,
                  box(
                    title = "Select Station-2", solidHeader = TRUE, status = "primary",
                    width = 12,
                    selectInput(inputId = "station2", label = NULL, c('UIC-Halstead'= 'uic',"O'Hare" = 'ohare', "Addison" = 'addison'), selected='ohare', selectize = FALSE),
                    plotOutput("plot1_right"),
                  ),
                  box(
                    title = "Choose Year and Filter", solidHeader = TRUE, status = "primary",
                    width = 12,
                    selectInput(inputId = "year2", label = NULL, years, selected='2021', selectize = FALSE),
                    selectInput(inputId = "timeframe2", label = NULL, c("Date" = "date","Month" = "month","Day" = 'day' ), selectize = FALSE),
                    plotOutput("plot2_right")
                  ),
                  box(
                    title = "Table", solidHeader = TRUE, status = "primary",
                    width = 12,
                    dataTableOutput("tab1_right")
                  )
                )
              )
            ),
      #contents of about page
      tabItem(tabName="about",
              h1("About App"),
              h5("________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________"),
              h2("Developed as part of Project 1 for CS424 (Visualization and Visual Analytics) - UIC Spring 2022"),
              h2("Author: Krishnan Chelakkarai Sivaraman"),
              h3(""),
              h3("This App presents the Chicago CTA Ridership data obtained from the Chicago Data Portal website"),
              h3(""),
              h3("* Data Source: https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
              h3("* Packages/Libraries Used: shiny, shinydashboard, ggplot2, lubridate"),              
              h3("* Created using R, RStudio, Shiny ")
              ),
      tabItem(tabName="interesting",
              fluidPage(
                box( width ='12', 
                     status = "primary",
                     h1(textOutput("interesting_text1")),
                     h2(textOutput("interesting_text2")),
                     plotOutput("interesting_plot1"),
                     #plotOutput("interesting_plot2")
            
                ),
                box(
                  title = "Choose menu option to see some interesting dates in the data!",
                  width = 5,
                  solidHeader = TRUE, status = "primary",
                  selectInput(inputId = "menu", label = "", c("Date-1" = '1', "Date-2" = '2', "Date-3" = '3', "Date-4" = '4',"Date-5" = '5', "Date-6" = '6', "Date-7" = '7', "Date-8" = '8', "Date-9" = '9', "Date-10" = '10'  ),selected = 1, selectize = FALSE),
                )
              ),
              fluidPage(
                
              )
      )
)
))
  
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 14)) 
  
  #To get the value of year
  year1Reactive <- reactive({ subset(station1Reactive(), year(station1Reactive()$date) == input$year1 )})
  year2Reactive <- reactive({ subset(station2Reactive(), year(station2Reactive()$date) == input$year2 )})
  
  #To get the value of station1
  station1Reactive <- reactive({ 
    station1 <- input$station1
    if(station1 == 'uic'){
      df <- uic_data
    } 
    else if(station1 =='ohare'){
      df <- ohare_data
    }else {
      df <- addison_data
    }
    return(df)
    })
  
  station2Reactive <- reactive({ 
    station2 <- input$station2
    if(station2 == 'uic'){
      df <- uic_data
    } 
    else if(station2 =='ohare'){
      df <- ohare_data
    }else {
      df <- addison_data
    }
    return(df)
  })
  
  menuReactive <- reactive({
    menu <- input$menu
    return(menu)
  })
#output$interesting_plot2 <- renderPlot({
#  menu <- menuReactive()
#  if(menu =='4'){
#    ggplot(addison_data, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "Train Service Halted Downtown Amid Protests In Wake Of George Floyd Death", x = "Date", y ="Rides") + coord_cartesian(xlim = c(as.Date("2020-06-01", "%Y-%m-%d"),as.Date("2020-07-1", "%Y-%m-%d")),ylim = c(0,8000)) 
#  }
#})  
  
  
  output$interesting_plot1 <- renderPlot({
    menu <- menuReactive()
    if(menu == '1'){
      ggplot(uic_data, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "UIC-Halstead Ridership", x = "Date", y ="Rides") + coord_cartesian(xlim = c(as.Date("2020-01-01", "%Y-%m-%d"),as.Date("2021-01-01", "%Y-%m-%d")),ylim = c(0,8000))  +  scale_y_continuous(label=comma)
    }
    else if(menu == '2'){
      ggplot(ohare_data, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "OHare Ridership", x = "Date", y ="Rides") + coord_cartesian(xlim = c(as.Date("2020-07-01", "%Y-%m-%d"),as.Date("2020-09-30", "%Y-%m-%d")),ylim = c(0,3700))  +  scale_y_continuous(label=comma)
    }
    else if(menu == '3'){
      ggplot(ohare_data, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "OHare Ridership", x = "Date", y ="Rides") + coord_cartesian(xlim = c(as.Date("2001-09-01", "%Y-%m-%d"),as.Date("2001-10-31", "%Y-%m-%d")),ylim = c(0,12000))  +  scale_y_continuous(label=comma)
    }
    else if(menu =='4'){
      ggplot(uic_data, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "OHare Ridership", x = "Date", y ="Rides") + coord_cartesian(xlim = c(as.Date("2020-05-01", "%Y-%m-%d"),as.Date("2020-06-30", "%Y-%m-%d")),ylim = c(0,600))  +  scale_y_continuous(label=comma)
    }
    else if(menu =='5'){
      ggplot(ohare_data, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "OHare Ridership", x = "Date", y ="Rides") + coord_cartesian(xlim = c(as.Date("2016-10-01", "%Y-%m-%d"),as.Date("2016-10-30", "%Y-%m-%d")),ylim = c(0,17500)) +  scale_y_continuous(label=comma) 
    }
    else if(menu =='6'){
      ggplot(uic_data, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "UIC-Halstead Ridership", x = "Date", y ="Rides") + coord_cartesian(xlim = c(as.Date("2018-12-01", "%Y-%m-%d"),as.Date("2019-01-31", "%Y-%m-%d")),ylim = c(0,9000))  +  scale_y_continuous(label=comma)
    }
    else if(menu =='7'){
      ggplot(ohare_data, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "OHare Ridership", x = "Date", y ="Rides")  + coord_cartesian(xlim = c(as.Date("2014-03-15", "%Y-%m-%d"),as.Date("2014-04-10", "%Y-%m-%d")), ylim = c(0,12500))  +  scale_y_continuous(label=comma) 
    }
    else if(menu =='8'){
      ggplot(ohare_data, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "OHare Ridership", x = "Date", y ="Rides") + coord_cartesian(xlim = c(as.Date("2019-09-23", "%Y-%m-%d"),as.Date("2019-10-20", "%Y-%m-%d")), ylim = c(0,19000))  +  scale_y_continuous(label=comma)
    }
    else if(menu =='9'){
      ggplot(uic_data, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "UIC Ridership", x = "Date", y ="Rides") + coord_cartesian(xlim = c(as.Date("2016-11-01", "%Y-%m-%d"),as.Date("2016-11-16", "%Y-%m-%d")),ylim = c(0,10000))  +  scale_y_continuous(label=comma)
    }
    else{
      ggplot(ohare_data, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "OHare Ridership", x = "Date", y ="Rides") + coord_cartesian(xlim = c(as.Date("2013-07-01", "%Y-%m-%d"),as.Date("2013-07-30", "%Y-%m-%d")),ylim = c(0,18000)) +  scale_y_continuous(label=comma) 
    }
    
  })
  output$interesting_text1 <- renderText(
    { menu <- menuReactive()
      if(menu == '1'){
        temp = 'UIC COVID 19'
      }
      else if(menu == '2'){
        temp = 'Cubs Game'      }
      else if(menu == '3'){
        temp = '9/11 Attacks'      }
      else if(menu =='4'){
        temp = 'BLM Protests on Jun-01 and Jun-02'      }
      else if(menu =='5'){
        temp = 'Obama Casts his Vote'      }
      else if(menu =='6'){
        temp = 'UIC Winterbreak'      }
      else if(menu =='7'){
        temp = 'Train Derails'      }
      else if(menu =='8'){
        temp = 'OHare CTA Station Renovation'      }
      else if(menu =='9'){
        temp = 'Chicago Cubs victory'      }
      else{
        temp = 'Chicago Cubs Game 2013'      }
      return(temp)
    }
  )
  output$interesting_text2 <- renderText(
    {
      menu <- menuReactive()
      if(menu == '1'){
        temp = 'Sharp decline in number of riders at UIC-Halstead stop after WHO declares Covid-19 a pandemic in March 2020'
      }
      else if(menu == '2'){
        temp = 'Two Sharp spikes in ridership at OHare due to the Cubs vs Cardinals Baseball game on Sep 4th and Sep 6th 2020'
        }
      else if(menu == '3'){
        temp = 'We can see a sudden drop in rider count at the OHare CTA right after the 9/11 World Trade Center Attacks in 2001'
      }
      else if(menu =='4'){
        temp = 'Missing data for Junecan be attributed to sudden halting of services due to protests in the wake of George Floyd death'
      }
      else if(menu =='5'){
        temp = 'The sudden spike on 7-Oct-2008 in ridership can be attributed to a number of reasons - Obama arrives at OHare to cast his vote, first match for the Cubs world series begins, and Kanye West was performing at United center '
      }
      else if(menu =='6'){
        temp = 'Sharp decline in number of riders at UIC-Halstead stop from Dec 15 2018 to Jan 15 2018 due to Winter Break'
      }
      else if(menu =='7'){
        temp = 'Crash of Blue Line at OHare on March 24'
      }
      else if(menu =='8'){
        temp = 'No data for brief period as Ohare CTA shuts down for renovation'
      }
      else if(menu =='9'){
        temp = 'Huge spike due to Chicago Cubs victory parade and rally on 4th Nov. 2016'
      }
      else{
        temp = 'Spike in July due to Cubs game at Wrigley Field'
      }
      return(temp)
    }
  )
  output$plot1 <- renderPlot({
    ggplot(data=station1Reactive(), aes(x=year(date), y=rides)) + geom_bar(fill="deepskyblue4", stat="identity")+  scale_y_continuous(label=comma)+ labs(title = "Yearly Entries", x = "Year", y ="Rides")
  })
  
  output$plot1_right <- renderPlot({
    ggplot(data=station2Reactive(), aes(x=year(date), y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "Ridership over the years", x = "Year", y ="Rides")+ scale_y_continuous(label=comma)
  })
  
  output$plot2 <- renderPlot({
    timeframe <- input$timeframe1
    df <- year1Reactive()
    
    if(timeframe == 'date')
    {
      ggplot(data=df, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") +  scale_y_continuous(label=comma)+ labs(title = "Daily Entries ", x = "Date", y ="Rides")
    }
    
    else if(timeframe == 'month')
    {
      ggplot(data=df, aes(x=month(date, label = TRUE), y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") +  scale_y_continuous(label=comma)+ labs(title = "Monthly Entries ", x = "Month", y ="Rides")
    }
    else
    {
      #Day of the week
      ggplot(data=df, aes(x=wday(date, label = TRUE), y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") +  scale_y_continuous(label=comma)+ labs(title = "Entries per Day of the Week", x = "Day of the Week", y ="Rides")

    }
  })
  output$plot2_right <- renderPlot({
    timeframe <- input$timeframe2
    df <- year2Reactive()
    
    if(timeframe == 'date')
    {
      ggplot(data=df, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") +  scale_y_continuous(label=comma)+ labs(title = "Daily Entries ", x = "Date", y ="Rides")
    }
    
    else if(timeframe == 'month')
    {
      ggplot(data=df, aes(x=month(date, label = TRUE), y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") +  scale_y_continuous(label=comma)+ labs(title = "Monthly Entries ", x = "Month", y ="Rides")
    }
    else
    {
      #Day of the week
      ggplot(data=df, aes(x=wday(date, label = TRUE), y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") +  scale_y_continuous(label=comma)+ labs(title = "Entries per Day of the Week", x = "Day of the Week", y ="Rides")
      
    }
  })
  output$tab1 <- renderDataTable(rownames=FALSE, options=list(pageLength=5),
    {
      timeframe <- input$timeframe1
      df <- year1Reactive()
      if(timeframe == 'date')
      {
        df = df[, c('date','rides')]
      }
      
      else if(timeframe == 'month')
      {
        df = summarize( group_by( df,"Months" = month(date, label = TRUE) ), "Entries"=sum(rides)) 
      }
      else
      {
        #Day of the week
        df = summarize( group_by( df,"Days of the Week" = wday(date, label = TRUE) ), "Entries"=sum(rides))    }
      return(df)
      
    }
    )
  output$tab1_right <- renderDataTable(rownames=FALSE,options=list(pageLength=5),
    {
      timeframe <- input$timeframe2
      df <- year2Reactive()
      if(timeframe == 'date')
      {
        df = df[, c('date','rides')]
      }
      
      else if(timeframe == 'month')
      {
        df = summarize( group_by( df,"Months" = month(date, label = TRUE) ), "Entries"=sum(rides)) 
      }
      else
      {
        #Day of the week
        df = summarize( group_by( df,"Days of the Week" = wday(date, label = TRUE) ), "Entries"=sum(rides))    }
      return(df)
    }
  )

}


# Run the application 
shinyApp(ui = ui, server = server)
