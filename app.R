#CS 424 -Spring 22- Project - Krishnan CS

#libraries to include
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(dplyr)

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

#converting date format
uic_data$date <- ymd((uic_data$date))
ohare_data$date <- ymd((ohare_data$date))
addison_data$date <- ymd((addison_data$date))

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
                  plotOutput("intersting")
                )
              ),
              fluidPage(
                box(
                  title = "Choose menu option to see some interesting dates in the data!",
                  width = 5,
                  solidHeader = TRUE, status = "primary",
                selectInput(inputId = "menu", label = NULL, c("1" = 1 ), selectize = FALSE),
                )
              )
      )
)
))
  
# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
  
  output$interesting <- renderPlot({
    menu <- menuReactive()
    if(menu == 1){
      ggplot(uic_data, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") + labs(title = "Rider data over the years UIC Halstead", x = "Date", y ="Rides") + coord_cartesian(xlim = c(as.Date("2020-01-01", "%Y-%m-%d"),as.Date("2020-06-01", "%Y-%m-%d"))) 
    }
    
  })
  
  output$plot1 <- renderPlot({
    ggplot(data=station1Reactive(), aes(x=year(date), y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") 
  })
  
  output$plot1_right <- renderPlot({
    ggplot(data=station2Reactive(), aes(x=year(date), y=rides)) + geom_bar(fill="deepskyblue4", stat="identity") 
  })
  
  output$plot2 <- renderPlot({
    timeframe <- input$timeframe1
    df <- year1Reactive()
    
    if(timeframe == 'date')
    {
      ggplot(data=df, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity")
    }
    
    else if(timeframe == 'month')
    {
      ggplot(data=df, aes(x=month(date, label = TRUE), y=rides)) + geom_bar(fill="deepskyblue4", stat="identity")
    }
    else
    {
      #Day of the week
      ggplot(data=df, aes(x=wday(date, label = TRUE), y=rides)) + geom_bar(fill="deepskyblue4", stat="identity")

    }
  })
  output$plot2_right <- renderPlot({
    timeframe <- input$timeframe2
    df <- year2Reactive()
    
    if(timeframe == 'date')
    {
      ggplot(data=df, aes(x=date, y=rides)) + geom_bar(fill="deepskyblue4", stat="identity")
    }
    
    else if(timeframe == 'month')
    {
      ggplot(data=df, aes(x=month(date, label = TRUE), y=rides)) + geom_bar(fill="deepskyblue4", stat="identity")
    }
    else
    {
      #Day of the week
      ggplot(data=df, aes(x=wday(date, label = TRUE), y=rides)) + geom_bar(fill="deepskyblue4", stat="identity")
      
    }
  })
  output$tab1 <- renderDataTable(
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
        df = summarize( group_by( df,"Days of the Week" = wday(date, label = TRUE) ), "Entries"=sum(rides))      }
      return(df)
    }
    )
  output$tab1_right <- renderDataTable(
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
        df = summarize( group_by( df,"Days of the Week" = wday(date, label = TRUE) ), "Entries"=sum(rides))      }
      return(df)
    }
  )

}


# Run the application 
shinyApp(ui = ui, server = server)
