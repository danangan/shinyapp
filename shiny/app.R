#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

bakery <- read.csv('data.csv')

bakery$Date <- ymd(bakery$Date)

bakery$weekday <- as.factor(weekdays(bakery$Date))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title="Sales Data"
  ),
   # Sidebar with a slider input for number of bins 
  dashboardSidebar(
    verticalLayout(
      dateRangeInput('dateRange', 'Date', start = min(bakery$Date), end = max(bakery$Date), max = max(bakery$Date), min = min(bakery$Date))
    ),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon=icon("home")),
      menuItem("Sales Time", tabName = "time", icon=icon("time", class = NULL, lib = "glyphicon"))
    )
   ),
  dashboardBody(
    tabItems(
      # Overview page
      tabItem("overview",
        fluidRow(
          infoBoxOutput("popularItem"),
          infoBoxOutput("totalSales"),
          infoBoxOutput("itemVariation")
        ),
        fluidRow(
          box(
            solidHeader=T,
            width=8,
            plotlyOutput("overviewPlot") 
          ),
          box(
            width=4,
            verticalLayout(
              radioButtons('displayTotal', 'Display sales', choices = c('Display total sales', 'Display by Item')),
              selectInput('showItem', 'Item', as.vector(head(bakery$Item, 10)), selected = c('Cookies', 'Coffee'), multiple = T),
              radioButtons('displayType', 'Show as', choices = c('One chart', 'Separated chart'))
            )
          )
        )
      ),
      tabItem("time",
        fluidRow(
          valueBoxOutput("bestDay"),
          valueBoxOutput("bestTime")
        ),
        fluidRow(
          box(
            solidHeader=T,
            width=12,
            plotlyOutput("timePlot", height = 600) 
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$overviewPlot <- renderPlotly({
      # draw the histogram with the specified number of bins
     bakery_ranged_day <- bakery %>%
       filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
     
     if(input$displayTotal == 'Display total sales') {
       display <- bakery_ranged_day %>%
         group_by(Date) %>%
         summarize(TotalSales=length(Date))
         
       plot <- ggplot(display, aes(x=Date, y=TotalSales)) +
         geom_line(col='goldenrod')+
         theme_dark()       
     } else{
       bakery_by_day <- bakery_ranged_day %>% 
         filter(Item == input$showItem) %>%
         group_by_(.dots=c("Date","Item")) %>% 
         summarize(Total=length(Item))
       
       if (input$displayType == 'One chart') {
         plot <- ggplot(bakery_by_day, aes(x=Date, y=Total, group=Item)) +
           geom_line(aes(col=Item))+
           theme_dark()
       } else {
         plot <- ggplot(bakery_by_day, aes(x=Date, y=Total)) +
          geom_line(aes(col=Item)) +    
         facet_grid(Item ~ .) +
        theme_dark() + theme(legend.position = 'none')
       }
     }
     
     plot <- plot + theme(legend.title = element_blank(), axis.title = element_blank())
     
     ggplotly(plot)
   })
   
   output$timePlot <- renderPlotly({
     # draw the histogram with the specified number of bins
     
     time <- bakery %>%
       filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))

      time$Time <- hms(time$Time)
       
      time$Hour <- hour(time$Time)
       
       x <- function(x) {
         if (x >= 7 && x < 10) {
           x <- '7am to 10am'
         } else if (x >= 10 && x < 13) {
           x <-'10am to 13pm'
         } else if (x >= 13 && x < 16) {
           x <- '13pm to 16pm'
         } else if (x >= 16 && x < 19) {
           x <- '16pm to 19pm'
         } else if (x >= 19 && x < 21) {
           x <- '19pm to 22pm'
         } else {
           x <- '> 22pm'
         }
       }
       
       time$TimePeriod <- mapply(time$Hour, FUN = x)
       
       time <- time %>%
         group_by(weekday, TimePeriod) %>%
         summarize(Total=length(Date))
       
    plot <- ggplot(time, aes(x = TimePeriod, y = Total)) + 
      geom_col(aes(fill=Total))+
      facet_grid(weekday ~ ., scale = "free_y")+
      theme_dark() +
      scale_fill_gradient(low = 'red3', high = 'green3')

     plot <- plot + theme(legend.position = 'none', axis.title = element_blank())
     
     ggplotly(plot)
   })

   output$bestTime <- renderValueBox({
     
     time <- bakery %>%
       filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
     
     time$Time <- hms(time$Time)
     
     time$Hour <- hour(time$Time)
     
     x <- function(x) {
       if (x >= 7 && x < 10) {
         x <- '7am to 10am'
       } else if (x >= 10 && x < 13) {
         x <-'10am to 13pm'
       } else if (x >= 13 && x < 16) {
         x <- '13pm to 16pm'
       } else if (x >= 16 && x < 19) {
         x <- '16pm to 19pm'
       } else if (x >= 19 && x < 21) {
         x <- '19pm to 22pm'
       } else {
         x <- '> 22pm'
       }
     }
     
     time$TimePeriod <- mapply(time$Hour, FUN = x)
     
     time <- time %>%
       group_by(weekday, TimePeriod) %>%
       summarize(Total=length(Date)) %>%
       arrange(-Total)
     
     valueBox(
       time[1, 'TimePeriod'], "Best sales time", icon = icon("time"),
       color = "blue"
     )
   })

   output$bestDay <- renderValueBox({
     
     time <- bakery %>%
       filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
     
     time$Time <- hms(time$Time)
     
     time$Hour <- hour(time$Time)
     
     x <- function(x) {
       if (x >= 7 && x < 10) {
         x <- '7am to 10am'
       } else if (x >= 10 && x < 13) {
         x <-'10am to 13pm'
       } else if (x >= 13 && x < 16) {
         x <- '13pm to 16pm'
       } else if (x >= 16 && x < 19) {
         x <- '16pm to 19pm'
       } else if (x >= 19 && x < 21) {
         x <- '19pm to 22pm'
       } else {
         x <- '> 22pm'
       }
     }
     
     time$TimePeriod <- mapply(time$Hour, FUN = x)
     
     time <- time %>%
       group_by(weekday, TimePeriod) %>%
       summarize(Total=length(Date)) %>%
       arrange(-Total)
     
     valueBox(
       time[[1, 'weekday']], "Best day for sales",
       color = "yellow"
     )
   })
   
   output$popularItem <- renderInfoBox({
     bakery_ranged_day <- bakery %>%
       filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
     
     item.by.totalSales <- bakery_ranged_day %>%
       group_by(Item) %>%
       summarize(total=length(Item)) %>%
       arrange(-total)
     
     infoBox(
       title = "Most Popular Item", 
       value = item.by.totalSales[1,1], icon=icon("trophy"), 
       subtitle = "Item with the most sales",
       color = "yellow"
     )
   })
   
   output$totalSales <- renderInfoBox({
     
     bakery_ranged_day <- bakery %>%
       filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
     
     item.by.totalSales <- bakery_ranged_day %>%
       group_by(Item) %>%
       summarize(total=length(Item)) %>%
       arrange(-total)
     
     infoBox(
       title = "Total All Sales", 
       value = sum(item.by.totalSales$total), icon=icon("money"), 
       subtitle = "Total sales within period",
       color = "green"
     )
   })
   
   output$itemVariation <- renderInfoBox({
     
     bakery_ranged_day <- bakery %>%
       filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
     
     item.by.totalSales <- bakery_ranged_day %>%
       group_by(Item) %>%
       summarize(total=length(Item)) %>%
       arrange(-total)
     
     infoBox(
       title = "Total Item Variation", 
       value = nrow(item.by.totalSales), icon=icon("apple", class = NULL, lib = "glyphicon"), 
       subtitle = "Item type sold within period",
       color = "blue"
     )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

