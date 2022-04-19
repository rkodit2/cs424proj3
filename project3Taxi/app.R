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
library(ggplot2)
library(lubridate)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(ggmap)
library(maps)
library(mapdata)
library(ggthemes)
library(sp)
library(stringr)
library(plyr)
library(dplyr)
library(DT)
# library(measurements)
require(scales)

options(scipen=10000)

temp = list.files(pattern="*_1.csv")
allData2 <- lapply(temp, read.csv)
allData3 <- do.call(rbind, allData2)
temp2 = list.files(pattern="*Company_names.csv")
allDataLL <- lapply(temp2, read.csv)
allDataLL3 <- do.call(rbind, allDataLL)


lubridateDate <- mdy_hms(allData3$'Trip.Start.Timestamp')

allData3$lubridateDate <- lubridateDate
allData3$month <- month(lubridateDate)
allData3$day <- day(lubridateDate)
allData3$year <- year(lubridateDate)
allData3$hour <- hour(lubridateDate)
allData3$minute <- minute(lubridateDate)
allData3$second <- second(lubridateDate)
allData3$weekday <- weekdays(allData3$lubridateDate)

allData3$lubridateDateOnly <- as.Date(mdy_hms(allData3$'Trip.Start.Timestamp'))


milesBreak <- c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)
kmBreak <- round(1.60934*milesBreak,3)
#kmBreak <- round(conv_unit(milesBreak, "mi", "km"),3)




daycount <- allData3 %>% group_by(lubridateDateOnly) %>%summarise(count = n())
hourcount <- allData3 %>% group_by(hour) %>%summarise(count = n())
hourcount$hourAMPM <- factor(hourcount$hour, labels = c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM"))
hourcount$hour <- factor(hourcount$hour, labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))

weekdaycount <- allData3 %>% group_by(weekday) %>%summarise(count = n())
weekdaycount$weekday <- factor(weekdaycount$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
monthcount <- allData3 %>% group_by(month) %>%summarise(count = n())
monthcount$month <- factor(monthcount$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"))
mileagecount <- allData3 %>% group_by(Trip.Miles) %>%summarise(count = n())
# mileagecount$km <- round(conv_unit(mileagecount$Trip.Miles, "mi", "km"),3)
mileagecount$km <- round(1.60934*mileagecount$Trip.Miles,3)
mileagecount <- mileagecount %>% mutate(mileage_bin = cut(Trip.Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
mileagecountFinal <- mileagecount %>% mutate(km_bin = cut(km, breaks=kmBreak))
mileagecountkmbin <- mileagecountFinal %>% group_by(km_bin) %>% summarise(Frequency = sum(count))
mileagecountmilesbin <- mileagecountFinal %>% group_by(mileage_bin) %>% summarise(Frequency = sum(count))

triptimecount <- allData3 %>% group_by(Trip.Seconds) %>%summarise(count = n())
triptimecount1 <- triptimecount %>% mutate(trip_time_bin = cut(Trip.Seconds, breaks=c(59,120,180,240,300,360,420,480,540,600,660,720,800,860,920,980,1020,1080,1140,1200,1260,1320,1380,1440,1500,1560,1620,1680,1740,1800,1860,1920,1980,2040,2100,2250,2500,2750,3000,3250,3500,3750,4000,5000,7500,10000,18000)))
triptimecount1bin <- triptimecount1 %>% group_by(trip_time_bin) %>% summarise(Frequency = sum(count))

pages <- c("Home","About Page")





ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
    
    # actionButton("reset_button", "Reset Map"),
    # selectInput("alphabetmaxmin", h3("Order of Display"), 
    #             choices = list("Alphabetical" = 1,
    #                            "Min-Max" = 2), selected = 1),
    selectInput("chart1", h3("Bar/Table Theme"), 
                choices = list("Barchart" = 1,
                               "Table" = 2), selected = 1),
    selectInput("miles_km", h3("miles/km"), 
                choices = list("miles" = 1,
                               "km" = 2), selected = 1),
    selectInput("hourmode", h3("AM-PM/24hour mode"), 
                choices = list("AM-PM" = 1,
                               "24hour" = 2), selected = 1),
    hr(),
    selectInput("page1", h3("Select the page"), pages, selected = "Home"),
    # hr(),
    # actionButton("prev_button","Previous Day"),
    # actionButton("next_button","Next Day"),
    # dateInput('date',
    #           label = ('Date input: yyyy-mm-dd'),
    #           value = "2021-08-23"
    # ),
    # hr(),
    # htmlOutput("textsidebar"),
    # dateInput('date1',
    #           label = ('Date1 for comparison'),
    #           value = "2021-08-23"
    # ),
    # dateInput('date2',
    #           label = ('Date2 for comparison'),
    #           value = "2021-08-24"
    # ),
    
    actionButton("enter_button", "Compare Dates"),
    actionButton("reset_bar", "Stop Compare")
    
  ),
  dashboardBody(
    conditionalPanel(
      condition = "input.page1 == 'Home'",
      # mainPanel(
      h1(textOutput("dateText")),
      fluidRow(
        column(6,
               fluidRow(
                 column(6,
                     # leafletOutput("mymap", height=700)
                 ),
                 column(6,
                        # box(title = "Total CTA Entries for all stations", solidHeader = TRUE, status = "primary", width = 12,
                        #     DTOutput("tbBarchart", height=600) 
                        # )
                 )
               )
        ),
        column(2,
               fluidRow(
                 box(title = "Rides in 2019 by day of year", solidHeader = TRUE, status = "primary", width = 12,
                     conditionalPanel(
                       condition = "input.chart1 == '1'",
                       plotOutput("hist1", height=600)
                     )
                     , conditionalPanel(
                       condition = "input.chart1 == '2'",
                       DTOutput("tb1", height=600)
                     )
                 )
               ),
               fluidRow(
                 box(title = "Rides in 2019 by hour of day", solidHeader = TRUE, status = "primary", width = 12,
                     conditionalPanel(
                       condition = "input.chart1 == '1' && input.hourmode == '1'",
                       plotOutput("hist2", height=600)
                     ),
                     conditionalPanel(
                       condition = "input.chart1 == '1' && input.hourmode == '2'",
                       plotOutput("hist2Military", height=600)
                     )
                     , conditionalPanel(
                       condition = "input.chart1 == '2' && input.hourmode == '1'",
                       DTOutput("tb2", height=600)
                     )
                     , conditionalPanel(
                       condition = "input.chart1 == '2' && input.hourmode == '2'",
                       DTOutput("tb2Military", height=600)
                     )
               ),
        ),
        ),
        column(2,
               fluidRow(
                 box(title = "Rides in 2019 by day of week", solidHeader = TRUE, status = "primary", width = 12,
                     conditionalPanel(
                       condition = "input.chart1 == '1'",
                       plotOutput("hist3", height=600)
                     )
                     , conditionalPanel(
                       condition = "input.chart1 == '2'",
                       DTOutput("tb3", height=600)
                     )
                 )
               ),
               fluidRow(
                 box(title = "Rides in 2019 by month of year", solidHeader = TRUE, status = "primary", width = 12,
                     conditionalPanel(
                       condition = "input.chart1 == '1'",
                       plotOutput("hist4", height=600)
                     )
                     , conditionalPanel(
                       condition = "input.chart1 == '2'",
                       DTOutput("tb4", height=600)
                     )
                 )
               ),
        ),
        column(2,
               fluidRow(
                   box(title = "Rides in 2019 by binned distance unit", solidHeader = TRUE, status = "primary", width = 12,
                     conditionalPanel(
                       condition = "input.chart1 == '1' && input.miles_km == '1'",
                       plotOutput("hist5", height=600)
                     ),
                     conditionalPanel(
                       condition = "input.chart1 == '1' && input.miles_km == '2'",
                       plotOutput("histkm", height=600)
                     )
                     , conditionalPanel(
                       condition = "input.chart1 == '2' && input.miles_km == '1'",
                       DTOutput("tb5", height=600)
                     )
                     , conditionalPanel(
                       condition = "input.chart1 == '2' && input.miles_km == '2'",
                       DTOutput("tbkm", height=600)
                     )
                 )
               ),
               fluidRow(
                 box(title = "Rides in 2019 by binned trip time", solidHeader = TRUE, status = "primary", width = 12,
                     conditionalPanel(
                       condition = "input.chart1 == '1'",
                       plotOutput("hist6", height=600)
                     )
                     , conditionalPanel(
                       condition = "input.chart1 == '2'",
                       DTOutput("tb6", height=600)
                     )
                 )
               ),
        ),
      ),
    ),
    conditionalPanel(
      condition = "input.page1 == 'About Page'",
      column(8,
             fluidRow(
               h3("The data is from City of Chicago https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f
                and https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme
                and the web app is made by Soel Mullenkuzhiyil Sunny and Rajashree Kodithyala last updated on 03/12/2022 and was made to compare the CTA entries between different stations from 2001-2021")
             ),
             fluidRow(h3(
               "This application allows you to visualize the number of rides on a given date for each CTA station in Chicago. It shows a bar chart and table for every station for the specified date. There are two buttons called previous day and next to be able to change the data easily. 
                 It also shows a map with markers to point out the locations of every station. If the marker is clicked, it will 
                 show the station name and the number of rides for that specific date. It was also display a few more bar charts to the right to help understand the data better. You can choose to see the bar charts as a table format as well. The map allows you to change between 
                 three different themes and these options are on the map itself. The map can be resetted back to the original location by using the reset button. In addition, the application lets you pick two dates to compare and the bar chart and its table should reflect these changes using a side by side bar chart."
             ))
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$hist1 <- renderPlot({
      ggplot(daycount, aes(x=lubridateDateOnly, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))

    })

    output$hist2 <- renderPlot({
      ggplot(hourcount, aes(x=hourAMPM, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })
    
    output$hist2Military <- renderPlot({
      ggplot(hourcount, aes(x=hour, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })
    

    output$hist3 <- renderPlot({
      ggplot(weekdaycount, aes(x=weekday, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })

    output$hist4 <- renderPlot({
      ggplot(monthcount, aes(x=month, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })

    output$hist5 <- renderPlot({
      ggplot(mileagecountmilesbin, aes(x= mileage_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })
    
    output$histkm <- renderPlot({
      ggplot(mileagecountkmbin, aes(x= km_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })

    output$hist6 <- renderPlot({
      ggplot(triptimecount1bin, aes(x= trip_time_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })
    
    output$tb1 = renderDT({
      dfbar <- data.frame(
        date = daycount$lubridateDateOnly,
        rides = daycount$count
      )
      dfbar <- dfbar[order(dfbar$date),]
      datatable(dfbar,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
    })
    
    output$tb2 = renderDT({
      dfbar1 <- data.frame(
        hours = hourcount$hourAMPM,
        rides = hourcount$count
      )
      dfbar1 <- dfbar1[order(dfbar1$hours),]
      datatable(dfbar1,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
    })
    
    output$tb2Military = renderDT({
      dfbar1 <- data.frame(
        hours = hourcount$hour,
        rides = hourcount$count
      )
      dfbar1 <- dfbar1[order(dfbar1$hours),]
      datatable(dfbar1,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
    })
    
    output$tb3 = renderDT({
      dfbar2 <- data.frame(
        weekdays = weekdaycount$weekday,
        rides = weekdaycount$count
      )
      dfbar2 <- dfbar2[order(dfbar2$weekdays),]
      datatable(dfbar2,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
    })
    
    output$tb4 = renderDT({
      dfbar3 <- data.frame(
        months = monthcount$month,
        rides = monthcount$count
      )
      dfbar3 <- dfbar3[order(dfbar3$months),]
      datatable(dfbar3,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
    })
    
    output$tb5 = renderDT({
      dfbar4 <- data.frame(
        miles = mileagecountmilesbin$mileage_bin,
        rides = mileagecountmilesbin$Frequency
      )
      dfbar4 <- dfbar4[order(dfbar4$miles),]
      datatable(dfbar4,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
    })
    
    output$tbkm = renderDT({
      dfbar4 <- data.frame(
        km = mileagecountkmbin$km_bin,
        rides = mileagecountkmbin$Frequency
      )
      dfbar4 <- dfbar4[order(dfbar4$km),]
      datatable(dfbar4,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
    })
    
    output$tb6 = renderDT({
      dfbar5 <- data.frame(
        time = triptimecount1bin$trip_time_bin,
        rides = triptimecount1bin$Frequency
      )
      dfbar5 <- dfbar5[order(dfbar5$time),]
      datatable(dfbar5,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
