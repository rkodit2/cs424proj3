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
library(leaflet)
library(scales)
library(dplyr)
library(DT)
library(rgdal)
library(readr)
library(data.table)

# library(measurements)


options(scipen=999)

temp = list.files(pattern="*_1.csv")
allData2 <- lapply(temp, fread)
allData3 <- do.call(rbind, allData2)
# temp2 = list.files(pattern="*Company_names.csv")
# allDataLL <- lapply(temp2, read_csv)
# allDataLL3 <- do.call(rbind, allDataLL)

# temp3 = list.files(pattern="*Areas.csv")
# allDataCA <- lapply(temp3, read.csv)
# commArea <- do.call(rbind, allDataCA)

# shfile <- st_read("CA/geo_export_2bbe4e78-a3d0-4e60-b4e3-8e1ceaa042d3.shp")
# downtown<-st_geometry(shfile)

downtown <- readOGR("CA/geo_export_2bbe4e78-a3d0-4e60-b4e3-8e1ceaa042d3.shp",
                    layer = "geo_export_2bbe4e78-a3d0-4e60-b4e3-8e1ceaa042d3", GDAL1_integer64_policy = TRUE)

allData3$lubridateDate <- mdy_hms(allData3$'Trip_Start_Timestamp')

allData3$month <- month(allData3$lubridateDate)
allData3$hour <- hour(allData3$lubridateDate)
allData3$weekday <- wday(allData3$lubridateDate)

allData3$lubridateDateOnly <- as.Date(allData3$lubridateDate)


data1 <- fread("FinalCommAreas.csv")
pickupCA <- allData3 %>% group_by(Pickup_Community_Area) %>% summarise(count = n()) %>% mutate(freq = round(count / sum(count), 4)*100)
dropCA <- allData3 %>% group_by(Drop_off_community_Area) %>% summarise(count = n()) %>% mutate(freq = round(count / sum(count), 4)*100)
pickupCAFinal <- merge(pickupCA,data1, by.x  = "Pickup_Community_Area", by.y="AREA_NUMBE")
dropCAFinal <- merge(dropCA,data1, by.x  = "Drop_off_community_Area", by.y="AREA_NUMBE")


downtownFinal1 <- merge(downtown,pickupCAFinal, by.x  = "area_numbe", by.y="Pickup_Community_Area")
downtownFinal2 <- merge(downtown,dropCAFinal, by.x  = "area_numbe", by.y="Drop_off_community_Area") 






milesBreak <- c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)
kmBreak <- round(1.60934*milesBreak,3)
#kmBreak <- round(conv_unit(milesBreak, "mi", "km"),3)




daycount <- allData3 %>% group_by(lubridateDateOnly) %>%summarise(count = n())
hourcount <- allData3 %>% group_by(hour) %>%summarise(count = n())
hourcount$hourAMPM <- factor(hourcount$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM"))
hourcount$hour <- factor(hourcount$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))

weekdaycount <- allData3 %>% group_by(weekday) %>%summarise(count = n())
weekdaycount$weekday <- factor(weekdaycount$weekday, levels = c(1,2,3,4,5,6,7), labels = c("Sun","Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
monthcount <- allData3 %>% group_by(month) %>%summarise(count = n())
monthcount$month <- factor(monthcount$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"))

mileagecount <- allData3 %>% group_by(Trip_Miles) %>%summarise(count = n())
mileagecount$km <- round(1.60934*mileagecount$Trip_Miles,3)
mileagecount <- mileagecount %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
mileagecountFinal <- mileagecount %>% mutate(km_bin = cut(km, breaks=kmBreak))
mileagecountkmbin <- mileagecountFinal %>% group_by(km_bin) %>% summarise(Frequency = sum(count))
mileagecountmilesbin <- mileagecountFinal %>% group_by(mileage_bin) %>% summarise(Frequency = sum(count))

triptimecount <- allData3 %>% group_by(Trip_Seconds) %>%summarise(count = n())
triptimecount1 <- triptimecount %>% mutate(trip_time_bin = cut(Trip_Seconds, breaks=c(59,120,180,240,300,360,420,480,540,600,660,720,800,860,920,980,1020,1080,1140,1200,1260,1320,1380,1440,1500,1560,1620,1680,1740,1800,1860,1920,1980,2040,2100,2250,2500,2750,3000,3250,3500,3750,4000,5000,7500,10000,18000), dig.lab=7))
triptimecount1bin <- triptimecount1 %>% group_by(trip_time_bin) %>% summarise(Frequency = sum(count))



pages <- c("Home","About Page")

company_names_df <- fread("Company_names.csv")





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
    selectInput("company_name", h3("Company Names"), choices = sort(company_names_df$Company), selected = "1085 - 72312 N and W Cab Co"),
    selectInput("chart1", h3("Bar/Table Theme"), 
                choices = list("Barchart" = 1,
                               "Table" = 2), selected = 1),
    selectInput("miles_km", h3("miles/km"), 
                choices = list("miles" = 1,
                               "km" = 2), selected = 1),
    selectInput("hourmode", h3("AM-PM/24hour mode"), 
                choices = list("AM-PM" = 1,
                               "24hour" = 2), selected = 1),
    selectInput("view_data", h3("View Data"), 
                choices = list("Community Area" = 1,
                               "Taxi Company" = 2), selected = 1),
    selectInput("ca_mode", h3("PickUp/DropOff"), 
                choices = list("PickUp(From Community)" = 1,
                               "DropOff(To Community)" = 2), selected = 1),
    actionButton("reset_bar", "Reset Graphs"),
    # hr(),
    selectInput("page1", h3("Select the page"), pages, selected = "Home")
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
    
    
  ),
  dashboardBody(
    conditionalPanel(
      condition = "input.page1 == 'Home'",
      # mainPanel(
      h1(textOutput("dateText")),
      fluidRow(
        column(3,
               fluidRow(
                 box(title = "Rides in Community Area", solidHeader = TRUE, status = "primary", width = 12, height = 675,
                 conditionalPanel(
                   condition = "input.ca_mode == '1' && input.view_data == '1'",
                   plotOutput("pickupCAPlot", height=600)
                 )
                 , conditionalPanel(
                   condition = "input.ca_mode == '2' && input.view_data == '1'",
                   plotOutput("dropCAPlot", height=600)
                 )
                 )
               ),
               fluidRow(
                 # leafletOutput("mymap", height=900)
                 conditionalPanel(
                   condition = "input.ca_mode == '1' && input.view_data == '1'",
                   leafletOutput("mymapPickup", height=600)
                 )
                 , conditionalPanel(
                   condition = "input.ca_mode == '2' && input.view_data == '1'",
                   leafletOutput("mymapDropOff", height=600)
                 ), 
                 conditionalPanel(
                   condition = "input.ca_mode == '2' && input.view_data == '2'",
                   leafletOutput("mymapDropoffCompany", height=600)
                 )
                 ,
                 conditionalPanel(
                   condition = "input.ca_mode == '1' && input.view_data == '2'",
                   leafletOutput("mymapPickUpCompany", height=600)
                 )
               )
        ),
        column(3,
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
        column(4,
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
               h3("The data is from City of Chicago https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy and https://www.chicago.gov/content/dam/city/depts/doit/general/GIS/Chicago_Maps/Citywide_Maps/Community_Areas_W_Numbers.pdfand and https://en.wikipedia.org/wiki/Community_areas_in_Chicago and https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6. The web app is made by Soel Mullenkuzhiyil Sunny
               and Rajashree Kodithyala last updated on 04/23/2022 and was made to vizualize Taxi rides for Community areas in Chicago during 2019. The application shows different
                  bar charts and tables to analyze the data. There are two different modes to view data. One is for Community Areas in Chicago
                  and one is for Taxi companies. The user has the option to select which mode and depending on the mode it will display the 
                  corresponding heat map. The map allows the user to click on a Community area and it will show the to/from percentage of rides 
                  associated with the selected Community. The user has control over viewing pick up data or drop off data for either 
                  Communites or for Taxi companies. The user can specify which Taxi company to view as well. Selecting a Community will 
                  update the original bar charts for that specific community. The map and the bar charts can be resetted back to the
                  original bar charts.")
             )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$hist1 <- renderPlot({
      ggplot(daycount, aes(x=lubridateDateOnly, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Total Rides per Day")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))

    })

    output$hist2 <- renderPlot({
      ggplot(hourcount, aes(x=hourAMPM, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Hour", title="Total Rides per Hour")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })
    
    output$hist2Military <- renderPlot({
      ggplot(hourcount, aes(x=hour, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Hour", title="Total rides per Hour")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })
    

    output$hist3 <- renderPlot({
      ggplot(weekdaycount, aes(x=weekday, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Weekday", title="Total rides per Weekday")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })

    output$hist4 <- renderPlot({
      ggplot(monthcount, aes(x=month, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Month", title="Total Rides per Month")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })

    output$hist5 <- renderPlot({
      ggplot(mileagecountmilesbin, aes(x= mileage_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Mileage Bin", title="Total Rides per Mileage Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })
    
    output$histkm <- renderPlot({
      ggplot(mileagecountkmbin, aes(x= km_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Kilometer bin", title="Total Rides per KM Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })

    output$hist6 <- renderPlot({
      ggplot(triptimecount1bin, aes(x= trip_time_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Time Bin", title="Total Rides per Trip Time Bin in Seconds")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })
    
    # output$pickupCAPlot <- renderPlot({
    #   ggplot(pickupCAFinal, aes(x= COMMUNITY, y=freq))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    # })
    
    # output$dropCAPlot <- renderPlot({
    #   ggplot(dropCAFinal, aes(x= COMMUNITY, y=freq))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    # })
    
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
    
    
    observeEvent(
      input$reset_bar,{
        
        from_to_freq_bar <- data.frame(
          COMMUNITY = c(0),
          freq = c(0)
        )
        
        to_from_freq_bar <- data.frame(
          COMMUNITY = c(0),
          freq = c(0)
        )
        
        output$pickupCAPlot <- renderPlot({
          ggplot(from_to_freq_bar)
        })
        
        output$dropCAPlot <- renderPlot({
          ggplot(to_from_freq_bar)
        })
        
        output$hist1 <- renderPlot({
          ggplot(daycount, aes(x=lubridateDateOnly, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Total Rides per Day")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
          
        })
        
        output$hist2 <- renderPlot({
          ggplot(hourcount, aes(x=hourAMPM, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Hour", title="Total Rides per Hour")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        })
        
        output$hist2Military <- renderPlot({
          ggplot(hourcount, aes(x=hour, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Hour", title="Total rides per Hour")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        })
        
        
        output$hist3 <- renderPlot({
          ggplot(weekdaycount, aes(x=weekday, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Weekday", title="Total rides per Weekday")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        })
        
        output$hist4 <- renderPlot({
          ggplot(monthcount, aes(x=month, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Month", title="Total Rides per Month")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        })
        
        output$hist5 <- renderPlot({
          ggplot(mileagecountmilesbin, aes(x= mileage_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Mileage Bin", title="Total Rides per Mileage Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        })
        
        output$histkm <- renderPlot({
          ggplot(mileagecountkmbin, aes(x= km_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Kilometer bin", title="Total Rides per KM Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        })
        
        output$hist6 <- renderPlot({
          ggplot(triptimecount1bin, aes(x= trip_time_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Time Bin", title="Total Rides per Trip Time Bin in Seconds")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        })
        
        # output$pickupCAPlot <- renderPlot({
        #   ggplot(pickupCAFinal, aes(x= COMMUNITY, y=freq))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        # })
        # 
        # output$dropCAPlot <- renderPlot({
        #   ggplot(dropCAFinal, aes(x= COMMUNITY, y=freq))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        # })
        # 
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
        
        
        output$mymapPickup <- renderLeaflet({
          # nnpal <- colorNumeric(colorFactor("Blues", NULL), domain = downtownFinal1$freq)
          leaflet(downtown) %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10) %>% addProviderTiles("CartoDB.Positron", group="bg1") %>%
            addPolygons(stroke = TRUE,
                        color = "grey",
                        weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe,label = ~community,
                        highlightOptions = highlightOptions(color = "red", weight = 2,
                                                            bringToFront = TRUE))
          # addLegend(pal = nnpal, values = ~freq, title = "Percentage" , opacity = 1)
        })
        
        
        output$mymapDropOff <- renderLeaflet({
          # nnpal <- colorNumeric(colorFactor("Blues", NULL), domain = downtownFinal1$freq)
          leaflet(downtown) %>% addProviderTiles("CartoDB.Positron", group="bg1") %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10) %>%
            addPolygons(stroke = TRUE,
                        color = "grey",
                        weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe, label = ~community,
                        highlightOptions = highlightOptions(color = "red", weight = 2,
                                                            bringToFront = TRUE))
          # addLegend(pal = nnpal, values = ~freq, title = "Percentage" , opacity = 1)
        })
        
        
        output$mymapDropoffCompany <- renderLeaflet({
          leaflet(downtown) %>% addProviderTiles("CartoDB.Positron", group="bg1") %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10) %>%
            addPolygons(stroke = TRUE,
                        color = "grey",
                        weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe, label = ~community,
                        highlightOptions = highlightOptions(color = "red", weight = 2,
                                                            bringToFront = TRUE))
        })
        
        
        output$mymapPickUpCompany <- renderLeaflet({
          leaflet(downtown) %>% addProviderTiles("CartoDB.Positron", group="bg1") %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10) %>%
            addPolygons(stroke = TRUE,
                        color = "grey",
                        weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe, label = ~community,
                        highlightOptions = highlightOptions(color = "red", weight = 2,
                                                            bringToFront = TRUE))
        })
        
        
        
      }
  )
    

    
    observe({
      click <- input$mymapPickup_shape_click
      # #print(click)
      # paste("Hello")
      if (is.null(click)) return()
      #print(click$id)
      
      # find_company <- company_names_df[company_names_df$id == input$company_name]
      

        reactivePickUp <- subset(allData3, Pickup_Community_Area == (click$id))

      

      
      
      from_to_freq <- reactivePickUp %>% group_by(Drop_off_community_Area) %>% summarise(count = n()) %>% mutate(freq = round(count / sum(count), 4)*100)
      q <- setdiff(1:77,from_to_freq$Drop_off_community_Area)
      
      if(length(q) > 0) {

        z <- c(q)
        #print(z)
        #print("Hello")
        for (i in 1:length(z)) {
          # from_to_freq[nrow(from_to_freq) + 1,] = c(z[i],0,0)
          # #print(z[i])
          # #print(z[[i]])
          w <- c(z[[i]],0,0)
          from_to_freq[nrow(from_to_freq) + 1, 1:3] <- as.list(w)
        }
      }
      
      from_to_freq_bar <- merge(from_to_freq,data1, by.x  = "Drop_off_community_Area", by.y="AREA_NUMBE")
      
      
      downtownFromTo <- merge(downtown,from_to_freq, by.x  = "area_numbe", by.y="Drop_off_community_Area") 
      
      
      reactivePickUpDayCount <- reactive({
        ##print(paste("Station name is:",stationClicked))
        reactivePickUp %>% group_by(lubridateDateOnly) %>%summarise(count = n())
      })
      
      
      
      reactivePUHourCount <- reactive({
        reactivePickUp %>% group_by(hour) %>%summarise(count = n())
      })
      
      
      
      # reactivePUWeekdaycount <- reactive({
      #   reactivePickUp %>% group_by(weekday) %>%summarise(count = n())
      # })
      
      
      reactivePUMonthCount <- reactive({
        reactivePickUp %>% group_by(month) %>%summarise(count = n())
      })
      
      reactivePUMileage <- reactive({
        reactivePickUp %>% group_by(Trip_Miles) %>%summarise(count = n())
      })
      
      
      reactivePUTripTime <- reactive({
        reactivePickUp %>% group_by(Trip_Seconds) %>%summarise(count = n())
      })
      
    
    
      
      output$tb1 = renderDT({
         ny1 <- reactivePickUpDayCount()
        
        dfbar <- data.frame(
          date = ny1$lubridateDateOnly,
          rides = ny1$count
        )
        dfbar <- dfbar[order(dfbar$date),]
        datatable(dfbar,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb2 = renderDT({
        # ny1 <- reactivePUHourCount()
        ny1 <- reactivePickUp %>% group_by(hour) %>% summarise(count = n())
        ny1$hourAMPM <- factor(ny1$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM"))
        
        dfbar1 <- data.frame(
          hours = ny1$hourAMPM,
          rides = ny1$count
        )
        dfbar1 <- dfbar1[order(dfbar1$hours),]
        datatable(dfbar1,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb2Military = renderDT({
        # ny1 <- reactivePUHourCount()
        ny1 <- reactivePickUp %>% group_by(hour) %>% summarise(count = n())
        ny1$hour <- factor(ny1$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
        
        
        dfbar1 <- data.frame(
          hours = ny1$hour,
          rides = ny1$count
        )
        dfbar1 <- dfbar1[order(dfbar1$hours),]
        datatable(dfbar1,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb3 = renderDT({
        
        ny1 <- reactivePickUp %>% group_by(weekday) %>% summarise(count = n())
        # ny1 <- reactivePUWeekdaycount
        ny1$weekday <- factor(ny1$weekday,levels = c(1,2,3,4,5,6,7), labels = c("Sun","Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
      
        
        dfbar2 <- data.frame(
          weekdays = ny1$weekday,
          rides = ny1$count
        )
        dfbar2 <- dfbar2[order(dfbar2$weekdays),]
        datatable(dfbar2,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb4 = renderDT({
        ny1 <- reactivePUMonthCount()
        ny1$month <- factor(ny1$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"))
        
        
        dfbar3 <- data.frame(
          months = ny1$month,
          rides = ny1$count
        )
        dfbar3 <- dfbar3[order(dfbar3$months),]
        datatable(dfbar3,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb5 = renderDT({
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        ny2bin <- ny2 %>% group_by(mileage_bin) %>% summarise(Frequency = sum(count))
        
        dfbar4 <- data.frame(
          miles = ny2bin$mileage_bin,
          rides = ny2bin$Frequency
        )
        dfbar4 <- dfbar4[order(dfbar4$miles),]
        datatable(dfbar4,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tbkm = renderDT({
        
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
      
        
        ny2bin <- ny2 %>% group_by(km_bin) %>% summarise(Frequency = sum(count))
        
        dfbar4 <- data.frame(
          km = ny2bin$km_bin,
          rides = ny2bin$Frequency
        )
        dfbar4 <- dfbar4[order(dfbar4$km),]
        datatable(dfbar4,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb6 = renderDT({
        
        ny1 <- reactivePUTripTime()
        
        ny2 <- ny1 %>% mutate(trip_time_bin = cut(Trip_Seconds, breaks=c(59,120,180,240,300,360,420,480,540,600,660,720,800,860,920,980,1020,1080,1140,1200,1260,1320,1380,1440,1500,1560,1620,1680,1740,1800,1860,1920,1980,2040,2100,2250,2500,2750,3000,3250,3500,3750,4000,5000,7500,10000,18000), dig.lab=7))
        ny2bin <- ny2 %>% group_by(trip_time_bin) %>% summarise(Frequency = sum(count))
        
        dfbar5 <- data.frame(
          time = ny2bin$trip_time_bin,
          rides = ny2bin$Frequency
        )
        dfbar5 <- dfbar5[order(dfbar5$time),]
        datatable(dfbar5,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$hist1 <- renderPlot({
        ny1 <- reactivePickUpDayCount()
        ggplot(ny1, aes(x=lubridateDateOnly, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides in Community", x="Date", title="Total Rides per Day")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        
      })
      
      output$hist2 <- renderPlot({
        ny1 <- reactivePUHourCount()
        ##print(ny1)
        ny1$hourAMPM <- factor(ny1$hour, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM"))
        
        ggplot(ny1, aes(x=hourAMPM, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Hour", title="Total Rides per Hour")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist2Military <- renderPlot({
        ny1 <- reactivePUHourCount()
        ny1$hour <- factor(ny1$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
        
        ggplot(ny1, aes(x=hour, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Hour", title="Total Rides per Hour")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      
      output$hist3 <- renderPlot({
        # ny1 <- reactivePUWeekdaycount
        ny1 <- reactivePickUp %>% group_by(weekday) %>% summarise(count = n())
        
        ny1$weekday <- factor(ny1$weekday, levels = c(1,2,3,4,5,6,7), labels = c("Sun","Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
        
        ggplot(ny1, aes(x=weekday, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Weekday", title="Total Rides per Weekday")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist4 <- renderPlot({
        ny1 <- reactivePUMonthCount()
        ny1$month <- factor(ny1$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"))
        
        ggplot(ny1, aes(x=month, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Month", title="Total Rides per Month")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist5 <- renderPlot({
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        ny2bin <- ny2 %>% group_by(mileage_bin) %>% summarise(Frequency = sum(count))
        
        ggplot(ny2bin, aes(x= mileage_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Mileage Bin", title="Total Rides per Mileage Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$histkm <- renderPlot({
        
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        
        ny2bin <- ny2 %>% group_by(km_bin) %>% summarise(Frequency = sum(count))
        
        ggplot(ny2bin, aes(x= km_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Kilometer Bin", title="Total Rides per KM Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist6 <- renderPlot({
        
        ny1 <- reactivePUTripTime()
        
        ny2 <- ny1 %>% mutate(trip_time_bin = cut(Trip_Seconds, breaks=c(59,120,180,240,300,360,420,480,540,600,660,720,800,860,920,980,1020,1080,1140,1200,1260,1320,1380,1440,1500,1560,1620,1680,1740,1800,1860,1920,1980,2040,2100,2250,2500,2750,3000,3250,3500,3750,4000,5000,7500,10000,18000), dig.lab=7))
        ny2bin <- ny2 %>% group_by(trip_time_bin) %>% summarise(Frequency = sum(count))
        
        ggplot(ny2bin, aes(x= trip_time_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Time Bin", title="Total Rides per Trip Time Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      
      # output$mymapPickup <- renderLeaflet({
      #   nnpal <- colorNumeric(colorFactor("Blues", NULL), domain = downtownFromTo$freq)
      #   leaflet(downtownFromTo) %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10) %>% addProviderTiles("CartoDB.Positron", group="bg1") %>%
      #     addPolygons(stroke = TRUE,
      #                 color = "grey", fillColor = ~nnpal(freq),
      #                 weight = 1, smoothFactor = 0.5,
      #                 opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe,
      #                 highlightOptions = highlightOptions(color = "red", weight = 2,
      #                                                     bringToFront = TRUE)) %>%
      #   addLegend(pal = nnpal, values = ~freq, title = "Percentage" , opacity = 1)
      # })
      
     # output$mymapPickup <- renderLeaflet({
        nnpal <- colorNumeric(colorFactor("Blues", NULL), domain = downtownFromTo$freq)
        leafletProxy("mymapPickup", data = downtownFromTo) %>%
          clearControls() %>%
          addPolygons(stroke = TRUE,  color = "grey",
                      fillColor = ~nnpal(freq),
                      weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe,label = ~community,
                      highlightOptions = highlightOptions(color = "red", weight = 2,
                                                          bringToFront = TRUE)) %>%
        addLegend(pal = nnpal, values = ~freq, title = "Percentage" , opacity = 1)
     # })
      
      justonepolygon <- subset(downtownFromTo, area_numbe == click$id)
      
      # nnpal <- colorNumeric(colorFactor("Blues", NULL), domain = downtownFromTo$freq)
      # 
      # leafletProxy("mymapPickup", data = justonepolygon) %>% addPolygons(stroke = TRUE,
      #                         color = "red",
      #                         weight = 1, smoothFactor = 0.5,fillColor = ~nnpal(freq),label = ~community,
      #                         opacity = 1.0, fillOpacity = 0.5, layerId=~area_numbe)
      
      output$pickupCAPlot <- renderPlot({
        ggplot(from_to_freq_bar, aes(x=COMMUNITY, y=freq))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Percentage", x="Community", title="Percentage of Rides from Community")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
    })
    
    output$mymapPickup <- renderLeaflet({
      # nnpal <- colorNumeric(colorFactor("Blues", NULL), domain = downtownFinal1$freq)
      leaflet(downtown) %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10) %>% addProviderTiles("CartoDB.Positron", group="bg1") %>%
        addPolygons(stroke = TRUE,
                    color = "grey",
                    weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe,label = ~community,
                    highlightOptions = highlightOptions(color = "red", weight = 2,
                                                        bringToFront = TRUE))
      # addLegend(pal = nnpal, values = ~freq, title = "Percentage" , opacity = 1)
    })
  
    
    
    observe({
      click <- input$mymapDropOff_shape_click
      # #print(click)
      # paste("Hello")
      if (is.null(click)) return()
      
      #print(click$id)
      
      
      reactivePickUp <- subset(allData3, Drop_off_community_Area == (click$id))
      
      to_from_freq <- reactivePickUp %>% group_by(Pickup_Community_Area) %>% summarise(count = n()) %>% mutate(freq = round(count / sum(count), 4)*100)
      q <- setdiff(1:77,to_from_freq$Pickup_Community_Area)
      
      if(length(q) > 0) {
        
        z <- c(q)
        #print(z)
        #print("Hello")
        for (i in 1:length(z)) {
          # from_to_freq[nrow(from_to_freq) + 1,] = c(z[i],0,0)
          # #print(z[i])
          # #print(z[[i]])
          w <- c(z[[i]],0,0)
          to_from_freq[nrow(to_from_freq) + 1, 1:3] <- as.list(w)
        }
      }
      
      to_from_freq_bar <- merge(to_from_freq,data1, by.x  = "Pickup_Community_Area", by.y="AREA_NUMBE")
      
      downtownToFrom <- merge(downtown,to_from_freq, by.x  = "area_numbe", by.y="Pickup_Community_Area")
      
      output$dropCAPlot <- renderPlot({
        ggplot(to_from_freq_bar, aes(x= COMMUNITY, y=freq))+geom_bar(stat="identity", fill="#AA4A44")+labs(y = "Percentage", x="Community", title="Percentage of Rides to Community")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      # output$mymapDropOff <- renderLeaflet({
      #   nnpal <- colorNumeric(colorFactor("Reds", NULL), domain = downtownToFrom$freq)
      #   leaflet(downtownToFrom) %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10)  %>% addProviderTiles("CartoDB.Positron", group="bg1") %>%
      #     addPolygons(stroke = TRUE,
      #                 color = "grey",
      #                 fillColor = ~nnpal(freq), weight = 1, smoothFactor = 0.5,
      #                 opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe,label = ~community,
      #                 highlightOptions = highlightOptions(color = "red", weight = 2,
      #                                                     bringToFront = TRUE)) %>%
      #     addLegend(pal = nnpal, values = ~freq, title = "Percentage" , opacity = 1)
      # })
      
      
      #output$mymapDropOff <- renderLeaflet({
        nnpal <- colorNumeric(colorFactor("Reds", NULL), domain = downtownToFrom$freq)
        leafletProxy("mymapDropOff", data = downtownToFrom) %>%
          clearControls() %>%
          addPolygons(stroke = TRUE,
                      color = "grey",
                      fillColor = ~nnpal(freq), weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe,label = ~community,
                      highlightOptions = highlightOptions(color = "red", weight = 2,
                                                          bringToFront = TRUE)) %>%
          addLegend(pal = nnpal, values = ~freq, title = "Percentage" , opacity = 1)
      #})
      

      
      
      
      
      reactivePickUpDayCount <- reactive({
        ##print(paste("Station name is:",stationClicked))
        reactivePickUp %>% group_by(lubridateDateOnly) %>%summarise(count = n())
      })
      
      
      
      reactivePUHourCount <- reactive({
        reactivePickUp %>% group_by(hour) %>%summarise(count = n())
      })
      
      
      
      # reactivePUWeekdaycount <- reactive({
      #   reactivePickUp %>% group_by(weekday) %>%summarise(count = n())
      # })
      
      
      reactivePUMonthCount <- reactive({
        reactivePickUp %>% group_by(month) %>%summarise(count = n())
      })
      
      reactivePUMileage <- reactive({
        reactivePickUp %>% group_by(Trip_Miles) %>%summarise(count = n())
      })
      
      
      reactivePUTripTime <- reactive({
        reactivePickUp %>% group_by(Trip_Seconds) %>%summarise(count = n())
      })
      
      
      
      output$tb1 = renderDT({
        ny1 <- reactivePickUpDayCount()
        
        dfbar <- data.frame(
          date = ny1$lubridateDateOnly,
          rides = ny1$count
        )
        dfbar <- dfbar[order(dfbar$date),]
        datatable(dfbar,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb2 = renderDT({
        # ny1 <- reactivePUHourCount()
        ny1 <- reactivePickUp %>% group_by(hour) %>% summarise(count = n())
        ny1$hourAMPM <- factor(ny1$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM"))
        
        dfbar1 <- data.frame(
          hours = ny1$hourAMPM,
          rides = ny1$count
        )
        dfbar1 <- dfbar1[order(dfbar1$hours),]
        datatable(dfbar1,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb2Military = renderDT({
        # ny1 <- reactivePUHourCount()
        ny1 <- reactivePickUp %>% group_by(hour) %>% summarise(count = n())
        ny1$hour <- factor(ny1$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
        
        
        dfbar1 <- data.frame(
          hours = ny1$hour,
          rides = ny1$count
        )
        dfbar1 <- dfbar1[order(dfbar1$hours),]
        datatable(dfbar1,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb3 = renderDT({
        
        ny1 <- reactivePickUp %>% group_by(weekday) %>% summarise(count = n())
        # ny1 <- reactivePUWeekdaycount
        ny1$weekday <- factor(ny1$weekday, levels = c(1,2,3,4,5,6,7), labels = c("Sun","Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
        
        
        dfbar2 <- data.frame(
          weekdays = ny1$weekday,
          rides = ny1$count
        )
        dfbar2 <- dfbar2[order(dfbar2$weekdays),]
        datatable(dfbar2,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb4 = renderDT({
        ny1 <- reactivePUMonthCount()
        ny1$month <- factor(ny1$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"))
        
        
        dfbar3 <- data.frame(
          months = ny1$month,
          rides = ny1$count
        )
        dfbar3 <- dfbar3[order(dfbar3$months),]
        datatable(dfbar3,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb5 = renderDT({
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        ny2bin <- ny2 %>% group_by(mileage_bin) %>% summarise(Frequency = sum(count))
        
        dfbar4 <- data.frame(
          miles = ny2bin$mileage_bin,
          rides = ny2bin$Frequency
        )
        dfbar4 <- dfbar4[order(dfbar4$miles),]
        datatable(dfbar4,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tbkm = renderDT({
        
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        
        ny2bin <- ny2 %>% group_by(km_bin) %>% summarise(Frequency = sum(count))
        
        dfbar4 <- data.frame(
          km = ny2bin$km_bin,
          rides = ny2bin$Frequency
        )
        dfbar4 <- dfbar4[order(dfbar4$km),]
        datatable(dfbar4,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb6 = renderDT({
        
        ny1 <- reactivePUTripTime()
        
        ny2 <- ny1 %>% mutate(trip_time_bin = cut(Trip_Seconds, breaks=c(59,120,180,240,300,360,420,480,540,600,660,720,800,860,920,980,1020,1080,1140,1200,1260,1320,1380,1440,1500,1560,1620,1680,1740,1800,1860,1920,1980,2040,2100,2250,2500,2750,3000,3250,3500,3750,4000,5000,7500,10000,18000), dig.lab=7))
        ny2bin <- ny2 %>% group_by(trip_time_bin) %>% summarise(Frequency = sum(count))
        
        dfbar5 <- data.frame(
          time = ny2bin$trip_time_bin,
          rides = ny2bin$Frequency
        )
        dfbar5 <- dfbar5[order(dfbar5$time),]
        datatable(dfbar5,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$hist1 <- renderPlot({
        ny1 <- reactivePickUpDayCount()
        ggplot(ny1, aes(x=lubridateDateOnly, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides in Community", x="Date", title="Total Rides per Day")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        
      })
      
      output$hist2 <- renderPlot({
        ny1 <- reactivePUHourCount()
        ##print(ny1)
        ny1$hourAMPM <- factor(ny1$hour, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM"))
        
        ggplot(ny1, aes(x=hourAMPM, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Hour", title="Total Rides per Hour")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist2Military <- renderPlot({
        ny1 <- reactivePUHourCount()
        ny1$hour <- factor(ny1$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
        
        ggplot(ny1, aes(x=hour, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Hour", title="Total Rides per Hour")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      
      output$hist3 <- renderPlot({
        # ny1 <- reactivePUWeekdaycount
        ny1 <- reactivePickUp %>% group_by(weekday) %>% summarise(count = n())
        
        ny1$weekday <- factor(ny1$weekday, levels = c(1,2,3,4,5,6,7), labels = c( "Sun","Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
        
        ggplot(ny1, aes(x=weekday, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Weekday", title="Total Rides per Weekday")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist4 <- renderPlot({
        ny1 <- reactivePUMonthCount()
        ny1$month <- factor(ny1$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"))
        
        ggplot(ny1, aes(x=month, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Month", title="Total Rides per Month")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist5 <- renderPlot({
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        ny2bin <- ny2 %>% group_by(mileage_bin) %>% summarise(Frequency = sum(count))
        
        ggplot(ny2bin, aes(x= mileage_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Mileage Bin", title="Total Rides per Mileage Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$histkm <- renderPlot({
        
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        
        ny2bin <- ny2 %>% group_by(km_bin) %>% summarise(Frequency = sum(count))
        
        ggplot(ny2bin, aes(x= km_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Kilometer Bin", title="Total Rides per KM Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist6 <- renderPlot({
        
        ny1 <- reactivePUTripTime()
        
        ny2 <- ny1 %>% mutate(trip_time_bin = cut(Trip_Seconds, breaks=c(59,120,180,240,300,360,420,480,540,600,660,720,800,860,920,980,1020,1080,1140,1200,1260,1320,1380,1440,1500,1560,1620,1680,1740,1800,1860,1920,1980,2040,2100,2250,2500,2750,3000,3250,3500,3750,4000,5000,7500,10000,18000), dig.lab=7))
        ny2bin <- ny2 %>% group_by(trip_time_bin) %>% summarise(Frequency = sum(count))
        
        ggplot(ny2bin, aes(x= trip_time_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Time Bin", title="Total Rides per Trip Time Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      
      
    })
    
    
    
    

    
    output$mymapDropOff <- renderLeaflet({
      # nnpal <- colorNumeric(colorFactor("Blues", NULL), domain = downtownFinal1$freq)
      leaflet(downtown) %>% addProviderTiles("CartoDB.Positron", group="bg1") %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10) %>%
        addPolygons(stroke = TRUE,
                    color = "grey",
                    weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe, label = ~community,
                    highlightOptions = highlightOptions(color = "red", weight = 2,
                                                        bringToFront = TRUE))
      # addLegend(pal = nnpal, values = ~freq, title = "Percentage" , opacity = 1)
    })

    
    # output$mymapDropOffCompany <- renderLeaflet({
    #   nnpal <- colorNumeric(colorFactor("Reds", NULL), domain = downtownFinal2$freq)
    #   leaflet(downtownFinal2) %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10)  %>% addProviderTiles("CartoDB.Positron", group="bg1") %>%
    #     addPolygons(stroke = TRUE,
    #                 color = "grey",
    #                 fillColor = ~nnpal(freq), weight = 1, smoothFactor = 0.5,
    #                 opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe,
    #                 highlightOptions = highlightOptions(color = "red", weight = 2,
    #                                                     bringToFront = TRUE)) %>%
    #     addLegend(pal = nnpal, values = ~freq, title = "Percentage" , opacity = 1)
    # })
    # 

    
    observe({
      click <- input$mymapPickUpCompany_shape_click
      # #print(click)
      # paste("Hello")
      if (is.null(click)) return()
      #print(click$id)
      
      find_company <- company_names_df[company_names_df$Company == input$company_name]
      #print("Reached here 1")
      #print(find_company)

      co_id <- find_company$id
      #print(paste("Reached here 2", co_id))
      reactivePickUp <- subset(allData3, Pickup_Community_Area == (click$id) & id == co_id)
      #print("Reached here 3")
      #print(reactivePickUp)

      
      
      
      
      from_to_freq <- reactivePickUp %>% group_by(Drop_off_community_Area) %>% summarise(count = n()) %>% mutate(freq = round(count / sum(count), 4)*100)
      #print(from_to_freq)
      q <- setdiff(1:77,from_to_freq$Drop_off_community_Area)
      
      if(length(q) > 0) {
        
        z <- c(q)
        #print(z)
        #print("Hello")
        for (i in 1:length(z)) {
          # from_to_freq[nrow(from_to_freq) + 1,] = c(z[i],0,0)
          # #print(z[i])
          # #print(z[[i]])
          w <- c(z[[i]],0,0)
          from_to_freq[nrow(from_to_freq) + 1, 1:3] <- as.list(w)
        }
      }
      
      from_to_freq_bar <- merge(from_to_freq,data1, by.x  = "Drop_off_community_Area", by.y="AREA_NUMBE")
      ##print(from_to_freq_bar)
      
      
      downtownFromTo <- merge(downtown,from_to_freq, by.x  = "area_numbe", by.y="Drop_off_community_Area")

      
      reactivePickUpDayCount <- reactive({
        ##print(paste("Station name is:",stationClicked))
        reactivePickUp %>% group_by(lubridateDateOnly) %>%summarise(count = n())
      })
      
      
      
      reactivePUHourCount <- reactive({
        reactivePickUp %>% group_by(hour) %>%summarise(count = n())
      })
      
      
      
      # reactivePUWeekdaycount <- reactive({
      #   reactivePickUp %>% group_by(weekday) %>%summarise(count = n())
      # })
      
      
      reactivePUMonthCount <- reactive({
        reactivePickUp %>% group_by(month) %>%summarise(count = n())
      })
      
      reactivePUMileage <- reactive({
        reactivePickUp %>% group_by(Trip_Miles) %>%summarise(count = n())
      })
      
      
      reactivePUTripTime <- reactive({
        reactivePickUp %>% group_by(Trip_Seconds) %>%summarise(count = n())
      })
      
      
      
      
      output$tb1 = renderDT({
        ny1 <- reactivePickUpDayCount()
        
        dfbar <- data.frame(
          date = ny1$lubridateDateOnly,
          rides = ny1$count
        )
        dfbar <- dfbar[order(dfbar$date),]
        datatable(dfbar,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb2 = renderDT({
        # ny1 <- reactivePUHourCount()
        ny1 <- reactivePickUp %>% group_by(hour) %>% summarise(count = n())
        ny1$hourAMPM <- factor(ny1$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM"))
        
        dfbar1 <- data.frame(
          hours = ny1$hourAMPM,
          rides = ny1$count
        )
        dfbar1 <- dfbar1[order(dfbar1$hours),]
        datatable(dfbar1,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb2Military = renderDT({
        # ny1 <- reactivePUHourCount()
        ny1 <- reactivePickUp %>% group_by(hour) %>% summarise(count = n())
        ny1$hour <- factor(ny1$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
        
        
        dfbar1 <- data.frame(
          hours = ny1$hour,
          rides = ny1$count
        )
        dfbar1 <- dfbar1[order(dfbar1$hours),]
        datatable(dfbar1,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb3 = renderDT({
        
        ny1 <- reactivePickUp %>% group_by(weekday) %>% summarise(count = n())
        # ny1 <- reactivePUWeekdaycount
        ny1$weekday <- factor(ny1$weekday,levels = c(1,2,3,4,5,6,7), labels = c("Sun","Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
        
        
        dfbar2 <- data.frame(
          weekdays = ny1$weekday,
          rides = ny1$count
        )
        dfbar2 <- dfbar2[order(dfbar2$weekdays),]
        datatable(dfbar2,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb4 = renderDT({
        ny1 <- reactivePUMonthCount()
        ny1$month <- factor(ny1$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"))
        
        
        dfbar3 <- data.frame(
          months = ny1$month,
          rides = ny1$count
        )
        dfbar3 <- dfbar3[order(dfbar3$months),]
        datatable(dfbar3,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb5 = renderDT({
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        ny2bin <- ny2 %>% group_by(mileage_bin) %>% summarise(Frequency = sum(count))
        
        dfbar4 <- data.frame(
          miles = ny2bin$mileage_bin,
          rides = ny2bin$Frequency
        )
        dfbar4 <- dfbar4[order(dfbar4$miles),]
        datatable(dfbar4,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tbkm = renderDT({
        
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        
        ny2bin <- ny2 %>% group_by(km_bin) %>% summarise(Frequency = sum(count))
        
        dfbar4 <- data.frame(
          km = ny2bin$km_bin,
          rides = ny2bin$Frequency
        )
        dfbar4 <- dfbar4[order(dfbar4$km),]
        datatable(dfbar4,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb6 = renderDT({
        
        ny1 <- reactivePUTripTime()
        
        ny2 <- ny1 %>% mutate(trip_time_bin = cut(Trip_Seconds, breaks=c(59,120,180,240,300,360,420,480,540,600,660,720,800,860,920,980,1020,1080,1140,1200,1260,1320,1380,1440,1500,1560,1620,1680,1740,1800,1860,1920,1980,2040,2100,2250,2500,2750,3000,3250,3500,3750,4000,5000,7500,10000,18000), dig.lab=7))
        ny2bin <- ny2 %>% group_by(trip_time_bin) %>% summarise(Frequency = sum(count))
        
        dfbar5 <- data.frame(
          time = ny2bin$trip_time_bin,
          rides = ny2bin$Frequency
        )
        dfbar5 <- dfbar5[order(dfbar5$time),]
        datatable(dfbar5,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$hist1 <- renderPlot({
        ny1 <- reactivePickUpDayCount()
        ggplot(ny1, aes(x=lubridateDateOnly, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides in Community", x="Date", title="Total Rides per Day")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        
      })
      
      output$hist2 <- renderPlot({
        ny1 <- reactivePUHourCount()
        ##print(ny1)
        ny1$hourAMPM <- factor(ny1$hour, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM"))
        
        ggplot(ny1, aes(x=hourAMPM, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Hour", title="Total Rides per Hour")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist2Military <- renderPlot({
        ny1 <- reactivePUHourCount()
        ny1$hour <- factor(ny1$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
        
        ggplot(ny1, aes(x=hour, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Hour", title="Total Rides per Hour")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      
      output$hist3 <- renderPlot({
        # ny1 <- reactivePUWeekdaycount
        ny1 <- reactivePickUp %>% group_by(weekday) %>% summarise(count = n())
        
        ny1$weekday <- factor(ny1$weekday, levels = c(1,2,3,4,5,6,7), labels = c("Sun","Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
        
        ggplot(ny1, aes(x=weekday, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Weekday", title="Total Rides per Weekday")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist4 <- renderPlot({
        ny1 <- reactivePUMonthCount()
        ny1$month <- factor(ny1$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"))
        
        ggplot(ny1, aes(x=month, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Month", title="Total Rides per Month")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist5 <- renderPlot({
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        ny2bin <- ny2 %>% group_by(mileage_bin) %>% summarise(Frequency = sum(count))
        
        ggplot(ny2bin, aes(x= mileage_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Mileage Bin", title="Total Rides per Mileage Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$histkm <- renderPlot({
        
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        
        ny2bin <- ny2 %>% group_by(km_bin) %>% summarise(Frequency = sum(count))
        
        ggplot(ny2bin, aes(x= km_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Kilometer Bin", title="Total Rides per KM Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist6 <- renderPlot({
        
        ny1 <- reactivePUTripTime()
        
        ny2 <- ny1 %>% mutate(trip_time_bin = cut(Trip_Seconds, breaks=c(59,120,180,240,300,360,420,480,540,600,660,720,800,860,920,980,1020,1080,1140,1200,1260,1320,1380,1440,1500,1560,1620,1680,1740,1800,1860,1920,1980,2040,2100,2250,2500,2750,3000,3250,3500,3750,4000,5000,7500,10000,18000), dig.lab=7))
        ny2bin <- ny2 %>% group_by(trip_time_bin) %>% summarise(Frequency = sum(count))
        
        ggplot(ny2bin, aes(x= trip_time_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Time Bin", title="Total Rides per Trip Time Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      
      # output$mymapPickup <- renderLeaflet({
      #   nnpal <- colorNumeric(colorFactor("Blues", NULL), domain = downtownFromTo$freq)
      #   leaflet(downtownFromTo) %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10) %>% addProviderTiles("CartoDB.Positron", group="bg1") %>%
      #     addPolygons(stroke = TRUE,
      #                 color = "grey", fillColor = ~nnpal(freq),
      #                 weight = 1, smoothFactor = 0.5,
      #                 opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe,
      #                 highlightOptions = highlightOptions(color = "red", weight = 2,
      #                                                     bringToFront = TRUE)) %>%
      #   addLegend(pal = nnpal, values = ~freq, title = "Percentage" , opacity = 1)
      # })
      
      # output$mymapPickup <- renderLeaflet({
      nnpal <- colorNumeric(colorFactor("Greens", NULL), domain = downtownFromTo$freq)
      leafletProxy("mymapPickUpCompany", data = downtownFromTo) %>%
        clearControls() %>%
        addPolygons(stroke = TRUE,  color = "grey",
                    fillColor = ~nnpal(freq),
                    weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe,label = ~community,
                    highlightOptions = highlightOptions(color = "red", weight = 2,
                                                        bringToFront = TRUE)) %>%
        addLegend(pal = nnpal, values = ~freq, title = "Percentage" , opacity = 1)
      # })
      
      justonepolygon <- subset(downtownFromTo, area_numbe == click$id)
      
      # nnpal <- colorNumeric(colorFactor("Blues", NULL), domain = downtownFromTo$freq)
      # 
      # leafletProxy("mymapPickup", data = justonepolygon) %>% addPolygons(stroke = TRUE,
      #                         color = "red",
      #                         weight = 1, smoothFactor = 0.5,fillColor = ~nnpal(freq),label = ~community,
      #                         opacity = 1.0, fillOpacity = 0.5, layerId=~area_numbe)
      
      # output$pickupCAPlot <- renderPlot({
      #   ggplot(from_to_freq_bar, aes(x=COMMUNITY, y=freq))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Percentage", x="Community", title="Percentage of Rides from Community")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      # })
      
    })
    
    
    output$mymapPickUpCompany <- renderLeaflet({
      leaflet(downtown) %>% addProviderTiles("CartoDB.Positron", group="bg1") %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10) %>%
        addPolygons(stroke = TRUE,
                    color = "grey",
                    weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe, label = ~community,
                    highlightOptions = highlightOptions(color = "red", weight = 2,
                                                        bringToFront = TRUE))
    })
    
    
    
    observe({
      click <- input$mymapDropoffCompany_shape_click

      if (is.null(click)) return()
      #print(click$id)
      
      find_company <- company_names_df[company_names_df$Company == input$company_name]
      #print("Reached here 1")
      #print(find_company)
      
      co_id <- find_company$id
      #print(paste("Reached here 2", co_id))
      reactivePickUp <- subset(allData3, Drop_off_community_Area == (click$id) & id == co_id)
      #print("Reached here 3")
      #print(reactivePickUp)
      
      
      
      
      
      from_to_freq <- reactivePickUp %>% group_by(Pickup_Community_Area) %>% summarise(count = n()) %>% mutate(freq = round(count / sum(count), 4)*100)
      #print(from_to_freq)
      q <- setdiff(1:77,from_to_freq$Pickup_Community_Area)
      
      if(length(q) > 0) {
        
        z <- c(q)
        #print(z)
        #print("Hello")
        for (i in 1:length(z)) {
          # from_to_freq[nrow(from_to_freq) + 1,] = c(z[i],0,0)
          # #print(z[i])
          # #print(z[[i]])
          w <- c(z[[i]],0,0)
          from_to_freq[nrow(from_to_freq) + 1, 1:3] <- as.list(w)
        }
      }
      
      from_to_freq_bar <- merge(from_to_freq,data1, by.x  = "Pickup_Community_Area", by.y="AREA_NUMBE")
      ##print(from_to_freq_bar)
      
      
      downtownFromTo <- merge(downtown,from_to_freq, by.x  = "area_numbe", by.y="Pickup_Community_Area")
      
      
      reactivePickUpDayCount <- reactive({
        ##print(paste("Station name is:",stationClicked))
        reactivePickUp %>% group_by(lubridateDateOnly) %>%summarise(count = n())
      })
      
      
      
      reactivePUHourCount <- reactive({
        reactivePickUp %>% group_by(hour) %>%summarise(count = n())
      })
      
      
      
      # reactivePUWeekdaycount <- reactive({
      #   reactivePickUp %>% group_by(weekday) %>%summarise(count = n())
      # })
      
      
      reactivePUMonthCount <- reactive({
        reactivePickUp %>% group_by(month) %>%summarise(count = n())
      })
      
      reactivePUMileage <- reactive({
        reactivePickUp %>% group_by(Trip_Miles) %>%summarise(count = n())
      })
      
      
      reactivePUTripTime <- reactive({
        reactivePickUp %>% group_by(Trip_Seconds) %>%summarise(count = n())
      })
      
      
      
      
      output$tb1 = renderDT({
        ny1 <- reactivePickUpDayCount()
        
        dfbar <- data.frame(
          date = ny1$lubridateDateOnly,
          rides = ny1$count
        )
        dfbar <- dfbar[order(dfbar$date),]
        datatable(dfbar,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb2 = renderDT({
        # ny1 <- reactivePUHourCount()
        ny1 <- reactivePickUp %>% group_by(hour) %>% summarise(count = n())
        ny1$hourAMPM <- factor(ny1$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM"))
        
        dfbar1 <- data.frame(
          hours = ny1$hourAMPM,
          rides = ny1$count
        )
        dfbar1 <- dfbar1[order(dfbar1$hours),]
        datatable(dfbar1,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb2Military = renderDT({
        # ny1 <- reactivePUHourCount()
        ny1 <- reactivePickUp %>% group_by(hour) %>% summarise(count = n())
        ny1$hour <- factor(ny1$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
        
        
        dfbar1 <- data.frame(
          hours = ny1$hour,
          rides = ny1$count
        )
        dfbar1 <- dfbar1[order(dfbar1$hours),]
        datatable(dfbar1,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb3 = renderDT({
        
        ny1 <- reactivePickUp %>% group_by(weekday) %>% summarise(count = n())
        # ny1 <- reactivePUWeekdaycount
        ny1$weekday <- factor(ny1$weekday,levels = c(1,2,3,4,5,6,7), labels = c("Sun","Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
        
        
        dfbar2 <- data.frame(
          weekdays = ny1$weekday,
          rides = ny1$count
        )
        dfbar2 <- dfbar2[order(dfbar2$weekdays),]
        datatable(dfbar2,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb4 = renderDT({
        ny1 <- reactivePUMonthCount()
        ny1$month <- factor(ny1$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"))
        
        
        dfbar3 <- data.frame(
          months = ny1$month,
          rides = ny1$count
        )
        dfbar3 <- dfbar3[order(dfbar3$months),]
        datatable(dfbar3,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb5 = renderDT({
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        ny2bin <- ny2 %>% group_by(mileage_bin) %>% summarise(Frequency = sum(count))
        
        dfbar4 <- data.frame(
          miles = ny2bin$mileage_bin,
          rides = ny2bin$Frequency
        )
        dfbar4 <- dfbar4[order(dfbar4$miles),]
        datatable(dfbar4,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tbkm = renderDT({
        
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        
        ny2bin <- ny2 %>% group_by(km_bin) %>% summarise(Frequency = sum(count))
        
        dfbar4 <- data.frame(
          km = ny2bin$km_bin,
          rides = ny2bin$Frequency
        )
        dfbar4 <- dfbar4[order(dfbar4$km),]
        datatable(dfbar4,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$tb6 = renderDT({
        
        ny1 <- reactivePUTripTime()
        
        ny2 <- ny1 %>% mutate(trip_time_bin = cut(Trip_Seconds, breaks=c(59,120,180,240,300,360,420,480,540,600,660,720,800,860,920,980,1020,1080,1140,1200,1260,1320,1380,1440,1500,1560,1620,1680,1740,1800,1860,1920,1980,2040,2100,2250,2500,2750,3000,3250,3500,3750,4000,5000,7500,10000,18000), dig.lab=7))
        ny2bin <- ny2 %>% group_by(trip_time_bin) %>% summarise(Frequency = sum(count))
        
        dfbar5 <- data.frame(
          time = ny2bin$trip_time_bin,
          rides = ny2bin$Frequency
        )
        dfbar5 <- dfbar5[order(dfbar5$time),]
        datatable(dfbar5,options  = list(lengthMenu = c(13,13)), rownames= FALSE)
      })
      
      output$hist1 <- renderPlot({
        ny1 <- reactivePickUpDayCount()
        ggplot(ny1, aes(x=lubridateDateOnly, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides in Community", x="Date", title="Total Rides per Day")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
        
      })
      
      output$hist2 <- renderPlot({
        ny1 <- reactivePUHourCount()
        ##print(ny1)
        ny1$hourAMPM <- factor(ny1$hour, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) ,labels = c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM"))
        
        ggplot(ny1, aes(x=hourAMPM, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Hour", title="Total Rides per Hour")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist2Military <- renderPlot({
        ny1 <- reactivePUHourCount()
        ny1$hour <- factor(ny1$hour,levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
        
        ggplot(ny1, aes(x=hour, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Hour", title="Total Rides per Hour")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      
      output$hist3 <- renderPlot({
        # ny1 <- reactivePUWeekdaycount
        ny1 <- reactivePickUp %>% group_by(weekday) %>% summarise(count = n())
        
        ny1$weekday <- factor(ny1$weekday, levels = c(1,2,3,4,5,6,7), labels = c("Sun","Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
        
        ggplot(ny1, aes(x=weekday, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Weekday", title="Total Rides per Weekday")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist4 <- renderPlot({
        ny1 <- reactivePUMonthCount()
        ny1$month <- factor(ny1$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"))
        
        ggplot(ny1, aes(x=month, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Month", title="Total Rides per Month")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist5 <- renderPlot({
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        ny2bin <- ny2 %>% group_by(mileage_bin) %>% summarise(Frequency = sum(count))
        
        ggplot(ny2bin, aes(x= mileage_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Mileage Bin", title="Total Rides per Mileage Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$histkm <- renderPlot({
        
        ny1 <- reactivePUMileage()
        
        ny1$km <- round(1.60934*ny1$Trip_Miles,3)
        ny1 <- ny1 %>% mutate(mileage_bin = cut(Trip_Miles, breaks=c(0.49, 1, 2,3,4,5,10,15,20,25,50,75,100)))
        ny2 <- ny1 %>% mutate(km_bin = cut(km, breaks=kmBreak))
        
        
        ny2bin <- ny2 %>% group_by(km_bin) %>% summarise(Frequency = sum(count))
        
        ggplot(ny2bin, aes(x= km_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Kilometer Bin", title="Total Rides per KM Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      output$hist6 <- renderPlot({
        
        ny1 <- reactivePUTripTime()
        
        ny2 <- ny1 %>% mutate(trip_time_bin = cut(Trip_Seconds, breaks=c(59,120,180,240,300,360,420,480,540,600,660,720,800,860,920,980,1020,1080,1140,1200,1260,1320,1380,1440,1500,1560,1620,1680,1740,1800,1860,1920,1980,2040,2100,2250,2500,2750,3000,3250,3500,3750,4000,5000,7500,10000,18000), dig.lab=7))
        ny2bin <- ny2 %>% group_by(trip_time_bin) %>% summarise(Frequency = sum(count))
        
        ggplot(ny2bin, aes(x= trip_time_bin, y=Frequency))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Time Bin", title="Total Rides per Trip Time Bin")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      })
      
      
      # output$mymapPickup <- renderLeaflet({
      #   nnpal <- colorNumeric(colorFactor("Blues", NULL), domain = downtownFromTo$freq)
      #   leaflet(downtownFromTo) %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10) %>% addProviderTiles("CartoDB.Positron", group="bg1") %>%
      #     addPolygons(stroke = TRUE,
      #                 color = "grey", fillColor = ~nnpal(freq),
      #                 weight = 1, smoothFactor = 0.5,
      #                 opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe,
      #                 highlightOptions = highlightOptions(color = "red", weight = 2,
      #                                                     bringToFront = TRUE)) %>%
      #   addLegend(pal = nnpal, values = ~freq, title = "Percentage" , opacity = 1)
      # })
      
      # output$mymapPickup <- renderLeaflet({
      nnpal <- colorNumeric(colorFactor("Purples", NULL), domain = downtownFromTo$freq)
      leafletProxy("mymapDropoffCompany", data = downtownFromTo) %>%
        clearControls() %>%
        addPolygons(stroke = TRUE,  color = "grey",
                    fillColor = ~nnpal(freq),
                    weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe,label = ~community,
                    highlightOptions = highlightOptions(color = "red", weight = 2,
                                                        bringToFront = TRUE)) %>%
        addLegend(pal = nnpal, values = ~freq, title = "Percentage" ,opacity = 1)
      # })
      
      justonepolygon <- subset(downtownFromTo, area_numbe == click$id)
      
      # nnpal <- colorNumeric(colorFactor("Blues", NULL), domain = downtownFromTo$freq)
      # 
      # leafletProxy("mymapPickup", data = justonepolygon) %>% addPolygons(stroke = TRUE,
      #                         color = "red",
      #                         weight = 1, smoothFactor = 0.5,fillColor = ~nnpal(freq),label = ~community,
      #                         opacity = 1.0, fillOpacity = 0.5, layerId=~area_numbe)
      
      # output$pickupCAPlot <- renderPlot({
      #   ggplot(from_to_freq_bar, aes(x=COMMUNITY, y=freq))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Percentage", x="Community", title="Percentage of Rides from Community")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      # })
      
    })
    
    
    output$mymapDropoffCompany <- renderLeaflet({
      leaflet(downtown) %>% addProviderTiles("CartoDB.Positron", group="bg1") %>% setView(lng = -87.623177,lat = 41.881832, zoom = 10) %>%
        addPolygons(stroke = TRUE,
                    color = "grey",
                    weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,popup = ~community, layerId=~area_numbe, label = ~community,
                    highlightOptions = highlightOptions(color = "red", weight = 2,
                                                        bringToFront = TRUE))
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
