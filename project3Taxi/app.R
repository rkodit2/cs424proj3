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
library(DT)
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

daycount <- allData3 %>% group_by(lubridateDateOnly) %>%summarise(count = n())


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      ggplot(daycount, aes(x=lubridateDateOnly, y=count))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Date", title="Per date count")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
