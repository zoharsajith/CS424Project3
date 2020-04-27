library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)
library(stringr)
movies <- read.csv("combined.csv")
movies3 <- movies

movies3$release <- as.character(movies3$release)
movies3$release <- as.POSIXct(x=movies3$release, format = "%d %B %Y")
#only the month code for a seperate dataset with the dates being only the month
onlymonth <- movies3
onlymonth$release <- format(as.Date(onlymonth$release), "%B")
onlymonth$release <- as.character(onlymonth$release)
names(onlymonth)[5] = "Month"
#movies per year code
moviesperyear <- table(a<-(movies$Year))
moviesperyear <- as.data.frame(moviesperyear)
#movies per month code
onlymonthtable <- table(a<-(onlymonth$Month))
onlymonthtable <- as.data.frame(onlymonthtable)
#distribution of certificate code
certificateAmount <- table(a<-(movies$Certificates))
certificateAmount <- as.data.frame(certificateAmount)
#distribution of runtime code
runtimeAmount <- table(a<-(movies$time))
runtimeAmount <- as.data.frame(runtimeAmount)
#distribution of Genres
genreAmount <- table(a<-(movies$genre))
genreAmount <- as.data.frame(genreAmount)


# Define UI for application that draws a histogram
ui <- dashboardPage(

    dashboardHeader(title = "CS 424 Spring 2020 Project 3"),
    dashboardSidebar(
        
    ),
    dashboardBody(
        fluidRow(
            column(12,
                   mainPanel(width = 12,
                             tabsetPanel(
                                 tabPanel("Films each year",
                                          box(title = "Amount of Films Released Each Year", solidHeader = TRUE, width = 12,
                                              DT::dataTableOutput("firsttable", height= 400)
                                          )
                                 ),
                                 tabPanel("films each month",
                                          box(title = "Amount of Films Released Each Month", solidHeader = TRUE, width = 12,
                                              DT::dataTableOutput("secondtable", height= 400)
                                          )
                                 ),
                                 tabPanel("Distribution of Certificates",
                                          box(title = "The Distribution of Certificates", solidHeader = TRUE, width = 12,
                                              DT::dataTableOutput("thirdtable", height= 400)
                                          )
                                 ),
                                 tabPanel("Distribution of Runtime",
                                          box(title = "The Distribution of Runtimes", solidHeader = TRUE, width = 12,
                                              DT::dataTableOutput("fourthtable", height= 400)
                                          )
                                 ),
                                 tabPanel("Distribution of Genres",
                                          box(title = "The Distribution of Genres", solidHeader = TRUE, width = 12,
                                              DT::dataTableOutput("fifthtable", height= 400)
                                          )
                                 ),
                                 tabPanel("Full Data Set",
                                          box(title = "The full Data Set we managed to clean", solidHeader = TRUE, width = 12,
                                              DT::dataTableOutput("sixthtable", height= 400)
                                          )
                                 )
                             )         
                   )
            )
            
            
        ),
    ),

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$firsttable <- DT::renderDataTable({
        names(moviesperyear)[1] = "Year"
        moviesperyear
        
        
    })
    output$secondtable <- DT::renderDataTable({
        names(onlymonthtable)[1] = "Month"
        onlymonthtable
        
        
    })
    output$thirdtable <- DT::renderDataTable({
        names(certificateAmount)[1] = "Certificate"
        certificateAmount
        
        
    })
    output$fourthtable <- DT::renderDataTable({
        names(runtimeAmount)[1] = "Runtime(Minutes)"
        runtimeAmount
        
        
    })
    output$fifthtable <- DT::renderDataTable({
        names(genreAmount)[1] = "Genre"
        genreAmount
        
        
    })
    output$sixthtable <- DT::renderDataTable({
        movies
        
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
