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

averagemonth <- onlymonthtable

averagemonth$Freq <- lapply(averagemonth$Freq, function(x){
    x <- x / (101*12)
    
})

averageyear <- moviesperyear

averageyear$Freq <- lapply(averageyear$Freq, function(x){
    x <- x / (101)
})



# Define UI for application that draws a histogram
ui <- dashboardPage(

    dashboardHeader(title = "CS 424 Spring 2020 Project 3"),
    dashboardSidebar(
        sidebarMenu(
            selectInput("Years", "Select a year", moviesperyear$Var1, selected=2017),
            selectInput("Genre", "Select a genre", genreAmount$Var1),
            menuItem("About", tabname="About")
        )
        
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
        fluidRow(
            column(12,
                   mainPanel(width = 12,
                             tabsetPanel(
                                 tabPanel("Films each year",
                                          box(title = "Amount of Films Released Each Year", solidHeader = TRUE, width = 12,
                                              plotOutput("secondGraph", height = 400))),
                                 tabPanel("Films each month",
                                          box(title = "Amount of Films Released Each Month", solidHeader = TRUE, width = 12,
                                              plotOutput("firstGraph", height = 400))),
                                 tabPanel("Distribution of Certificates",
                                          box(title = "The Distribution of Certificates", solidHeader = TRUE, width = 12,
                                              plotOutput("fourthGraph", height = 400))),
                                 tabPanel("Distribution of Runtime",
                                          box(title = "The Distribution of Runtimes", solidHeader = TRUE, width = 12,
                                              plotOutput("fifthGraph", height = 400))),
                                 tabPanel("Distribution of Genres",
                                          box(title = "The Distribution of Genres", solidHeader = TRUE, width = 12,
                                              plotOutput("thirdGraph", height = 400)))
                             ))
                   )
        ),
        fluidRow(
            column(12,
                   mainPanel(width = 6,
                             tabsetPanel(
                                 tabPanel("Graph",
                                          box("Avg Films per Month", solidHeader = TRUE, width = 12,
                                              plotOutput("avgMonthGraph", height = 400))),
                                 tabPanel("Table",
                                          box("Avg Films per Month", solidHeader = TRUE, width = 12,
                                              DT::dataTableOutput("avgMonth", height = 400)))
                             )),
                   mainPanel(width = 6,
                             tabsetPanel(
                                 tabPanel("Graph",
                                          box("Avg Films per Year", solidHeader = TRUE, width = 12,
                                              plotOutput("avgYearGraph", height = 400))),
                                 tabPanel("Table",
                                          box("Avg Films per Year", solidHeader = TRUE, width = 12,
                                              DT::dataTableOutput("avgYear", height = 400)))
                             )
                                                      
                   )
            )
        ),
        tabItem("About",
                h2("Project 3 for CS424 from Prachal Patel and Zohar Sajith.
                 The data being used was taken from ftp://ftp.fu-berlin.de/pub/misc/movies/database/frozendata/
                 which held data from the 2017 IMDB. The libraries being used are shiny, shinydashboard, leaflet, etc.")
                )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ReactiveYear <- reactive({subset(moviesperyear, year(moviesperyear$Var1) == input$Years)})
    ReativeGenre <- reactive({subset(genreAmount, genreAmount$Var1 == input$Genre)})
    
    
    output$avgYear <- DT::renderDataTable({
        averageyear <- moviesperyear
        
        averageyear$Freq <- lapply(averageyear$Freq, function(x){
            x <- x / (101)
        })
        
        averageYear <- as.data.frame(averageyear)
        averageYear$Freq <- as.numeric(averageYear$Freq)
        averageYear$Freq <- round(averageYear$Freq, digits=2)
        averageYear
    })
    
    output$avgYearGraph <- renderPlot({
        averageYear <- as.data.frame(averageyear)
        averageYear$Freq <- as.numeric(averageYear$Freq)
        averageYear$Freq <- round(averageYear$Freq, digits=2)
        averageYear
        
        
        plot2 <- ggplot(data= averageYear, aes(x=Var1, y=Freq)) + 
            geom_bar(stat="identity", width=.5, color="black", fill="white") + 
            ggtitle("Average amount of movies per year") +
            labs(x = "year", y = "# of Movies") +
            geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25) +
            theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=90))
        plot2
    })
    
    
    
    output$avgMonth <- DT::renderDataTable({
        averagemonth <- onlymonthtable
        
        averagemonth$Freq <- lapply(averagemonth$Freq, function(x){
            x <- x / (101*12)
            
        })
        averageMonth <- as.data.frame(averagemonth)
        averageMonth$Freq <- as.numeric(averageMonth$Freq)
        averageMonth$Freq <- round(averageMonth$Freq, digits=3)
        averageMonth
    })
    
    output$avgMonthGraph <- renderPlot({
        
        averageMonth <- as.data.frame(averagemonth)
        averageMonth$Freq <- as.numeric(averageMonth$Freq)
        averageMonth$Freq <- round(averageMonth$Freq, digits=3)
        names(averageMonth)[1] <- "Var1"
        averageMonth
        
        monthPlot <- ggplot(data = averageMonth, aes(x=Var1, y =Freq)) +
            geom_bar(stat="identity", width=.5, color="black", fill="white") + 
            ggtitle("Average Movies per month") +
            labs(x = "Month", y = "# of Movies") +
            geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25) +
            theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=90))
        monthPlot 
    })
    
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
    
    output$firstGraph <- renderPlot({
        onlymonth <- movies3
        onlymonth$release <- format(as.Date(onlymonth$release), "%B")
        onlymonth$release <- as.character(onlymonth$release)
        names(onlymonth)[5] = "Month"
        onlymonth
        onlymonthtable <- table(a<-(onlymonth$Month))
        onlymonthtable <- as.data.frame(onlymonthtable)
        names(onlymonthtable)[1] = "Month"
        onlymonthtable
        
        plot6 <- ggplot(data= onlymonthtable, aes(x=Month, y=Freq)) + 
            geom_bar(stat="identity", width=.5, color="black", fill="pink") + 
            ggtitle("Amount Per Month") +
            labs(x = "Month", y = "Amount") +
            geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25) + 
            theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=90))
        plot6
    })
    output$secondGraph <- renderPlot({
        plot2 <- ggplot(data= moviesperyear, aes(x=Var1, y=Freq)) + 
            geom_bar(stat="identity", width=.5, color="black", fill="white") + 
            ggtitle("movies per year for whole dataset") +
            labs(x = "year", y = "# of Movies") +
            geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25) +
            theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=90))
        plot2
    })
    output$thirdGraph <- renderPlot({
        plot3 <- ggplot(data= genreAmount, aes(x=Var1, y=Freq)) + 
            geom_bar(stat="identity", width=.5, color="black", fill="pink") + 
            ggtitle("Amount Per Genre") +
            labs(x = "Genre", y = "Amount") +
            geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25)+
            theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=90))
        plot3
    })
    output$fourthGraph <- renderPlot({
        plot4 <- ggplot(data= certificateAmount, aes(x=Var1, y=Freq)) + 
            geom_bar(stat="identity", width=.5, color="black", fill="pink") + 
            ggtitle("Amount Per Certificate") +
            labs(x = "Certificate", y = "Amount") +
            geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25) +
            theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=90))
        plot4
    })
    output$fifthGraph <- renderPlot({
        plot5 <- ggplot(data= runtimeAmount, aes(x=Var1, y=Freq)) + 
            geom_bar(stat="identity", width=.5, color="black", fill="pink") + 
            ggtitle("Amount Per Runtime") +
            labs(x = "minutes", y = "amount of movies") +
            geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25) +
            theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=90))
        plot5
    })
    output$sixthGraph <- renderPlot({
        
    })
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)
