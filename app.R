#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggridges)

#Reece Arbogast - Shiny Week 2

#Read in Data Set

persondata <- read.csv("person.csv")

#Create Data Subset
persondata <- persondata[,c("MOD_YEAR","VPICMAKENAME","VPICMODELNAME")] 

#Remove Missing Values
persondata <- na.omit(persondata)

#Change Variable Classes
persondata$MOD_YEAR <- as.numeric(persondata$MOD_YEAR)
persondata$VPICMAKENAME <- as.factor(persondata$VPICMAKENAME)
persondata$VPICMODELNAME <- as.factor(persondata$VPICMODELNAME)

#Subset Variables
library(dplyr)
persondata <- filter( 
  persondata, VPICMAKENAME == c("Jeep", "Ford", "Kia", "Subaru", "Toyota", "Dodge", "Honda", "Chevrolet", "Hyundai") & MOD_YEAR < 2023 & MOD_YEAR > 1998) 

#Created Modified Data Frame
persondata <- persondata %>%
  group_by(VPICMAKENAME, MOD_YEAR) %>%
  summarise(TotalAccidents = n())

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Accidents by Car Make"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("select_col", "Select Make:",
                      choices = unique(persondata$VPICMAKENAME)),
          radioButtons("color", "Select Color:",
                       c("darkblue",
                         "blue",
                         "lightblue",
                         "lightgreen")),
          checkboxInput("CHECKBOX","Line Chart", FALSE)

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
      
      
      filtered_data <- reactive({
        filter_data <- persondata
        if (input$select_col != "All") {
          filter_data <- filter_data %>% filter(VPICMAKENAME == input$select_col)
        }
        return(filter_data)
      })
      if(input$CHECKBOX == TRUE) {
        
        ggplot(filtered_data(), aes(x = MOD_YEAR, y = TotalAccidents, group=1)) +
          geom_line(color = input$color)+
          geom_point(shape = 23, size = 4, fill = "red") +
          labs(title = "Total Accidents by Car Make",
               x = "Year of Model",
               y = "Total Accidents")
        
      }else{
      
      ggplot(filtered_data(), aes(x = as.factor(MOD_YEAR), y = TotalAccidents)) +
        geom_bar(stat = "identity", fill = input$color) +
        labs(title = "Total Accidents by Car Make",
             x = "Year of Model",
             y = "Total Accidents") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 80, hjust = 1))}
     
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
