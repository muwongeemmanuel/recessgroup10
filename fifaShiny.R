###Loading all packages
library(data.table) # Imprting data
library(tidyverse) # Data manupulation
library(tibble) # used to create tibbles
library(tidyr) # used to tidy up data
library(dplyr) # used for data manipulation
library(DT) # used for datatable function for displaying dataset
library("ggthemes") #for using themes for plots
library(shiny)

fifa <- read.csv("C:/Users/Mr. Kakooza/Desktop/software 2.2/recess2/rp/Data_set_10_Fifa18_complete_player_dataset/fifa-18-more-complete-player-dataset/Complete.csv", header=TRUE)
fifa_1 <- tbl_df(fifa)
fifa_2 <- data.frame(fifa_1[1:158],
                     PrefferedPosition = colnames(fifa_1[159:185])[apply(
                       X = fifa_1[159:185],
                       MARGIN = 1,
                       FUN = function(x) which(x == "True")[1])],
                     stringsAsFactors = FALSE)
fifa_2$PrefferedPosition[is.na(fifa_2$preferredPosition)] = "No Choice"
fifa_2$PrefferedPosition <- gsub("prefers_", "", fifa_2$PrefferedPosition) #removing characters 'prefers_'from player's PrefferedPosition
fifa_2[is.na(fifa_2)] <- 0 #Replace all NA values with 0
fifa_3 <- fifa_2[!duplicated(fifa_2$ID),] # Removing duplicates
fifa18 <- fifa_3

# Define UI for app that draws a histogram ----
ui <- pageWithSidebar(
  
  headerPanel("Fifa 18 Datasets Analysis"),
  sidebarPanel(
    selectInput("Analysis", "Please Select Analysis Type",
                choices = c("Tables", "Plots" , "Predictions")),
    sliderInput("sampleSize", "Please Select Sample Size: ",
                min = 100, max = 5000, value = 1000,step = 100),
    conditionalPanel(condition = "input.Analysis == 'Tables'",
                     textInput("Rows", "Please Select the top N row", 10)),
    conditionalPanel(condition = "input.Analysis == 'Plots'",
                     textInput("lambda", "Please Select the Exponential Lambda", 1)),
    conditionalPanel(condition = "input.Analysis == 'Predictions'",
                     textInput("Team_1", "Please Select The First Team", "Chelsea"),
                     textInput("Team_2", "Please Select the Second Team", "Juventus"))
  ),
    mainPanel(
      conditionalPanel(condition = "input.Analysis == 'Tables'",
                         
                             tabsetPanel(
                               
                               tabPanel("Top Players",fluidRow(dataTableOutput("top"))),
                               tabPanel("Positions",fluidRow(dataTableOutput("best")))
                               
                             )
                         
                       
       ),
      conditionalPanel(condition = "input.Analysis == 'Plots'",
                       
                         tabsetPanel(
                           
                           tabPanel("Distribution",fluidRow(plotOutput("distribution"))),
                           tabPanel("phatcom",fluidRow(dataTableOutput("phat")))
                           
                         )
                       
                       
      ),
      
      conditionalPanel(condition = "input.Analysis == 'Predictions'",
                       
                         tabsetPanel(
                           
                           tabPanel("Sundeo",fluidRow(plotOutput("sd"))),
                           tabPanel("Cathy",fluidRow(dataTableOutput("cat")))
                           
                         )
                       
                       
      )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session){
  
  output$top <- renderDataTable({
    
    # 1. top 11 players according to their Overall fifa rankings. It can be seen that these players have a very high valuation and command high wages.
    h2("Top 11 players according to their Overall fifa rankings.")
    datatable(select(top_n(arrange(fifa18,-overall),as.numeric(input$Rows),wt = overall) 
                      ,name, age,overall,club,eur_value) , options = list(scrollX = TRUE, pageLength = as.numeric(input$Rows)),
              caption = "Top 11 players according to their Overall fifa rankings.") 
    
  })
  
  output$best <- renderDataTable({
    
    # 2. top player in each of the given position a player plays in.
    fifa18 %>% group_by(PrefferedPosition) %>%
      arrange(-overall) %>% 
      top_n(1,wt = overall) %>% 
      select( PrefferedPosition,name, overall,club,nationality) %>% 
      datatable(options = list(scrollX = TRUE, pageLength = as.numeric(input$Rows)),
                caption = "Top player in each of the given position a player plays in.")
    
  })
  
  output$distribution <- renderPlot({
    
    # 3. Distribution of overall ratings
    fifa18  %>% 
      ggplot(aes(x = overall, fill = factor(overall))) +
      geom_bar() + guides(fill = guide_legend(title = "Overall rating")) +
      labs(title = "Distribution of overall ratings") +
      theme(legend.position = "right", panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)