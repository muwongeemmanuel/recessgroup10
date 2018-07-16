library(shiny)
library(sqldf)
###Loading all packages
library(data.table) # Imprting data
library(tidyverse) # Data manupulation
library(tibble) # used to create tibbles
library(tidyr) # used to tidy up data
library(dplyr) # used for data manipulation
library(DT) # used for datatable function for displaying dataset
library(ggthemes) #for using themes for plots
library(shiny)

fifa <- read.csv("data/complete.csv", header=TRUE)
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


x <- as.factor(fifa18$PrefferedPosition)

levels(x) <- list(GK  = c("gk"), 
                  DEF = c("cb", "lb", "rb", "rwb", "lwb"), 
                  MID = c("cm","rm","cam","lm","cdm","cf"), 
                  FWD = c("st", "rw", "lw"))

fifa18 <- mutate(fifa18, Position = x)

shinyServer(function(input, output,session) {
  
 
  db <- dbConnect(SQLite(), dbname="database.sqlite")
  
  
  USER <- reactiveValues(Logged = FALSE)
  
  observeEvent(input$login, {
 
    c<-sqldf("SELECT * FROM user ", dbname = "database.sqlite")  
    nrow(c)
    rt<- 0;
    for (row in 1:nrow(c)) {
      username <- c[row, "username"]
      passwordd  <- c[row, "password"]
      t<- grepl(input$username, username)
      f<- grepl(input$password, passwordd)
      if(t == TRUE & f == TRUE) {
        rt<- rt+1
      }
    }
    if(rt>0){
      USER$Logged <- TRUE
      output$well = renderText("Welcome Mr/Ms.")
      output$valid = renderText(input$username)
    }else{
      output$message = renderText("Invalid user name or password")
      
      show("message")
      
      
      #delay(10000, hide("message", anim = TRUE, animType = "fade"))
      
    }
    
  
  })
  
  
  output$tplayers <- renderDataTable({
    
    Top <- input$Top
    
    if(Top == "overall"){
      # 1. top 11 players according to their Overall fifa rankings. It can be seen that these players have a very high valuation and command high wages.
      h2("Top 11 players according to their Overall fifa rankings.")
      datatable(select(top_n(arrange(fifa18,-overall),as.numeric(input$Rows),wt = overall) 
                       ,name, age,overall,club,eur_value) , options = list(scrollX = TRUE, pageLength = as.numeric(input$Rows)),
                caption = "Top 11 players according to their Overall fifa rankings.") 
      
    }
    else{
      
      # 1. top 11 players according to their Potential fifa rankings. It can be seen that these players have a very high valuation and command high wages.
      h2("Top 11 players according to their Potential fifa rankings.")
      datatable(select(top_n(arrange(fifa18,-potential),as.numeric(input$Rows),wt = potential) 
                       ,name, age,potential,club,eur_value) , options = list(scrollX = TRUE, pageLength = as.numeric(input$Rows)),
                caption = "Top 11 players according to their Potential fifa rankings.") 
      
    }
    
  })
  
  output$tclubs <- renderDataTable({
    
    Top <- input$Top
    
    if(Top == "overall"){
      # 1. top 11 players according to their Potential fifa rankings. It can be seen that these players have a very high valuation and command high wages.
      #h2("Top 11 players according to their Potential fifa rankings.")
      #fifa18 %>%
      # group_by(club) %>%
      #summarise(MeanOverallRating = round(x = mean(overall), digits = 2)) %>%
      #arrange(desc(MeanOverallRating)) %>%
      #top_n(10,wt = MeanOverallRating) %>%
      #datatable(class = "nowrap hover row-border", escape = FALSE, 
      #          options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
      fifa18 %>%
        group_by(club) %>%
        summarise(OverallScore = mean(overall)) %>%
        mutate(OverallScore = round(OverallScore, 1)) %>%
        arrange(desc(OverallScore)) %>%
        head(100)
    }
    else{
      
      # 1. top 11 players according to their Potential fifa rankings. It can be seen that these players have a very high valuation and command high wages.
      #h2("Top 11 players according to their Potential fifa rankings.")
      #fifa18 %>%
      # group_by(club) %>%
      # summarise(MeanPotentialRating = round(x = mean(potential), digits = 2)) %>%
      # arrange(desc(MeanPotentialRating)) %>%
      # top_n(10,wt = MeanPotentialRating) %>%
      # datatable(class = "nowrap hover row-border", escape = FALSE, 
      #          options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
      fifa18 %>%
        group_by(club) %>%
        summarise(Potentialscore = mean(potential)) %>%
        mutate(Potentialscore = round(Potentialscore, 1)) %>%
        arrange(desc(Potentialscore)) %>%
        head(100)
    }
    
  })
  
  output$tcountries <- renderDataTable({
    
    Top <- input$Top
    
    if(Top == "overall"){
      # 1. top 11 players according to their Potential fifa rankings. It can be seen that these players have a very high valuation and command high wages.
      #h2("Top 11 players according to their Potential fifa rankings.")
      
      # fifa18 %>%
      # group_by(nationality) %>%
      # summarise(MeanOverallRating = round(x = mean(overall), digits = 2), n = n()) %>%
      # filter(n > 100) %>%
      # arrange(desc(MeanOverallRating)) %>%
      # top_n(10,wt = MeanOverallRating) %>%
      # select(nationality,MeanOverallRating) %>%  
      # datatable(class = "nowrap hover row-border", escape = FALSE, 
      #           options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE)) 
      fifa18 %>%
        group_by(nationality) %>%
        summarise(OverallScore = mean(overall)) %>%
        mutate(OverallScore = round(OverallScore, 1)) %>%
        arrange(desc(OverallScore)) %>%
        head(10)
    }
    else{
      
      # 1. top 11 players according to their Potential fifa rankings. It can be seen that these players have a very high valuation and command high wages.
      #h2("Top 11 players according to their Potential fifa rankings.")
      
      #fifa18 %>%
      #  group_by(nationality) %>%
      # summarise(MeanPotentialRating = round(x = mean(potential), digits = 2), n = n()) %>%
      #filter(n > 100) %>%
      #  arrange(desc(MeanPotentialRating)) %>%
      #  top_n(10,wt = MeanPotentialRating) %>%
      #  select(nationality,MeanPotentialRating) %>%  
      # datatable(class = "nowrap hover row-border", escape = FALSE, 
      #          options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE)) 
      fifa18 %>%
        group_by(nationality) %>%
        summarise(PotentialScore = mean(potential)) %>%
        mutate(PotentialScore = round(PotentialScore, 1)) %>%
        arrange(desc(PotentialScore)) %>%
        head(100)
    }
    
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
  
  #--Plot top 10 Clubs--#
  output$tnation <- renderPlot({
    
    best_nation <- reactive( fifa18 %>%
                               group_by(nationality) %>%
                               summarise(OverallScore = mean(overall)) %>%
                               mutate(OverallScore = round(OverallScore, 1)) %>%
                               arrange(desc(OverallScore)) %>%
                               head(10)
    )
    
    ggplot(best_nation(),
           aes(x = reorder(nationality, -OverallScore),
               y = OverallScore,
               fill = OverallScore)) +
      geom_col() +
      geom_text(aes(label = OverallScore), hjust = -0.5) +
      ylim(0, 100) +
      coord_flip() +
      theme(legend.position = "none") +
      labs(x = "Nationality", y = "Overall score") +
      ggtitle("Overall score by Nationality")
    
  })
  
  #--Plot top 10 clubs--#
  output$tclub <- renderPlot({
    
    best_club <- reactive(fifa18 %>%
                            group_by(club) %>%
                            summarise(OverallScore = mean(overall)) %>%
                            mutate(OverallScore = round(OverallScore, 1)) %>%
                            arrange(desc(OverallScore)) %>%
                            head(10)
    )
    ggplot(best_club(),
           aes(x = reorder(club, -OverallScore),
               y = OverallScore,
               fill = OverallScore)) +
      geom_col() +
      geom_text(aes(label = OverallScore), hjust = -0.5) +
      ylim(0, 100) +
      coord_flip() +
      theme(legend.position = "none") +
      labs(x = "Clubs", y = "Overall score") +
      ggtitle("Overall score by Teams")
    
  })
  
  output$tleague <- renderPlot({
    best_league <- reactive(fifa18 %>%
                              group_by(league) %>%
                              summarise(OverallScore = mean(overall)) %>%
                              mutate(OverallScore = round(OverallScore, 1)) %>%
                              arrange(desc(OverallScore)) %>%
                              head(10)
    )
    ggplot(best_league(),
           aes(x = reorder(league, -OverallScore),
               y = OverallScore,
               fill = OverallScore)) +
      geom_col() +
      geom_text(aes(label = OverallScore), hjust = -0.5) +
      ylim(0, 100) +
      coord_flip() +
      theme(legend.position = "none") +
      labs(x = "Leagues", y = "Overall score") +
      ggtitle("Overall score by Leagues")
    
  })
  
  output$agevso <- renderPlot({
    age_overall <- reactive (fifa18 %>%
                               select(age, overall) %>%
                               group_by(age, overall) %>%
                               count()
    )
    ggplot(age_overall(), aes(x = age, y = overall)) +
      geom_point(aes(size = n, color = n)) +
      geom_smooth(method = "loess", color = "red") +
      theme(legend.position = "none") +
      ggtitle("Correlation between Age and Overall")
    
  })
  
  output$columns <- renderTable({
    summary(select(fifa18,input$sum))
  })
  
  output$ppc <- renderDataTable({
    fifa18 %>%
      filter(!is.na(club)) %>%
      group_by(club) %>%
      count()
  })
  
  output$ppn <- renderDataTable({
    fifa18 %>%
      filter(!is.na(nationality)) %>%
      group_by(nationality) %>%
      count()
  })
  
  output$dataset <- renderDataTable({
    fifa18 
  })
  # Begin debugging the "Invalid icon. Use Shiny's 'icon()' function to generate a valid icon" error
  validateIcon <- function(icon) {
    if (is.null(icon) || identical(icon, character(0))) {
      return(icon)
    } else if (inherits(icon, "shiny.tag") && icon$name == "i") {
      return(icon)
    } else {
      stop("Invalid icon. Use Shiny's 'icon()' function to generate a valid icon")
    }
  }
  
  
  
 
  output$sidepage = renderUI(
    if (!isTRUE(USER$Logged)) {
      fluidRow(column(width=4, offset = 4,
                      wellPanel(id = 'login',
                                textInput('username', 'Username:'),
                                passwordInput('password', 'Password:'),
                                div(actionButton('login', 'Log in'), style='text-align: center;')
                      ),
                      textOutput("message")
      ))
    } else {
      pageWithSidebar( 
        
        headerPanel(h1("Fifa 18 Datasets Analysis", style = "color:blue",actionButton("logout","Logout"))),
        sidebarPanel(
          selectInput("Analysis", "Please Select Analysis Type",
                      choices = c("Tables", "Plots" , "Predictions" , "Summary")),
          sliderInput("sampleSize", "Please Select Sample Size: ",
                      min = 100, max = 5000, value = 1000,step = 100),
          conditionalPanel(condition = "input.Analysis == 'Tables'",
                           textInput("Rows", "Please Select the top N row", 10)
                           
                           
          ),
          conditionalPanel(condition="input.tabAnalysis==1",
                           selectInput("Top", "Please Select Arrangement Criteria",
                                       choices = c("overall", "potential" )) 
          ),
          conditionalPanel(condition = "input.Analysis == 'Plots'",
                           textInput("lambda", "Please Select the Exponential Lambda", 1)),
          conditionalPanel(condition = "input.Analysis == 'Predictions'",
                           textInput("Team_1", "Please Select The First Team", "Chelsea"),
                           textInput("Team_2", "Please Select the Second Team", "Juventus")),
          conditionalPanel(condition = "input.Analysis == 'Summary'",
                           textInput("sum", "Please Enter The Column To Summarize", "age"))
        ),
        mainPanel( style = "background:white",
                   conditionalPanel(condition = "input.Analysis == 'Tables'",
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("Top Players",value = 1,fluidRow(dataTableOutput("tplayers"))),
                                      tabPanel("Top Clubs",value = 1,fluidRow(dataTableOutput("tclubs"))),
                                      tabPanel("Top Countries",value = 1,fluidRow(dataTableOutput("tcountries"))),
                                      tabPanel("Positions",value = 2,fluidRow(dataTableOutput("best"))),
                                      id = "tabAnalysis"
                                      
                                    )
                                    
                                    
                   ),
                   conditionalPanel(condition = "input.Analysis == 'Plots'",
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("DISTRIBUTION",fluidRow(plotOutput("distribution"))),
                                      tabPanel("TOP CLUBS",fluidRow(plotOutput("tclub"))),
                                      tabPanel("TOP NATIONS",fluidRow(plotOutput("tnation"))),
                                      tabPanel("TOP LEAGUES",fluidRow(plotOutput("tleague"))),
                                      tabPanel("AGE VS OVERALL",fluidRow(plotOutput("agevso")))
                                      
                                      
                                    )
                                    
                                    
                   ),
                   
                   conditionalPanel(condition = "input.Analysis == 'Predictions'",
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("Signings",fluidRow(plotOutput("signings"))),
                                      tabPanel("Odds",fluidRow(dataTableOutput("odds")))
                                      
                                    )
                                    
                                    
                   ),
                   
                   conditionalPanel(condition = "input.Analysis == 'Summary'",
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("Columns",fluidRow(tableOutput("columns"))),
                                      tabPanel("PLAYERS PER CLUB",fluidRow(dataTableOutput("ppc"))),
                                      tabPanel("PLAYERS PER NATION",fluidRow(dataTableOutput("ppn"))),
                                      tabPanel("VIEW ENTIRE DATASET",fluidRow(dataTableOutput("dataset"))),
                                      tabPanel("Clubs",fluidRow(dataTableOutput("clubs"))),
                                      tabPanel("Country",fluidRow(dataTableOutput("country"))),
                                      tabPanel("Continent",fluidRow(dataTableOutput("continent")))
                                      
                                    )
                                    
                                    
                   )
        )
      )
      
      
      
      
    }
    
    
  )
  
  # help tab
  output$search_plot <- renderUI({
    searchInput(inputId = "Id009", 
                label = "Enter the address", 
                placeholder = "A placeholder", 
                btnSearch = icon("search"), 
                btnReset = icon("remove"), 
                width = "100%")
  })  
  
  
 #logout function
  observeEvent(input$logout,{
    USER$Logged<-FALSE
  })
  
  
  
  
  
  
  
  
  
  


  
  
  
  
  
})
