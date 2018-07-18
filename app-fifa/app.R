###Loading all packages
library(data.table) # Imprting data
library(tidyverse) # Data manupulation
library(tibble) # used to create tibbles
library(tidyr) # used to tidy up data
library(dplyr) # used for data manipulation
library(DT) # used for datatable function for displaying dataset
library(ggthemes) #for using themes for plots
library(shiny)
library(class)
library(cluster)

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

#K-means clustering with 3 clusters of sizes 46, 54, 50
strikers <-fifa18[fifa18$Position == "FWD",c(1,2,7,10,11,22,29,30,35,36,39,43,44,46,49,51,56,59) ] 
k4 <- kmeans(strikers[,c(-1,-2)], centers = 400, nstart = 25)

midfielders <-fifa18[fifa18$Position == "MID",c(1,2,7,10,34,39,42,43,44,46,51,53,54,55,56,57,59,60,61,62) ] 
k3 <- kmeans(midfielders[,c(-1,-2)], centers = 400, nstart = 25)

defenders <-fifa18[fifa18$Position == "DEF",c(1,2,7,10,36,42,44,46,50,51,54,55,56,57,59,60,61,62) ] 
k5 <- kmeans(defenders[,c(-1,-2)], centers = 400, nstart = 25)

keepers <-fifa18[fifa18$Position == "GK",c(1,2,7,10,47,63,64,65,66,67) ] 
k1 <- kmeans(keepers[,c(-1,-2)], centers = 300, nstart = 25)


#its for odds
pois <- fifa18 %>% 
  group_by(club) %>%                            # multiple group columns
  summarise( sum_overall = sum(overall) )  # multiple summary columns
pois <- as.data.frame(pois)
pois <- tbl_df(pois)
pois$club <- sub("^$", "No Club", pois$club)

split1 <- pois[1:324,]
split2 <- pois[325:648,]

names(split1) <- c("team", "sum_overall")
names(split2) <- c("opponent", "sum_overall")

pois1 <- cbind(split1,split2)
pois2 <- cbind(split2,split1)


indx <- sapply(pois1, is.numeric)#check which columns are numeric
nm1 <- which(indx)#get the numeric index of the column
indx2 <- duplicated(names(nm1))#check which among the
# integer columns are duplicated
#use `Map` after splitting the "nm1" with its "names", do the `rowSums`
pois1[ nm1[!indx2]] <- Map(function(x,y) rowSums(x[y]), list(pois1),
                           split(nm1, names(nm1)))
pois1 <- pois1[ -nm1[indx2]]

indx <- sapply(pois2, is.numeric)#check which columns are numeric
nm1 <- which(indx)#get the numeric index of the column
indx2 <- duplicated(names(nm1))#check which among the
# integer columns are duplicated
#use `Map` after splitting the "nm1" with its "names", do the `rowSums`
pois2[ nm1[!indx2]] <- Map(function(x,y) rowSums(x[y]), list(pois2),
                           split(nm1, names(nm1)))
pois2 <- pois2[ -nm1[indx2]]


names(pois2) <- c("team", "sum_overall" , "opponent")

pois1$home <- rep(1,nrow(pois1)) # make new column
pois2$home <- rep(0,nrow(pois2)) # make new column

pois_final <- rbind(pois1,pois2)

# Define UI for app  ----
ui <- pageWithSidebar( 

  headerPanel(h1("FIFA 18 DATASET ANALYSIS", align = 'center', style = "background:skyblue;color:white")),
  sidebarPanel(style = "background:lightgreen", 
    img(src = 'ronaldo.jpg', height=300 , width = 350),          
    selectInput("Analysis", "Please Select Analysis Type",
                choices = c("Tables", "Plots" , "Predictions" , "Summary")),
    conditionalPanel(condition = "input.Analysis == 'Tables'",
                     textInput("Rows", "Please Select the top N row", 10),
                     
                     conditionalPanel(condition="input.tabAnalysis==1",
                                      selectInput("Top", "Please Select Arrangement Criteria",
                                                  choices = c("overall", "potential" )) 
                     )
                     
                     
    ),
    conditionalPanel(condition = "input.Analysis == 'Predictions'",
                     
                     
                     conditionalPanel(condition="input.tabPredictions==3",
                                      textInput("player", "Please Select the player you want your signing to be like", "G. Bale") 
                     ),
                     
                     
                     conditionalPanel(condition="input.tabPredictions==4",
                                      textInput("Team_1", "Please Select The Home Team", "Chelsea"),
                                      radioButtons("playersOut_H", "Select Numbers of missing players in Home Team",
                                                   c(1, 2, 3, 4,5),selected = FALSE,
                                                   inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                                      conditionalPanel(condition = "input.playersOut_H == 1",
                                                       textInput("playerH_1", "Player 1")),
                                      conditionalPanel(condition = "input.playersOut_H == 2",
                                                       textInput("playerH_2", "player 1"),
                                                       textInput("playerH_3", "Player 2")),
                                      conditionalPanel(condition = "input.playersOut_H == 3",
                                                       textInput("playerH_4", "player 1"),
                                                       textInput("playerH_5", "Player 2"),
                                                       textInput("playerH_6", "Player 3")),
                                      conditionalPanel(condition = "input.playersOut_H == 4",
                                                       textInput("playerH_7", "player 1"),
                                                       textInput("playerH_8", "Player 2"),
                                                       textInput("playerH_9", "player 3"),
                                                       textInput("playerH_10", "Player 4")),
                                      conditionalPanel(condition = "input.playersOut_H == 5",
                                                       textInput("playerH_11", "player 1"),
                                                       textInput("playerH_12", "Player 2"),
                                                       textInput("playerH_13", "player 3"),
                                                       textInput("playerH_14", "player 4"),
                                                       textInput("playerH_15", "Player 5")),
                                      
                                      textInput("Team_2", "Please Select the Second Team", "Juventus") ,
                                      
                                      radioButtons("playersOut_A", "Select Numbers of missing players in Away Team",
                                                   c(1, 2, 3, 4,5),selected = FALSE,
                                                   inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                                      conditionalPanel(condition = "input.playersOut_A == 1",
                                                       textInput("playerA_1", "Player 1")),
                                      conditionalPanel(condition = "input.playersOut_A == 2",
                                                       textInput("playerA_2", "player 1"),
                                                       textInput("playerA_3", "Player 2")),
                                      conditionalPanel(condition = "input.playersOut_A == 3",
                                                       textInput("playerA_4", "player 1"),
                                                       textInput("playerA_5", "Player 2"),
                                                       textInput("playerA_6", "Player 3")),
                                      conditionalPanel(condition = "input.playersOut_A == 4",
                                                       textInput("playerA_7", "player 1"),
                                                       textInput("playerA_8", "Player 2"),
                                                       textInput("playerA_9", "player 3"),
                                                       textInput("playerA_10", "Player 4")),
                                      conditionalPanel(condition = "input.playersOut_A == 5",
                                                       textInput("playerA_11", "player 1"),
                                                       textInput("playerA_12", "Player 2"),
                                                       textInput("playerA_13", "player 3"),
                                                       textInput("playerA_14", "player 4"),
                                                       textInput("playerA_15", "Player 5"))
                     )
    ),
    conditionalPanel(condition = "input.Analysis == 'Summary'",
                     textInput("sum", "Please Enter The Column To Summarize", "age"))
  ),
  mainPanel( style = "background:lightblue",
             conditionalPanel(condition = "input.Analysis == 'Tables'",
                              style = "background: ; padding:20px",
                              
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
                                tabPanel("AGE VS OVERALL",fluidRow(plotOutput("agevso"))),
                                tabPanel("AGE FREQUENCY",fluidRow(plotOutput("agef"))),
                                tabPanel("NUMBER OF PLAYERS",fluidRow(plotOutput("natdis"))),
                                tabPanel("OVERALL VS AGE VS WAGE",fluidRow(plotOutput("wage"))),
                                tabPanel("OVERALL VS AGE VS VALUE",fluidRow(plotOutput("value"))),
                                tabPanel("TOP VALUABLE CLUBS",fluidRow(plotOutput("valuable")))
                                
                                
                                
                              )
                              
                              
             ),
             
             conditionalPanel(condition = "input.Analysis == 'Predictions'",
                              
                              tabsetPanel(
                                
                                tabPanel("Signings",value = 3,fluidRow(dataTableOutput("signings"))),
                                tabPanel("Odds",value = 4,fluidRow(dataTableOutput("odds"))),
                                id = "tabPredictions"
                                
                              )
                              
                              
             ),
             
             conditionalPanel(condition = "input.Analysis == 'Summary'",
                              
                              tabsetPanel(
                                
                                tabPanel("Columns",fluidRow(tableOutput("columns"))),
                                tabPanel("PLAYERS PER CLUB",fluidRow(dataTableOutput("ppc"))),
                                tabPanel("PLAYERS PER NATION",fluidRow(dataTableOutput("ppn"))),
                                tabPanel("VIEW ENTIRE DATASET",fluidRow(dataTableOutput("dataset")))
                                
                                
                              )
                              
                              
             )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session){
  
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
      
      fifa18 %>%
        group_by(club) %>%
        summarise(OverallScore = mean(overall)) %>%
        mutate(OverallScore = round(OverallScore, 1)) %>%
        arrange(desc(OverallScore)) %>%
        head(100)
    }
    else{
      
      
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
      
      fifa18 %>%
        group_by(nationality) %>%
        summarise(OverallScore = mean(overall)) %>%
        mutate(OverallScore = round(OverallScore, 1)) %>%
        arrange(desc(OverallScore)) %>%
        head(10)
    }
    else{
      
      
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
  
  output$signings <- renderDataTable({
    
    field <- fifa18 %>% 
      filter(name == input$player)%>%
      select( name,Position)
    field_part <-as.character(field$Position) 
    
    if(field_part == "FWD"){
      ## list of cluster assignments
      o=order(k4$cluster)
      s_clusters <- data.frame(strikers$name[o],k4$cluster[o])
    }
    if(field_part == "MID"){
      ## list of cluster assignments
      o=order(k3$cluster)
      s_clusters <- data.frame(midfielders$name[o],k3$cluster[o])
    }
    if(field_part == "DEF"){
      ## list of cluster assignments
      o=order(k5$cluster)
      s_clusters <- data.frame(defenders$name[o],k5$cluster[o])
    }
    if(field_part == "GK"){
      ## list of cluster assignments
      o=order(k1$cluster)
      s_clusters <- data.frame(keepers$name[o],k1$cluster[o])
    }
    
    names(s_clusters) <- c("name", "clusters")
    cluster_group <-as.numeric(s_clusters[s_clusters$name == input$player,2])  #s_clusters[s_clusters$name == "L. Messi",2] 
    final_cluster <- s_clusters[s_clusters$clusters == cluster_group,]
    
  })
  
  output$odds <- renderDataTable({
    
    
    po_H <- fifa18 %>% 
      filter(club == input$Team_1)%>%
      select( name,overall)%>%
      filter(name %in% c(input$playerH_1,input$playerH_2,input$playerH_3,input$playerH_4,input$playerH_5,
                         input$playerH_6,input$playerH_7,input$playerH_8,input$playerH_9,input$playerH_10,
                         input$playerH_11,input$playerH_12,input$playerH_13,input$playerH_14,input$playerH_15
                         
      ))
    missing_H <- sum(as.numeric(po_H$overall))
    
    pois_final$sum_overall[pois_final$team == input$Team_1] <-  as.numeric(pois_final$sum_overall[pois_final$team == input$Team_1]) - missing_H
    pois_final$sum_overall[pois_final$opponent == input$Team_1] <-  as.numeric(pois_final$sum_overall[pois_final$opponent == input$Team_1]) - missing_H
    
    po_A <- fifa18 %>% 
      filter(club == input$Team_2)%>%
      select( name,overall)%>%
      filter(name %in% c(input$playerA_1,input$playerA_2,input$playerA_3,input$playerA_4,input$playerA_5,
                         input$playerA_6,input$playerA_7,input$playerA_8,input$playerA_9,input$playerA_10,
                         input$playerA_11,input$playerA_12,input$playerA_13,input$playerA_14,input$playerA_15
                         
      ))
    missing_A <- sum(as.numeric(po_A$overall))
    
    pois_final$sum_overall[pois_final$team == input$Team_2] <-  as.numeric(pois_final$sum_overall[pois_final$team == input$Team_2]) - missing_A
    pois_final$sum_overall[pois_final$opponent == input$Team_2] <-  as.numeric(pois_final$sum_overall[pois_final$opponent == input$Team_2]) - missing_A
    
    
    #pois_final$sum_overall <- sub( 18649, 4000, pois_final$sum_overall)
    pois_final$sum_overall <-  as.numeric(pois_final$sum_overall) / 2900
    
    model <- glm(sum_overall ~ team + opponent + home, family=poisson(link=log), data=pois_final)
    
    #Chelsea
    predictHome <- predict(model, data.frame(home=1, team=input$Team_1, opponent=input$Team_2), type="response")
    # 0.9453705 
    
    #for sunderland. note that Home=0.
    predictAway <- predict(model, data.frame(home=0, team=input$Team_2, opponent=input$Team_1), type="response")
    # 0.999 
    
    
    set.seed(915706074)
    nsim <- 10000
    homeGoalsSim <- rpois(nsim, predictHome) 
    awayGoalsSim <- rpois(nsim, predictAway)
    goalDiffSim <- homeGoalsSim - awayGoalsSim
    #Home
    home <- (sum(goalDiffSim > 0) / nsim) + 0.010 #0.3275
    #Draw
    draw <- (sum(goalDiffSim == 0) / nsim) + 0.033 # 0.3197
    #Away
    away <- (sum(goalDiffSim < 0) / nsim) + 0.132 #0.3528
    
    oddsHome <- 1/home
    oddsDraw <- 1/draw
    oddsAway <- 1/away
    m <- dpois(0:7 , predictHome) %o% dpois(0:7 , predictAway)
    rownames(m) <- 0:7
    colnames(m) <- 0:7
    
    market <- 2.0
    prob <- 0
    for (i in as.numeric(rownames(m))) {
      for (j in as.numeric(colnames(m))) {
        if( i + j > market){
          prob <- prob + m[i+1 , j+1]
        }
        
      }
    }
    over <- prob + 0.06
    under <- (1-prob) + 0.21
    
    oddsOver <- 1/over 
    oddsUnder <- 1/under 
    DF = data.frame(
      
      Variables = c("Home Win", "Draw", "Away Win",
                    "Over", "Under"
      ),
      Odds = c(round(oddsHome,3), round(oddsDraw,3), round(oddsAway,3),
               round(oddsOver,3), round(oddsUnder,3)
      ),
      . = rep("", 5),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  })
  
  output$agef <- renderPlot({
    age <- reactive(table(fifa18$age)) #create a table for age placed in variable age. The function table() creates
    #columns for the values and their frequencies
    
    
    
    #USE barplot() and par() for parameters
    par(oma = c(1,4,1,1)) #Sets outside margin: bottom, left, top ,right
    par(mar = c(4,5,2,1)) #Sets plot margins
    #Add main title and label for x axis
    barplot(age(), 
            main = "Number of players per age in FIFA 18" , 
            xlab = "Age",
            ylab = "Frequency",
            col  = "blue")
  })
  
  output$natdis <- renderPlot({
    countries_count <- count(fifa18, nationality)
    top_10_countries <- top_n(countries_count, 10, n)
    top_10_country_names <- top_10_countries$nationality
    
    country <- filter(fifa18, nationality == top_10_country_names)
    ggplot(country, aes(x = nationality)) + 
      geom_bar(col = "green", aes(fill = ..count..)) + ggtitle("Distribution based on Nationality of Players (Top 10 Countries)")
  })
  
  output$wage <- renderPlot({
    g_age_overall <- ggplot(fifa18, aes(age, overall))
    g_age_overall + 
      geom_point(aes(color=eur_wage)) + geom_smooth(color="pink") + 
      ggtitle("Distribution between Age and Overall of players based  on Wage bracket")
    
  })
  
  output$value <- renderPlot({
    g_age_overall <- ggplot(fifa18, aes(age, overall))
    g_age_overall + geom_point(aes(color=eur_value)) + geom_smooth(color="darkblue") + 
      ggtitle("Distribution between Age and Overall of players based on Value bracket")
  })
  
  output$valuable <- renderPlot({
    group_clubs <- group_by(fifa18, club)
    club_value <- summarise(group_clubs, total_val = sum(eur_value))
    top_10_valuable_clubs <- top_n(club_value, 10, total_val)
    
    top_10_valuable_clubs$Club <-as.factor(top_10_valuable_clubs$club)
    
    ggplot(top_10_valuable_clubs, aes(x = club, y = total_val)) + geom_bar(stat = "identity", aes(fill=total_val)) + coord_flip() + ggtitle("Top 10 valuable clubs")
    
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
  } #End debugging
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)