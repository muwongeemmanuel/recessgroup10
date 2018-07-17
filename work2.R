library(gridExtra)
library(ggplot2) 
library(readr) 
library(dplyr)       # Data Preparation
library(stringr)     # for string manipulations
library(data.table)  # Importing the dataset efficiently and effectively
library(tidyr)       # Tidying up data
library(formattable)
library(recommenderlab)
library(knitr)
library(plotly)
library(ggthemes) # For displaying graphs in different themes
library(tidyverse)  # for data wrangling
library(tibble)     # Creating tibble
library(Rtsne)      # implements the t-SNE algorithm
library(kohonen)    # implements self organizing maps
library(hrbrthemes) # nice themes for ggplot
library(GGally)     # to produce scatterplot matrices   
library(DT)         # Displaying dataset in asthetic format
library(kableExtra) # Construct complex tables and customize styles 
library(corrplot)
library(qgraph)
library(methods)

# Data Import
data <- read.csv("Complete.csv")
# Dimensions of the dataset
dim(data)
# Displaying Column names
names(data)
# Keeping only the required columns
data<- data[, -c(1,48,50:51,53:54,56:59,61,63:65,67:70,72,74)]
# Dimension of the final dataset
dim(data)
# Names of the final dataset
names(data)

# Top 6 players based on Potential Rating
head( data%>% select(name,club,age,potential) %>% arrange(desc(potential)))
# Histogram of the Player Overall rating
hist(data$overall, xlab = "Player Overall Rating", col="black", border="blue", main = "Histogram of Player overall Rating")
# Boxplot of the Player Age
boxplot(data$age, xlab = "Player Age",col="yellow",border="blue", main = "Boxplot of Player Age") 

# Age Density
ggplot(data,aes(x = age, fill = factor(age))) + geom_bar() + guides(fill = FALSE) + xlab("Player Age") + 
ylab("Number of Players") + scale_x_continuous(breaks = c(16:47)) + ggtitle("Player's Age") + 
theme(plot.title = element_text(hjust = 0.5))

# Boxplot for Overall Rating
ggplot(data,aes(x = "", y =overall)) + geom_boxplot() + xlab("") +  ylab("Overall Rating") + 
ggtitle("Boxplot for Overall Rating") + theme(plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(40, 100))  

# Boxplot for Potential Rating
ggplot(data,aes(x = "", y = potential)) + geom_boxplot() + xlab("") +  ylab("Potential Rating") + 
ggtitle("Boxplot for Potential Rating") + theme(plot.title = element_text(hjust = 0.5)) + 
coord_cartesian(ylim = c(40, 100))  

#Age vs Overall Rating
ggplot(data,aes(x = age,y = overall)) + geom_point(aes(color = factor(age))) + 
geom_smooth(method = "lm") + xlab("Player Age") + ylab("Overall Rating") + 
ggtitle("Player Age vs Overall Rating") + theme(plot.title = element_text(hjust = 0.5)) 

#Age vs Potential Rating
ggplot(data,aes(x = age,y = potential)) + geom_point(aes(color = factor(age))) + geom_smooth(method = "lm") +
xlab("Player Age") + ylab("Potential Rating") + ggtitle("Player Age vs Potential Rating") + 
theme(plot.title = element_text(hjust = 0.5)) 

#Age vs Overall - Potential Rating
ggplot(data,aes(x = age,y = overall - potential)) + geom_point(aes(color = factor(age))) + 
geom_smooth(method = "lm") + xlab("Player Age") + ylab("Overall - Potential Rating") + 
ggtitle("Player Age vs Overall - Potential Rating") + theme(plot.title = element_text(hjust = 0.5))  

# Top 10 Potential Nationality
data %>% group_by(nationality) %>% summarise(MeanPotentialRating = round(x = mean(potential), digits = 2), n = n()) %>%
  filter(n > 100) %>% arrange(desc(MeanPotentialRating)) %>% top_n(10,wt = MeanPotentialRating) %>%
  select(nationality,MeanPotentialRating) %>% datatable(class = "nowrap hover row-border", escape = FALSE, 
                                                        options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
#Top 10 Overall Nationality
data %>% group_by(nationality) %>% summarise(MeanOverallRating = round(x = mean(overall), digits = 2), n = n()) %>%
  filter(n > 100) %>% arrange(desc(MeanOverallRating)) %>% top_n(10,wt = MeanOverallRating) %>%
  select(nationality,MeanOverallRating) %>% datatable(class = "nowrap hover row-border", escape = FALSE,                                                       options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))  

names(data)[64] <- "PreferedPosition" 
# Splitting a player's prefered positions
fifa <- separate(data, PreferedPosition, c("PreferedPosition"), sep = " ") 

#To identify variables containing NULL values
colSums(is.na(fifa)) 
fifa[is.na(fifa)] <- 0 #Replace all NA values with 0
colSums(is.na(fifa)) #Check if all NA values are converted to 0
(is.null(fifa)) #Check for NULL values

# Converting Value to proper format
fifa$eur_value <- gsub(".*¬", "",fifa$eur_value) 
fifa$eur_value <- gsub("M$", "", fifa$eur_value) #removing million character 'M'from player's value
fifa$eur_wage <- gsub(".*¬", "", fifa$eur_wage) # Converting Wage to proper format
fifa$eur_wage<- gsub("K$", "", fifa$eur_wage) #removing thousand character 'K' from wage
fifa<- df %>%  subset(eur_value != 0) %>% subset(eur_wage != 0) # removing all players' whose Valuation and Wage is 0.

#converting into data frame as tibble doesnt give appropriate results for sub function
fifa <- as.data.frame(fifa)
# Converting columns with player attributes to numeric
for (i in 14:47) 
{
  fifa[,i] <- sub("\\+.*", "", fifa[,i])
  fifa[,i] <- sub("\\-.*", "", fifa[,i])
}
fifa <- as_tibble(fifa)   #Converting back to tibble
colnames(fifa)[16] <- "Value in Million Euros"
colnames(fifa)[17] <- "Wage in '000 Euros"
# Converting columns with player attributes to numeric
for (i in 11:47) 
{
  fifa[,i] <- as.numeric(unlist(fifa[,i] ))
}

#the top 10 players according to their Overall fifa rankings.
fifa %>% arrange(-overall) %>% 
  top_n(10,wt = overall) %>% 
  select( name, age,overall,club,PreferedPosition) %>% 
  datatable(options = list(scrollX = TRUE, pageLength = 10))

#the top player in each of the given position a player plays in
fifa %>% group_by(PreferedPosition) %>%
  arrange(-overall) %>% 
  top_n(1,wt = overall) %>% 
  select( `PreferedPosition`,name, overall,club,nationality, `Value in Million Euros`,`Wage in '000 Euros`) %>% 
  datatable(options = list(scrollX = TRUE, pageLength = 10))

#the distributions of player rankings
fifa  %>% ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar() + guides(fill = guide_legend(title = "overall rating")) + labs(title = "Player Ratings") +
  theme(legend.position = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Players playing in European powerhouse leagues have much higher wages than player in other leagues.
ggplot(data = fifa, aes(`Wage in '000 Euros`, fill =     factor(PreferedPosition))) + 
  geom_bar() + scale_x_discrete(breaks = c(0,10,20,30,40,50,60,70,80,90,100,120,130)) +
  guides(fill = guide_legend(title = "")) + 
  labs(title = "COMPARISON OF NUMBER OF PLAYERS AT A WAGE BY POSITION") 

#a relationship between a players age and their wages per week.
ggplot(data = fifa, aes(x = age, y = `Wage in '000 Euros`)) +
  geom_line(color = "red",size = 2) + labs(title = "WAGE vs AGE OF PLAYERS") +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))

#player playing in attacking role is receving higher wage than the player playing in traditionally defensive role.
ggplot(data = fifa, aes(x = PreferedPosition, y = `Wage in '000 Euros`, color = overall)) + geom_point() +
  geom_jitter() + labs(title = "") +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Correlation between a player's overall rating and the potential rating
ggplot(fifa,aes(overall, potential)) + geom_point( size = 2, alpha = .9) + geom_jitter() + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Variations with Age 
ggplot(fifa) + geom_tile(aes(overall, potential, fill = age)) + 
  scale_fill_distiller(palette = "Spectral") +  theme( panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),panel.background = element_blank(), 
  axis.line = element_line(colour = "black"))

# Top 10 Potential Clubs
fifa %>% group_by(club) %>% summarise(MeanPotentialRating = round(x = mean(potential), digits = 2)) %>%
    arrange(desc(MeanPotentialRating)) %>% top_n(10,wt = MeanPotentialRating) %>%
    datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
       
# Top 10 Overall Clubs
fifa %>% group_by(club) %>% summarise(MeanOverallRating = round(x = mean(overall), digits = 2)) %>%
    arrange(desc(MeanOverallRating)) %>% top_n(10,wt = MeanOverallRating) %>%
    datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))  

glimpse(fifa)
#attributes and prefered positions of players
fifa<- fifa %>% select(acceleration:volleys,PreferedPosition)
       head(fifa$PreferedPosition)
       
            
