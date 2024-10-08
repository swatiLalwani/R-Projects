#UNDERSTANDING AND WORKING WITH DATA
#Q1 Importing the Batting File
batting<-read.csv("Batting.csv")
#Q2 Reading the Batting File
head(batting)
#Q3 Understanding the Structure of the data 
str(batting)
summary(batting)
#Q4 Calling some columns
head(batting$AB)
head(batting$X2B)
#STATISTICAL ADDITIONS USED IN MONEYBALL
#a) Batting Average
batting$BA<-batting$H/batting$AB
#checking batting$BA column
head(batting$BA)
tail(batting$BA,5)
#b) ON BASE PERCENTAGE
batting$OBP<-(batting$H+batting$BB+batting$HBP)/(batting$AB+batting$BB+batting$HBP+batting$SF)
#checking batting$OBP column
head(batting$OBP)
tail(batting$OBP,5)
#c)  Creating X1B (Singles)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR
#d) Creating Slugging Average (SLG)
batting$SLGA <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB
#checking batting$SLGA column
head(batting$SLGA)
tail(batting$SLGA,5)
#Checking the structure of the data frame after adding some columns
str(batting)
#MERGING TWO DATA TABLES (SALARY AND BATTING)
#Q5 Importing the salary File
Salaries <- read.csv("Salaries.csv")
#Q6 Reading the salary File
head(Salaries)
#Q7 Understanding the Structure of the data 
str(Salaries)
summary(Salaries)
#Q8 Calling some columns
head(Salaries$playerID)
head(Salaries$yearID)
#Q9 While looking at the summaries of both the data tables it is seen that the batting table year ID starts at 1871 while  in the salaries table it starts at 1985 so,making it even to merge both the data sets
batting <- subset(batting,yearID >= 1985)
summary(batting)
#Q10 Merging two data frames
Game<-merge(batting,Salaries,by=c('playerID','yearID'))
summary(Game)
head(Game)
#ANALYZING THE PLAYERS WHO LOST
lost_players <- subset(Game,playerID %in% c('giambja01','damonjo01','saenzol01') )
lost_players
lost_players <- subset(lost_players,yearID == 2001)
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLGA','BA','AB')]
head(lost_players)
#REPLACING THE LOST PLAYERS
#The total combined salaries of the selected candidates should not be more than 15 million
#Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
#Their mean OBP had to equal to or greater than the mean OBP of the lost players
#Q11 Available players in 2001
library(dplyr)
players_available<-filter(Game, yearID == 2001)
library(ggplot2)
library(plotly)
pl1<-ggplot(players_available,aes(x=OBP,y=salary)) + geom_point()
ggplotly(pl1)
#choosing the cutoff and removing the players with OBP ==0
players_available <- filter(players_available,salary<8000000,OBP>0)
#The total AB of the lost players
sum(lost_players$AB)
mean(lost_players$OBP)
#cutting of AB at round of 1500
players_available <- filter(players_available,AB >= 500)
#Sorting OBP to understand what is there
possiblity <- head(arrange(players_available,desc(OBP)),10)
possiblity
#Narrowing down the columns
possiblity <- possiblity[,c('playerID','OBP','AB','salary')]
possiblity
#can't choose 1 because he is already lost player but can choose 2,3 and 9 which will be $10 million of salary which also saves 5 million of our targeted salary, even their AB is also more and even their mean OBP is also more than the lost players.
chosen_players <- subset(possiblity,playerID %in% c("heltoto01","berkmla01","pujolal01") )
chosen_players
sum(chosen_players$AB)
mean(chosen_players$OBP)