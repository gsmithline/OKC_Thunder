library(dplyr)
library(scales)
library("purrr")
#input shot data 
#edit file path depending on data directory
shotData <- read.csv(file = '~/Desktop/OKC/OKC_PROJ/shots_data.csv', header = TRUE)
#results data frame 
results <- data.frame(matrix(ncol = 6, nrow = 2))
colnames(results) <- c('%_2PT', '%_C3', '%_NC3', '%_eFG_2PT', '%_eFG_C3', '%_eFG_NC3')
rownames(results) <- c('Team A', 'Team B')
#add column for shot zone based off of location on court 
shotData["shot_region"] <- NA
shotData["points"] <-NA
# Clean data
fill_region <- function(x, y) {
  if (x >= 22 && y <= 7.8 || x <= -22 && y <=7.8){
    shotData$shot_region <- "C3"
  } else if(y > 23.5){
    shotData$shot_region <-"NC3"
  } else {
    shotData$shot_region <- "2PT"
  }
}
#set points
fill_points <- function(region, make){
  if(make == 0){
    shotData$points <- 0
  }else if (make == 1 && region == "C3"  || region == "NC3"){
    shotData$points <- 3
  } else if (make == 1 && region == "2PT"){
    shotData$points <- 2
  } 
}

arg_list <- list(x = shotData$x,  y = shotData$y)
shotData <- shotData %>% mutate("shot_region" = unlist(purrr::pmap(arg_list, fill_region)))
arg_list <- list(region = shotData$shot_region,  make = shotData$fgmade)
shotData <- shotData %>% mutate("points" = unlist(purrr::pmap(arg_list, fill_points)))

#Analysis
#team A and B tables
teamA <- subset(shotData, team == "Team A")
teamB <- subset(shotData, team == "Team B")
#num shots for each team
shotsA <-nrow(teamA)
shotsB <-nrow(teamB)
#2pt shots 
twoA <- teamA %>% filter_all(any_vars(. %in% c('2PT')))
twoB <- teamB %>% filter_all(any_vars(. %in% c('2PT')))
results$`%_2PT`[1] <-round((nrow(twoA) / shotsA), digits = 4)
results$`%_2PT`[2] <-round((nrow(twoB) / shotsB), digits = 4)
#3pt non corner shots 
threePNCA <- teamA %>% filter_all(any_vars(. %in% c('NC3')))
threePNCB <- teamB %>% filter_all(any_vars(. %in% c('NC3')))
results$`%_NC3`[1] <-round((nrow(threePNCA) / shotsA), digits = 4)
results$`%_NC3`[2] <-round((nrow(threePNCB) / shotsB), digits = 4)
#3pt corner shots 
threePCA <- teamA %>% filter_all(any_vars(. %in% c('C3')))
threePCB <- teamB %>% filter_all(any_vars(. %in% c('C3')))
results$`%_C3`[1] <-round((nrow(threePCA) / shotsA), digits = 4)
results$`%_C3`[2] <-round((nrow(threePCB)/ shotsB), digits = 4)
#field goal percentages 2 pt regions
made2ptA <- subset(twoA, fgmade == 1)
made2ptB <- subset(twoB, fgmade == 1)
results$`%_eFG_2PT`[1] <- round((nrow(made2ptA) + .5*0) / nrow(twoA), digits = 4)
results$`%_eFG_2PT`[2] <- round((nrow(made2ptB) + .5*0) / nrow(twoB), digits = 4)
#field goal percentages 3 pt corner regions
made3CptA <- subset(threePCA , fgmade == 1)
made3CptB <- subset(threePCB , fgmade == 1)
results$`%_eFG_C3`[1] <- round((nrow(made3CptA) + .5*nrow(made3CptA)) / nrow(threePCA), digits = 4)
results$`%_eFG_C3`[2] <- round((nrow(made3CptB) + .5*nrow(made3CptB)) / nrow(threePCB), digits = 4)
#field goal percentage 3pt no corner
made3NCptA <- subset(threePNCA , fgmade == 1)
made3NCptB <- subset(threePNCA , fgmade == 1)
results$`%_eFG_NC3`[1] <- round((nrow(made3NCptA) + .5*nrow(made3NCptA)) / nrow(threePNCA), digits = 4)
results$`%_eFG_NC3`[2] <- round((nrow(made3NCptB) + .5*nrow(made3NCptB)) / nrow(threePNCB), digits = 4)
###Answer
print(results)



  



