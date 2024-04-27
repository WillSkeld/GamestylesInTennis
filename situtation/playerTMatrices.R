library(tidyverse)
library(dplyr)
sts <- sbs %>% filter(Type=="men's singles")
sts$`Shot Number`[as.numeric(sts$`Shot Number`)>2] <- 'Rally'

player<-"Roger Federer"
RF<-playerStateMatrixSitu("Roger Federer")
playerGamestyles<-sts %>% select(Player, `Player Gamestyle`) %>% distinct() %>% 
  drop_na()%>% as.list()
fullstatelist<-statesIDs_sit
playerMatrixdf<-data.frame(Player=character(),`Player Gamestyle`=character(),
                           AI_AE=character(),AI_DE=character(),AI_NE=character(),
                           AI_AI=character(),AI_DI=character(),AI_NI=character(),
                           AI_AW=character(),AI_DW=character(),AI_NW=character(),
                           DI_AE=character(),DI_DE=character(),DI_NE=character(),
                           DI_AI=character(),DI_DI=character(),DI_NI=character(),
                           DI_AW=character(),DI_DW=character(),DI_NW=character(),
                           NI_AE=character(),NI_DE=character(),NI_NE=character(),
                           NI_AI=character(),NI_DI=character(),NI_NI=character(),
                           NI_AW=character(),NI_DW=character(),NI_NW=character())
for(i in 1:86){
  playerMatrixList[[playerGamestyles$Player[i]]]<-playerStateMatrixSitu(playerGamestyles$Player[i])
  pVector<-t(playerMatrixList[[playerGamestyles$Player[i]]][4:6,1:9]) %>% as.vector()
  pVector
  playerMatrixdf[i,]<-c(playerGamestyles$Player[i],playerGamestyles$`Player Gamestyle`[i], pVector)
}
transform(playerMatrixdf, AI_AE=as.numeric(AI_AE),AI_DE=as.numeric(AI_DE),AI_NE=as.numeric(AI_NE),
          AI_AI=as.numeric(AI_AE),AI_DI=as.numeric(AI_DE),AI_NI=as.numeric(AI_NE),
          AI_AW=as.numeric(AI_AE),AI_DW=as.numeric(AI_DE),AI_NW=as.numeric(AI_NE),
          DI_AE=as.numeric(AI_AE),DI_DE=as.numeric(AI_DE),DI_NE=as.numeric(AI_NE),
          DI_AI=as.numeric(AI_AE),DI_DI=as.numeric(AI_DE),DI_NI=as.numeric(AI_NE),
          DI_AW=as.numeric(AI_AE),DI_DW=as.numeric(AI_DE),DI_NW=as.numeric(AI_NE),
          NI_AE=as.numeric(AI_AE),NI_DE=as.numeric(AI_DE),NI_NE=as.numeric(AI_NE),
          NI_AI=as.numeric(AI_AE),NI_DI=as.numeric(AI_DE),NI_NI=as.numeric(AI_NE),
          NI_AW=as.numeric(AI_AE),NI_DW=as.numeric(AI_DE),NI_NW=as.numeric(AI_NE))
warnings()

playerStateMatrixSitu<-function(player){
  
  sts1 <- sts %>% select(c('Date','Player','Opponent','Point Index', 'Shot Index','Shot Outcome', 'Shot Number', 
                           'Shot Type', 'Approach', 'Situation', 'Shot Stroke', 'Shot By', 'Player Gamestyle')) %>% group_by(Date,Player,Opponent,`Point Index`) %>%
    mutate(ovrID=cur_group_id()) %>% ungroup() %>% filter(!(`Shot Outcome` == 'Double Fault' & `Shot Number` == 'Return') &
                                                            !(is.na(`Shot Outcome`) & `Shot Number` == 'Serve') & 
                                                            !(`Shot Outcome` == 'Out') & 
                                                            !(is.na(`Shot Stroke`) & !(`Shot Number` == 'Serve')) &
                                                            !(`Shot Outcome` == 'Return WInner') & !(`Shot Outcome` == 'WInner') &
                                                            !(is.na(`Shot Number`)) & 
                                                            !(`Shot Outcome` == 'Winner' &`Shot Number` == 'Return') &
                                                            !(`Shot Number`== 'Serve' & !is.na(`Shot Stroke`))
    ) %>% filter(Player == player) #Filter data and reduce columns
  
  reducedSts<- sts1 %>% mutate(`Shot Outcome` = case_when(`Shot Outcome` == 'Return Error' | 
                                                            `Shot Outcome` == 'Unforced Error' |
                                                            `Shot Outcome` == 'Forced Error' ~ 'Error', 
                                                          `Shot Outcome` == 'Return Winner' |
                                                            `Shot Outcome` == 'Winner'  ~ 'Winner',
                                                          `Shot Outcome` == 'In'  ~ 'In')) %>% filter(!(is.na(Situation)))
  
  reducedOutcomes_sit <- reducedSts %>% filter(`Shot Number`!= 'Serve') %>% select(c('Shot Outcome', 'Situation')) %>% distinct()
  
  #-------------------------------------------SET UP----------------------------------------------------------
  
  statesIDs_sit <- fullstatelist
  
  seqShotIDs_sit <- reducedSts %>%  filter(`Shot Number`!= 'Serve') %>% 
    select(c('Shot Index','Shot Outcome', 'Shot Number', 'Situation', 'ovrID', 'Shot By')) %>%
    left_join(statesIDs_sit, by=join_by(`Shot Outcome`, `Situation`)) %>%
    select(stateID, `Shot Index`, ovrID, `Shot By`) #group each shot to find sequential points using a point ID
  
  pairStates_sit <- seqShotIDs_sit %>% left_join(seqShotIDs_sit, by=join_by(ovrID, closest('Shot Index'<'Shot Index'))) %>% 
    filter(`Shot By.x` == 0) %>% select(c('stateID.x', 'stateID.y')) %>% 
    filter(((stateID.x == 2 | stateID.x == 1 | stateID.x == 3 | stateID.x == 9 | stateID.x == 8 | stateID.x == 7) 
            & is.na(stateID.y)) | ((stateID.x == 5 | stateID.x == 6 | stateID.x == 4) & !is.na(stateID.y)))
  #join to find sequential shots within the same point
  
  statePairTallies_sit <- pairStates_sit %>% group_by(stateID.x, stateID.y) %>% tally() %>% 
    group_by(stateID.x) %>% arrange(.by_group = TRUE) %>% replace_na(list(stateID.y = 10))# state pair totals
  
  
  curStatePairTallies_sit <- pairStates_sit %>% 
    group_by(stateID.x) %>% tally() %>%
    arrange(.by_group = TRUE)#Individual state tallies
  
  if(!(8 %in% curStatePairTallies_sit$stateID.x)){
    curStatePairTallies_sit <- curStatePairTallies_sit %>% add_row(stateID.x =8,n=0)%>%
      arrange(stateID.x)
  }
  
  #-------------------------------------------SET UP----------------------------------------------------------
  
  tpsLite1_sit <- function(){
    M <- matrix(nrow = nrow(statesIDs_sit)+1, ncol = nrow(statesIDs_sit)+1)
    for(i in 0:nrow(curStatePairTallies_sit)+1){
      for(j in 0:nrow(statesIDs_sit)+1){
        if(nrow(statePairTallies_sit %>% filter(stateID.x == i & stateID.y == j)) <1){
          M[i,j] <- 0
        }
        else{
          M[i,j] <- as.numeric((statePairTallies_sit %>% filter(stateID.x == i & stateID.y == j) %>%
                                  ungroup() %>% select(n)))/as.numeric((curStatePairTallies_sit %>% 
                                                                          filter(stateID.x == i) %>% select(n)))
        }
      }
    }
    return(M)
  } #create tranisition matrix
  
  
  TP_sit <- tpsLite1_sit()
  TP_sit[,10] <-c(1,1,1,0,0,0,1,1,1,1)
  rownames(TP_sit)<- c('Attacking Error', 'Defensive Error', 'Neutral Error',
                       'Attacking In', 'Defensive In', 'Neutral In',
                       'Attacking Winner', 'Defensive Winner', 'Neutral Winner', 'Point End')
  colnames(TP_sit)<- c('Attacking Error', 'Defensive Error', 'Neutral Error',
                       'Attacking In', 'Defensive In', 'Neutral In',
                       'Attacking Winner', 'Defensive Winner', 'Neutral Winner', 'Point End')
  return(TP_sit)
}
