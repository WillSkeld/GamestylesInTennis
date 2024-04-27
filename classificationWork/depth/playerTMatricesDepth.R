library(tidyverse)
library(dplyr)
sts <- sbs %>% filter(Type=="men's singles")
sts$`Shot Number`[as.numeric(sts$`Shot Number`)>2] <- 'Rally'

player<-"Roger Federer"
RF<-playerStateMatrixDepth("Roger Federer")
playerGamestyles<-sts %>% select(Player, `Player Gamestyle`) %>% distinct() %>% 
  drop_na()%>% as.list()
fullstatelistDepth<-statesIDs_conLoc
playerMatrixdfDepth<-data.frame(Player=character(),`Player Gamestyle`=character(),
                               BI_BE=character(),BI_FE=character(),BI_OE=character(),
                               BI_BI=character(),BI_FI=character(),BI_OI=character(),
                               BI_BW=character(),BI_FW=character(),BI_OW=character(),
                               FI_BE=character(),FI_FE=character(),FI_OE=character(),
                               FI_BI=character(),FI_FI=character(),FI_OI=character(),
                               FI_BW=character(),FI_FW=character(),FI_OW=character(),
                               OI_BE=character(),OI_FE=character(),OI_OE=character(),
                               OI_BI=character(),OI_FI=character(),OI_OI=character(),
                               OI_BW=character(),OI_FW=character(),OI_NW=character())
for(i in 1:86){
  playerMatrixList[[playerGamestyles$Player[i]]]<-playerStateMatrixDepth(playerGamestyles$Player[i])
  pVector<-t(playerMatrixList[[playerGamestyles$Player[i]]][4:6,1:9]) %>% as.vector()
  pVector
  playerMatrixdfDepth[i,]<-c(playerGamestyles$Player[i],playerGamestyles$`Player Gamestyle`[i], pVector)
}
colnames(TP_conLoc)<-c('BE', 'FE', 'OE',
                       'BI', 'FI', 'OI',
                       'BW', '	FW',  'OW',
                       'PE')
warnings()

playerStateMatrixDepth<-function(player){
  
  sts1 <- sts %>% select(c('Date','Player','Opponent','Point Index', 'Shot Index','Shot Outcome', 'Shot Number', 
                           'Shot Type', 'Approach', 'Situation', 'Contact Location', 'Shot By','Shot Stroke', 'Player Gamestyle')) %>% group_by(Date,Player,Opponent,`Point Index`) %>%
    mutate(ovrID=cur_group_id()) %>% ungroup() %>% filter(!(`Shot Outcome` == 'Double Fault' & `Shot Number` == 'Return') &
                                                            !(is.na(`Shot Outcome`) & `Shot Number` == 'Serve') & 
                                                            !(`Shot Outcome` == 'Out') & 
                                                            !(is.na(`Shot Stroke`) & !(`Shot Number` == 'Serve')) &
                                                            !(`Shot Outcome` == 'Return WInner') & !(`Shot Outcome` == 'WInner') &
                                                            !(is.na(`Shot Number`)) & 
                                                            !(`Shot Outcome` == 'Winner' &`Shot Number` == 'Return') &
                                                            !(`Shot Number`== 'Serve' & !is.na(`Shot Stroke`))
    ) %>% filter(Player== player)#Filter data and reduce columns
  
  reducedSts<- sts1 %>% mutate(`Shot Outcome` = case_when(`Shot Outcome` == 'Return Error' | 
                                                            `Shot Outcome` == 'Unforced Error' |
                                                            `Shot Outcome` == 'Forced Error' ~ 'Error', 
                                                          `Shot Outcome` == 'Return Winner' |
                                                            `Shot Outcome` == 'Winner'  ~ 'Winner',
                                                          `Shot Outcome` == 'In'  ~ 'In'))
  
  reducedOutcomes_conLoc <- reducedSts %>% filter(`Shot Number`!= 'Serve' &!is.na(`Contact Location`)) %>% select(c('Shot Outcome', 'Contact Location')) %>% distinct()
  
  #-------------------------------------------SET UP----------------------------------------------------------
  
  statesIDs_conLoc <- fullstatelistDepth
  
  seqShotIDs_conLoc <- reducedSts %>%  filter(`Shot Number`!= 'Serve') %>% 
    select(c('Shot Index','Shot Outcome', 'Shot Number', 'Contact Location', 'ovrID', 'Shot By')) %>%
    left_join(statesIDs_conLoc, by=join_by(`Shot Outcome`, `Contact Location`)) %>%
    select(stateID, `Shot Index`, ovrID, `Shot By`) #group each shot to find sequential points using a point ID
  
  pairStates_conLoc <- seqShotIDs_conLoc %>% left_join(seqShotIDs_conLoc, by=join_by(ovrID, closest('Shot Index'<'Shot Index'))) %>% 
    filter(`Shot By.x` == 0) %>% select(c('stateID.x', 'stateID.y')) %>% filter(((stateID.x == 2 | stateID.x == 1 | stateID.x == 3 |
                                                                                    stateID.x == 7 | stateID.x == 8 | stateID.x == 9)
                                                                                 & is.na(stateID.y)) | 
                                                                                  ((stateID.x == 4 | stateID.x == 6 | stateID.x == 5) & !is.na(stateID.y)))
  #join to find sequential shots within the same point
  
  statePairTallies_conLoc <- pairStates_conLoc %>% group_by(stateID.x, stateID.y) %>% tally() %>% 
    group_by(stateID.x) %>% arrange(.by_group = TRUE) %>% replace_na(list(stateID.y = 10)) # state pair totals
  
  curStatePairTallies_conLoc <- pairStates_conLoc %>% 
    group_by(stateID.x) %>% tally() %>%
    arrange(.by_group = TRUE) #Individual state tallies
  
  #-------------------------------------------SET UP----------------------------------------------------------
  
  tpsLite1_conLoc <- function(){
    M <- matrix(nrow = nrow(statesIDs_conLoc)+1, ncol = nrow(statesIDs_conLoc)+1)
    for(i in 0:nrow(curStatePairTallies_conLoc)+1){
      for(j in 0:nrow(statesIDs_conLoc)+1){
        if(nrow(statePairTallies_conLoc %>% filter(stateID.x == i & stateID.y == j)) <1){
          M[i,j] <- 0
        }
        else{
          M[i,j] <- as.numeric((statePairTallies_conLoc %>% filter(stateID.x == i & stateID.y == j) %>%
                                  ungroup() %>% select(n)))/as.numeric((curStatePairTallies_conLoc %>% 
                                                                          filter(stateID.x == i) %>% select(n)))
        }
      }
    }
    return(M)
  } #create tranisition matrix
  
  TP_conLoc<- tpsLite1_conLoc()
  TP_conLoc[10,10] <-1
  rownames(TP_conLoc)<- c('BE', 'FE', 'OE',
                          'BI', 'FI', 'OI',
                          'BW', '	FW',  'OW',
                          'PE')
  colnames(TP_conLoc)<-c('BE', 'FE', 'OE',
                         'BI', 'FI', 'OI',
                         'BW', '	FW',  'OW',
                         'PE')
  return(TP_conLoc)
}
