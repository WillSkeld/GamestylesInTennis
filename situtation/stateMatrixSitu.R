library(tidyverse)
library(dplyr)



sts <- sbs
sts$`Shot Number`[as.numeric(sts$`Shot Number`)>2] <- 'Rally'

#stateMatrixSitu<-function(gamestyles=c("")){

sts1 <- sts %>% select(c('Date','Player','Opponent','Point Index', 'Shot Index','Shot Outcome', 'Shot Number', 
                         'Shot Type', 'Approach', 'Situation', 'Shot Stroke')) %>% group_by(Date,Player,Opponent,`Point Index`) %>%
  mutate(ovrID=cur_group_id()) %>% ungroup() %>% filter(!(`Shot Outcome` == 'Double Fault' & `Shot Number` == 'Return') &
                                                          !(is.na(`Shot Outcome`) & `Shot Number` == 'Serve') & 
                                                          !(`Shot Outcome` == 'Out') & 
                                                          !(is.na(`Shot Stroke`) & !(`Shot Number` == 'Serve')) &
                                                          !(`Shot Outcome` == 'Return WInner') & !(`Shot Outcome` == 'WInner') &
                                                          !(is.na(`Shot Number`)) & 
                                                          !(`Shot Outcome` == 'Winner' &`Shot Number` == 'Return') &
                                                          !(`Shot Number`== 'Serve' & !is.na(`Shot Stroke`))
  ) #Filter data and reduce columns

reducedSts<- sts1 %>% mutate(`Shot Outcome` = case_when(`Shot Outcome` == 'Return Error' | 
                                                          `Shot Outcome` == 'Unforced Error' |
                                                          `Shot Outcome` == 'Forced Error' ~ 'Error', 
                                                        `Shot Outcome` == 'Return Winner' |
                                                          `Shot Outcome` == 'Winner'  ~ 'Winner',
                                                        `Shot Outcome` == 'In'  ~ 'In')) %>% filter(!(is.na(Situation)))

reducedOutcomes_sit <- reducedSts %>% filter(`Shot Number`!= 'Serve') %>% select(c('Shot Outcome', 'Situation')) %>% distinct()

#-------------------------------------------SET UP----------------------------------------------------------

statesIDs_sit <- reducedOutcomes_sit %>% group_by(`Shot Outcome`, `Situation`) %>% arrange(.by_group = TRUE) %>% 
  mutate(stateID=cur_group_id()) #gives each state a no. instead of using full state desc.

seqShotIDs_sit <- reducedSts %>%  filter(`Shot Number`!= 'Serve') %>% 
  select(c('Shot Index','Shot Outcome', 'Shot Number', 'Situation', 'ovrID')) %>%
  left_join(statesIDs_sit, by=join_by(`Shot Outcome`, `Situation`)) %>%
  select(stateID, `Shot Index`, ovrID) #group each shot to find sequential points using a point ID

pairStates_sit <- seqShotIDs_sit %>% left_join(seqShotIDs_sit, by=join_by(ovrID, closest('Shot Index'<'Shot Index'))) %>% 
  select(c('stateID.x', 'stateID.y')) %>% filter(((stateID.x == 2 | stateID.x == 1 | stateID.x == 3 |
                                                    stateID.x == 9 | stateID.x == 8 | stateID.x == 7)
                                                    & is.na(stateID.y)) | 
                                                   ((stateID.x == 5 | stateID.x == 6 | stateID.x == 4) &
                                                   !is.na(stateID.y)))
#join to find sequential shots within the same point

statePairTallies_sit <- pairStates_sit %>% group_by(stateID.x, stateID.y) %>% tally() %>% 
  group_by(stateID.x) %>% arrange(.by_group = TRUE) %>% replace_na(list(stateID.y = 10))# state pair totals


curStatePairTallies_sit <- pairStates_sit %>% 
  group_by(stateID.x) %>% tally() %>%
  arrange(.by_group = TRUE) #Individual state tallies

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
TP_sit[10,10] <-1
rownames(TP_sit)<- c('Attacking Error', 'Defensive Error', 'Neutral Error',
                     'Attacking In', 'Defensive In', 'Neutral In',
                     'Attacking Winner', 'Defensive Winner', 'Neutral Winner', 'Point End')
colnames(TP_sit)<- c('Attacking Error', 'Defensive Error', 'Neutral Error',
                     'Attacking In', 'Defensive In', 'Neutral In',
                     'Attacking Winner', 'Defensive Winner', 'Neutral Winner', 'Point End')
return(TP_sit)

