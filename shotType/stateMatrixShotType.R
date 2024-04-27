library(tidyverse)
library(dplyr)
sts <- sbs %>% filter(Type=="men's singles")
sts$`Shot Number`[as.numeric(sts$`Shot Number`)>2] <- 'Rally'

TPpop_type<-stateMatrixType()
TPagg_type<-stateMatrixType("Aggressive baseliner")
TPctp_type<-stateMatrixType("Baseliner / Counter Puncher")
TPsag_type<-stateMatrixType("Big server / Aggressive baseliner")
TPser_type<-stateMatrixType("Big server")
TPacp_type<-stateMatrixType("All court player")

stateMatrixType<-function(gamestyles=c("Aggressive baseliner", "Baseliner / Counter Puncher", "Big server / Aggressive baseliner", 
                                       "Big server", "All court player")){
  
  sts1 <- sts %>% select(c('Date','Player','Opponent','Point Index', 'Shot Index','Shot Outcome', 'Shot Number', 
                           'Shot Type', 'Serve', 'Situation', 'Shot Stroke','Shot Type', 'Shot By', 'Player Gamestyle')) %>% group_by(Date,Player,Opponent,`Point Index`) %>%
    mutate(ovrID=cur_group_id()) %>% ungroup() %>% filter(!(`Shot Outcome` == 'Double Fault' & `Shot Number` == 'Return') &
                                                            !(is.na(`Shot Outcome`) & `Shot Number` == 'Serve') & 
                                                            !(`Shot Outcome` == 'Out') & 
                                                            !(is.na(`Shot Stroke`) & !(`Shot Number` == 'Serve')) &
                                                            !(`Shot Outcome` == 'Return WInner') & !(`Shot Outcome` == 'WInner') &
                                                            !(is.na(`Shot Number`)) & 
                                                            !(`Shot Outcome` == 'Winner' &`Shot Number` == 'Return') &
                                                            !(`Shot Number`== 'Serve' & !is.na(`Shot Stroke`))
    ) %>% filter(`Player Gamestyle` %in% gamestyles) #Filter data and reduce columns
  
  reducedSts<- sts1 %>% mutate(`Shot Outcome` = case_when(`Shot Outcome` == 'Return Error' | 
                                                            `Shot Outcome` == 'Unforced Error' |
                                                            `Shot Outcome` == 'Forced Error' ~ 'Error', 
                                                          `Shot Outcome` == 'Return Winner' |
                                                            `Shot Outcome` == 'Winner'  ~ 'Winner',
                                                          `Shot Outcome` == 'In'  ~ 'In'),
                               `Shot Type` = case_when(`Shot Type` == 'Drop Shot' |`Shot Type` == 'Drop' |`Shot Type` == 'Drop Volley' ~ 'Drop Shot', 
                                                       `Shot Type` == 'Volley' |`Shot Type` == 'Half Volley' | `Shot Type` == 'Drive Volley' ~ 'Volley',
                                                       #`Shot Type` == 'Lob' |`Shot Type` == 'Lob Volley'  ~ 'Lob',
                                                       `Shot Type` == 'Other' |`Shot Type` == 'Unclassified' | `Shot Type` == 'Pick-Up' | `Shot Type` == 'Block' | `Shot Type` == 'Smash'  ~ 'Other',
                                                       `Shot Type` == 'Groundstroke' ~ 'Groundstroke',
                                                       `Shot Type` == 'Slice' ~ 'Slice'  
                                                       )) %>% filter(!(is.na(Situation)) & !(is.na(`Shot Type`)))
  
  reducedOutcomes_type <- reducedSts %>% filter(`Shot Number`!= 'Serve') %>% select(c('Shot Outcome', 'Shot Type')) %>% distinct()
  
  
  #-------------------------------------------SET UP----------------------------------------------------------
  
  statesIDs_type <- reducedOutcomes_type %>% group_by(`Shot Outcome`, `Shot Type`) %>% arrange(.by_group = TRUE) %>% 
    mutate(stateID=cur_group_id()) #gives each state a no. instead of using full state desc.
  
  seqShotIDs_type <- reducedSts %>%  filter(`Shot Number`!= 'Serve') %>% 
    select(c('Shot Index','Shot Outcome', 'Shot Number', 'Shot Type', 'ovrID', 'Shot By')) %>%
    left_join(statesIDs_type, by=join_by(`Shot Outcome`, `Shot Type`)) %>%
    select(stateID, `Shot Index`, ovrID, `Shot By`) #group each shot to find sequential points using a point ID
  
  pairStates_type <- seqShotIDs_type %>% left_join(seqShotIDs_type, by=join_by(ovrID, closest('Shot Index'<'Shot Index'))) %>% 
    filter(`Shot By.x` == 0) %>% select(c('stateID.x', 'stateID.y')) %>% 
    filter(((stateID.x == 2 | stateID.x == 1 | stateID.x == 3 | stateID.x == 5 | stateID.x == 4 |
               stateID.x == 15 | stateID.x == 14 | stateID.x == 13 | stateID.x == 12 | stateID.x == 11) 
            & is.na(stateID.y)) | ((stateID.x == 6 | stateID.x == 7 |  stateID.x == 9 | stateID.x == 8 | stateID.x == 10) & !is.na(stateID.y)))
  #join to find sequential shots within the same point
  
  statePairTallies_type <- pairStates_type %>% group_by(stateID.x, stateID.y) %>% tally() %>% 
    group_by(stateID.x) %>% arrange(.by_group = TRUE) %>% replace_na(list(stateID.y = 16))# state pair totals
  
  
  curStatePairTallies_type <- pairStates_type %>% 
    group_by(stateID.x) %>% tally() %>%
    arrange(.by_group = TRUE)#Individual state tallies
  
  #-------------------------------------------SET UP----------------------------------------------------------
  
  tpsLite1_type <- function(){
    M <- matrix(nrow = nrow(statesIDs_type)+1, ncol = nrow(statesIDs_type)+1)
    for(i in 0:nrow(curStatePairTallies_type)+1){
      for(j in 0:nrow(statesIDs_type)+1){
        if(nrow(statePairTallies_type %>% filter(stateID.x == i & stateID.y == j)) <1){
          M[i,j] <- 0
        }
        else{
          M[i,j] <- as.numeric((statePairTallies_type %>% filter(stateID.x == i & stateID.y == j) %>%
                                  ungroup() %>% select(n)))/as.numeric((curStatePairTallies_type %>% 
                                                                          filter(stateID.x == i) %>% select(n)))
        }
      }
    }
    return(M)
  } #create tranisition matrix
  
  
  TP_type <- tpsLite1_type()
  TP_type[16,16] <-1
  TP_type[16,]<-c(rep(0,15),1)
  rownames(TP_type)<- c('DSE', 'GE',
                        'OE', 'SE','VE',
                        'DSI', 'GI',
                        'OI', 'SI', 'VI',
                        'DSW', 'GW',
                        'OW', 'SW','VW', 'PE')
  colnames(TP_type)<- c('DSE', 'GE',
                        'OE', 'SE','VE',
                        'DSI', 'GI',
                        'OI', 'SI', 'VI',
                        'DSW', 'GW',
                        'OW', 'SW','VW', 'PE')

  return(TP_type)
  
}

