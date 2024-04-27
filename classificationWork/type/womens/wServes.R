library(tidyverse)
library(dplyr)
library(reshape2)


wplayerGamestyles<-wsts %>% select(Player, `Player Gamestyle`) %>% distinct() %>% 
  drop_na()%>% as.list()
wfullstatelistType<-statesIDs_type
wplayerMatrixdfServe<-data.frame(Player=character(),`Player Gamestyle`=character(),
                                Ace=character(),DF=character(),FSI=character(),SSI=character())

wplayerMatrixList<-list()
for(i in 1:80){
  wplayerMatrixList[[wplayerGamestyles$Player[i]]]<-wserveDist(wplayerGamestyles$Player[i])[,3]
  pVector<-t(wplayerMatrixList[[wplayerGamestyles$Player[i]]]) %>% as.vector()
  wplayerMatrixdfServe[i,]<-c(wplayerGamestyles$Player[i],wplayerGamestyles$`Player Gamestyle`[i], pVector)
}
wplayerMatrixdfServe
wsts1 <- wsts %>% select(c('Date','Player','Opponent','Point Index', 'Shot Index','Shot Outcome', 'Shot Number',
                         'Serve', 'Shot By', 'Player Gamestyle')) %>% group_by(Date,Player,Opponent,`Point Index`) %>%
  mutate(ovrID=cur_group_id()) %>% ungroup() %>% filter(`Shot Number`== 'Serve')

wserveDist<-function(player){ 
  
  serveResults <- wsts1 %>% filter(Player == player & `Shot By`==1 ) %>% 
    select(c('Serve','Shot Outcome', 'Serve')) %>% mutate(`Serve Result` = case_when(
      `Shot Outcome` == 'Ace'  ~ 'Ace', 
      `Shot Outcome` == 'Double Fault' & `Serve` == 'Second Serve Out'  ~ 'Double Fault',
      `Shot Outcome` == 'In' & `Serve` == 'Second Serve In'  ~ 'Second Serve In',
      `Shot Outcome` == 'In'& `Serve` == 'First Serve In'  ~ 'First Serve In',
    ) )%>% select(c('Serve Result')) %>% group_by(`Serve Result`)%>% tally() %>% drop_na() %>%
    mutate(freq = round(n / sum(n), 3))
  if(!("Double Fault" %in% serveResults$`Serve Result`)){
    serveResults<-serveResults%>%add_row(`Serve Result`= "Double Fault",n = 0, freq=0)
  }
  if(!("Ace" %in% serveResults$`Serve Result`)){
    serveResults<-serveResults%>%add_row(`Serve Result`= "Ace",n = 0, freq=0)
  }
  return(serveResults%>%arrange(`Serve Result`))
  
}
