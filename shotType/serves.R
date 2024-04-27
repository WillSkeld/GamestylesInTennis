

popServes<- serveDist()
aggServes<- serveDist("Aggressive baseliner")
sagServes<- serveDist("Big server / Aggressive baseliner")
serServes<- serveDist("Big server")
ctpServes<- serveDist("Baseliner / Counter Puncher")
acpServes<- serveDist("All court player")
sts1 <- sts %>% select(c('Date','Player','Opponent','Point Index', 'Shot Index','Shot Outcome', 'Shot Number',
                           'Serve', 'Shot By', 'Player Gamestyle')) %>% group_by(Date,Player,Opponent,`Point Index`) %>%
    mutate(ovrID=cur_group_id()) %>% ungroup() %>% filter(`Shot Number`== 'Serve') %>% filter(`Player Gamestyle` %in% gamestyles) #Filter data and reduce columns
  
  serveDist<-function(gamestyles=c("Aggressive baseliner", "Baseliner / Counter Puncher", "Big server / Aggressive baseliner", 
                                       "Big server", "All court player")){ 
    
  
  serveResults <- sts1 %>% filter(`Player Gamestyle` %in% gamestyles & `Shot By`==1 ) %>% 
    select(c('Serve','Shot Outcome', 'Serve')) %>% mutate(`Serve Result` = case_when(
      `Shot Outcome` == 'Ace'  ~ 'Ace', 
      `Shot Outcome` == 'Double Fault' & `Serve` == 'Second Serve Out'  ~ 'Double Fault',
      `Shot Outcome` == 'In' & `Serve` == 'Second Serve In'  ~ 'Second Serve In',
      `Shot Outcome` == 'In'& `Serve` == 'First Serve In'  ~ 'First Serve In',
      ) )%>% select(c('Serve Result')) %>% group_by(`Serve Result`)%>% tally() %>% drop_na() %>%
    mutate(freq = round(n / sum(n), 3))
  return(serveResults)
  
  }
  