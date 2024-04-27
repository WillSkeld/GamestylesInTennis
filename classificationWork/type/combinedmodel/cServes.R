wlda_set %>% filter(Player.Gamestyle == 'Big Server / Aggressive Baseliner') %>% 
  summarise(Ace = mean(as.numeric(Ace)),
            `Double Fault` = mean(as.numeric(DF)),
            `First Serve In` = mean(as.numeric(FSI)),
            `Second Serve In` = mean(as.numeric(SSI)))

lda_set %>% filter(Player.Gamestyle == 'Aggressive Baseliner') %>% 
  summarise(Ace = mean(as.numeric(Ace)),
            `Double Fault` = mean(as.numeric(DF)),
            `First Serve In` = mean(as.numeric(FSI)),
            `Second Serve In` = mean(as.numeric(SSI)))
lda_set %>% filter(Player.Gamestyle == 'Big Server / Aggressive Baseliner') %>% 
  summarise(Ace = mean(as.numeric(Ace)),
            `Double Fault` = mean(as.numeric(DF)),
            `First Serve In` = mean(as.numeric(FSI)),
            `Second Serve In` = mean(as.numeric(SSI)))
