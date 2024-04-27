playerMatrixdfDepth<- playerMatrixdfDepth %>% mutate(`Player.Gamestyle` = 
                                                       case_when(`Player.Gamestyle` == 'Aggressive baseliner' | 
                                                                   `Player.Gamestyle` == 'Aggressive Baseliner' ~ 
                                                                   'Aggressive Baseliner',
                                                                 `Player.Gamestyle` == 'Big server / aggressive baseliner'|
                                                                   `Player.Gamestyle` == 'Big server / Aggressive baseliner' |
                                                                   `Player.Gamestyle` == 'Big Server / Aggressive Baseliner' ~
                                                                   'Big Server / Aggressive Baseliner',
                                                                 `Player.Gamestyle` == 'All court player' ~ 'All Court Player',
                                                                 `Player.Gamestyle` == 'Baseliner / Counter Puncher' ~ 'Counter Puncher',
                                                                 `Player.Gamestyle` == 'Big server' ~ ' Big Server',
                                                       ))

pca_set <- playerMatrixdfDepth[,3:29]
pca_set<- scale(sapply(pca_set, as.numeric))
pcadepth<-prcomp(pca_set)
summary(pcadepth)
