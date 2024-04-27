library(MASS)
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

lda_set <- playerMatrixdfDepth[,2:29]
lda_set[,2:28]<- scale(sapply(lda_set[,2:28], as.numeric))
ldadepth<-MASS::lda(Player.Gamestyle ~ ., data = lda_set)
p_depth <- predict(ldadepth,lda_set)
MASS::ldahist(data = p_depth$x[,2], g = lda_set$Player.Gamestyle)
