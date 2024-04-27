playerMatrixdfType<- playerMatrixdfType %>% mutate(`Player.Gamestyle` = 
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
dim(playerMatrixdfType)
lda_set <- playerMatrixdfType[,2:77]
lda_set[,2:76]<- scale(sapply(lda_set[,2:76], as.numeric))
lda_set <- lda_set[,-c(44:45)]
ldatype<-MASS::lda(Player.Gamestyle ~ ., data = lda_set)
?lda()
predict(ldatype,lda_set)
p <- predict(ldatype,lda_set)
MASS::ldahist(data = p$x[,1], g = lda_set$Player.Gamestyle)
MASS::ldahist(data = p$x[,2], g = lda_set$Player.Gamestyle)
?ldahist
par(mar=c(1,1,1,1))
ggord::ggord(ldatype, lda_set$Player.Gamestyle, ylim = c(-20, 20))

