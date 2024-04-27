playerMatrixdfSitu<- playerMatrixdfSitu %>% mutate(`Player.Gamestyle` = 
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

pca_set <- playerMatrixdfSitu[,3:29]
pca_set<- scale(sapply(pca_set, as.numeric))
pcasitu<-prcomp(pca_set)
summary(pcasitu)

eigs<-pcasitu$sdev^2
eigs[1]/sum(eigs)
pcasituplot<-plot(cumsum(eigs)/sum(eigs), xlab = "Number of Principal Components", 
                  ylab = "Cumulative Variance Explained", 
                  main = "Plot of Variance Explained by Principal Components, Shot Situation Data")

