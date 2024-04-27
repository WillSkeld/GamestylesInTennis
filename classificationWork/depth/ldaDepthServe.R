pmdfD<-playerMatrixdfDepth
pmdfDS<- pmdfD %>% left_join(pmdfS)
dim(pmdfDS)
lda_setD <- pmdfDS[,2:33]
lda_setD[,2:32]<- sapply(lda_setD[,2:32], as.numeric)
ldads<-MASS::lda(Player.Gamestyle ~ ., data = lda_set)
?lda()
predict(ldads,lda_set)
p_ds <- predict(ldads,lda_set)
MASS::ldahist(data = p_ds$x[,1], g = lda_set$Player.Gamestyle)
MASS::ldahist(data = p_ds$x[,2], g = lda_set$Player.Gamestyle)
?ldahist
par(mar=c(1,1,1,1))
ggord::ggord(ldatype, lda_set$Player.Gamestyle, ylim = c(-20, 20))