pmdfSiS<- pmdfSi %>% left_join(pmdfS)
dim(pmdfSiS)
lda_set <- pmdfSiS[,2:33]
lda_set[,2:32]<- scale(sapply(lda_set[,2:32], as.numeric))
ldass<-MASS::lda(Player.Gamestyle ~ ., data = lda_set)
?lda()
predict(ldats,lda_set)
p_ss <- predict(ldass,lda_set)
MASS::ldahist(data = p_ss$x[,1], g = lda_set$Player.Gamestyle)
MASS::ldahist(data = p_ss$x[,2], g = lda_set$Player.Gamestyle)
?ldahist
par(mar=c(1,1,1,1))
ggord::ggord(ldatype, lda_set$Player.Gamestyle, ylim = c(-20, 20))