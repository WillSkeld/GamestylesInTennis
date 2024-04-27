shotsplayed<-count(sts %>% group_by(Player) %>% 
                     filter(`Shot Number` == "Rally" & 
                              Player %in% playerGamestyles$Player & `Shot By` == 1))
avgrk<-sts %>% group_by(Player) %>% select(Player, Date, `Player Ranking`) %>% distinct() %>% summarise(
  rk = mean(`Player Ranking`)
)


#plot degree of confidence against points played

shotsPlayedData<-left_join(pmdfTS,shotsplayed, by = c("Player"))
rkData<-left_join(pmdfTS,avgrk, by = c("Player"))
sparseLDAGrid32  <-  expand.grid(.NumVars = c(20), .lambda = c(0.1))

lda_set_p<-lda_set
lda_set_p$Player.Gamestyle<-make.names(lda_set_p$Player.Gamestyle)
p_model<-caret::train(Player.Gamestyle~ ., data = lda_set_p, 
                         method="sparseLDA", importance=TRUE, tuneGrid = sparseLDAGrid32,
                         preProc = c("center", "scale"), trControl= trainControl(method = "LOOCV", savePredictions = "all", classProbs = TRUE))
pdvs<-p_model$finalModel$beta

scaled_data_p<-scale(lda_set_p[,2:80], center = p_model$preProcess$mean, 
                     scale = p_model$preProcess$std)
projected_features_p<-as.data.frame(as.matrix(scaled_data_p[,p_model$finalModel$varIndex]) %*% 
                                      as.matrix(pdvs))

p_preds<-p_model$pred
p_preds<-bind_cols(p_preds,projected_features_p)
p_preds$pts<-shotsPlayedData$n
p_preds$rk<-rkData$rk
colnames(p_preds)
for (i in 1:86){
  p_preds$maxodds[i]<-max(
    p_preds[i,c('Aggressive.Baseliner', 'All.Court.Player','Big.Server','Big.Server...Aggressive.Baseliner', 'Counter.Puncher')])
}

p_predsf<-p_preds %>% filter(rk<101)

lm1<-lm(p_preds$maxodds ~p_preds$pts)

title()
plot(p_predsf$pts,p_predsf$cor_c, main = "Scatter plot of Points Played vs Classification Correctness", xlab = "Points Played",
     ylab = "Classification Correctness - (Correct/Incorrect = 1/0 )")

plot(p_predsf$pts,p_predsf$maxodds, main = "Scatter plot of Points Played vs Classification Confidence", xlab = "Points Played",
     ylab = "Classification Confidence")


###
plot(p_predsf$rk,p_predsf$cor_c, main = "Scatter plot of Average Ranking vs Classification Correctness", 
     xlab = "Average Ranking",
     ylab = "Classification Correctness - (Correct/Incorrect = 1/0 )")

plot(p_predsf$rk,p_predsf$maxodds, main = "Scatter plot of Average Ranking vs Classification Confidence", 
     xlab = "Average Ranking",
     ylab = "Classification Confidence")


plot_ly(p_preds, x = ~V1, y = ~V2, color = ~class, 
        opacity = ~maxodds)







p100pts<-(p_predsf$pts)/100

summary(glm(p_predsf$cor_c ~ p100pts, family =binomial(link = "logit")))

summary(glm(p_predsf$cor_c ~ p_predsf$rk, family =binomial(link = "logit")))



1-exp(-0.023188)


plot(p_preds$maxodds,p_preds$cor_c) 

summary(lm1)


low_c<-p_preds%>%filter(maxodds<0.8) #CTP vsAGG

p_preds$cor_c<-as.numeric(p_preds$pred==p_preds$obs)

mis_c

confusionMatrix(p_preds$pred,p_preds$obs)



sda_model1$bestTune
predict(sda_model1, type_test)
