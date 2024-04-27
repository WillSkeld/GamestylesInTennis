library(caret)
library(tidyverse)
library(plotly)
library(sparseLDA)
set.seed(42)
type_trn <- lda_set[tt_ids,]
type_test <- lda_set[-tt_ids,]

sparseLDAGridx  <-  expand.grid(.NumVars = c(5,10,15), .lambda = c(0.01,0.1,1))
sparseLDAGrid1  <-  expand.grid(.NumVars = c(5), .lambda = c(1))
sda_model1<-caret::train(Player.Gamestyle~ ., data = type_trn, 
                         method="sparseLDA", importance=TRUE, tuneGrid = sparseLDAGrid1,
                         preProc = c("center", "scale"), trControl= trainControl(method = "none"))
sda_model1$bestTune
predict(sda_model1, type_test)

cml1<-confusionMatrix(predict(sda_model1, type_test),as.factor(type_test$Player.Gamestyle))
write.csv(as.table(cml1),"matrixl1.csv")

sdadvs1<-sda_model1$finalModel$beta 
dim(lda_set)
dim(sdadvs1)

scaled_data_4<-scale(lda_set[,2:80], center = sda_model1$preProcess$mean, 
                     scale = sda_model1$preProcess$std)
projected_features_4<-as.data.frame(as.matrix(scaled_data_4[,sda_model1$finalModel$varIndex]) %*% 
                                      as.matrix(sdadvs1))
projected_features_4$class<-lda_set$Player.Gamestyle
projected_features_4$pred<-predict(sda_model1, lda_set)

ggplot(projected_features_4) + geom_point(aes(V1, V2, colour = pred, shape = class), size = 2.5)
ggplot(projected_features_4) + geom_point(aes(V1, V2, colour = pred, shape = class), size = 2.5)

plot_ly(projected_features_4, x = ~V1, y = ~V2, z = ~V3, color = ~pred) %>% 
  layout(scene =list(aspectmode='cube'))
sda_model1$finalModel$fit$svd^2/sum(sda_model1$finalModel$fit$svd^2)


rownames(sdadvs1)<-colnames(scaled_data_4[,sda_model1$finalModel$varIndex])
dv_df<-as.data.frame(sdadvs1)
dv_df$var<-rownames(sdadvs1)
ggplot(dv_df[,c(2,5)] %>% filter(V2 !=0), aes(x = var, y = V2,fill = V2)) + geom_bar(stat='identity') +
  coord_flip() +scale_fill_gradient2(low="blue", mid ="purple", high="red", midpoint=0)


sparseLDAGrid2  <-  expand.grid(.NumVars = c(5,10,15,20), .lambda = c(0.01,0.1,1,10,100))
sparseLDAGrid21  <-  expand.grid(.NumVars = c(10), .lambda = c(10))
sparseLDAGrid22  <-  expand.grid(.NumVars = c(20), .lambda = c(100))
sparseLDAGrid23  <-  expand.grid(.NumVars = c(20), .lambda = c(0.1))
folds <- createFolds(type_trn$Player.Gamestyle, k = 5, returnTrain = T, list = T)
sda_model2<-caret::train(Player.Gamestyle~ ., data = lda_set, method="sparseLDA", tuneGrid = sparseLDAGrid22,
                         trControl= trainControl(method = "CV", number=5,savePredictions='all'),
                                                 preProc = c("center", "scale") )


sda_model2_preds<-sda_model2$pred %>% filter(lambda ==sda_model2$bestTune$lambda &
                                                   NumVars ==sda_model2$bestTune$NumVars) %>% 
  arrange(rowIndex)
?confusionMatrix
confusionMatrix(predict(sda_model2,type_test),as.factor(type_test$Player.Gamestyle))
sdadvs2<-sda_model2$finalModel$beta
dim(lda_set)
dim(sdadvs1)

scaled_data_5<-scale(lda_set[,2:80], center = sda_model2$preProcess$mean, 
                     scale = sda_model2$preProcess$std)

projected_features_5<-as.data.frame(as.matrix(scaled_data_5[,sda_model2$finalModel$varIndex]) %*% 
                                      as.matrix(sdadvs2))
projected_features_5$class<-lda_set$Player.Gamestyle
projected_features_5$pred<-predict(sda_model2, lda_set)

ggplot(projected_features_5) + geom_point(aes(V1, V2, colour = pred, shape = class), size = 2.5)
plot_ly(projected_features_5, x = ~V1, y = ~V2, z = ~V3, color = ~pred) %>% 
  layout(scene =list(aspectmode='cube'))
plot_ly(projected_features_5, x = ~V1, y = ~V2, z = ~V3, color = ~class) %>% 
  layout(scene =list(aspectmode='cube'))


sda_model3<-caret::train(Player.Gamestyle~ ., data = lda_set, method="sparseLDA", tuneGrid = sparseLDAGrid23,
                         trControl= trainControl(method = "LOOCV",savePredictions='all' ),
                         preProc = c("center", "scale"))
predict(sda_model3,type_test)
confusionMatrix(as.factor(make.names(predict(sda_model3,type_test))),
                as.factor(make.names(type_test$Player.Gamestyle)),)

sda_model3_preds<-sda_model3$pred %>% filter(lambda ==sda_model3$bestTune$lambda &
                                                   NumVars ==sda_model3$bestTune$NumVars) %>% 
  arrange(rowIndex)

write.csv(as.table(confusionMatrix(sda_model3_preds[[1]],as.factor(lda_set$Player.Gamestyle))),"matrix.csv")
confusionMatrix(sda_model3_preds[[1]],as.factor(lda_set$Player.Gamestyle))


sdadvs3<-sda_model3$finalModel$beta
dim(lda_set[,sda_model3$finalModel$varIndex])
dim(sdadvs3)
scaled_data_6<-scale(lda_set[,2:80], center = sda_model3$preProcess$mean, 
                     scale = sda_model3$preProcess$std)
projected_features_6<-as.data.frame(as.matrix(scaled_data_6[,sda_model3$finalModel$varIndex]) %*% 
                                      as.matrix(sdadvs3))
projected_features_6$class<-lda_set$Player.Gamestyle
projected_features_6$pred<-predict(sda_model3, lda_set)

ggplot(projected_features_6) + geom_point(aes(V1, V2, colour = pred, shape = class), size = 2.5)
plot_ly(projected_features_6, x = ~V1, y = ~V2, z = ~V3, color = ~pred) %>% 
  layout(scene =list(aspectmode='cube'))
plot_ly(projected_features_6, x = ~V1, y = ~V2, z = ~V3, color = ~class) %>% 
  layout(scene =list(aspectmode='cube'))



#################################

sts %>% filter(`Player Gamestyle` == 'Big server / aggressive baseliner') %>% select(Player) %>% distinct()

sparseLDAGrid31  <-  expand.grid(.NumVars = c(10), .lambda = c(10))
sparseLDAGrid32  <-  expand.grid(.NumVars = c(20), .lambda = c(0.1))
sda_model4<-caret::train(Player.Gamestyle~ ., data = type_trn, 
                         method="sparseLDA", importance=TRUE, tuneGrid = sparseLDAGrid32,
                         preProc = c("center", "scale"), trControl= trainControl(method = "none"))
sda_model1$bestTune
predict(sda_model1, type_test)

cm7<-confusionMatrix(predict(sda_model4, type_test),as.factor(type_test$Player.Gamestyle))

cm1 %>% write.csv(as.table(cm7),"matrix.csv")

sdadvs7<-sda_model4$finalModel$beta 
dim(lda_set)
dim(sdadvs1)

scaled_data_7<-scale(lda_set[,2:80], center = sda_model4$preProcess$mean, 
                     scale = sda_model4$preProcess$std)
projected_features_7<-as.data.frame(as.matrix(scaled_data_7[,sda_model4$finalModel$varIndex]) %*% 
                                      as.matrix(sdadvs7))

rownames(sdadvs7)<-colnames(scaled_data_7[,sda_model4$finalModel$varIndex])
dv_df<-as.data.frame(sdadvs7)
dv_df$var<-rownames(sdadvs7)

ggplot(dv_df[,c(1,5)] %>% filter(V1 != 0), aes(x = var, y = V1,fill = V1)) + geom_bar(stat='identity') +
  coord_flip() +scale_fill_gradient(low="blue",  high="red")

ggplot(dv_df[,c(2,5)] %>% filter(V2 != 0), aes(x = var, y = V2, fill = V2)) + geom_bar(stat='identity') +
  coord_flip() +scale_fill_gradient(low="blue",  high="red")

projected_features_7$class<-lda_set$Player.Gamestyle
projected_features_7$pred<-predict(sda_model4, lda_set)

ggplot(projected_features_7) + geom_point(aes(V1, V2, colour = pred, shape = class), size = 2.5)
ggplot(projected_features_7) + geom_point(aes(V1, V2, colour = pred, shape = class), size = 2.5)

plot_ly(projected_features_7, x = ~V1, y = ~V2, z = ~V3, color = ~pred) %>% 
  layout(scene =list(aspectmode='cube'))
sda_model4$finalModel$fit$svd^2/sum(sda_model4$finalModel$fit$svd^2)











































