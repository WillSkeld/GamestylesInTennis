library(caret)
library(tidyverse)
library(sparseLDA)

dim(lda_set)
lda_set$Player.Gamestyle<-as.factor(make.names((lda_set$Player.Gamestyle)))
lda_set<-lda_set %>% filter(!((Player.Gamestyle=="All.Court.Player") | (Player.Gamestyle=="Big.Server...Aggressive.Baseliner")))
cvpred<-array(dim = 86)


tempdf<-sda(lda_set[,2:76], lda_set[,1])

tempdf$beta
folds <- createFolds(lda_set$Player.Gamestyle, k = 10, returnTrain = T, list = T)
lda_model2<-caret::train(Player.Gamestyle~ ., data = lda_set, method="lda", 
                  trControl= trainControl(method = "CV", number=10,savePredictions='all'),
                                          preProc = c("center", "scale") )
lda_model2_preds<-lda_model2$pred %>% arrange(rowIndex)
confusionMatrix(as.factor(lda_model2_preds$pred),as.factor(lda_set$Player.Gamestyle))
dvs2<-lda_model2$finalModel$scaling
dim(lda_set)
dim(dvs2)

mean(lda_set$DSI_DSE)

scaled_data_2<-scale(lda_set[,2:76], center = lda_model2$preProcess$mean , scale = lda_model2$preProcess$std)

projected_features_2<-as.data.frame(as.matrix(scaled_data_2) %*% as.matrix(dvs2))
projected_features_2$class<-lda_set$Player.Gamestyle
projected_features_2$pred<- lda_model2_preds$pred

ggplot(projected_features_2) + geom_point(aes(LD1, LD2, colour = class), size = 2.5)
ggplot(projected_features_2) + geom_point(aes(LD1, LD2, colour = pred), size = 2.5)
plot_ly(projected_features_2, x = ~LD1, y = ~LD2, z = ~LD3, color = ~pred) %>% 
  layout(scene =list(aspectmode='cube'))
plot_ly(projected_features_2, x = ~LD1, y = ~LD2, z = ~LD3, color = ~class) %>% 
  layout(scene =list(aspectmode='cube'))


lda_model3<-caret::train(Player.Gamestyle~ ., data = lda_set, method="lda", 
                         trControl= trainControl(method = "LOOCV",savePredictions='all' ),
                         preProc = c("center", "scale"))
lda_model3_preds<-lda_model3$pred %>% arrange(rowIndex)
confusionMatrix(lda_model3_preds[[1]],as.factor(lda_set$Player.Gamestyle))
dvs3<-lda_model3$finalModel$scaling
dim(lda_set)
dim(dvs1)
scaled_data_3<-scale(lda_set[,2:76], center = lda_model3$preProcess$mean , scale = lda_model3$preProcess$std)
projected_features_3<-as.data.frame(as.matrix(scaled_data_3) %*% as.matrix(dvs3))
projected_features_3$class<-lda_set$Player.Gamestyle
projected_features_3$pred<-lda_model3_preds[[1]]

ggplot(projected_features_3) + geom_point(aes(LD1, LD2, colour = pred), size = 2.5)
plot_ly(projected_features_3, x = ~LD1, y = ~LD2, z = ~LD3, color = ~pred) %>% 
  layout(scene =list(aspectmode='cube'))
plot_ly(projected_features_3, x = ~LD1, y = ~LD2, z = ~LD3, color = ~class) %>% 
  layout(scene =list(aspectmode='cube'))









df2<-caret::train(Player.Gamestyle~ ., data = lda_set, method="lda", trControl= trainControl(method = "LOOCV"))
df2cm<-df2$pred
confusionMatrix(df2cm[[1]],lda_set$Player.Gamestyle)
df2cm<-df1$pred %>% filter(gamma == 0.5 & lambda == 1)


sparseLDAGridx  <-  expand.grid(.NumVars = 20, .lambda = 100)

t_df3<-caret::train(Player.Gamestyle~ ., data = lda_set, method="sparseLDA", tuneGrid = sparseLDAGridx, trControl= trainControl(method = "LOOCV"), importance=TRUE,
                  preProc = c("center", "scale"))
df3cm<-df3$pred %>% filter(NumVars == 38 & lambda == 0.1)
confusionMatrix(t_df3$pred[[1]],lda_set$Player.Gamestyle)
t_df3$finalModel
predictors(t_df3$finalModel$beta)
varImp(t_df3)

t_df10<-caret::train(Player.Gamestyle~ ., data = lda_set, method="sparseLDA", tuneGrid = sparseLDAGridx, 
                     trControl= trainControl(method = "CV", number=10,savePredictions='all' ), importance=TRUE,
                    preProc = c("center", "scale"))
confusionMatrix(t_df10$pred[[1]],lda_set$Player.Gamestyle)
predictors(t_df10$finalModel$beta)
varImp(t_df10)











df4<-caret::train(Player.Gamestyle~ ., data = lda_set, method="rda", trControl= trainControl(method = "CV", number=10, savePredictions = 'all' ))
confusionMatrix(df4cm[[1]],lda_set$Player.Gamestyle)
df4cm<-df4$pred %>% filter(gamma == 0.5 & lambda == 1)

df6<-caret::train(Player.Gamestyle~ ., data = lda_set, method="rda", trControl= trainControl(method = "LOOCV"))
confusionMatrix(df6cm[[1]],lda_set$Player.Gamestyle)
df6cm<-df6$pred %>% filter(gamma == 0.5 & lambda == 1)

df5<-caret::train(Player.Gamestyle~ ., data = lda_set, method="sparseLDA", trControl= trainControl(method = "repeatedcv",
                                                                                                    repeats = 5, number = 10,
                                                                                                    verbose = TRUE,
                                                                                                    classProbs = TRUE,savePredictions='all'), 
                  importance=TRUE,
                  preProc = c("center", "scale"))
df5cm<-df5$pred %>% filter(NumVars == 2 & lambda == 0)
confusionMatrix(df5cm[[1]],lda_set$Player.Gamestyle)
getModelInfo("sparseLDA")
