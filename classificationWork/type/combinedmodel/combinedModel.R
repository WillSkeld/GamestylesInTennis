sparseLDAGrid32  <-  expand.grid(.NumVars = c(15), .lambda = c(0.01))
dim(mlda_set)
dim(lda_set)
dim(wlda_set)
mlda_set<- lda_set %>% filter(Player.Gamestyle != 'Big Server')

mtw_model1<-caret::train(Player.Gamestyle~ ., data = mlda_set, 
                         method="sparseLDA", importance=TRUE, tuneGrid = sparseLDAGrid2,
                         preProc = c("center", "scale"), trControl= trainControl(method = "LOOCV"))

mtw_model1<-caret::train(Player.Gamestyle~ ., data = mlda_set, 
                         method="sparseLDA", importance=TRUE, tuneGrid = sparseLDAGrid32,
                         preProc = c("center", "scale"), trControl= trainControl(method = "none"))
sda_model1$bestTune
predict(mtw_model1, wlda_set)
dim()
confusionMatrix(predict(mtw_model1, wlda_set),as.factor(wlda_set$Player.Gamestyle))

mtwdvs1<-mtw_model1$finalModel$beta 
dim(lda_set)
dim(sdadvs1)
dim(bind_rows(mlda_set,wlda_set))
scaled_data_8<-scale(bind_rows(mlda_set,wlda_set)[,2:80] , center = mtw_model1$preProcess$mean, 
                     scale = mtw_model1$preProcess$std)
projected_features_8<-as.data.frame(as.matrix(scaled_data_8[,mtw_model1$finalModel$varIndex]) %*% 
                                      as.matrix(mtwdvs1))
projected_features_8$class<-bind_rows(mlda_set,wlda_set)$Player.Gamestyle
projected_features_8$pred<-predict(mtw_model1, bind_rows(mlda_set,wlda_set))

ggplot(projected_features_8[c(79:158),]) + geom_point(aes(V1, V2, colour = pred, shape = class), size = 2.5)
ggplot(projected_features_8) + geom_point(aes(V1, V2, colour = pred, shape = class), size = 2.5)

plot_ly(projected_features_8[c(79:158),], x = ~V1, y = ~V2, z = ~V3, color = ~pred) %>% 
  layout(scene =list(aspectmode='cube'))
sda_model1$finalModel$fit$svd^2/sum(sda_model1$finalModel$fit$svd^2)