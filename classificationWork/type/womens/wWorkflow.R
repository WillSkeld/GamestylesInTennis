wsda_model1<-caret::train(Player.Gamestyle~ ., data = wlda_set,
method="sparseLDA", importance=TRUE, tuneGrid = sparseLDAGrid2,
preProc = c("center", "scale"), trControl= trainControl(method = "LOOCV",
savePredictions = "all", classProbs = TRUE))
wlda_set <- subset(wlda_set, select = -c(DSI_VE, DSI_VI))
wlda_set$Player.Gamestyle<-make.names(wlda_set$Player.Gamestyle)
wlda_set$Player.Gamestyle<-make.names(wlda_set$Player.Gamestyle)
wsda_model1<-caret::train(Player.Gamestyle~ ., data = wlda_set,
method="sparseLDA", importance=TRUE, tuneGrid = sparseLDAGrid2,
preProc = c("center", "scale"), trControl= trainControl(method = "LOOCV",
savePredictions = "all", classProbs = TRUE))
wsparseLDAGrid<-expand.grid(.NumVars = c(3,5,7,9), .lambda = c(0.01,0.1,1,10,100))
wsda_model1<-caret::train(Player.Gamestyle~ ., data = wlda_set,
method="sparseLDA", importance=TRUE, tuneGrid = wsparseLDAGrid,
preProc = c("center", "scale"), trControl= trainControl(method = "LOOCV",
savePredictions = "all", classProbs = TRUE))
wp_preds<-wsda_model1$pred %>% filter(NumVars == 5 & lambda == 0.01)
confusionMatrix(wsda_model1$pred[,1],wsda_model1$pred[,2])
confusionMatrix(wp_preds[,1],wp_preds[,2])
confusionMatrix(predict(sda_model2,type_test),as.factor(type_test$Player.Gamestyle))
