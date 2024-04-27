library(rsample)
library(tidyverse)
library(caret)
library(mmr)
library(ggplot2)
dim(lda_set)

#lda_set<-lda_set %>% filter(!((Player.Gamestyle=="All.Court.Player") | (Player.Gamestyle=="Big.Server...Aggressive.Baseliner")))
lda_set_r<-subset(lda_set, select = -c(LI_DSE, LI_OW, DSI_LW, DSI_VE, Player.Ranking))
lda_set_r$Player.Gamestyle<-as.factor(make.names((lda_set_r$Player.Gamestyle)))
tt_ids<-createDataPartition(lda_set_r$Player.Gamestyle, p=0.7, list = FALSE)
type_trn <- lda_set_r[tt_ids,]

type_test <- lda_set_r[-tt_ids,]

sparseLDAGridx  <-  expand.grid(.NumVars = c(5,15,25,35), .lambda = c(.1,1,10))

tt_5<-caret::train(Player.Gamestyle~ ., data = type_trn, method="sparseLDA", tuneGrid = sparseLDAGridx, trControl= trainControl(method = "LOOCV"), importance=TRUE,
                    preProc = c("center", "scale"))

sda5ClassesS

cm<-confusionMatrix(sda5Classes, type_test$Player.Gamestyle)

sdaTypeResults<-data.frame(true=lda_set[-tt_ids,"Player.Gamestyle"], pred=sda5Classes, 
                           rank = lda_set[-tt_ids,"Player.Ranking"])

sdaTypeResults$match<-as.integer(sdaTypeResults$true==sdaTypeResults$pred)
plot(sdaTypeResults$rank, sdaTypeResults$match)
tt_5$finalModel$beta
a<-tt_5$finalModel
varImp(tt_5)

plot(tt_5)
all<-predict(tt_5, lda_set_r)
confusionMatrix(all,lda_set_r$Player.Gamestyle)
#WORK OUT TOP FACTORS

#PLOT VECTORS
dim(lda_set_r[,a$varNames])
dim(a$beta)
projected_matrix <- mm(lda_set_r[,a$varNames], a$beta)
projected_matrix$gs<-lda_set$Player.Gamestyle
projected_matrix$pred<-all
MASS::ldahist(data = projected_matrix[,1], g = lda_set$Player.Gamestyle)
ggplot(projected_matrix) + geom_point(aes(V1, V2, colour = all), size = 2.5)
plot_ly(projected_matrix, x = ~V1, y = ~V2, z = ~V3, color = ~pred) %>% 
  layout(scene =list(aspectmode='cube'))
#WRITE IT UP
