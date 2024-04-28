library(caret)
library(tidyverse)
library(plotly)
library(sparseLDA)

#Example code to show workflow of SDA model, plots may differ based on train/test splits

#Normalise Player Gamestyle titles
pmdfS<- playerMatrixdfServe %>% mutate(`Player.Gamestyle` = 
                                         case_when(`Player.Gamestyle` == 'Aggressive baseliner' | 
                                                     `Player.Gamestyle` == 'Aggressive Baseliner' ~ 
                                                     'Aggressive Baseliner',
                                                   `Player.Gamestyle` == 'Big server / aggressive baseliner'|
                                                     `Player.Gamestyle` == 'Big server / Aggressive baseliner' |
                                                     `Player.Gamestyle` == 'Big Server / Aggressive Baseliner' ~
                                                     'Big Server / Aggressive Baseliner',
                                                   `Player.Gamestyle` == 'All court player' ~ 'All Court Player',
                                                   `Player.Gamestyle` == 'Baseliner / Counter Puncher' ~ 'Counter Puncher',
                                                   `Player.Gamestyle` == 'Big server' ~ 'Big Server',
                                         ))

pmdfT<- playerMatrixdfType %>% mutate(`Player.Gamestyle` = 
                                        case_when(`Player.Gamestyle` == 'Aggressive baseliner' | 
                                                    `Player.Gamestyle` == 'Aggressive Baseliner' ~ 
                                                    'Aggressive Baseliner',
                                                  `Player.Gamestyle` == 'Big server / aggressive baseliner'|
                                                    `Player.Gamestyle` == 'Big server / Aggressive baseliner' |
                                                    `Player.Gamestyle` == 'Big Server / Aggressive Baseliner' ~
                                                    'Big Server / Aggressive Baseliner',
                                                  `Player.Gamestyle` == 'All court player' ~ 'All Court Player',
                                                  `Player.Gamestyle` == 'Baseliner / Counter Puncher' ~ 'Counter Puncher',
                                                  `Player.Gamestyle` == 'Big server' ~ 'Big Server',
                                        ))
#Join Shot Type and Serving data
pmdfTS<- pmdfT %>% left_join(pmdfS)

#Format and split dataset
observation_set <- pmdfTS[,2:82]
observation_set[,2:81]<- sapply(observation_set[,2:81], as.numeric)
observation_set<-subset(observation_set, select = -c( Player.Ranking))
test_train_ids<-createDataPartition(observation_set$Player.Gamestyle, p=0.7, list = FALSE)
training_data <- observation_set[test_train_ids,]
testing_data <- observation_set[-test_train_ids,]

#set hyperparameters
hyperparameters  <-  expand.grid(.NumVars = c(20), .lambda = c(10))

#run model
example_model<-caret::train(Player.Gamestyle~ ., data = training_data, 
                         method="sparseLDA", importance=TRUE, tuneGrid = hyperparameters,
                         preProc = c("center", "scale"), trControl= trainControl(method = "none"))

#build confusion matrix
M_20_10_CM<-confusionMatrix(predict(example_model, testing_data),as.factor(testing_data$Player.Gamestyle))

#Save discriminant vectors
discriminant_vectors<-example_model$finalModel$beta 

#rescale data according to the model's training set
rescaled_data_set<-scale(observation_set[,2:80], center = example_model$preProcess$mean, 
                     scale = example_model$preProcess$std)

#project observation set onto the disrcriminant vectors
projected_features<-as.data.frame(as.matrix(rescaled_data_set[,example_model$finalModel$varIndex]) %*% 
                                      as.matrix(discriminant_vectors))

#format a DV data frame for visualisations
rownames(discriminant_vectors)<-colnames(rescaled_data_set[,example_model$finalModel$varIndex])
discriminant_vectors_df<-as.data.frame(discriminant_vectors)
discriminant_vectors_df$var<-rownames(discriminant_vectors)


#Discriminant Vector Visualisations
ggplot(discriminant_vectors_df[,c(1,5)] %>% filter(V1 != 0), aes(x = var, y = V1,fill = V1)) + geom_bar(stat='identity') +
  coord_flip() +scale_fill_gradient2(low="blue", mid="purple", high="red", midpoint = 0)

ggplot(discriminant_vectors_df[,c(2,5)] %>% filter(V2 != 0), aes(x = var, y = V2, fill = V2)) + geom_bar(stat='identity') +
  coord_flip() +scale_fill_gradient2(low="blue", mid="purple", high="red", midpoint = 0)

#Format Projected Features data frame for visualisations
projected_features$class<-observation_set$Player.Gamestyle
projected_features$pred<-predict(example_model, observation_set)

ggplot(projected_features) + geom_point(aes(V1, V2, colour = pred, shape = class), size = 2.5)

plot_ly(projected_features, x = ~V1, y = ~V2, z = ~V3, color = ~pred) %>% 
  layout(scene =list(aspectmode='cube'))

