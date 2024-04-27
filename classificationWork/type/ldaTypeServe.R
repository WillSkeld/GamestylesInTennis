set.seed(42)
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



MASS:lda(Player.Gamestyle~ ., data = type_trn)


#Test Train Split, Run, Get Predictions, Plot data
pmdfTS<- pmdfT %>% left_join(pmdfS)
dim(pmdfTS)
dim(lda_set)
lda_set <- pmdfTS[,2:82]
lda_set[,2:81]<- sapply(lda_set[,2:81], as.numeric)
lda_set<-subset(lda_set, select = -c( Player.Ranking))
tt_ids<-createDataPartition(lda_set$Player.Gamestyle, p=0.7, list = FALSE)
type_trn <- lda_set[tt_ids,]
type_test <- lda_set[-tt_ids,]

lda_model1<-caret::train(Player.Gamestyle~ ., data = type_trn, 
                         method="lda", importance=TRUE)
p<-predict(lda_model1, type_test)

p

lda_model1$

cm1<-confusionMatrix(as.factor(predict(lda_model1, type_test)), 
                     as.factor(type_test$Player.Gamestyle))

cm1 %>% writexl::write_xlxs("matrix.xlsx")
cm1 %>% write.csv(as.table(cm1),"matrix.csv")

dvs1<-lda_model1$finalModel$scaling

rownames(dvs1)<-colnames(scaled_data_1)
dv_df<-as.data.frame(dvs1)
dv_df$var<-rownames(dvs1)

ggplot(dv_df[,c(1,5)], aes(x = var, y = LD1,fill = LD1)) + geom_bar(stat='identity') +
  coord_flip() +scale_fill_gradient(low="blue",  high="red")
length(dvs1)

lda_model1$finalModel$means
dim(lda_set)
dim(dvs1)
lda_model1$finalModel$svd^2/sum(lda_model1$finalModel$svd^2)

lda_model1$preProcess


dim(scaled_data_1)
projected_features_1<-as.data.frame(as.matrix(lda_set[,2:76]) %*% as.matrix(dvs1))
projected_features_1$class<-lda_set$Player.Gamestyle
projected_features_1$pred<-predict(lda_model1, lda_set)

confusionMatrix(as.factor(make.names(type_test$Player.Gamestyle)),
                as.factor(make.names(predict(lda_model1, type_test))))

ggplot(projected_features_1) + geom_point(aes(LD1, LD2, colour = pred, shape = class), size = 2.5)
plot_ly(projected_features_1, x = ~LD1, y = ~LD2, z = ~LD3, color = ~class) %>% layout(scene =list(aspectmode='cube'))



?ldahist
par(mar=c(1,1,1,1))
ggord::ggord(ldatype, lda_set$Player.Gamestyle, ylim = c(-20, 20))

