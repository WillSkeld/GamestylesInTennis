playerMatrixdfType<- playerMatrixdfType %>% mutate(`Player.Gamestyle` = 
                                                       case_when(`Player.Gamestyle` == 'Aggressive baseliner' | 
                                                                   `Player.Gamestyle` == 'Aggressive Baseliner' ~ 
                                                                   'Aggressive Baseliner',
                                                                 `Player.Gamestyle` == 'Big server / aggressive baseliner'|
                                                                   `Player.Gamestyle` == 'Big server / Aggressive baseliner' |
                                                                   `Player.Gamestyle` == 'Big Server / Aggressive Baseliner' ~
                                                                   'Big Server / Aggressive Baseliner',
                                                                 `Player.Gamestyle` == 'All court player' ~ 'All Court Player',
                                                                 `Player.Gamestyle` == 'Baseliner / Counter Puncher' ~ 'Counter Puncher',
                                                                 `Player.Gamestyle` == 'Big server' ~ ' Big Server',
                                                       ))

pca_set <- pmdfTS[,4:82]
pca_set<- sapply(pca_set, as.numeric)
pca_set <- pca_set[,-c(43:44)]
pcatype<-prcomp(pca_set)

eigs<-pcatype$sdev^2
eigs[1]/sum(eigs)
pcatypeplot<-plot(cumsum(eigs)/sum(eigs), xlab = "Number of Principal Components", 
                  ylab = "Cumulative Variance Explained", 
                  main = "Plot of Variance Explained by Principal Components, Shot Type Data")
dim(pca_set)
plots<-list()
for(i in c(1:10)){
  p<-ggplot() + aes(pca_set[,i]) + geom_histogram()
  append(plots,p)
}

plots
ggplot() + aes(pca_set[,2]) + geom_histogram()

?prcomp
summary(pcatype)
aa<-pcatype$rotation[,c(1:46)]
pca_set<-data.frame(player = pmdfTS$Player, gamestyle=pmdfTS$Player.Gamestyle,
                    pca = pcatype$x)


split<-initial_split(pca_set, prop = 0.9, gamestyle, pool = 0.1)
train<-training(split)
test<-testing(split)


typeserve.train <- train
typeserve.test <- test
dim(typeserve.test)
train_set <- typeserve.train[,2:79]
train_set[,2:78]<- scale(sapply(train_set[,2:78], as.numeric))
test_set <- typeserve.test[,2:79]
test_set[,2:78]<- scale(sapply(test_set[,2:78], as.numeric))
scaled_train_set <- train_set[colSums(!is.na(train_set)) > 0 & colSums(!is.na(test_set)) > 0]
scaled_test_set <- test_set[colSums(!is.na(train_set)) > 0 & colSums(!is.na(test_set)) > 0]

lda_train<-MASS::lda(gamestyle ~ ., data = scaled_train_set)
dim(scaled_train_set)

a<-cor(scaled_train_set[2:78])

p_tr<-predict(lda_train, scaled_train_set)
p_te<-predict(lda_train, scaled_test_set)
mean(p_tr$class == typeserve.train$`gamestyle`)
mean(p_te$class == typeserve.test$`gamestyle`)

lda_tr_data <- data.frame(type = p_tr$class, lda = p_tr$x, gamestyle=train$gamestyle)
lda_te_data <- data.frame(type = p_te$class, lda = p_te$x, gamestyle=test$gamestyle)

ggplot(lda_tr_data) + geom_point(aes(lda.LD1, lda.LD2, colour = type, shape = gamestyle), size = 2.5)
ggplot(lda_te_data) + geom_point(aes(lda.LD1, lda.LD2, colour = type, shape = gamestyle), size = 2.5)
