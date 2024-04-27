library(expm)
library(ggplot2)
library(reshape2)
??melt
init<-matrix(c(0,0,0,0,0,1,0,0,0),nrow=1) 
redistMatrix<-matrix(nrow=11, ncol=9)

redistMatrix[1,]<-c(0,0,0,0,0,1,0,0,0)
for(i in 2:11){
  redistMatrix[i,]<-init%*%(TPagg%^%i)
}
melted_data <- melt(redistMatrix)
ggplot(melted_data, aes(x = Var2, y = reorder(Var1, -Var1), fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "yellow",
                       low = "white",
                       midpoint = 0.45,) +
  labs(x = "Column", y = "Row", 
       title = "Redist Matrix - Situation")
colnames(redistMatrix)<- c('Attacking Error', 'Defensive Error', 'Neutral Error',
                     'Attacking In', 'Defensive In', 'Neutral In',
                     'Attacking Winner', 'Defensive Winner', 'Neutral Winner')

#make zeroes 1s - so each fial state is asoring

Tpagg_lim <- init%*%(TPagg%^%100)
