#heatmaps
library(expm)
library(ggplot2)
install.packages('ggpubr')
library(gridExtra)
library(reshape2)
TPpop
TPagg
TPser
TPacp
TPctp
TPsag
figure <- grid.arrange(agg, ser, acp, ctp, sag)
figure
#-----------------------------------Pop----------------------------------------------------------------------
melted_data <- melt(TPpop) %>% filter((Var1 =='AI' |Var1 == 'DI' | Var1 =='NI') &(Var2!='PE') )
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Pop Matrix - Situation") +
  theme(axis.text.x = element_text(angle = 45))

#-----------------------------------Agg----------------------------------------------------------------------
melted_data <- melt(TPagg) %>% filter((Var1 =='AI' |Var1 == 'DI' | Var1 =='NI') &(Var2!='PE') )
agg<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Aggressive Baseliner - Situation Heat Map") +
  theme(axis.text.x = element_text(angle = 45))
#-----------------------------------Ser----------------------------------------------------------------------
melted_data <- melt(TPser) %>% filter((Var1 =='AI' |Var1 == 'DI' | Var1 =='NI') &(Var2!='PE') )
ser<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Big Server - Situation Heat Map") +
  theme(axis.text.x = element_text(angle = 45))
#-----------------------------------acp----------------------------------------------------------------------
melted_data <- melt(TPacp) %>% filter((Var1 =='AI' |Var1 == 'DI' | Var1 =='NI') &(Var2!='PE') )
acp<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "All Court Player - Situation Heat Map") +
  theme(axis.text.x = element_text(angle = 45))
#-----------------------------------ctp----------------------------------------------------------------------
melted_data <- melt(TPctp) %>% filter((Var1 =='AI' |Var1 == 'DI' | Var1 =='NI') &(Var2!='PE') )
ctp<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Counter Puncher - Situation Heat Map") +
  theme(axis.text.x = element_text(angle = 45))
#-----------------------------------sag----------------------------------------------------------------------
melted_data <- melt(TPsag) %>% filter((Var1 =='AI' |Var1 == 'DI' | Var1 =='NI') &(Var2!='PE') )
sag<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Big Server/Aggressive Baseliner - Situation Heat Map") +
  theme(axis.text.x = element_text(angle = 45))
#-----------------------------------sagctp----------------------------------------------------------------------
melted_data <- melt(TPsag-TPctp) %>% filter((Var1 =='Attacking In' |Var1 == 'Defensive In' | Var1 =='Neutral In') &(Var2!='Point End') )
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "white",
                       low = "blue",
                       midpoint = 0,) +
  labs(x = "Column", y = "Row", 
       title = "Pop Matrix - Situation") +
  theme(axis.text.x = element_text(angle = 45))

