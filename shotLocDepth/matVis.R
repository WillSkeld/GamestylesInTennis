#heatmaps
library(expm)
library(ggplot2)
library(reshape2)
library(tidyverse)
clTPpop
clTPagg
clTPser
clTPacp
clTPctp
clTPsag
figure <- grid.arrange(agg, ser, acp, ctp, sag)
figure

#-----------------------------------Pop----------------------------------------------------------------------
melted_data <- melt(clTPpop) %>% filter((Var1 =='BI' |Var1 == 'FI' |Var1 == 'OI') &(Var2!='PE') )
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Pop Matrix - Full Shot Location") +
  theme(axis.text.x = element_text(angle = 90))

#-----------------------------------Agg----------------------------------------------------------------------
melted_data <- melt(clTPagg) %>% filter((Var1 =='BI' |Var1 == 'FI' |Var1 == 'OI') &(Var2!='PE') )
agg<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Aggressive Baseliner - Shot Depth Heat Map") +
  theme(axis.text.x = element_text(angle = 90))

#-----------------------------------Ser----------------------------------------------------------------------
melted_data <- melt(clTPser) %>% filter((Var1 =='BI' |Var1 == 'FI' |Var1 == 'OI') &(Var2!='PE') )
ser<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Big Server - Shot Depth Heat Map") +
  theme(axis.text.x = element_text(angle = 90))
#-----------------------------------acp----------------------------------------------------------------------
melted_data <- melt(clTPacp) %>% filter((Var1 =='BI' |Var1 == 'FI' |Var1 == 'OI') &(Var2!='PE') )
acp<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "All Court Player - Shot Depth Heat Map") +
  theme(axis.text.x = element_text(angle = 90))
#-----------------------------------ctp----------------------------------------------------------------------
melted_data <- melt(clTPctp) %>% filter((Var1 =='BI' |Var1 == 'FI' |Var1 == 'OI') &(Var2!='PE') )
ctp<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Counter Puncher - Shot Depth Heat Map") +
  theme(axis.text.x = element_text(angle = 90))
#-----------------------------------sag----------------------------------------------------------------------
melted_data <- melt(clTPsag) %>% filter((Var1 =='BI' |Var1 == 'FI' |Var1 == 'OI') &(Var2!='PE') )
sag<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Big Server/Aggressive Baseliner - Shot Depth Heat Map") +
  theme(axis.text.x = element_text(angle = 90))

#-----------------------------------sagser----------------------------------------------------------------------
melted_data <- melt(clTPsag-clTPser) %>% filter((Var1 =='Backcourt In' |Var1 == 'Frontcourt In' |Var1 == 'Out In') &(Var2!='Point End') )
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "white",
                       low = "blue",
                       midpoint = 0,) +
  labs(x = "Column", y = "Row", 
       title = "SAG-SER Matrix - shotloc") +
  theme(axis.text.x = element_text(angle = 90))


