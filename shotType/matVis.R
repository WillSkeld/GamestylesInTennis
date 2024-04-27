#heatmaps
library(expm)
library(ggplot2)
library(gridExtra)
library(reshape2)
TPpop_type
TPagg_type
TPser_type
TPacp_type
TPctp_type
TPsag_type

grid.arrange(agg,ser,acp,ctp,sag)

#-----------------------------------Pop----------------------------------------------------------------------
melted_data <- melt(TPpop_type) %>% filter((Var1 %in% c('DSI', 'GI',
                                            'OI', 'SI', 'VI')) &(Var2!='PE') )
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Pop Matrix - Situation") +
  theme(axis.text.x = element_text(angle = 90))

#-----------------------------------Agg----------------------------------------------------------------------
melted_data <- melt(TPagg_type) %>% filter((Var1 %in% c('DSI', 'GI',
                                                        'OI', 'SI', 'VI')) &(Var2!='PE') )
agg<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Aggressive Baseliner - Shot Type Heat Map") +
  theme(axis.text.x = element_text(angle = 90))
#-----------------------------------Ser----------------------------------------------------------------------
melted_data <- melt(TPser_type) %>% filter((Var1 %in% c('DSI', 'GI',
                                                        'OI', 'SI', 'VI')) &(Var2!='PE') )
ser<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Big Server - Shot Type Heat Map") +
  theme(axis.text.x = element_text(angle = 90))
#-----------------------------------acp----------------------------------------------------------------------
melted_data <- melt(TPacp_type) %>% filter((Var1 %in% c('DSI', 'GI',
                                                        'OI', 'SI', 'VI')) &(Var2!='PE'))
acp<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "All Court Player - Shot Type Heat Map") +
  theme(axis.text.x = element_text(angle = 90))
#-----------------------------------ctp----------------------------------------------------------------------
melted_data <- melt(TPctp_type) %>% filter((Var1 %in% c('DSI', 'GI',
                                                        'OI', 'SI', 'VI')) &(Var2!='PE') )
ctp<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Counter Puncher - Shot Type Heat Map") +
  theme(axis.text.x = element_text(angle = 90))
#-----------------------------------sag----------------------------------------------------------------------
melted_data <- melt(TPsag_type) %>% filter((Var1 %in% c('DSI', 'GI',
                                                        'OI', 'SI', 'VI')) &(Var2!='PE') )
sag<-ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Big Server/Aggressive Baseliner - Shot Type Heat Map") +
  theme(axis.text.x = element_text(angle = 90))
#-----------------------------------AggSer----------------------------------------------------------------------
melted_data <- melt(TPagg_type-TPser_type) %>% filter((Var1 %in% c('Drop Shot In', 'Groundstroke In',
                                                        'Lob In', 'Other In', 'Volley In')) &(Var2!='Point End') )
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "white",
                       low = "blue",
                       midpoint = 0,) +
  labs(x = "Column", y = "Row", 
       title = "Pop Matrix - Situation") +
  theme(axis.text.x = element_text(angle = 90))
