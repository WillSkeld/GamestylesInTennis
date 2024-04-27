mats<-list(TPagg_type,TPacp_type,TPser_type,TPctp_type,TPsag_type)
M<-matrix(nrow = 5, ncol = 5)

for(i in 1:5){
  for(j in 1:5){
    M[i,j]<- norm(mats[[i]]-mats[[j]], 'F')
  }
}
colnames(M)<- c('AGG', 'ACP', 'SER', 'CTP', 'SAG')
rownames(M)<- c('AGG', 'ACP', 'SER', 'CTP', 'SAG')
melted_data <- melt(M)
stm<-ggplot(melted_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "yellow",
                       low = "white",
                       midpoint = 0.25,) +
  labs(
       title = "Distance Heat Map - Shot Type Model") +
  theme(axis.text.x = element_text(angle = 90))
grid.arrange(sm,sdm,stm,nrow = 2, ncol = 2)
