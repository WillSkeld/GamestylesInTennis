mats<-list(clTPagg,clTPacp,clTPser,clTPctp,clTPsag)
M<-matrix(nrow = 5, ncol = 5)

for(i in 1:5){
  for(j in 1:5){
    M[i,j]<- norm(mats[[i]]-mats[[j]], 'F')
  }
}
colnames(M)<- c('AGG', 'ACP', 'SER', 'CTP', 'SAG')
rownames(M)<- c('AGG', 'ACP', 'SER', 'CTP', 'SAG')
melted_data <- melt(M)
sdm<-ggplot(melted_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "yellow",
                       low = "white",
                       midpoint = 0.25,) +
  labs(
       title = "Distance Heat Map - Shot Depth Model") +
  theme(axis.text.x = element_text(angle = 90))


