shibboleth.line <- function(data){
  
  ggplot(data=data, aes(x=generation,y=Freq, color=Var2))+
  geom_line()+
  scale_color_manual(values=unique(data$Var2))+labs(y="frequency")+theme_bw()+theme(legend.position="none")
  
}
