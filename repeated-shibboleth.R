shibboleth.repeat <- function(N=50, traits=10, generations=200, cost=10, B=.2, runs){
  
  data <- replicate(runs, shibboleth(N, traits, generations, cost, B))
  
  output <- data.frame(Var2=rep(NA, traits*generations*runs), Freq=rep(NA, traits*generations*runs), generation=rep(NA, traits*generations*runs), run=rep(1:runs, each=traits*generations))
  
  e.total <- data.frame(e=rep(NA, runs), e.var=rep(NA, runs), run=1:runs)
  
  for(i in 1:runs){
    
    output[(traits*generations*i-((traits*generations)-1)):(traits*generations*i),1:3] <- data.frame(data[,i][[1]])
    
    e.total$e[i] <- data[,i][[4]][1]
    
    e.total$e.var[i] <- data[,i][[4]][2]
    
  }
  
  e.total$e <- as.numeric(e.total$e)
  
  e.total$e.var <- as.character(e.total$e.var)
  
  e.total$e.var <- as.factor(e.total$e.var)
  
  list(output,e.total)
   
}
