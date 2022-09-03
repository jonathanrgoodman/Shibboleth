#take average trait frequencies per generation over all runs

trait.avg <- function(data){
  
  traits <- unique(data$Var2)
  
  generations <- max(data$generation)
  
  trait.set <- rep(traits, generations)
  
  output <- data.frame(Var2=rep(traits, generations), Freq=rep(NA, generations*length(traits)), generation=rep(1:generations, each=length(traits)))
    
    for(t in 1:nrow(output)){
      
      g <- output[t,3]
    
      output$Freq[t] <- mean(subset(data[data$Var2==trait.set[t],], generation==g)$Freq)
    
    }
  
  output
  
}
