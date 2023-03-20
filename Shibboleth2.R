shibboleth.2 <- function(N=50, traits=10, generations=200, cost=10, B=.2){
  
  #determine and list which traits have been selected from blue-green spectrum using colorRamps package
  
  unique.traits <- blue2green(traits)
  
  unique.traits1 <- unique.traits[1:(length(unique.traits)/2)]
  
  unique.traits2 <- unique.traits[(length(unique.traits)/2+1):(length(unique.traits))]
  
  #determine boundaries of mimicry and tolerance values, and adjustment rate, using B
  
  adjustment <- round((traits-1)*B)
  
  mimicry.boundaries <- adjustment
    
  tolerance.boundaries <- adjustment
  
  #create first population of N individuals with matching sampled traits
  
  individual <- 1:N
  
  population1 <- data.frame(individual, trait=sample(unique.traits1, N, replace=TRUE), fitness=round(abs(rnorm(N,mean=50,sd=10))), mimicry=sample((0:mimicry.boundaries), N, replace=TRUE))
  
  population2 <- data.frame(individual, trait=sample(unique.traits2, N, replace=TRUE), fitness=round(abs(rnorm(N,mean=50,sd=10))), tolerance=sample((0:tolerance.boundaries), N, replace=TRUE))
  
  #create dataframe for output with columns for traits, frequencies of traits, and corresponding generations
  
  output.pop1 <- data.frame(Var2=rep(unique.traits, generations), Freq=rep(NA, length(unique.traits)*generations), generation=rep(1:generations, each=length(unique.traits)))
  
  output.pop2 <- data.frame(Var2=rep(unique.traits, generations), Freq=rep(NA, length(unique.traits)*generations), generation=rep(1:generations, each=length(unique.traits)))
  
  #determine the trait frequencies from the first populations
  
  Freq.pop1 <- as.data.frame(t(table(factor(population1$trait, levels=unique.traits))))
  
  Freq.pop2 <- as.data.frame(t(table(factor(population2$trait, levels=unique.traits))))
  
  #slot first population trait frequencies into outputs
  
  output.pop1[1:length(unique.traits), 2] <- Freq.pop1[1:length(unique.traits), 3]
  
  output.pop2[1:length(unique.traits), 2] <- Freq.pop2[1:length(unique.traits), 3]
  
  #determine reproduction over generations
  
  for (generations in 2:generations) {
    
    #determine whether individual mimicry range falls inside tolerance range, and distribute fitness accordingly
    
    for (i in 1:nrow(population1)){
      
      #subset trait ranges by mimicry and tolerance scores
      
      #bluebeards
      
      for(i in 1:nrow(population1)){
        
        if (((which(population1$trait[i]==unique.traits)-population1$mimicry[i]) > 0) & (which(population1$trait[i]==unique.traits)-1) > 0)
          
        {
          
          individual.range <- c(unique.traits[(which(population1$trait[i]==unique.traits)+1):(which(population1$trait[i]==unique.traits)+population1$mimicry[i])], 
                                unique.traits[(which(population1$trait[i]==unique.traits)-population1$mimicry[i]):(which(population1$trait[i]==unique.traits))])
          
        } else { 
          
          individual.range <- c(unique.traits[(which(population1$trait[i]==unique.traits)+1):(which(population1$trait[i]==unique.traits)+population1$mimicry[i])],
                                unique.traits[1:(which(population1$trait[i]==unique.traits))])
          
        }
        
        individual.range <- individual.range[!is.na(individual.range)]
        
        mimicry.range <- c(population1$trait[i], sample((subset(individual.range,!population1$trait[i]==individual.range)), population1$mimicry[i], replace=FALSE))
        
      }
      
      #greenbeards
      
      for(i in 1:nrow(population2)){
        
        if (((which(population2$trait[i]==unique.traits)-population2$tolerance[i]) > 0) & (which(population2$trait[i]==unique.traits)-1) > 0)
          
        {
          
          individual.range <- c(unique.traits[(which(population2$trait[i]==unique.traits)+1):(which(population2$trait[i]==unique.traits)+population2$tolerance[i])], 
                                unique.traits[(which(population2$trait[i]==unique.traits)-population2$tolerance[i]):(which(population2$trait[i]==unique.traits))])
          
        }
        
        else { 
          
          individual.range <- c(unique.traits[(which(population2$trait[i]==unique.traits)+1):(which(population2$trait[i]==unique.traits)+population2$tolerance[i])],
                                unique.traits[1:(which(population2$trait[i]==unique.traits))])
          
        }
        
        individual.range <- individual.range[!is.na(individual.range)]
        
        tolerance.range <- c(population2$trait[i], sample((subset(individual.range,!population2$trait[i]==individual.range)), population2$tolerance[i], replace=FALSE))
        
      }
      
      #distribute costs based on tolerance and mimicry ranges
      
      if (TRUE %in% (mimicry.range %in% tolerance.range)){
        
        population1$fitness[i] <- population1$fitness[i] + (length(unique.traits)-population2$tolerance[i])
        
        population2$fitness[i] <- population2$fitness[i] - (length(unique.traits)-population2$tolerance[i])
        
      }
      
      else{
        
        population1$fitness[i] <- population1$fitness[i] - (cost + population1$mimicry[i])
        
        population2$fitness[i] <- population2$fitness[i] + (cost + population1$mimicry[i])
        
      }
      
    }
    
    #ensure no fitness values are below 0 or above 100
    
    population1$fitness[population1$fitness>100] <- 100
    population1$fitness[population1$fitness<0] <- 0
    
    population2$fitness[population2$fitness>100] <- 100
    population2$fitness[population2$fitness<0] <- 0
    
    #amalgamate populations by fitness
    
    population.total <- rbind(data.frame(population1[1:(nrow(population1)),1:4]),data.frame(population2[1:(nrow(population2)),1:3],mimicry=sample(0:mimicry.boundaries, nrow(population2), replace=TRUE)))
    
    #reproduce population1 based on fitness
    
    population1 <- cbind(individual, population.total[sample(1:nrow(population.total), N, replace=TRUE, prob=population.total$fitness), 2:4])
    
    #adjust tolerance boundaries if mimic traits rise above 50% or fall below 25% of total population traits
    
    Freq1.pop1 <- as.data.frame(t(table(factor(population1$trait, levels=unique.traits))))
    
    if (sum(Freq1.pop1$Freq) >= N/2){
      
      tolerance.boundaries <- tolerance.boundaries - adjustment
      
      if(tolerance.boundaries <= 0){
        
        tolerance.boundaries <- 1
        
      }
      
    }
    
    if (sum(Freq1.pop1$Freq) < N/2){
      
      tolerance.boundaries <- tolerance.boundaries + adjustment
      
      if(tolerance.boundaries >= traits){
        
        tolerance.boundaries <- traits
        
      }
      
      if(tolerance.boundaries <= 0){
        
        tolerance.boundaries <- 1
        
      }
      
    }
    
    output.pop1[match(NA, output.pop1[, 2]):(match(NA, output.pop1[, 2])+length(unique.traits)-1), 2] <- Freq1.pop1[1:length(unique.traits), 3]
    
    #reproduce population2 based on fitness
    
    population2 <- cbind(individual, population.total[sample(1:nrow(population.total), N, replace=TRUE, prob=population.total$fitness), 2:3], tolerance=sample((0:tolerance.boundaries), N, replace=TRUE))
    
    Freq1.pop2 <- as.data.frame(t(table(factor(population2$trait, levels=unique.traits))))
    
    output.pop2[match(NA, output.pop2[, 2]):(match(NA, output.pop2[, 2])+length(unique.traits)-1), 2] <- Freq1.pop2[1:length(unique.traits), 3]
    
    #output for overall population
    
    output <- data.frame(Var2=output.pop1$Var2,Freq=output.pop1$Freq+output.pop2$Freq,generation=rep(1:max(output.pop1$generation),each=length(unique(output.pop1$Var2))))
  
    #determine whether and when equilibrium is reached
    
    ifelse(any(output$Freq==(N*2)), e <- output$generation[output$Freq==(N*2)][1], e <- NA)
    
    ifelse(any(output$Freq==(N*2)), e.var <- output$Var2[output$Freq==(N*2)][1], e.var <- NA)
    
    }
  
  list(output,subset(output,Freq>0 & generation==1), subset(output,Freq>0 & generation==generations), data.frame(e,e.var))
  
}
