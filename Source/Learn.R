Learn = function(pop, education){
  #iterate over demo groups
  for(i in 1:3){
    #holdpop = pop[pop$type=="tourist" | pop$demo!=i,]
    #subpop  = pop[pop$demo==i & pop$type=="local",]
    holdpop = pop[pop$demo!=i,]
    subpop  = pop[pop$demo==i,]
    
    #increase knowledge based on education content
    knowmod = rnorm(nrow(subpop), mean=education[i], sd=0.1) 
    subpop$knowledge = subpop$knowledge + knowmod
    pop = rbind(holdpop, subpop)
  }
  
  #check values are in bounds
  pop$knowledge[pop$knowledge<0] = 0
  pop$knowledge[pop$knowledge>1] = 1
  
  return(pop)
}
