Learn = function(pop, education){
  #iterate over demo groups
  for(i in 1:3){
    holdpop = pop[pop$type=="tourist" | pop$demo!=i,]
    subpop  = pop[pop$demo==i & pop$type=="local",]
    
    #increase knowledge based on education content
    knowmod = rnorm(nrow(subpop), education[i], 0.1)
    knowmod[knowmod<0] = 0
    subpop$knowledge = subpop$knowledge + knowmod
    pop = rbind(holdpop, subpop)
  }
  
  #check values are in bounds
  pop$knowledge[pop$knowledge>1] = 1
  
  return(pop)
}
