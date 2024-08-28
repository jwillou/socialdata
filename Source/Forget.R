Forget = function(pop, forget, forget.amt){
  #determine who forgets, and by how much
  knowmod = rbinom(nrow(pop), size=1, prob=forget) * rnorm(nrow(pop), mean=forget.amt, sd=0.1) #
  
  #check for negative values and correct these
  knowmod[knowmod<0] = 0
  
  #update population
  pop$knowledge = pop$knowledge - knowmod
  pop$knowledge[pop$knowledge<0] = 0
  pop$knowledge[pop$knowledge>1] = 1
  
  return(pop)
}