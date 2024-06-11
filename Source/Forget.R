Forget = function(pop, forget, forget.amt){
  #determine who forgets, and by how much
  knowmod = rbinom(nrow(pop), 1, forget) * rnorm(nrow(pop), forget.amt, 0.01)
  
  #check for negative values and correct these
  knowmod[knowmod<0] = 0
  
  #update population
  pop$knowledge = pop$knowledge - knowmod
  
  return(pop)
}