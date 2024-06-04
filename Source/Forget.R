Forget = function(pop, forget){
  #determine what is forgotten for each individual
  knowmod = rnorm(nrow(pop), forget, 0.1)
  
  #check for negative values and correct these
  knowmod[knowmod<0] = 0
  
  #update population
  pop$knowledge = pop$knowledge - knowmod
  
  return(pop)
}