Interact = function(group1, group2, intprob, knowtrans){
  #intprob = scenarios$tours_local[1]
  group1$interact = rbinom(nrow(group1), 1, intprob)
  
  for(g1 in 1:nrow(group1)){
    #check if interaction
    if(group1$interact[g1]==0){next}
    
    #find someone to interact with randomly
    inter = sample(1:nrow(group2), 1)
    
    #modify group1 person's knowledge
    mvmt = (group2$knowledge[inter] - group1$knowledge[g1]) * (knowtrans + rnorm(1, 0, 0.1))
    group1$knowledge[g1] = group1$knowledge[g1] + mvmt
    
    #modify group2 person's knowledge
    mvmt = (group1$knowledge[g1] - group2$knowledge[inter]) * (knowtrans + rnorm(1, 0, 0.1))
    group2$knowledge[inter] = group2$knowledge[inter] + mvmt
  }
  
  #remove temporary column
  group1$interact = NULL
  
  return(rbind(group1, group2))
} 