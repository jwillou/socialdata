Interact = function(group1, group2, intprob, adjust, knowtrans, tourtolocoal){
  #intprob = scenarios$tours_local[r]
  #adjust = scenarios$tours_localAdj[r]
  group1$interact = rbinom(nrow(group1), 1, (intprob*adjust))
  reset=0
  
  for(g1 in 1:nrow(group1)){
    #check if interaction
    if(group1$interact[g1]==0){next}
    
    #find someone to interact with randomly
    if(is.null(group2)){
      inter  = sample(1:nrow(group1), 1)
      group2 = group1[inter,,drop=F]
      inter  = 1
      reset  = 1
    }else{
      inter = sample(1:nrow(group2), 1)
    }
    #modify group1 person's knowledge
    mvmt = (group2$knowledge[inter] - group1$knowledge[g1]) * (rnorm(1, knowtrans, 0.001))
    group1$knowledge[g1] = group1$knowledge[g1] + mvmt

    if(tourtolocoal==1){   
    #modify group2 person's knowledge
    mvmt = (group1$knowledge[g1] - group2$knowledge[inter]) * (rnorm(1, knowtrans, 0.001))
    group2$knowledge[inter] = group2$knowledge[inter] + mvmt
    } 
    
    if(reset==1){
      group2 = NULL
    }
  }
  
  #remove temporary column
  group1$interact = NULL
  
  return(rbind(group1, group2))
} 