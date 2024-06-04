RunModel = function(r, directory, demo.locals, demo.tours, num_locals, num_tours, years, knowtrans, scenarios){
  
    #initialize population of locals and tourists, assuming 3 local groups proportions set by run parameters
    pop = Initialize(num_locals, num_tours, demo_locals, demo_tours)
    
    #initialize output dataframe
    OUT = NULL
    
    for(y in 1:years){
      #record knowledge per group (per year)
      towrite = Output(r, demo.locals, demo.tours, num_locals, num_tours, years, knowtrans, scenarios[r], pop, y)
      
      #interact tourist to locals, within demo groups
      for(i in 1:3){
        holdpop = pop[pop$demo!=i,]
        group1 = pop[pop$demo==i & pop$type=="tourist",,drop=F]
        group2 = pop[pop$demo==i & pop$type=="local",,drop=F]
        subpop = Interact(group1, group2, scenarios$tours_local[r], knowtrans)
        pop = rbind(holdpop, subpop)
      }
      
      #interact tourist to tourist, within demo groups
      for(i in 1:3){
        holdpop = pop[pop$demo!=i,]
        group1 = pop[pop$demo==i & pop$type=="tourist",,drop=F]
        group2 = pop[pop$demo==i & pop$type=="tourist",,drop=F]
        subpop = Interact(group1, group2, scenarios$tours_tours[r], knowtrans)
        pop = rbind(holdpop, subpop)
      }
      
      #interact local to local, within demo groups
      for(i in 1:3){
        holdpop = pop[pop$demo!=i,]
        group1 = pop[pop$demo==i & pop$type=="local",,drop=F]
        group2 = pop[pop$demo==i & pop$type=="local",,drop=F]
        subpop = Interact(group1, group2, scenarios$local_local[r], knowtrans)
        pop = rbind(holdpop, subpop)
      }
      
      #interact local to local, among demo groups
      for(i in 1:3){
        holdpop = pop[pop$demo!=i,]
        group1 = pop[pop$demo==i & pop$type=="local",,drop=F]
        group2 = pop[pop$demo!=i & pop$type=="local",,drop=F]
        subpop = Interact(group1, group2, scenarios$local_misd[r], knowtrans)
        pop = rbind(holdpop, subpop)
      }
    
    #Output results for full run
    
  }
}



