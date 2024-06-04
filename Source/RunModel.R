RunModel = function(r, directory, demo.locals, demo.tours, num_locals, num_tours, years, forget, knowtrans, education, scenarios){
  
    #initialize population of locals and tourists, assuming 3 local groups proportions set by run parameters
    pop = Initialize(num_locals, num_tours, demo_locals, demo_tours)
    
    #initialize output dataframe
    OUT = NULL
    
    for(y in 1:years){
      #record knowledge per group (per year)
      towrite = Output(r, demo.locals, demo.tours, num_locals, num_tours, years, knowtrans, scenarios, pop, y)
      if(y==1){
        write.table(towrite[0,], paste(directory, "/Output/yearlysummaryHeader.csv", sep=""), row.names=F, col.names=T, sep=",", append=F)
        write.table(towrite, paste(directory, "/Output/yearlysummary.csv", sep=""), row.names=F, col.names=F, sep=",", append=F)
      }else{
        write.table(towrite, paste(directory, "/Output/yearlysummary.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)
      }

      #interact tourist to locals, within demo groups
      for(i in 1:3){
        holdpop = pop[pop$demo!=i,]
        group1 = pop[pop$demo==i & pop$type=="tourist",,drop=F]
        group2 = pop[pop$demo==i & pop$type=="local",,drop=F]
        subpop = Interact(group1, group2, scenarios$tours_local[r], knowtrans)
        pop = rbind(holdpop, subpop)
      }
      
      #interact tourist to tourist, within and among demo groups
      holdpop = pop[pop$type=="local",]
      group1 = pop[pop$type=="tourist",,drop=F]
      group2 = NULL
      subpop = Interact(group1, group2, scenarios$tours_tours[r], knowtrans)
      pop = rbind(holdpop, subpop) 
      
      #interact local to local, within demo groups
      for(i in 1:3){
        holdpop = pop[pop$demo!=i | pop$type=="tourist",]
        group1 = pop[pop$demo==i & pop$type=="local",,drop=F]
        group2 = NULL
        subpop = Interact(group1, group2, scenarios$local_local[r], knowtrans)
        pop = rbind(holdpop, subpop)
      }
      
      #interact local to local, among demo groups
      for(i in 1:3){
        holdpop = pop[pop$type=="tourist",]
        group1 = pop[pop$demo==i & pop$type=="local",,drop=F]
        group2 = pop[pop$demo!=i & pop$type=="local",,drop=F]
        subpop = Interact(group1, group2, scenarios$local_misd[r], knowtrans)
        pop = rbind(holdpop, subpop)
      }
      
      #apply targeted education for locals
      for(i in 1:3){
        holdpop = pop[pop$type=="tourist" | pop$demo!=i,]
        subpop  = pop[pop$demo==i & pop$type=="local",]
        knowmod = rnorm(nrow(subpop), education[i], 0.1)
        knowmod[knowmod<0] = 0
        subpop$knowledge = subpop$knowledge + knowmod
        pop = rbind(holdpop, subpop)
      }
      pop$knowledge[pop$knowledge>1] = 1
      
      #apply stochastic knowledge loss to locals and tourists
      knowmod = rnorm(nrow(pop), forget, 0.1)
      knowmod[knowmod<0] = 0
      pop$knowledge = pop$knowledge - knowmod
   }
}




