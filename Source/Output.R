Output = function(r, demo.locals, demo.tours, num_locals, num_tours, years, knowtrans, scenarios, pop, y){
  knowsumm = NULL
  for(k in 1:3){
    t = pop[pop$demo==k,,drop=F]
    tt = t[t$type=="tourist",,drop=F]
    tl = t[t$type=="local",,drop=F]
    knowsumm = c(knowsumm, nrow(tt), mean(tt$knowledge, na.rm=T), nrow(tl), mean(tl$knowledge, na.rm=T))
  }
  
  towrite = c(demo.locals, demo.tours, num_locals, num_tours, years, knowtrans, 
    scenarios$tours_local[r], scenarios$tours_tours[r], scenarios$local_local[r], scenarios$local_misd[r], knowsumm)
  return(towrite)
}