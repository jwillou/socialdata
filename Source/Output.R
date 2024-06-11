Output = function(r, demo.locals, demo.tours, num_locals, num_tours, years, knowtrans, scenarios, pop, y, rr){
  knowsumm = NULL
  for(k in 1:3){
    t = pop[pop$demo==k,,drop=F]
    tt = t[t$type=="tourist",,drop=F]
    tl = t[t$type=="local",,drop=F]
    knowsumm = c(knowsumm, nrow(tt), mean(tt$knowledge, na.rm=T), nrow(tl), mean(tl$knowledge, na.rm=T))
  }
  
  towrite = c(rr, y, demo.locals, demo.tours, num_locals, num_tours, years, knowtrans, 
    scenarios$tours_local[r], scenarios$tours_tours[r], scenarios$local_local[r], scenarios$local_misd[r], knowsumm)
  towrite = t(as.data.frame(towrite))
  colnames(towrite) = c("rep", "year",
                        "demo.locals1", "demo.locals2", "demo.locals3", "demo.tours1", "demo.tours2","demo.tours3",
                        "num_locals", "num_tours", "years", "knowtrans", 
                        "tours_local", "tours_tours", "local_local", "local_misd", 
                        "dem1toursN", "dem1toursK", "dem1localN", "dem1localK", 
                        "dem2toursN", "dem2toursK", "dem2localN", "dem2localK", 
                        "dem3toursN", "dem3toursK", "dem3localN", "dem3localK")  
  
  return(towrite)
}