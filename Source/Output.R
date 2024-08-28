Output = function(r, demo.locals, demo.tours, num_locals, num_tours, years, knowtrans, scenarios, tours_local.A, tours_tours.A, local_local.A, local_misd.A, pop, y, rr){
  knowsumm = c(
    length(pop$knowledge[pop$demo==1 & pop$type=="tourist"]),
    mean(pop$knowledge[pop$demo==1 & pop$type=="tourist"]),
    
    length(pop$knowledge[pop$demo==1 & pop$type=="local"]),
    mean(pop$knowledge[pop$demo==1 & pop$type=="local"]),
    
    length(pop$knowledge[pop$demo==2 & pop$type=="tourist"]),
    mean(pop$knowledge[pop$demo==2 & pop$type=="tourist"]),    
    
    length(pop$knowledge[pop$demo==2 & pop$type=="local"]),
    mean(pop$knowledge[pop$demo==2 & pop$type=="local"]),

    length(pop$knowledge[pop$demo==3 & pop$type=="tourist"]),
    mean(pop$knowledge[pop$demo==3 & pop$type=="tourist"]),
    
    length(pop$knowledge[pop$demo==3 & pop$type=="local"]),
    mean(pop$knowledge[pop$demo==3 & pop$type=="local"])
  )
  
  towrite = c(rr, y, 
              demo.locals, demo.tours, 
              num_locals, num_tours, years, knowtrans, 
              scenarios$tours_local[r], scenarios$tours_tours[r], scenarios$local_local[r], scenarios$local_misd[r],
              tours_local.A, tours_tours.A, local_local.A, local_misd.A, knowsumm)
  towrite = t(as.data.frame(towrite))
  colnames(towrite) = c("rep", "year",
                        "demo.locals1", "demo.locals2", "demo.locals3", "demo.tours1", "demo.tours2","demo.tours3",
                        "num_locals", "num_tours", "years", "knowtrans", 
                        "tours_local", "tours_tours", "local_local", "local_misd",
                        "tours_local.A1", "tours_local.A2", "tours_local.A3", "tours_tours.A", "local_local.A1", "local_local.A2", "local_local.A3", "local_misd.A",
                        "dem1toursN", "dem1toursK", "dem1localN", "dem1localK", 
                        "dem2toursN", "dem2toursK", "dem2localN", "dem2localK", 
                        "dem3toursN", "dem3toursK", "dem3localN", "dem3localK")  
  
  return(towrite)
}