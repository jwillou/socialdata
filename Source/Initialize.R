Initialize = function(num_locals, num_tours, demo_locals, demo_tours){
  locals = data.frame(
    id = 1:num_locals,
    demo = sample(1:3, num_locals, replace = TRUE, prob=demo.locals),
    knowledge = 0,
    type = "local"
  )
  locals$knowledge[locals$demo==1] = rnorm(length(locals$knowledge[locals$demo==1]), 0.7, 0.1)
  locals$knowledge[locals$demo==2 | locals$demo==3 ] = rnorm(length(locals$knowledge[locals$demo==3 | locals$demo==2 ]), 0.1, 0.1)
  
  tourists = data.frame(
    id = (1:num_tours) + num_locals,
    demo = sample(1:3, num_tours, replace = TRUE, prob=demo.locals),
    knowledge = rnorm(num_tours, 0.1, 0.1),
    type = "tourist"
  )
  
  # Combine locals and tourists into a single data frame
  agents = rbind(locals, tourists)
  
  #add ceiling and floor to knowledge values
  agents$knowledge[agents$knowledge<0] = 0
  agents$knowledge[agents$knowledge>1] = 1
  
  return(agents)
}