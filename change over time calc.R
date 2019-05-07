#### Calculate average difference between first and last wave ####

sorted <- rac_ec_adj[with(rac_ec_adj, order(country, S020)), ]


clist <- sorted %>% dlply('country')

lapply(clist, function(x) {
  x[dim(x)[1], 2] - x[1, 2]
}) %>% 
  unlist() %>% 
  mean()
