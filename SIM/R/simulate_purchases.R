simulate_purchases=function(guest_list_in,prices_in){

  my_program_use=apply(guest_list_in[[2]][,2:9],2,function(x) rpois(length(x),x))

  my_purchases=sweep(as.matrix(my_program_use),2,unlist(prices_in),"*")

  names(my_purchases)=c('playground_receipt','tutoring_receipt','coworking_receipt',
                        'cafe_receipt','party_receipt','workshop_receipt',
                        'camp_receipt','merchandise_receipt')

  return(data.frame(my_purchases))

}
