update_data=function(credits_in,day_in,market_in){

  #day_in[[1]][] <- lapply(day_in[[1]], function(x) if(is.character(x)) as.numeric(x) else x)

  my_index=match(credits_in$id,day_in[[2]]$id)

  market_in[!is.na(my_index),]=day_in[[1]][my_index[!is.na(my_index)],]
  credits_in[!is.na(my_index),]=day_in[[2]][my_index[!is.na(my_index)],]



  #market_in=as.data.frame(lapply(market_in,function(x) if(is.character(x)) as.numeric(x) else x))
  #credits_in=as.data.frame(lapply(credits_in,function(x) if(is.character(x)) as.numeric(x) else x))


  #market_in[market_in$id%in%day_in[[1]]$id,]=day_in[[1]][]
  #credits_in[credits_in$id%in%day_in[[2]]$id,]=day_in[[2]]

  return(list(market_in,credits_in))

}


