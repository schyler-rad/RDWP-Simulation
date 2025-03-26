roll_for_conversions=function(conversions_in){

  conversions_out=apply(conversions_in,1,function(x) sample(c('r','s','u','nm'),1,prob=x))

  return(conversions_out)

}



