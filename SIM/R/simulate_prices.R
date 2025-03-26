simulate_prices=function(){

  prices=data.frame(playground=seq(5,25,length.out=100),#an hour of playground time is between $1 and $25
                    tutoring=seq(5,50,length.out=100),
                    coworking=seq(5,25,length.out=100),
                    cafe=seq(1,10,length.out=100),#average cafe purchase is between $1 and $10
                    party=seq(100,500,length.out=100),
                    workshop=seq(5,50,length.out=100),
                    camp=seq(25,500,length.out=100),
                    merchandise=seq(1,50,length.out=100))

  prices_out=data.frame(prices=apply(prices,2,function(x) sample(x,1)))

  return(prices_out)

}
