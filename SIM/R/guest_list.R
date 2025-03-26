guest_list=function(market_in,
                    mean_daily_visitors_in,
                    program_use_intentions_in,
                    conversions_in,
                    credits_in){

  thinking_about_it=rbinom(nrow(market_in),1,market_in$visit_prob)

  my_daily_visitors=rpois(1,mean_daily_visitors_in)

  visitors_out=market_in[sample(market_in$id[thinking_about_it==1],my_daily_visitors),]

  program_use_intentions_out=program_use_intentions_in[program_use_intentions_in$id%in%visitors_out$id,]

  conversions_out=conversions_in[conversions_in$id%in%visitors_out$id,]

  credits_out=credits_in[credits_in$id%in%visitors_out$id,]

  return(list(visitors=visitors_out,
              program_use_intentions=program_use_intentions_out,
              conversions=conversions_out,
              credits=credits_out))

}
