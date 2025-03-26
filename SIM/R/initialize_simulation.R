initialize_simulation=function(target_market_size_in,
                               average_memberships_in,
                               customer_rationality_in,
                               mean_daily_visitors_in){

  #target_market_size_in=20000
  #average_memberships_in=c(0.6,0.3,0.1,-0.4)#means for r, s, u, nm meberships
  #customer_rationality_in=1.2#Strength of correlation between memberships and lambdas
  init_market=simulate_target_market(target_market_size_in)
  init_program_use_intentions=simulate_program_use_intentions(target_market_size_in)
  init_credits=simulate_credit_initialization(target_market_size_in)
  init_memberships=simulate_memberships()
  init_conversion_probabilities=simulate_conversion_probabilities(target_market_size_in,
                                                                  average_memberships_in,
                                                                  customer_rationality_in,
                                                                  init_program_use_intentions)
  init_prices=simulate_prices()
  init_calendar=simulate_calendar()

  return(list(market_out=init_market,
              program_use_intentions_out=init_program_use_intentions,
              credits_out=init_credits,
              memberships_out=init_memberships,
              conversion_probabilities_out=init_conversion_probabilities,
              prices_out=init_prices,
              calendar_out=init_calendar,
              mean_daily_visitors_out=mean_daily_visitors_in))

}


#my_target_market_size=20000
#my_average_memberships=c(0.6,0.3,0.1,-0.4)#means for r, s, u, nm meberships
#my_customer_rationality=1.2#Strength of correlation between memberships and lambdas
#my_market=simulate_target_market(my_target_market_size)
#my_program_use_intentions=simulate_program_use_intentions(my_target_market_size)
#my_credits=simulate_credit_initialization(my_target_market_size)
#my_memberships=simulate_memberships()
#my_conversion_probabilities=simulate_conversion_probabilities(my_target_market_size,
#                                                              my_average_memberships,
#                                                              my_customer_rationality,
#                                                              my_program_use_intentions)
#my_prices=simulate_prices()
#my_calendar=simulate_calendar()

#my_mean_daily_visitors=7
