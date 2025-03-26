simulate_day=function(market_in,
                      mean_daily_visitors_in,
                      program_use_intentions_in,
                      conversion_probabilities_in,
                      credits_in,
                      prices_in,
                      memberships_in,
                      visit_prob_multiplier){

  my_guest_list=guest_list(market_in,
                           mean_daily_visitors_in,
                           program_use_intentions_in,
                           conversion_probabilities_in,
                           credits_in)

  my_guest_list$visitors[,5]=my_guest_list$visitors[,5]+1

  my_purchases=simulate_purchases(my_guest_list,prices_in)

  my_conversions=roll_for_conversions(my_guest_list$conversions[,2:5])

  my_receipts=simulate_receipts(my_conversions,
                                my_guest_list$credits,
                                memberships_in,
                                my_purchases,
                                my_guest_list$visitors)

  non_nm_index=my_receipts[[1]]$member_type!='nm'

  my_receipts[[1]][non_nm_index,4]=ifelse(my_receipts[[1]][non_nm_index,4]>=0.99,
                                          0.99,
                                          my_receipts[[1]][non_nm_index,4]+
                                            (0.99-my_receipts[[1]][non_nm_index,4])*
                                            (visit_prob_multiplier-1))

  return(list(guest_list=my_guest_list,
              purchases=my_purchases,
              conversions=my_conversions,
              receipts=my_receipts))

}




#my_guest_list=guest_list(my_market,
#                         my_mean_daily_visitors,
#                         my_program_use_intentions,
#                         my_conversion_probabilities,
#                         my_credits)

#my_purchases=simulate_purchases(my_guest_list,my_prices)

#my_conversions=roll_for_conversions(my_guest_list$conversions[,2:5])

#my_receipts=simulate_receipts(my_conversions,
#                              my_guest_list$credits,
#                              my_memberships,
#                              my_purchases,
#                              my_guest_list$visitors)

#my_update=update_data(my_credits,my_receipts,my_market)
#my_market=my_update[[1]]
#my_credits=my_update[[2]]
