#define gaining credits
apply_credits=function(converted,credits_in,memberships_in){

  credits_out=data.frame(credits_in)

  if(converted=='nm'){

    new_member_type='nm'

  }else{
    if(converted=='r'){

      #credits_out$member_type='r'
      credits_out$spent=memberships_in$r[1]
      credits_out$playground_credits=memberships_in$r[2]
      credits_out$tutoring_credits=memberships_in$r[3]
      credits_out$coworking_credits=memberships_in$r[4]
      credits_out$cafe_credits=memberships_in$r[5]
      credits_out$party_credits=memberships_in$r[6]
      credits_out$workshop_credits=memberships_in$r[7]
      credits_out$camp_credits=memberships_in$r[8]
      credits_out$merchandise_credits=memberships_in$r[9]

    }else{
      if(converted=='s'){

        #credits_out$member_type='s'
        credits_out$spent=memberships_in$s[1]
        credits_out$playground_credits=memberships_in$s[2]
        credits_out$tutoring_credits=memberships_in$s[3]
        credits_out$coworking_credits=memberships_in$s[4]
        credits_out$cafe_credits=memberships_in$s[5]
        credits_out$party_credits=memberships_in$s[6]
        credits_out$workshop_credits=memberships_in$s[7]
        credits_out$camp_credits=memberships_in$s[8]
        credits_out$merchandise_credits=memberships_in$s[9]

      }else{
        if(converted=='t'){

          credits_out$member_type='q'
          #t type is under construction, needs market data, treated as q because we dont have what they want

        }else{
          if(converted=='u'){

            #credits_out$member_type='u'
            credits_out$spent=memberships_in$u[1]
            credits_out$playground_credits=memberships_in$u[2]
            credits_out$tutoring_credits=memberships_in$u[3]
            credits_out$coworking_credits=memberships_in$u[4]
            credits_out$cafe_credits=memberships_in$u[5]
            credits_out$party_credits=memberships_in$u[6]
            credits_out$workshop_credits=memberships_in$u[7]
            credits_out$camp_credits=memberships_in$u[8]
            credits_out$merchandise_credits=memberships_in$u[9]

          }}}}}

  #credits_out$visits=credits_out$visits+1
  credits_out$savings=0
  new_member_type=converted

  return(list(credits_out,new_member_type))

}
