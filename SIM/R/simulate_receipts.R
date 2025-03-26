simulate_receipts=function(conversions_in,
                           guest_list_credits_in,
                           memberships_in,
                           purchases_in,
                           visitors_in){

  for(i in 1:length(conversions_in)){

    my_receipt=apply_credits(conversions_in[i],
                             guest_list_credits_in[i,],
                             memberships_in)

    guest_list_credits_in[i,]=my_receipt[[1]]

    #guest_list_credits_in[i,3:10]=as.numeric(guest_list_credits_in[i,3:10])
    #purchases_in[i,]=as.numeric(purchases_in[i,])

    guest_list_credits_in[i,3:10]=guest_list_credits_in[i,3:10]-purchases_in[i,]

    non_credited=sum(guest_list_credits_in[i,3:10][guest_list_credits_in[i,3:10]<0])

    guest_list_credits_in[i,3:10][guest_list_credits_in[i,3:10]<0]=0

    guest_list_credits_in[i,2]=guest_list_credits_in[i,2]+non_credited

    guest_list_credits_in[i,11]=guest_list_credits_in[i,11]+sum(purchases_in[i,])

    visitors_in[i,3]=my_receipt[[2]]

    if(sum(guest_list_credits_in[i,3:10])==0){

      visitors_in[i,3]='nm'

    }

  }

  return(list(visitors_in,guest_list_credits_in))

}


