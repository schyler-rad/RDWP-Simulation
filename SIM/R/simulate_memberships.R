simulate_memberships=function(conversion){

  memberships=list(r=data.frame(membership_cost=round(seq(5000,40000,length.out=100)),
                                playground_credits=round(seq(2000,2500,length.out=100)),
                                tutoring_credits=round(seq(0,24,length.out=100)),
                                cowowrking_credits=round(seq(2000,2500,length.out=100)),
                                cafe_credits=round(seq(100,1000,length.out=100)),
                                party_credits=round(seq(1,2,length.out=100)),
                                workshop_credits=round(seq(1,12,length.out=100)),
                                camp_credits=round(seq(1,8,length.out=100)),
                                merchandise_credits=round(seq(1,4,length.out=100))),

                   s=data.frame(membership_cost=round(seq(500,10000,length.out=100)),
                                playground_credits=round(seq(40,400,length.out=100)),
                                tutoring_credits=round(seq(0,12,length.out=100)),
                                cowowrking_credits=round(seq(0,40,length.out=100)),
                                cafe_credits=round(seq(0,100,length.out=100)),
                                party_credits=round(seq(0,1,length.out=100)),
                                workshop_credits=round(seq(0,1,length.out=100)),
                                camp_credits=round(seq(0,1,length.out=100)),
                                merchandise_credits=round(seq(0,1,length.out=100))),

                   #t=data.frame(),
                   u=data.frame(membership_cost=round(seq(50,2500,length.out=100)),
                                playground_credits=round(seq(8,40,length.out=100)),
                                tutoring_credits=round(seq(12,100,length.out=100)),
                                cowowrking_credits=round(seq(8,40,length.out=100)),
                                cafe_credits=round(seq(0,50,length.out=100)),
                                party_credits=round(seq(0,1,length.out=100)),
                                workshop_credits=round(seq(0,1,length.out=100)),
                                camp_credits=round(seq(0,4,length.out=100)),
                                merchandise_credits=round(seq(0,1,length.out=100))))

  memberships_out=lapply(memberships,function(x) apply(x,2,function(y) sample(y,1)))

  return(memberships_out)

}
