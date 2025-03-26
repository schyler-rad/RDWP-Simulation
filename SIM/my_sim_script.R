require(devtools)
#require(regexcite)
require(MCMCpack)
require(moments)

#use_r('simulate_day')

load_all()

#simulation parameters
n_sim=200
n_days=365
target_market_size=round(rnorm(n_sim,20000,2000))
conversion_means=data.frame(r=sample(seq(.1,1,.1),n_sim,replace=T),
                            s=sample(seq(.1,1,.1),n_sim,replace=T),
                            u=sample(seq(.1,1,.1),n_sim,replace=T),
                            nm=sample(seq(-.5,-.1,.1),n_sim,replace=T))
rationality=sample(seq(.5,2,.1),n_sim,replace=T)
mean_daily_visitors=sample(seq(7,77,1),n_sim,replace=T)
visit_change=sample(seq(1.001,1.05,.001),n_sim,replace=T)

#results
sim_list=list()
markets_list=vector('list',n_sim)
program_use_intentions_list=vector('list',n_sim)
credits_list=vector('list',n_sim)
memberships_list=vector('list',n_sim)
conversion_probabilities_list=vector('list',n_sim)
prices_list=vector('list',n_sim)
calendars_list=vector('list',n_sim)
mean_daily_visitors_list=vector('list',n_sim)

for(sims in 1:n_sim){

  #initialize simulation
  sim_list[[sims]]=initialize_simulation(target_market_size[sims],as.numeric(conversion_means[sims,]),
                                         rationality[sims],mean_daily_visitors[sims])

  #simulate a day
  for(days in 1:n_days){

    my_day=simulate_day(sim_list[[sims]][[1]],sim_list[[sims]][[8]],sim_list[[sims]][[2]],sim_list[[sims]][[5]],
                        sim_list[[sims]][[3]],sim_list[[sims]][[6]],sim_list[[sims]][[4]],visit_change[sims])

    #end of day db update
    my_update=update_data(sim_list[[sims]][[3]],my_day$receipts,sim_list[[sims]][[1]])
    sim_list[[sims]][[1]]=my_update[[1]]
    sim_list[[sims]][[3]]=my_update[[2]]

    cat(sims,'.',days,'\n',sep='')

  }

  markets_list[[sims]]=sim_list[[sims]][[1]]
  program_use_intentions_list[[sims]]=sim_list[[sims]][[2]]
  credits_list[[sims]]=sim_list[[sims]][[3]]
  memberships_list[[sims]]=sim_list[[sims]][[4]]
  conversion_probabilities_list[[sims]]=sim_list[[sims]][[5]]
  prices_list[[sims]]=sim_list[[sims]][[6]]
  calendars_list[[sims]]=sim_list[[sims]][[7]]
  mean_daily_visitors_list[[sims]]=sim_list[[sims]][[8]]

}

#metadata
my_sims=1:n_sim
my_target_market_sizes=cbind(my_sims,target_market_size)
my_conversion_means=cbind(my_sims,conversion_means)
my_rationality=cbind(my_sims,rationality)
my_mean_daily_visitors=cbind(my_sims,mean_daily_visitors)
my_visit_change=cbind(my_sims,visit_change)
my_prices=cbind(my_sims,as.data.frame(do.call(rbind,lapply(prices_list,function(x) t(x)))))
rownames(my_prices)=NULL
my_memberships=cbind(my_sims,as.data.frame(do.call(rbind,lapply(memberships_list,function(x) unlist(x)))))

metadata_list=list(my_target_market_sizes,
                   my_conversion_means,
                   my_rationality,
                   my_visit_change,
                   my_mean_daily_visitors,
                   my_prices,
                   my_memberships)

my_metadata=Reduce(function(x,y) merge(x,y,by="my_sims"),metadata_list)

#data

my_data=do.call(rbind,lapply(my_sims,
                             function(x) cbind(markets_list[[x]][,2:5],
                                               program_use_intentions_list[[x]][,2:9],
                                               credits_list[[x]][,2:11],
                                               conversion_probabilities_list[[x]][,2:5],
                                               my_sims=my_sims[x])))



my_aggregation=function(y,stat){

  aggregate(cbind(visit_prob,visits,
                  playground_lambda,tutoring_lambda,coworking_lambda,cafe_lambda,
                  party_lambda,workshop_lambda,camp_lambda,merchandise_lambda,
                  spent,
                  playground_credits,tutoring_credits,coworking_credits,
                  cafe_credits,party_credits,workshop_credits,camp_credits,merchandise_credits,
                  savings,r,s,u,nm) ~ y,data=my_data,FUN=stat)

}

dat_by_sim_mean=my_aggregation(my_data$my_sims,mean)
dat_by_sim_sd=my_aggregation(my_data$my_sims,sd)
dat_by_sim_kurt=my_aggregation(my_data$my_sims,kurtosis)
dat_by_sim_skew=my_aggregation(my_data$my_sims,skew)
dat_by_vis_mean=my_aggregation(my_data$visits,mean)
dat_by_vis_sd=my_aggregation(my_data$visits,sd)
dat_by_vis_kurt=my_aggregation(my_data$visits,kurtosis)
dat_by_vis_skew=my_aggregation(my_data$visits,skew)
customer_types_tab=matrix(table(my_data$my_sims,my_data$customer_types),nrow=200,dimnames=list(c(),c('PWT','SAHP','WFHP')))
member_type_tab=matrix(table(my_data$my_sims,my_data$member_type),nrow=200,dimnames=list(c(),c('nm','r','s','u')))
visits_tab=matrix(table(my_data$my_sims,my_data$visits),nrow=200,dimnames=list(c(),c(0:max(my_data$visits))))

my_results_sims=data.frame(metadata=my_metadata,
                           dxsxbar=dat_by_sim_mean,dxssd=dat_by_sim_sd,
                           dxsk=dat_by_sim_kurt,dxssk=dat_by_sim_skew,
                           custyp=customer_types_tab,memtyp=member_type_tab,vis=visits_tab)

my_results2_visits=data.frame(dat_by_vis_mean,dat_by_vis_sd,dat_by_vis_kurt,dat_by_vis_skew)

write.csv(my_results2_visits,'visits.csv')

#kurt>3: heavy tails
#kurt<0: light tails
#kurt=3: normal
#skew>0: most lo but hi outliers
#skew<0: most hi but lo outliers
#skew=0: most lo but hi outliers

my_sims_cor=cor(my_results_sims)
#my_sims_cor[upper.tri(my_sims_cor)]=NA  # Hide upper triangle

write.csv(my_sims_cor,"my_cor.csv")

#write.csv(cor(my_results_sims),'my_cor.csv')

require(pwr)

visits_lm=lm(dxsxbar.visits~metadata.target_market_size+
               metadata.r+
               metadata.s+
               metadata.u+
               metadata.nm+
               metadata.rationality+
               metadata.visit_change+
               metadata.playground+
               metadata.tutoring+
               metadata.coworking+
               metadata.cafe+
               metadata.party+
               metadata.workshop+
               metadata.camp+
               metadata.merchandise+
               metadata.r.membership_cost+
               metadata.r.playground_credits+
               metadata.r.tutoring_credits+
               metadata.r.cowowrking_credits+
               metadata.r.cafe_credits+
               metadata.r.party_credits+
               metadata.r.workshop_credits+
               metadata.r.camp_credits+
               metadata.r.merchandise_credits+
               metadata.s.membership_cost+
               metadata.s.playground_credits+
               metadata.s.tutoring_credits+
               metadata.s.cowowrking_credits+
               metadata.s.cafe_credits+
               metadata.s.party_credits+
               metadata.s.workshop_credits+
               metadata.s.camp_credits+
               metadata.s.merchandise_credits+
               metadata.u.membership_cost+
               metadata.u.playground_credits+
               metadata.u.tutoring_credits+
               metadata.u.cowowrking_credits+
               metadata.u.cafe_credits+
               metadata.u.party_credits+
               metadata.u.workshop_credits+
               metadata.u.camp_credits+
               metadata.u.merchandise_credits+
               dxsxbar.visit_prob+
               dxsxbar.playground_lambda+
               dxsxbar.tutoring_lambda+
               dxsxbar.coworking_lambda+
               dxsxbar.cafe_lambda+
               dxsxbar.party_lambda+
               dxsxbar.workshop_lambda+
               dxsxbar.camp_lambda+
               dxsxbar.merchandise_lambda+
               dxsxbar.spent+
               dxsxbar.playground_credits+
               dxsxbar.tutoring_credits+
               dxsxbar.coworking_credits+
               dxsxbar.cafe_credits+
               dxsxbar.party_credits+
               dxsxbar.workshop_credits+
               dxsxbar.camp_credits+
               dxsxbar.merchandise_credits+
               dxsxbar.savings+
               #dxsxbar.r+
               #dxsxbar.s+
               #dxsxbar.u+
               #dxsxbar.nm+
               dxssd.visit_prob+
               dxssd.playground_lambda+
               dxssd.tutoring_lambda+
               dxssd.coworking_lambda+
               dxssd.cafe_lambda+
               dxssd.party_lambda+
               dxssd.workshop_lambda+
               dxssd.camp_lambda+
               dxssd.merchandise_lambda+
               dxssd.spent+
               dxssd.playground_credits+
               dxssd.tutoring_credits+
               dxssd.coworking_credits+
               dxssd.cafe_credits+
               dxssd.party_credits+
               dxssd.workshop_credits+
               dxssd.camp_credits+
               dxssd.merchandise_credits+
               dxssd.savings+
               dxssd.r+
               dxssd.s+
               dxssd.u+
               dxssd.nm+
               dxsk.visit_prob+
               dxsk.visits+
               dxsk.playground_lambda+
               dxsk.tutoring_lambda+
               dxsk.coworking_lambda+
               dxsk.cafe_lambda+
               dxsk.party_lambda+
               dxsk.workshop_lambda+
               dxsk.camp_lambda+
               dxsk.merchandise_lambda+
               dxsk.spent+
               dxsk.playground_credits+
               dxsk.tutoring_credits+
               dxsk.coworking_credits+
               dxsk.cafe_credits+
               dxsk.party_credits+
               dxsk.workshop_credits+
               dxsk.camp_credits+
               dxsk.merchandise_credits+
               dxsk.savings+
               dxsk.r+
               dxsk.s+
               dxsk.u+
               dxsk.nm+
               dxssk.visit_prob+
               dxssk.visits+
               dxssk.playground_lambda+
               dxssk.tutoring_lambda+
               dxssk.coworking_lambda+
               dxssk.cafe_lambda+
               dxssk.party_lambda+
               dxssk.workshop_lambda+
               dxssk.camp_lambda+
               dxssk.merchandise_lambda+
               dxssk.spent+
               dxssk.playground_credits+
               dxssk.tutoring_credits+
               dxssk.coworking_credits+
               dxssk.cafe_credits+
               dxssk.party_credits+
               dxssk.workshop_credits+
               dxssk.camp_credits+
               dxssk.merchandise_credits+
               dxssk.r+
               dxssk.s+
               dxssk.u+
               dxssk.nm,#+
             #custyp.PWT+
             #custyp.SAHP+
             #custyp.WFHP+
             #memtyp.nm+
             #memtyp.r+
             #memtyp.s+
             #memtyp.u,
             data=my_results_sims)

spent_lm=lm(dxsxbar.spent~metadata.target_market_size+
              metadata.r+
              metadata.s+
              metadata.u+
              metadata.nm+
              metadata.rationality+
              metadata.visit_change+
              metadata.mean_daily_visitors+
              metadata.playground+
              metadata.tutoring+
              metadata.coworking+
              metadata.cafe+
              metadata.party+
              metadata.workshop+
              metadata.camp+
              metadata.merchandise+
              metadata.r.membership_cost+
              metadata.r.playground_credits+
              metadata.r.tutoring_credits+
              metadata.r.cowowrking_credits+
              metadata.r.cafe_credits+
              metadata.r.party_credits+
              metadata.r.workshop_credits+
              metadata.r.camp_credits+
              metadata.r.merchandise_credits+
              metadata.s.membership_cost+
              metadata.s.playground_credits+
              metadata.s.tutoring_credits+
              metadata.s.cowowrking_credits+
              metadata.s.cafe_credits+
              metadata.s.party_credits+
              metadata.s.workshop_credits+
              metadata.s.camp_credits+
              metadata.s.merchandise_credits+
              metadata.u.membership_cost+
              metadata.u.playground_credits+
              metadata.u.tutoring_credits+
              metadata.u.cowowrking_credits+
              metadata.u.cafe_credits+
              metadata.u.party_credits+
              metadata.u.workshop_credits+
              metadata.u.camp_credits+
              metadata.u.merchandise_credits+
              dxsxbar.visit_prob+
              dxsxbar.visits+
              dxsxbar.playground_lambda+
              dxsxbar.tutoring_lambda+
              dxsxbar.coworking_lambda+
              dxsxbar.cafe_lambda+
              dxsxbar.party_lambda+
              dxsxbar.workshop_lambda+
              dxsxbar.camp_lambda+
              dxsxbar.merchandise_lambda+
              dxsxbar.playground_credits+
              dxsxbar.tutoring_credits+
              dxsxbar.coworking_credits+
              dxsxbar.cafe_credits+
              dxsxbar.party_credits+
              dxsxbar.workshop_credits+
              dxsxbar.camp_credits+
              dxsxbar.merchandise_credits+
              dxsxbar.savings+
              #dxsxbar.r+
              #dxsxbar.s+
              #dxsxbar.u+
              #dxsxbar.nm+
              dxssd.visit_prob+
              dxssd.visits+
              dxssd.playground_lambda+
              dxssd.tutoring_lambda+
              dxssd.coworking_lambda+
              dxssd.cafe_lambda+
              dxssd.party_lambda+
              dxssd.workshop_lambda+
              dxssd.camp_lambda+
              dxssd.merchandise_lambda+
              dxssd.spent+
              dxssd.playground_credits+
              dxssd.tutoring_credits+
              dxssd.coworking_credits+
              dxssd.cafe_credits+
              dxssd.party_credits+
              dxssd.workshop_credits+
              dxssd.camp_credits+
              dxssd.merchandise_credits+
              dxssd.savings+
              dxssd.r+
              dxssd.s+
              dxssd.u+
              dxssd.nm+
              dxsk.visit_prob+
              dxsk.visits+
              dxsk.playground_lambda+
              dxsk.tutoring_lambda+
              dxsk.coworking_lambda+
              dxsk.cafe_lambda+
              dxsk.party_lambda+
              dxsk.workshop_lambda+
              dxsk.camp_lambda+
              dxsk.merchandise_lambda+
              dxsk.spent+
              dxsk.playground_credits+
              dxsk.tutoring_credits+
              dxsk.coworking_credits+
              dxsk.cafe_credits+
              dxsk.party_credits+
              dxsk.workshop_credits+
              dxsk.camp_credits+
              dxsk.merchandise_credits+
              dxsk.savings+
              dxsk.r+
              dxsk.s+
              dxsk.u+
              dxsk.nm+
              dxssk.visit_prob+
              dxssk.visits+
              dxssk.playground_lambda+
              dxssk.tutoring_lambda+
              dxssk.coworking_lambda+
              dxssk.cafe_lambda+
              dxssk.party_lambda+
              dxssk.workshop_lambda+
              dxssk.camp_lambda+
              dxssk.merchandise_lambda+
              dxssk.spent+
              dxssk.playground_credits+
              dxssk.tutoring_credits+
              dxssk.coworking_credits+
              dxssk.cafe_credits+
              dxssk.party_credits+
              dxssk.workshop_credits+
              dxssk.camp_credits+
              dxssk.merchandise_credits+
              dxssk.savings+
              dxssk.r+
              dxssk.s+
              dxssk.u+
              dxssk.nm,#+
            #custyp.PWT+
            #custyp.SAHP+
            #custyp.WFHP+
            #memtyp.nm+
            #memtyp.r+
            #memtyp.s+
            #memtyp.u,
            data=my_results_sims)

savings_lm=lm(dxsxbar.savings~metadata.target_market_size+
                metadata.r+
                metadata.s+
                metadata.u+
                metadata.nm+
                metadata.rationality+
                metadata.visit_change+
                metadata.mean_daily_visitors+
                metadata.playground+
                metadata.tutoring+
                metadata.coworking+
                metadata.cafe+
                metadata.party+
                metadata.workshop+
                metadata.camp+
                metadata.merchandise+
                metadata.r.membership_cost+
                metadata.r.playground_credits+
                metadata.r.tutoring_credits+
                metadata.r.cowowrking_credits+
                metadata.r.cafe_credits+
                metadata.r.party_credits+
                metadata.r.workshop_credits+
                metadata.r.camp_credits+
                metadata.r.merchandise_credits+
                metadata.s.membership_cost+
                metadata.s.playground_credits+
                metadata.s.tutoring_credits+
                metadata.s.cowowrking_credits+
                metadata.s.cafe_credits+
                metadata.s.party_credits+
                metadata.s.workshop_credits+
                metadata.s.camp_credits+
                metadata.s.merchandise_credits+
                metadata.u.membership_cost+
                metadata.u.playground_credits+
                metadata.u.tutoring_credits+
                metadata.u.cowowrking_credits+
                metadata.u.cafe_credits+
                metadata.u.party_credits+
                metadata.u.workshop_credits+
                metadata.u.camp_credits+
                metadata.u.merchandise_credits+
                dxsxbar.visit_prob+
                dxsxbar.visits+
                dxsxbar.playground_lambda+
                dxsxbar.tutoring_lambda+
                dxsxbar.coworking_lambda+
                dxsxbar.cafe_lambda+
                dxsxbar.party_lambda+
                dxsxbar.workshop_lambda+
                dxsxbar.camp_lambda+
                dxsxbar.merchandise_lambda+
                dxsxbar.spent+
                dxsxbar.playground_credits+
                dxsxbar.tutoring_credits+
                dxsxbar.coworking_credits+
                dxsxbar.cafe_credits+
                dxsxbar.party_credits+
                dxsxbar.workshop_credits+
                dxsxbar.camp_credits+
                dxsxbar.merchandise_credits+
                #dxsxbar.r+
                #dxsxbar.s+
                #dxsxbar.u+
                #dxsxbar.nm+
                dxssd.visit_prob+
                dxssd.visits+
                dxssd.playground_lambda+
                dxssd.tutoring_lambda+
                dxssd.coworking_lambda+
                dxssd.cafe_lambda+
                dxssd.party_lambda+
                dxssd.workshop_lambda+
                dxssd.camp_lambda+
                dxssd.merchandise_lambda+
                dxssd.spent+
                dxssd.playground_credits+
                dxssd.tutoring_credits+
                dxssd.coworking_credits+
                dxssd.cafe_credits+
                dxssd.party_credits+
                dxssd.workshop_credits+
                dxssd.camp_credits+
                dxssd.merchandise_credits+
                dxssd.savings+
                dxssd.r+
                dxssd.s+
                dxssd.u+
                dxssd.nm+
                dxsk.visit_prob+
                dxsk.visits+
                dxsk.playground_lambda+
                dxsk.tutoring_lambda+
                dxsk.coworking_lambda+
                dxsk.cafe_lambda+
                dxsk.party_lambda+
                dxsk.workshop_lambda+
                dxsk.camp_lambda+
                dxsk.merchandise_lambda+
                dxsk.spent+
                dxsk.playground_credits+
                dxsk.tutoring_credits+
                dxsk.coworking_credits+
                dxsk.cafe_credits+
                dxsk.party_credits+
                dxsk.workshop_credits+
                dxsk.camp_credits+
                dxsk.merchandise_credits+
                dxsk.savings+
                dxsk.r+
                dxsk.s+
                dxsk.u+
                dxsk.nm+
                dxssk.visit_prob+
                dxssk.visits+
                dxssk.playground_lambda+
                dxssk.tutoring_lambda+
                dxssk.coworking_lambda+
                dxssk.cafe_lambda+
                dxssk.party_lambda+
                dxssk.workshop_lambda+
                dxssk.camp_lambda+
                dxssk.merchandise_lambda+
                dxssk.spent+
                dxssk.playground_credits+
                dxssk.tutoring_credits+
                dxssk.coworking_credits+
                dxssk.cafe_credits+
                dxssk.party_credits+
                dxssk.workshop_credits+
                dxssk.camp_credits+
                dxssk.merchandise_credits+
                dxssk.savings+
                dxssk.r+
                dxssk.s+
                dxssk.u+
                dxssk.nm,#+
              #custyp.PWT+
              #custyp.SAHP+
              #custyp.WFHP+
              #memtyp.nm+
              #memtyp.r+
              #memtyp.s+
              #memtyp.u,
              data=my_results_sims)

pwr.f2.test(u=131,v=68,f2=.9948,sig.level=.05)
pwr.f2.test(u=134,v=65,f2=.9921,sig.level=.05)
pwr.f2.test(u=134,v=65,f2=.9978,sig.level=.05)

sink('output.csv')
summary(visits_lm)
summary(spent_lm)
summary(savings_lm)
sink()

write.csv(my_results_sims,'results.csv')

table(my_results_sims$dxsxbar.savings,my_results_sims$metadata.r)




matrix(c(
mean(my_results_sims$metadata.r[my_results_sims$dxsxbar.savings<40]),
mean(my_results_sims$metadata.r[my_results_sims$dxsxbar.savings>=40&my_results_sims$dxsxbar.savings<80]),
mean(my_results_sims$metadata.r[my_results_sims$dxsxbar.savings>=80&my_results_sims$dxsxbar.savings<120]),
mean(my_results_sims$metadata.r[my_results_sims$dxsxbar.savings>=120&my_results_sims$dxsxbar.savings<160]),
mean(my_results_sims$metadata.r[my_results_sims$dxsxbar.savings>=160&my_results_sims$dxsxbar.savings<200]),

mean(my_results_sims$metadata.s[my_results_sims$dxsxbar.savings<40]),
mean(my_results_sims$metadata.s[my_results_sims$dxsxbar.savings>=40&my_results_sims$dxsxbar.savings<80]),
mean(my_results_sims$metadata.s[my_results_sims$dxsxbar.savings>=80&my_results_sims$dxsxbar.savings<120]),
mean(my_results_sims$metadata.s[my_results_sims$dxsxbar.savings>=120&my_results_sims$dxsxbar.savings<160]),
mean(my_results_sims$metadata.s[my_results_sims$dxsxbar.savings>=160&my_results_sims$dxsxbar.savings<200]),

mean(my_results_sims$metadata.u[my_results_sims$dxsxbar.savings<40]),
mean(my_results_sims$metadata.u[my_results_sims$dxsxbar.savings>=40&my_results_sims$dxsxbar.savings<80]),
mean(my_results_sims$metadata.u[my_results_sims$dxsxbar.savings>=80&my_results_sims$dxsxbar.savings<120]),
mean(my_results_sims$metadata.u[my_results_sims$dxsxbar.savings>=120&my_results_sims$dxsxbar.savings<160]),
mean(my_results_sims$metadata.u[my_results_sims$dxsxbar.savings>=160&my_results_sims$dxsxbar.savings<200])),
nrow=5,dimnames=list(c(),c('r','s','u')))

#average visits * no of visits * optimal spent per day
data.frame(prod=apply(cbind(apply(visits_tab,2,mean),0:13,9603.247697/365),1,prod))
