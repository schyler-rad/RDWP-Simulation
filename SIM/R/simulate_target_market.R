simulate_target_market=function(n_agents){

  customer_types=sample(x=c('SAHP','WFHP','PWT'),
                        size=n_agents,
                        replace=TRUE)

  agents=data.frame(id=1:n_agents,
                    customer_types,
                    member_type='nm',
                    visit_prob=sample(seq(0,1,length.out=1000),size=n_agents,replace=T),
                    visits=0)

  return(agents)

}
