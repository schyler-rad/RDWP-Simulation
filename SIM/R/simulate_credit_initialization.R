simulate_credit_initialization=function(n_agents){

  agents=data.frame(cbind(id=1:n_agents,
                          spent=rep(0,n_agents),
                          playground_credits=rep(0,n_agents),
                          tutoring_credits=rep(0,n_agents),
                          coworking_credits=rep(0,n_agents),
                          cafe_credits=rep(0,n_agents),
                          party_credits=rep(0,n_agents),
                          workshop_credits=rep(0,n_agents),
                          camp_credits=rep(0,n_agents),
                          merchandise_credits=rep(0,n_agents),
                          savings=rep(0,n_agents)#,
                          #visits=rep(0,n_agents)
                          ))

  return(agents)
}
