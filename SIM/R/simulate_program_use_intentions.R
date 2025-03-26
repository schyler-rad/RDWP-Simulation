simulate_program_use_intentions=function(n_agents){

  agents=data.frame(id=1:n_agents,
                    playground_lambda=program_use_intention(0,10,n_agents),
                    tutoring_lambda=program_use_intention(0,2,n_agents),
                    coworking_lambda=program_use_intention(0,10,n_agents),
                    cafe_lambda=program_use_intention(0,2,n_agents),
                    party_lambda=program_use_intention(0,1/365,n_agents),
                    workshop_lambda=program_use_intention(0,1/365,n_agents),
                    camp_lambda=program_use_intention(0,1/365,n_agents),
                    merchandise_lambda=program_use_intention(0,1/365,n_agents))

  return(agents)

}
