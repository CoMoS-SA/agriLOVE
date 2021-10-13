market_f <- function(){
    supply[t,p]             <<- sum(output[,,t,p])
    excess_demand[t,p]      <<- (demand[t,p] - supply[t,p])/supply[t,p]
    food_price[t,p]         <<- food_price[t-1,p]*(1 + eps_price* ((demand[t,p] - supply[t,p])/supply[t,p]) )
    
    if(!isTRUE(all.equal(sum(mkt_share[,t-1,p]),1))){print(paste("ERROR at time ",t,": Market shares from previous period do not sum up to 1", sep = ""))}
    
    #compute fitness
    if(t==2){
        fitness[,t,p]  <<- 1/p_cost[,t,p]
        
    }else{
        fitness[,t,p]  <<- 1/(omega_cost*p_cost[,t,p] + omega_unfilled*pmax(0,p_unfilled_demand[,t,p]))
    } 
    cost_share_fitness[,t,p] <<- omega_cost*p_cost[,t,p]/(omega_cost*p_cost[,t,p] + omega_unfilled*pmax(0,p_unfilled_demand[,t,p]))
    mean_fit <<- weighted.mean(fitness[,t,p],mkt_share[,t-1,p])
    
    #compute market shares (Replicator Dynamics)
    z <<- 1
    for (z in 1:dim(existing_producers)[1]) {
        if(existing_producers[z,t,p]==1){
            mkt_share[z,t,p] <<- mkt_share[z,t-1,p]*(1 + eps_share*(fitness[z,t,p] - mean_fit)/mean_fit)
        }else{
            mkt_share[z,t,p] <<- 0
        }
    }
    
    if(!isTRUE(all.equal(sum(mkt_share[,t,p]),1))){print(paste("ERROR at time ", t,": Market shares do not sum up to 1", sep = ""))}
    
    if(demand[t,p]>supply[t,p]){
        exit_rest <<- demand[t,p]-supply[t,p]
    }else{
        exit_rest <<- 0
    }
    
    mkt_iter[t,p]       <<- 1
    res_demand          <<- demand[t,p]
    update_mkt_share    <<- mkt_share[,t,p]
    res_output          <<- p_output[,t,p]
    temp_sales          <<- p_sales[,t,p] #is already full of zeros
    
    repeat{
        unexploited <<- 0 
        updater     <<- 0
        z <<- 1
        for (z in 1:dim(existing_producers)[1]) {
            if(existing_producers[z,t,p]==1){
                if(res_demand*update_mkt_share[z]>=res_output[z]){
                    temp_sales[z]       <<- temp_sales[z] + res_output[z]
                    unexploited         <<- unexploited + res_demand*update_mkt_share[z] - res_output[z]
                    update_mkt_share[z] <<- 0
                    res_output[z]       <<- 0
                } else{
                    temp_sales[z]       <<- temp_sales[z] + res_demand*update_mkt_share[z]
                    updater             <<- updater + update_mkt_share[z]
                    res_output[z]       <<- res_output[z] - res_demand*update_mkt_share[z]
                }
            }
        }
        
        if(isTRUE(all.equal(unexploited,exit_rest, tolerance = 1e-6))){
            break
        }
        
        update_mkt_share <<- update_mkt_share/updater 
        res_demand       <<- unexploited
        mkt_iter[t,p]    <<- mkt_iter[t,p] + 1
    }
    
    p_sales[,t,p] <<- temp_sales
    
}
