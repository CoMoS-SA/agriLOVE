#PRODUCER PROFITS AND WEALTH

p_profit_f <- function(){
    
    if((flag_conv_tax==1|flag_conv_tax==2)&(t>ergodic_trans)){              # redistribute tax money from conventionals to sustainables (equally)
        print(paste("collected",sum_tax[t,p]))
        print(paste("pre revenues",sum(revenues[,,t,p][agri[,,t,p]==2])))
        revenues[,,t,p][agri[,,t,p]==2] <<- revenues[,,t,p][agri[,,t,p]==2] + (sum_tax[t,p]/sum(agri[,,t,p]==2))
        print(paste("post revenues",sum(revenues[,,t,p][agri[,,t,p]==2])))
        profit[,,t,p][agri[,,t,p]==2] <<- profit[,,t,p][agri[,,t,p]==2] + (sum_tax[t,p]/sum(agri[,,t,p]==2))
    }
    
    z <<- 1
    for (z in 1:dim(existing_producers)[1]) {
        property <<- which(world[,,t,p]==z, arr.ind = T)
        if(nrow(property)!=0){                                              # if z producer is still active
            p_revenues[z,t,p] <<- sum(revenues[,,t,p][property])
            p_tot_cost[z,t,p] <<- sum(tot_cost[,,t,p][property])
            p_profit[z,t,p]   <<- sum(profit[,,t,p][property])
            p_wealth[z,t,p]   <<- sum(wealth[,,t,p][property])
        }
    }
}
