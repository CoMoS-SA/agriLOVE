# CELL PROFIT

profit_f <- function(){
    if(world[i,k,t,p]==0 | world[i,k,t,p]==9999){                                      #if forest
        revenues[i,k,t,p]   <<- 0 
        tot_cost[i,k,t,p]   <<- 0 
        profit[i,k,t,p]     <<- 0
    } else{
        sales[i,k,t,p]      <<- p_sales[world[i,k,t,p],t,p]*output[i,k,t,p]/p_output[world[i,k,t,p],t,p]
        revenues[i,k,t,p]   <<- food_price[t,p]*sales[i,k,t,p]
        tot_cost[i,k,t,p]   <<- wage[t,p]*L[i,k,t,p] + price_land[i,k,t,p] + RDint[i,k,t,p] + IMint[i,k,t,p]
        
        if((flag_conv_tax==1)&(t>ergodic_trans)){ #collect tax money from conventionals, proportional to revenues
            if(agri[i,k,t,p]==1){#if conventional
                contribute <<- revenues[i,k,t,p]*(rd_effort)*conv_tax#revenues[i,k,t,p]*conv_tax
                revenues[i,k,t,p] <<- revenues[i,k,t,p] - contribute
                sum_tax[t,p] <<- sum_tax[t,p] + contribute
            }
        }
        if((flag_conv_tax==2)&(t>ergodic_trans)){ #collect tax money from conventionals, proportional to profits
            if(agri[i,k,t,p]==1){#if conventional
                contribute <<- (revenues[i,k,t,p] - tot_cost[i,k,t,p])*conv_tax
                if(contribute>0){
                    revenues[i,k,t,p] <<- revenues[i,k,t,p] - contribute
                    sum_tax[t,p]      <<- sum_tax[t,p] + contribute
                }
            }
        }
        
        if(flag_conv_tax==3){
            if(agri[i,k,t,p]==1){
                tot_cost[i,k,t,p]   <<- wage[t,p]*L[i,k,t,p] + price_land[i,k,t,p] + RDint[i,k,t,p] + IMint[i,k,t,p] + tax[i,k,t,p]
            }
        }
        
        if(flag_conv_tax==4){
            if(agri[i,k,t,p]==2){
                tot_cost[i,k,t,p]   <<- wage[t,p]*L[i,k,t,p] + price_land[i,k,t,p] + RDint[i,k,t,p] + IMint[i,k,t,p] - subs[i,k,t,p]
            }
        }
        
        if((flag_conv_tax==5)&(t>ergodic_trans)){
            if(agri[i,k,t,p]==1){
                revenues[i,k,t,p] <<- revenues[i,k,t,p]*(1-conv_tax)
            }
        }
        
        profit[i,k,t,p]     <<- revenues[i,k,t,p] - tot_cost[i,k,t,p]
        #print(paste("profit time ", t,profit[i,k,t,p]))
        
        lab_share_cost[i,k,t,p]  <<- wage[t,p]*L[i,k,t,p]/tot_cost[i,k,t,p]
        land_share_cost[i,k,t,p] <<- price_land[i,k,t,p]/tot_cost[i,k,t,p]
    }
    wealth[i,k,t,p] <<- wealth[i,k,t-1,p] + profit[i,k,t,p]     #current profits accrues (or erodes) stock of past wealth
}
