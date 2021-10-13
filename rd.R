# Perform R&D, assign price and productivites of lands, computes soil degradation, production at cell level takes place
rd_f <- function(){
    if(world[i,k,t,p]==0 | world[i,k,t,p]==9999){                               # if forest
        RDint[i,k,t,p]      <<- 0
        price_land[i,k,t,p] <<- price_forest
        theta[i,k,t,p]      <<- theta_forest
        output[i,k,t,p]     <<- 0
        cost[i,k,t,p]       <<- Inf              
    }else{
        # Innovation takes place
        if(sales[i,k,t-1,p]<=0){                                                # previous profits were negative
            RDint[i,k,t,p]  <<- 0
        }else{
            RDint[i,k,t,p]  <<- revenues[i,k,t-1,p]*(rd_effort)
            if((flag_conv_tax==3)&(t>ergodic_trans)){
                if(agri[i,k,t,p]==1){
                    tax[i,k,t,p]    <<- revenues[i,k,t-1,p]*(rd_effort)*(conv_tax)
                    RDint[i,k,t,p]  <<- (revenues[i,k,t-1,p]*(rd_effort)) - tax[i,k,t,p]
                }
            }
            if((flag_conv_tax==4)&(t>ergodic_trans)){
                if(agri[i,k,t,p]==2){
                    subs[i,k,t,p]   <<- revenues[i,k,t-1,p]*(rd_effort)*(conv_tax)
                    RDint[i,k,t,p]  <<- revenues[i,k,t-1,p]*(rd_effort) + subs[i,k,t,p]
                }
            }
        }
        max_RD              <<- max(RDint[,,t-1,p])
        RD_scaling[i,k,t,p] <<- RDint[i,k,t-1,p]/max_RD
        innovate            <<- exp(-iota*RD_scaling[i,k,t,p])
        if(rbernoulli(1, 1-innovate)==TRUE){
            innovators[t,p]     <<- innovators[t,p] + 1
            if(agri[i,k,t,p] == 1){
                gain[i,k,t,p]       <<- (rbeta(1,2,2)* (abs(theta_max) + abs(theta_min)) ) + theta_min
            }else{
                gain[i,k,t,p]       <<- (rbeta(1,2,2)* (abs((theta_max)*(1-agri_growth_penalty)) + abs(theta_min)) ) + theta_min
            }
        } else{
            gain[i,k,t,p]       <<- 0
        }
        
        # Imitation takes place
        if(flag_imit==1){
            if(sales[i,k,t-1,p]<=0){                                            # previous profits were negative
                IMint[i,k,t,p]  <<- 0
            }else{
                IMint[i,k,t,p]  <<- sales[i,k,t-1,p]*food_price[t-1,p]*(imit_effort)    # RD is a costly process in terms of time mand resources
            }
            max_IM              <<- max(IMint[,,t-1,p])
            IM_scaling[i,k,t,p] <<- IMint[i,k,t-1,p]/max_IM
            imitation           <<- exp(-iota*IM_scaling[i,k,t,p])
            if(rbernoulli(1, 1-innovate)==TRUE){
                imitators[t,p]      <<- imitators[t,p] + 1
                neighb              <<- get_close_cells(world[,,t,p],i,k,1)     # observational ray set to 1 (King's moves)
                conv_neighb         <<- neighb[which(agri[,,t,p][neighb] == 1),]    # find conventional neighborhood
                sust_neighb         <<- neighb[which(agri[,,t,p][neighb] == 2),]
                if(agri[i,k,t,p]==1){
                    IMM[i,k,t,p]        <<- max(theta[,,t-1,p][conv_neighb])    # imitated cell
                }else{
                    IMM[i,k,t,p]        <<- max(theta[,,t-1,p][sust_neighb])    # imitated cell
                }
                if(IMM[i,k,t,p]<theta[i,k,t-1,p]){
                    IMM[i,k,t,p]        <<- theta[i,k,t-1,p]
                }
            }else{
                IMM[i,k,t,p]        <<- theta[i,k,t-1,p]
            }
        }else{
            IMM[i,k,t,p]        <<- theta[i,k,t-1,p]
        }
        
        # compute losses and other quantities differing among agricultural types
        if(agri[i,k,t,p] == 1){
            unshocked_price_land[i,k,t,p]   <<- unshocked_price_land[i,k,t-1,p]*(1+prod_growth)
            temp_price                      <<- unshocked_price_land[i,k,t,p]*(1 + rnorm(1,0,price_land_var))
            price_land[i,k,t,p]             <<- (temp_price*(1-sharable_cost_frac)) + ((sharable_cost_frac*temp_price)/sum(world[,,t,p]==world[i,k,t,p]))
            if(no_land_degradation!=1 & t>ergodic_trans){
                if(land_degr_flag==1){
                    pos_loss[i,k,t,p]           <<- pos_loss[i,k,t-1,p] + 1
                    loss[i,k,t,p]               <<- gen_logis(pos_loss[i,k,t,p],a=lower_asymp, k=upper_asymp_conv, b=growth_conv, m= start_time_conv ,v= asymp_asym_conv)
                }else{
                    if(t-deg_t>=2){
                        loss[i,k,t,p]               <<- lambda*mean(output[i,k,(t:(t-deg_t)),p])
                    }else{
                        loss[i,k,t,p]               <<- lambda*mean(output[i,k,(t:(t-2)),p])
                    }
                }
            }else{
                loss[i,k,t,p]               <<- 0
            }
        }
        
        if(agri[i,k,t,p] == 2){
            unshocked_price_land[i,k,t,p]   <<- unshocked_price_land[i,k,t-1,p]*(1+prod_growth)
            temp_price                      <<- unshocked_price_land[i,k,t,p]*(1 + rnorm(1,0,price_land_var))
            price_land[i,k,t,p]             <<- (temp_price*(1-sharable_cost_frac)) + ((sharable_cost_frac*temp_price)/sum(world[,,t,p]==world[i,k,t,p]))
            if(no_land_degradation!=1 & t>ergodic_trans){
                if(land_degr_flag==1){
                    if(pos_loss[i,k,t-1,p]>0){
                        pos_loss[i,k,t,p]   <<- pos_loss[i,k,t-1,p] -1
                        loss[i,k,t,p]       <<- -gen_logis(pos_loss[i,k,t,p],a=lower_asymp, k=upper_asymp_conv, b=growth_conv, m= start_time_conv ,v= asymp_asym_conv)
                    }else{
                        pos_loss[i,k,t-1,p] <<- 0
                        loss[i,k,t,p]       <<- 0
                    }
                }else{
                    loss[i,k,t,p]       <<- 0
                }
            }else{
                loss[i,k,t,p]       <<- 0
            }
        }
        
        property                <- which(world[,,t,p]==world[i,k,t,p], arr.ind = T)
        if(nrow(property)>1){                                                   # if ever been a forest)
            pip                     <- which.max(theta[,,t-1,p][property])
            best_cell               <- t(as.matrix(property[pip,]))
            if(!all(best_cell==c(i,k)) & !any(world[best_cell[1,"row"],best_cell[1,"col"],1:t,p]==0)){ # is the current cell different from the max productive among those owned?
                theta_band[i,k,t,p]     <<- theta[,,t-1,p][best_cell]
            }else{
                theta_band[i,k,t,p]     <<- theta[i,k,t-1,p] # copy itself
            }
        }else{
            theta_band[i,k,t,p]     <<- theta[i,k,t-1,p] # copy itself
        }
        
        # Compute actual land productivity
        theta[i,k,t,p]          <<- (1-mu_imit - mu_band)*theta[i,k,t-1,p]*(1-theta_lost[i,k,t-1,p]) + gain[i,k,t,p] + mu_imit*IMM[i,k,t,p] + mu_band*theta_band[i,k,t,p] - loss[i,k,t,p]
        
        if(theta[i,k,t,p]<1){theta[i,k,t,p]<<-1} 
        
        # Production at cell level: compute output and costs
        pot_output[i,k,t,p]     <<- theta[i,k,t,p]*(L[i,k,t,p])^alpha           # pot_output is output before flood, the one you pay the costs on
        output[i,k,t,p]         <<- pot_output[i,k,t,p] * (1-output_lost[i,k,t-1,p]) 
        cost[i,k,t,p]           <<- (wage[t,p]*(L[i,k,t,p]^(1-alpha)))/theta[i,k,t,p] + price_land[i,k,t,p]
    }      

}


