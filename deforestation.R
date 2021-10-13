deforestation_f <- function(){
    if(flag_deforestation==1&t>50){
        forests_coord <<- which(world[,,t,p]==0, arr.ind = T)                                   # get all forests coordinates
        if(nrow(forests_coord)!=0){                                                             # if there are still forests
            for(o in 1:nrow(forests_coord)){                                                    # scroll forests
                if(rbernoulli(1,deforestation_prob)==1){
                    for_i           <<- forests_coord[o,"row"]                                  #g et single forest coordinates
                    for_k           <<- forests_coord[o,"col"]
                    forest_neighb   <<- get_close_cells(world[,,t,p],for_i,for_k,1)             # find coordinates of neighborhood
                    conv_neighb     <<- forest_neighb[which(agri[,,t,p][forest_neighb] == 1),,drop=F] # only those in ray 1 are allowed to deforest
                    
                    if(nrow(conv_neighb)!=0 & t> ud_mean_time){                                 # if the cell is surrounded by conventional or forest/wastelands, than nothing happens
                        conv_neighb     <<- cbind(conv_neighb, rep(0, nrow(conv_neighb)))       # add a column indicating whether the neighbour is a wannabe deforester
                        colnames(conv_neighb)[3] <<- "deforester"
                        for(oc in 1:nrow(conv_neighb)){                                         # scroll conventional neighbors
                            mean_ud_loc <- mean(p_unfilled_demand[world[conv_neighb[oc,"row"],conv_neighb[oc,"col"],t,p],t:(t-ud_mean_time),p]) #compute mean unfilled demand
                            if(mean_ud_loc>0){
                                conv_neighb[oc,"deforester"] <<- rbernoulli(1,1-exp(-def_rho*mean_ud_loc))  # bernoulli trial: is this cell willing to deforest?
                            }
                        }
                        pot_deforester <<- conv_neighb[conv_neighb[,"deforester"]==1,(1:2), drop=F] # coordinates of those willing to deforest
                        
                        if(nrow(pot_deforester)!=0){                                            # if there is at least one potential deforester, proceed with deforestation
                            def_id <<- pot_deforester[sample(1:nrow(pot_deforester),1),,drop=F] # randomly extract one deforester cell among those willing to
                            deforester <<- world[,,t,p][def_id]                                 # retrieve owner of deforester cell
                            forest_gone[t,p] <<- forest_gone[t,p] +1                            # update counter of deforester
                            
                            print(paste("Cutting trees at time", t)) 
                            print(paste("Deforester", deforester, "cut forest"))
                            print(forests_coord[o,])
                            
                            # ATTENTION: currently price for deforestation is 0. If increased, insert a flag to check whether deforester can afford to deforest. 
                            world[for_i,for_k,t,p]               <<- deforester
                            reborn[for_i,for_k,t,p]              <<- 2                                                                  # code for just reforested 
                            def_property                         <<- which(world[,,t,p]==deforester, arr.ind = T)
                            theta[for_i,for_k,t,p]               <<- theta[def_id[1,"row"],def_id[1,"col"],t,p]*(1+forest_theta_gain)   # a bonus in theta as is a virgin land
                            L[for_i,for_k,t,p]                   <<- L[def_id[1,"row"],def_id[1,"col"],t,p]                             # same variables as the deforester cell
                            RDint[for_i,for_k,t,p]               <<- RDint[def_id[1,"row"],def_id[1,"col"],t,p]    
                            sales[for_i,for_k,t,p]               <<- sales[def_id[1,"row"],def_id[1,"col"],t,p]    
                            revenues[for_i,for_k,t,p]            <<- revenues[def_id[1,"row"],def_id[1,"col"],t,p] 
                            agri[for_i,for_k,t,p]                <<- agri[def_id[1,"row"],def_id[1,"col"],t,p]
                            unshocked_price_land[for_i,for_k,t,p]<<- unshocked_price_land[def_id[1,"row"],def_id[1,"col"],t,p]
                            cost[for_i,for_k,t,p]                <<- cost[def_id[1,"row"],def_id[1,"col"],t,p]
                            
                            wealth[for_i,for_k,t,p]   <<- (p_wealth[deforester,t,p] - price_deforestation)/(nrow(def_property)+1)   
                            def_exp                   <<- wealth[for_i,for_k,t,p] + price_deforestation 
                            for(h in 1:nrow(def_property)){                                                 # scroll buyer properties
                                wealth_share_prop                                           <- wealth[,,t,p][def_property[h,"row"],def_property[h,"col"]]/p_wealth[deforester,t,p]   # each cell owned contributes proportionally
                                wealth[,,t,p][def_property[h,"row"],def_property[h,"col"]]  <<- wealth[,,t,p][def_property[h,"row"],def_property[h,"col"]] - def_exp*wealth_share_prop
                            }
                            
                            p_wealth[deforester,t,p]  <<- p_wealth[deforester,t,p] - price_deforestation    # update producer wealth
                        }
                    }
                }    
            }
        }
    }
}
