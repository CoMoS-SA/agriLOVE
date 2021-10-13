hire_f <- function(){
    #z <<- 1
    for (z in 1:dim(existing_producers)[1]) {
        property <<- which(world[,,t,p]==z, arr.ind = T)
        #decide if want to use all the cells or not 
        if(nrow(property)!=0){
            # Decide how much labor to hire 
            if(t==2){                                               # in the very first period, let L be the same and unfilled demand do not affect fitness
                p_L[z,t,p]                      <<- p_L[z,t-1,p]
                p_unfilled_demand[z,t,p]        <<- 0
                p_unfilled_demand_lab[z,t,p]    <<- 0
            }else{
                if(p_wealth[z,t-1,p]>0){                            # if cell has still some liquid assets
                    p_unfilled_demand[z,t,p] <<- (demand[t-1,p]*mkt_share[z,t-1,p] - p_output[z,t-1,p])/p_output[z,t-1,p]
                    
                    # unfilled demand for labor: different from p_unfilled_demand only when a climate shock hits. 
                    p_unfilled_demand_lab[z,t,p] <<- (demand[t-1,p]*mkt_share[z,t-1,p] - p_pot_output[z,t-1,p])/p_pot_output[z,t-1,p]
                    
                    p_L[z,t,p] <<- p_L[z,t-1,p]*(1+ud_sens*p_unfilled_demand_lab[z,t,p])
                    if(p_L[z,t,p]*wage[t,p]>=wealth_buffer*p_wealth[z,t-1,p]){
                        p_L[z,t,p] <<- (wealth_buffer*p_wealth[z,t-1,p])/wage[t,p]
                    }
                    
                    # Maximum level of employment (proxy for fixed labor supply)
                    if(p_L[z,t,p]>=max_labor*nrow(property)){
                        p_L[z,t,p] <<- max_labor*nrow(property)
                    }
                    
                }else{                                              # if negative liquid assets at the cell level but producer still active, stick with previous labor
                    p_L[z,t,p] <<- p_L[z,t-1,p]
                }
            }
            
            if(nrow(property)==1){
                il <<- property[1,"row"]
                ik <<- property[1,"col"]
                L[il,ik,t,p] <<- p_L[z,t,p]
            }else{
                property <<- cbind(property, theta[,,t-1,p][property], output[,,t-1,p][property])
                colnames(property)[3] <<- "theta"
                colnames(property)[4] <<- "output"
                property <<- cbind(property,1)
                colnames(property)[5] <<- "empty"
                store_L <- array(0, nrow(property))
                resid_L <- p_L[z,t,p]
                repeat{
                    L_allocated<- array(0, nrow(property))
                    theta_rel <- property[,"empty"]*property[,"theta"]/sum(property[,"empty"]*property[,"theta"])
                    for(ip in 1:nrow(property)){
                        tent_L <- resid_L*theta_rel[ip]             # how much labor with respect to what already hired
                        tmp_L <- store_L[ip] + tent_L               # temporary labor: is it already above the limit?
                        if(tmp_L<max_labor){
                            store_L[ip] <- store_L[ip] + tent_L
                            property[ip,"empty"] <<- 1              # can still be filled
                            L_allocated[ip] <- tent_L
                        }else{
                            L_allocated[ip] <- max_labor - store_L[ip]
                            store_L[ip] <- max_labor
                            property[ip,"empty"] <<- 0              # it's full
                        }
                    }
                    
                    resid_L <- resid_L - sum(L_allocated)
                    
                    if(isTRUE(all.equal(resid_L,0))){
                        break
                    }
                }
                if(any(store_L==0)){
                    print(paste("time",t,"producer",z))
                }
                
                #assign labour
                for(ip in 1:nrow(property)){
                    il <<- property[ip,"row"]
                    ik <<- property[ip,"col"]
                    L[il,ik,t,p] <<- store_L[ip]
                }
                
            }
        }
    }
}
