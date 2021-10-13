#### Market for cells or rebound to the mean in case of default
entry_f <- function(){
    if(existing_producers[z,t,p]==1){
        if(p_wealth[z,t,p]<0 | mkt_share[z,t,p]<0.001){
            bankrupt_counter[p]         <<- bankrupt_counter[p] +1
            bankrupt[t,p]               <<- bankrupt[t,p] + 1
            p_bankrupt[z,t,p]           <<- 1
            #Start to scroll the properties to be sold (or rebounded)
            cells_on_sale               <<- which(world[,,t,p]==z, arr.ind = T)
            mkt_share_cell_on_sale[,]   <<- 0                                                               # refill with zeros the moving market share matrix, erasing leftovers from previously defaulted producer
            l <<-1
            for (l in 1:nrow(cells_on_sale)) {                                                              # save each cell share of market shares to move them across new owners
                mkt_share_cell_on_sale[l,"ms"] <<- mkt_share[z,t,p]*output[cells_on_sale[l,"row"],cells_on_sale[l,"col"],t,p]/p_output[z,t,p]
            }
            
            j <<- 1
            for (j in 1:nrow(cells_on_sale)) {                                                              # for each cell owned by the defaulted producer, call the auction or mean_entry module
                if(flag_auction==1){
                    auction_f(j)
                }else{
                    mean_entry_f(j)
                }
            }
            
            if(flag_auction==1 & flag_land_abandon==1){
                mkt_share[z,t,p]            <<- 0                                                           # land is abandoned, producer ceases to exist
                existing_producers[z,t,p]   <<- 0
                if(flag_auction==1 & sum(mkt_share_cell_on_sale[mkt_share_cell_on_sale[,"sold"]==0,"ms"])!=0){ # some land has been abandoned, need to redistribute market share
                    temp_old_mkt_shares <<- sum(mkt_share_cell_on_sale[mkt_share_cell_on_sale[,"sold"]==0,"ms"]) 
                    for(zzz in 1:dim(existing_producers)[1]){
                        if(existing_producers[zzz,t,p]==1){
                            if(zzz!=z){
                                mkt_share[zzz,t,p] <<- mkt_share[zzz,t,p] + (temp_old_mkt_shares/(sum(existing_producers[,t,p])))
                            }
                        }
                    }
                }
            }else{
                if(flag_auction==1 & sum(mkt_share_cell_on_sale[mkt_share_cell_on_sale[,"sold"]==0,"ms"])==0){
                    mkt_share[z,t,p]    <<- 0
                }else{
                    temp_old_mkt_shares <<- sum(mkt_share_cell_on_sale[mkt_share_cell_on_sale[,"sold"]==0,"ms"]) 
                    temp_new_mkt_shares <<- mean(mkt_share[which(mkt_share[,t,p]<Inf)!=z & mkt_share[,t,p]!=0,t,p])
                    for(zzz in 1:dim(existing_producers)[1]){
                        if(existing_producers[zzz,t,p]==1){
                            if(zzz!=z){
                                mkt_share[zzz,t,p] <<- mkt_share[zzz,t,p] + (temp_old_mkt_shares/(sum(existing_producers[,t,p])-1))
                                mkt_share[zzz,t,p] <<- mkt_share[zzz,t,p] - (temp_new_mkt_shares/(sum(existing_producers[,t,p])-1))
                            }
                        }
                    }
                    mkt_share[z,t,p] <<- temp_new_mkt_shares
                }
                
                if(sum(mkt_share[,t,p]<0)>0){
                    red_neg_mkt_share <<- sum(mkt_share[,t,p][mkt_share[,t,p]<0])                               # select negative values
                    mkt_share[,t,p][mkt_share[,t,p]>0] <<- mkt_share[,t,p][mkt_share[,t,p]>0] + red_neg_mkt_share/length(mkt_share[,t,p][mkt_share[,t,p]>0])
                    mkt_share[,t,p][mkt_share[,t,p]<0] <<- 0 
                }
                
                
                local_cell_reb      <- which(mkt_share_cell_on_sale[1:nrow(cells_on_sale),"sold"]==0)           # locally assigned, no need to store this
                p_wealth[z,t,p]     <<- sum(wealth[,,t,p][cells_on_sale[local_cell_reb,]])                      # compute defaulted producer residual market share (interpreted as belonging to a new entrant with same code)
            }
            
            if(!isTRUE(all.equal(sum(mkt_share[,t,p]),1))){print(paste("AUCTION ERROR at time ",t,": Market shares from previous period do not sum up to 1", sep = ""))}
        }
    }
}
