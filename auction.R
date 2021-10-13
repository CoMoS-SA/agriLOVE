auction_f <- function(j){
    zz <<- 1
    for (zz in 1:dim(existing_producers)[1]) {
        property    <<- which(world[,,t-1,p]==zz, arr.ind = T)                                                                      # t-1 as if the current zz just bought another property from z
        value_sale  <<- min_land_price
        if(existing_producers[zz,t,p]==1 & (z!= zz) & ((wealth_share_bid*p_wealth[zz,t,p]) > value_sale) ){                         # if z producer is still active and has enough money to bid; excluded owner of cell on sale
            close_cell      <<- ray_closest_cell(property,cells_on_sale[j,"row"],cells_on_sale[j,"col"])                            # find the closest cell among those owned by producer z. NOTE close cell is an array, so not use matrix subsetting notation in the following
            ray_sep         <<- ray_separation(close_cell["row"],close_cell["col"],cells_on_sale[j,"row"],cells_on_sale[j,"col"])   # return position of closest cell owned to that on sale 
            if(t-ud_mean_time>0){
                if(mean(p_unfilled_demand[zz,t:(t-ud_mean_time),p])<=0){
                    auction_ud <<-0 
                }else{
                    auction_ud <<-1
                }
            }else{
                auction_ud  <<-0 
            }

            ber_mean        <<- auction_ud*exp(-rho*ray_sep)
            if(ber_mean<0){print(paste("WARNING: Producer",zz,"with negative probability of bidding"))}
            p_bidding[zz]   <<- rbernoulli(1, ber_mean)
        }else{
            p_bidding[zz]   <<- FALSE
        }
    }
    # the set of bidders can now be determined
    bidders         <<- which(p_bidding==TRUE)                                                                                          # positions of those placing a bid
    bids            <<- wealth_share_bid*p_wealth[,t,p][bidders]                                                                        # minimum price set by government, value of assets in the cell, multiplied by share of wealth 
    ordered_bids    <<- order(bids, decreasing =TRUE)
    # if there is at least one bidder, proceede with the auction, otherwise call mean_entry module
    if(length(bidders)>0){ 
        print("sold in auction")
        prop_agri       <<- which(world[,,t,p]==bidders[ordered_bids[1]], arr.ind = T)[1,]                                              # find which type of agriculture the highest bidder is using. [1,] in case he owns more than one property 
        agri[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]   <<- agri[,,t,p][prop_agri["row"],prop_agri["col"]]                    # export type of agriculture to acquired cell
        if(length(bidders)==1){
            bid_exp         <<- bids[ordered_bids[1]]                                                                                   # determine actual expenditure (second highest bids, or first if there was only one bidder)
        }else{
            bid_exp         <<- bids[ordered_bids[2]]
        }
        prop            <<- which(world[,,t-1,p]==bidders[ordered_bids[1]], arr.ind = T)                                                # cells previously owned before the one just acquired
        wealth[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p] <<- (p_wealth[,t,p][bidders[ordered_bids[1]]] - bid_exp)/(nrow(prop)+1)           # split existing wealth with new cell
        acq_exp         <<- wealth[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p] + bid_exp                                         # total expenditure for acquisition to be split among owned cells
        h <<- 1                                                                                                                         # we deduce the cost from previously owned cells
        for(h in 1:nrow(prop)){ # scroll buyer properties
            wealth_share_prop                           <<- wealth[,,t,p][prop[h,"row"],prop[h,"col"]]/p_wealth[,t,p][bidders[ordered_bids[1]]]     # each cell owned contributes proportionally
            wealth[,,t,p][prop[h,"row"],prop[h,"col"]]  <<- wealth[,,t,p][prop[h,"row"],prop[h,"col"]] - acq_exp*wealth_share_prop
        }
        p_wealth[,t,p][bidders[ordered_bids[1]]]                    <<- p_wealth[,t,p][bidders[ordered_bids[1]]] - acq_exp              # update producer wealth
        world[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]    <<- bidders[ordered_bids[1]]                                        # assign new owner
        mkt_share[bidders[ordered_bids[1]],t,p]                     <<- mkt_share[bidders[ordered_bids[1]],t,p] + mkt_share_cell_on_sale[j, "ms"]   #buyers gets old cell market shares
        mkt_share_cell_on_sale[ j, "sold"]                          <<- 1 
        sales_counter[p]                                            <<- sales_counter[p] +1
        reborn[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]   <<- 2 #code for acquired cell
    }else{                                                                                                                              # if no one is bidding call mean_entry module
        if(flag_land_abandon==1){
            abandon_counter[p]                                          <<- abandon_counter[p] + 1
            print(paste("abandoned in auction", t, "producer", z ))
            world[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]    <<- 9999                                                        # code for abandoned 
            agri[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]     <<- 0                                                           # assign no type of agriculture
        }else{
            abandon_counter[p]                                          <<- abandon_counter[p] + 1
            mean_entry_f(j)
        }
    }
}
