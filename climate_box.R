#### Climate Box
climate_box_f <- function(){
    if(flag_climate == 2){
        if(t==time_floods){
            shocks <<- rtruncnorm(floods_no,0,1, 0.18, var_flood) 
            for(r in 1:floods_no){
                flood_shock[shocked_cells[r,"row"],shocked_cells[r,"col"],t,p] <<- shocks[r]
            }
            
            output_lost[,,t,p] <<- flood_shock[,,t,p]
            for(u in 1:nrow(shocked_cells)){                                                                            # compute cumulative shocks for each cell, accounting for spatial dependency
                for(d in 1:max_flood_dist){
                    if(d==1){
                        flood_neighb <- get_close_cells(output_lost[,,t,p],shocked_cells[u,1],shocked_cells[u,2],d)     # get surrounding cells
                    }else{
                        flood_neighb <- get_ray_exact_cells(output_lost[,,t,p],shocked_cells[u,1],shocked_cells[u,2],d) # get only the exact ray and not those closer than ray
                    }
                    output_lost[,,t,p][flood_neighb] <<- output_lost[,,t,p][flood_neighb] + exp(-spatial_corr_flood*d)*output_lost[shocked_cells[u,1],shocked_cells[u,2],t,p] #compute actual shocks
                }
                output_lost[,,t,p][output_lost[,,t,p]>=1] <<- 0.99                                                      # damage cannot be greater than 1
                
            }
        }
    }
}
