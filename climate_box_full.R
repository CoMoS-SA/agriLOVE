#### Climate Box
weather_f <- function(){
    if(flag_climate == 2 | flag_climate == 3){
        num_floods[t,p] <<- rpois(1,flood_frequency) #how many floods this period
        if(num_floods[t,p]!=0){
            shocks <- c(rbeta(num_floods[t,p], alpha_flood, beta_flood), rep(0,((x*y)-num_floods[t,p])) )
            repeat{ #repeat extraction until no forests are involved
                flood_shock[,,t,p] <<- sample(shocks) #assign shocks randomly
                shocked_cells <<- which(flood_shock[,,t,p]!=0, arr.ind = T)
                if(all(!world[,,t,p][shocked_cells]==0)){break}
            }
            
            output_lost[,,t,p] <<- flood_shock[,,t,p]
            for(u in 1:nrow(shocked_cells)){ #compute cumulative shocks for each cell, accounting for spatial dependency
                for(d in 1:max_flood_dist){
                    if(d==1){
                        flood_neighb <- get_close_cells(output_lost[,,t,p],shocked_cells[u,1],shocked_cells[u,2],d)  #get surrounding cells
                    }else{
                        flood_neighb <- get_ray_exact_cells(output_lost[,,t,p],shocked_cells[u,1],shocked_cells[u,2],d) #get only the exact ray and not those closer than ray
                    }
                    output_lost[,,t,p][flood_neighb] <<- output_lost[,,t,p][flood_neighb] + exp(-spatial_corr_flood*d)*output_lost[shocked_cells[u,1],shocked_cells[u,2],t,p] #compute actual shocks
                }
                output_lost[,,t,p][output_lost[,,t,p]>=1] <<- 0.99 #damage cannot be greater than 1
                
            }
        }
    }
    
    
    
    if(flag_climate == 1 | flag_climate == 3){
        num_droughts[t,p] <<- rpois(1,droughts_frequency) #how many floods this period
        if(num_droughts[t,p]!=0){
            shocks <- c(rbeta(num_droughts[t,p], alpha_drought, beta_drought)*0.1, rep(0,((x*y)-num_droughts[t,p])) )
            repeat{ #repeat extraction until no forests are involved
                droughts_shock[,,t,p] <<- sample(shocks) #assign shocks randomly
                shocked_cells <<- which(droughts_shock[,,t,p]!=0, arr.ind = T)
                if(all(!world[,,t,p][shocked_cells]==0)){break}
            }
            
            theta_lost[,,t,p] <<- droughts_shock[,,t,p]
            for(u in 1:nrow(shocked_cells)){ #compute cumulative shocks for each cell, accounting for spatial dependency
                for(d in 1:max_drought_dist){
                    if(d==1){
                        drought_neighb <- get_close_cells(theta_lost[,,t,p],shocked_cells[u,1],shocked_cells[u,2],d)  #get surrounding cells
                    }else{
                        drought_neighb <- get_ray_exact_cells(theta_lost[,,t,p],shocked_cells[u,1],shocked_cells[u,2],d) #get only the exact ray and not those closer than ray
                    }
                    theta_lost[,,t,p][drought_neighb] <<- theta_lost[,,t,p][drought_neighb] + exp(-spatial_corr_drought*d)*theta_lost[shocked_cells[u,1],shocked_cells[u,2],t,p] #compute actual shocks
                }
                theta_lost[,,t,p][theta_lost[,,t,p]>=1] <<- 0.99 #damage cannot be greater than 1
                
            }
        }
    }
    
    
    
}



