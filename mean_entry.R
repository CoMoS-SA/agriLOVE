mean_entry_f <- function(j){
    print(paste("Rebound at time", t, "cell", cells_on_sale[j,"row"],cells_on_sale[j,"col"]))
    repeat{                                                                 # pick up a non-forest random cell with positive wealth
        ii <<- sample(1:x,1)
        kk <<- sample(1:y,1)
        
        if(wealth[ii,kk,t,p]>0 & world[ii,kk,t,p]!=0){
            break
        }
    }
    RDint[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]               <<- RDint[ii,kk,t,p]    
    sales[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]               <<- sales[ii,kk,t,p]    
    wealth[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]              <<- wealth[ii,kk,t,p]   
    revenues[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]            <<- revenues[ii,kk,t,p] 
    tot_cost[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]            <<- tot_cost[ii,kk,t,p]
    L[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]                   <<- L[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]
    reborn[cells_on_sale[j,"row"],cells_on_sale[j,"col"],t,p]              <<- 1
    
    #update rebound counter
    rebound_counter[p]                                                     <<- rebound_counter[p] +1
}
