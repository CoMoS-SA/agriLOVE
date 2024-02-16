#PRODUCTION MODULE
# It only computes the production at the producer level (i.e. summing production in owned cells)
production_f <- function(){
    z <<- 1
    for (z in 1:dim(existing_producers)[1]) {
        property <<- which(world[,,t,p]==z, arr.ind = T)
        if(nrow(property)!=0){                          #if z producer is still active
            if(nrow(property)==1){
                p_pot_output[z,t,p]         <<- pot_output[property[1,"row"],property[1,"col"],t,p]
                p_output[z,t,p]             <<- output[property[1,"row"],property[1,"col"],t,p]
                p_cost[z,t,p]               <<- cost[property[1,"row"],property[1,"col"],t,p]
            }else{
                p_pot_output[z,t,p]         <<- sum(pot_output[,,t,p][property])
                p_output[z,t,p]             <<- sum(output[,,t,p][property])    
                p_cost[z,t,p]               <<- weighted.mean(cost[,,t,p][property], output[,,t,p][property]) 
            }
        }
    }
}
