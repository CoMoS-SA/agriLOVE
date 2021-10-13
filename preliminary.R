#### PRELIMINARIES
#Contains preliminary code chunks to be reproduced at the beginning of each time step

preliminary_f <- function(){
    
    if(flag_suspend_ergodic==1){
        if(t<ergodic_trans){
            flag_auction          <<- 0
            flag_deforestation    <<- 0
            flag_land_abandon     <<- 0 
        }else{
            if(t==ergodic_trans){
                flag_auction          <<- 1
                flag_deforestation    <<- 1
                flag_land_abandon     <<- 1 
            }
        }
    }
    
    
    # World and agriculture
    world[,,t,p] <<- world[,,t-1,p]                 # inerith distribution of properties from previous period (changes to property happen in the entry module, at the end of any period)
    agri[,,t,p] <<- agri[,,t-1,p]                   # inerith type of agriculture from previous period (switching happens at the end of every period)
    
    # Turn wastelands into forests
    if(t>time_to_forest){
        wasteland_coord <<- which(world[,,t,p]==9999, arr.ind = T) #get all forests coordinates
        if(nrow(wasteland_coord)!=0){
            for(o in 1:nrow(wasteland_coord)){ #scroll forests
                if(!any(world[wasteland_coord[o,"row"],wasteland_coord[o,"col"],t:(t-time_to_forest),p]!=9999)){#if enough periods have passed, turn into forest
                    world[wasteland_coord[o,"row"],wasteland_coord[o,"col"],t,p] <<- 0
                    loss[wasteland_coord[o,"row"],wasteland_coord[o,"col"],t,p] <<- 0
                } 
            }
        }
    }
    
    #rate of growth of productivity, we increase wage and price of land at this rate
    if(t==2){
        prod_growth <<- 0.1
    }else{
        prod_growth <<- log(mean(theta[,,t-1,p])) - log(mean(theta[,,t-2,p]))
    }
    
    if(t==2){
        wage_shock  <<- 0
    }else{
        wage_shock  <<- log(sum(L[,,t-1,p])) - log(sum(L[,,t-2,p]))
    }
   
    # Evolution of demand and wages
    demand_noise[t,p] <<- rnorm(1,0,0.01)
    demand[t,p]       <<- (demand[t-1,p] + demand_rate)*(1 + demand_noise[t,p]) #linear demand
    wage[t,p]         <<- wage[t-1,p]*(1+prod_growth)*(1+eps_wage*wage_shock)
    
    
    if(flag_climate==2){
        demand[t,p] <<- as.numeric(baseline_climate_demand[baseline_climate_demand$time==t & baseline_climate_demand$mc==p,"demand"] )
    }
    
    # Update existing producers
    z <<- 1
    for (z in 1:dim(existing_producers)[1]){
        property <<- which(world[,,t,p]==z, arr.ind = T)
        if(nrow(property)==0){                                # if z producers do not have properties (has defaulted in the past or not born yet)
            existing_producers[z,t,p]   <<- 0
            n_property[z,t,p]           <<- 0 
        }else{
            existing_producers[z,t,p]   <<- 1                 # active producer
            n_property[z,t,p]           <<- nrow(property)    # compure number of properties for each farmer
        }
    }
    
    # How many forests this period?
    forests[t,p] <<- sum(world[,,t,p]==0)
    if(forests[t,p]==(x*y)){
        print(paste("WARNING: World is covered with forests at time ",t, ": Welcome to the jungle.", sep = ""))
    }
    
    wastelands[t,p] <<- sum(world[,,t,p]==0)
    if(wastelands[t,p]==(x*y)){
        print(paste("WARNING: World is covered with wastelands at time ",t, sep = ""))
    }
    # Where are the forests?
    non_forest_pos         <- which(world[,,t,1]!=0, arr.ind = T) 
    
    
    if(flag_def_policy!=0){
        agri_share_def[t,p] <<- sum(agri[,,t,p]==2)/sum(agri[,,t,p]!=0)
        if(t>ergodic_trans){
            if( (agri_share_def[t-1,p]<tresh_def_pol & agri_share_def[t,p]>tresh_def_pol) | (agri_share_def[t-1,p]>tresh_def_pol & agri_share_def[t,p]<tresh_def_pol) ){
                flag_deforestation <<- 0
            }
        }
    }
    
    #if(flag_conv_tax==1|flag_conv_tax==2){ #if no more sustainables, suspend policy
    #    if((flag_conv_tax==1)&(t==2)){
    #        flag_conv_tax<<-1
    #    }
    #    if((flag_conv_tax==2)&(t==2)){
    #        print("yo")
    #        flag_conv_tax<<-2
    #    }
    #    if(sum(agri[,,t,p]==2)==0){
    #        print("POLICY SUPPRESSED")
    #        flag_conv_tax <<- 0
    #    }
    #}
    
}