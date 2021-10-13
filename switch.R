#### Type of agriculture switching
switch_f <- function(){
    if(existing_producers[z,t,p]==1 & flag_agri_init!=0  & t>ergodic_trans){    # if farmer still own at least a cell, or if the model was initialized with some sustainables
        if(t-switcher[z,p]==switch_time){
            switcher[z,p]   <<- t
            property        <<- which(world[,,t,p]==z, arr.ind = T)
            o <<- 1
            for (o in 1:nrow(property)) {
                part_neighb <<- get_close_cells(agri[,,t,p],property[o,"row"],property[o,"col"],ray)    #find coordinates of neighborhood
                if(o==1){
                    neighb  <<- part_neighb
                } else{
                    neighb  <<- rbind(neighb,part_neighb)
                }
            }
            
            if(flag_switch_own_prop==1){
                neighb      <<- rbind(neighb,property)                          # include own cells in the observed set
            }
            
            neighb          <<- neighb[!duplicated(neighb), ]                   # eliminate duplicates
            conv_neighb     <<- neighb[which(agri[,,t,p][neighb] == 1),,drop=F] # find conventional neighborhood. Drop makes sure the object is still a matrix
            sust_neighb     <<- neighb[which(agri[,,t,p][neighb] == 2),,drop=F] # find sustainable neighborhood
            
            if(length(conv_neighb)==0){
                conv_neighb_output <<- 0
            }else{
                # fitness measure of conventional
                if(flag_type_switch==1){
                    conv_neighb_output      <<- (sum(apply(output[,,(t-wind_switch+1):t,p],1:2,sum)[conv_neighb])) / (wind_switch*nrow(conv_neighb))
                }else{
                    conv_neighb_output_aux  <<- apply(output[,,(t-wind_switch+1):t,p]/L[,,(t-wind_switch+1):t,p],1:2,sum,na.rm = T)[conv_neighb]
                        conv_neighb_output  <<- mean(conv_neighb_output_aux/wind_switch, na.rm = T)
                }
                
            }
            if(length(sust_neighb)==0){
                sust_neighb_output          <<- 0
            }else{
                # fitness measure of sustainable
                if(flag_type_switch==1){
                    sust_neighb_output      <<- (sum(apply(output[,,(t-wind_switch+1):t,p],1:2,sum)[sust_neighb])) / (wind_switch*nrow(sust_neighb))
                }else{
                    sust_neighb_output_aux  <<- apply(output[,,(t-wind_switch+1):t,p]/L[,,(t-wind_switch+1):t,p],1:2,sum,na.rm = T)[sust_neighb]
                    sust_neighb_output      <<- mean(sust_neighb_output_aux/wind_switch, na.rm = T)
                }
                
            }
            
            #### dynamic tau
            if(flag_tau==1){                                                    # conventional higher tau
                if(agri[property[1,"row"],property[1,"col"],t,p]==1){
                    tau_l <- tau*(1 + tau_delta)
                }
                if(agri[property[1,"row"],property[1,"col"],t,p]==2){
                    tau_l <- tau*(1 - tau_delta)
                }
            }else{
                if(flag_tau==2){                                                # conventional lower tay
                    if(agri[property[1,"row"],property[1,"col"],t,p]==1){
                        tau_l <- tau*(1 - tau_delta)
                    }
                    if(agri[property[1,"row"],property[1,"col"],t,p]==2){
                        tau_l <- tau*(1 + tau_delta)
                    }
                }else{                                                          # all equal
                    tau_l <<- tau
                }
            }
            #### 
            
            m_conv <<- exp(tau_l*conv_neighb_output)/( exp(tau_l*conv_neighb_output) + exp(tau_l*sust_neighb_output) )
            m_sust <<- exp(tau_l*sust_neighb_output)/( exp(tau_l*conv_neighb_output) + exp(tau_l*sust_neighb_output) )
            
            if(agri[property[1,"row"],property[1,"col"],t,p]==1 & rbernoulli(1, (m_sust-0.5)*2)==TRUE & (m_sust>m_conv)){
                o <<- 1
                for (o in 1:nrow(property)) {
                    agri[property[o,"row"],property[o,"col"],t,p]   <<- 2
                }
                switch_counter[p]                                   <<- switch_counter[p] + 1
            }
            if(agri[property[1,"row"],property[1,"col"],t,p]==2 & rbernoulli(1, (m_conv-0.5)*2)==TRUE & (m_conv>m_sust)){
                o <<- 1
                for (o in 1:nrow(property)) {
                    agri[property[o,"row"],property[o,"col"],t,p]   <<- 1
                }
                switch_counter[p]                                   <<- switch_counter[p] + 1
            }
        }
    }
}