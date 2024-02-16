library(data.table)
library(tidyverse)

path <- "../agrilowe/sussub_sust_500/" 


macro_var <- 1                                          #set to 1 if only macro variables were saved

exp <- c(
    "sussub0.1_time100",
    "sussub0.1_sust0.1",
    "sussub0.1_sust0.2",
    "sussub0.1_sust0.3",
    
    "sussub0.2_time100",
    "sussub0.2_sust0.1",
    "sussub0.2_sust0.2",
    "sussub0.2_sust0.3",
    
    "sussub0.3_time100",
    "sussub0.3_sust0.1",
    "sussub0.3_sust0.2",
    "sussub0.3_sust0.3",
    
    "sussub0.4_time100",
    "sussub0.4_sust0.1",
    "sussub0.4_sust0.2",
    "sussub0.4_sust0.3",
    
    "sussub0.5_time100",
    "sussub0.5_sust0.1",
    "sussub0.5_sust0.2",
    "sussub0.5_sust0.3"
)

############################################################################################################

if(macro_var == 1){                                     #grabbed variables
    vs <- c(
        "data",
        "data_t",
        "data_p",
        "data_market_conc"
    )
}else{
    vs <- c(
        "data",
        "data_p",
        "data_t"
    )
}

vsc <- c("sales_counter",                               #grabbed counters
         "bankrupt_counter",  
         "rebound_counter",   
         "switch_counter",    
         "abandon_counter")


for(simul in 1:length(exp)){
    load(paste(path,exp[simul],".RData", sep=""))
    print(paste("Loaded Simulation", simul))
    
    #create differently named objectes for different objects from different experiments
    for(i in 1:length(vs)){
        aux_simul<-as.data.frame(rep(simul,nrow(get(vs[i]))))
        colnames(aux_simul)[1] <- "simul"
        temp_data<-bind_cols(get(vs[i]),aux_simul)
        
        assign(paste(vs[i],"_",simul,sep = "") , temp_data)
        remove(list=vs[i]) #remove original object
    }
    
    #separate loop for counters
    for(k in 1:length(vsc)){
        assign(paste(vsc[k],"_",simul,sep = "") , cbind(get(vsc[k]),simul))
        remove(list=vsc[k]) #remove original object
    }
    
}
remove(temp_data)
remove(aux_simul)
#bind together object
i <- 1
for(i in 1:length(vs)){
    temp_list <- list()
    simul <- 1
    for(simul in 1:length(exp)){
        temp_list[[simul]] <- get(paste(vs[i],"_",simul,sep = "")) #transfer copied object into a list
        remove(list=paste(vs[i],"_",simul,sep = "")) #remove copied object
    }
    assign(vs[i],rbindlist(temp_list)) #collapse list into one dataframe
    
}

#bind together counters
k <- 1
for(k in 1:length(vsc)){
    simul <- 1
    for(simul in 1:length(exp)){
        if(simul==1){#if first simul, create object
            assign(vsc[k],get(paste(vsc[k],"_",simul,sep = "")) )
        }else{#otherwise, bind on it
            assign(vsc[k],rbind(get(vsc[k]), get(paste(vsc[k],"_",simul,sep = ""))))
        }
        remove(list=paste(vsc[k],"_",simul,sep = ""))
    }
}

