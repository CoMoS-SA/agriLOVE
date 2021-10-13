#### FLAGS

flag_climate          <- 0                  # 0: baseline model, no shock; 1: drought, shock on land productivity; 2: flood, shock on output; 3: both. If using the single shock climate box, if 2 then flood on, if 4 only partial suspension of set.seed (for baseline) is on.
no_land_degradation   <- 1                  # if 1, no land-degradation whatsoever. (To investigate macro engine)
flag_agri_init        <- 0                  # if 1, 50% sustainable, if 2 25% susustainable, if 3 15% sustainable,  if 0 no sustainables. 
flag_prod             <- 1                  # regulates initial productivities (see initialization file)
flag_auction          <- 1                  # if 1, auction activated. Otherwise, only mean farm entering
flag_imit             <- 1                  # if 0, no imitation. if 1, k+s style imitation.
flag_init_prod        <- 1                  # if 1, one producer per cell initialization; if 2, a fraction of producer has two cells, everything with same initial mkt_shares; if 3, like 2 but who owns two cells begins with double mkt_share
land_degr_flag        <- 1                  # if 1 logistic deterministic loss; otherwise, depending on past output extracted
flag_deforestation    <- 0                  # if 1, deforestation allowed for conventional famers
flag_central_forest   <- 1                  # if 1, forests placed centrally in the grid
flag_switch_own_prop  <- 1                  # if 1, own property is included in evaluating relative performances of sust vs conv
flag_type_switch      <- 0                  # if 1, based on output; otherwise, based on output/labor
flag_land_abandon     <- 1                  # if 1, land abandonement when nobody bids at the auction; otherwise, random incumbent copy with mean market share
scen_flag             <- 0                  # if 0, usual initialization file. Otherwise: 1) central forest, random sust (baseline) 2) random forest, random sust 3) central forest, ring sust 4) random forest, ring sust 5) ring forest, central sust 6) random forest, ring sust
flag_def_policy       <- 0                  # if different from 0, deforestation policy active
flag_conv_tax         <- 0                  # if 1, tax on conventionals active, proportional to revenues. If f 2, proportional to profits
 
flag_clustered_agri   <- 0                  # if 1, sustainables are forced in the north, if 2 are forced in the center, otherwise random (works also with central forest)
flag_suspend_ergodic  <- 0                  # if 1, deforestation, land abandonment and auction are suspended during transient (lenght set in parameters). Set to 0 if deforestation and land abandonment are off! Note, switch and soil degradation are always suspended. 
flag_tau              <- 0                  # if 1, conventional have higher tau, sustainable lower tau. If 2, the other way around. Proportions set via tau_delta. Otherwise, tau equal for all agents.

flag_shock_cell       <- 0                  # see parametrization
