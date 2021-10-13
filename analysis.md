---
title: "AgriLOWE Simulation Report"
author: "Matteo Coronese, Martina Occelli, Francesco Lamperti, Andrea Roventini"
date: "23/6/2018"
output: 
  html_document: 
    keep_md: yes
---






# Parametrization

### Baseline


Table: Labor

 alpha   ud_sens   wealth_buffer
------  --------  --------------
   0.8       0.3             0.7



Table: Market Behavior

 eps_price   eps_share   omega_cost   omega_unfilled
----------  ----------  -----------  ---------------
      0.02         0.1         0.05             0.95



Table: Miscellaneous

 demand_rate   price_land_var
------------  ---------------
           4             0.05



Table: Cells on Sale

 rho   psi   min_land_price   wealth_share_bid   ud_mean_time
----  ----  ---------------  -----------------  -------------
 0.3    50              0.3                0.1              5



Table: Switching Behavior

 ray   tau   switch_time   wind_switch
----  ----  ------------  ------------
   2     1            20            20



Table: Innovation

 rd_effort   theta_min   theta_max   agri_growth_penalty   iota
----------  ----------  ----------  --------------------  -----
       0.1        -0.2         0.4                  0.03      2



Table: Imitation

 imit_effort   imit_frac   mu_imit   mu_band
------------  ----------  --------  --------
        0.05        0.01      0.01      0.01



Table: Initialization

 forest_theta_gain   price_deforestation   time_to_forest   def_rho
------------------  --------------------  ---------------  --------
              0.05                     0               50       0.1



Table: Initialization

 price_conv   theta_init   theta_init_pen_sust   var_theta_init
-----------  -----------  --------------------  ---------------
          2            2                   0.7                1



Table: Flags

 flag_agri_init   flag_prod   flag_auction   flag_imit   flag_init_prod   land_degr_flag
---------------  ----------  -------------  ----------  ---------------  ---------------
              2           1              1           1                1                1



 flag_deforestation   flag_central_forest   flag_switch_own_prop   flag_type_switch   flag_land_abandon
-------------------  --------------------  ---------------------  -----------------  ------------------
                  0                     1                      1                  0                   0

### Soil Deterioration

**Land Degradation**: None



### Climate Box


Table: Climate Box

 alpha_drought   beta_drought   alpha_flood   beta_flood
--------------  -------------  ------------  -----------
           0.2              4           0.4            4

<img src="analysis_files/figure-html/climate_box_par-1.png" width="1000px" />

**Active Climate Shocks:** Flood

# Summary Graphs

### Cell Variables

<img src="analysis_files/figure-html/summary cell-1.png" width="1000px" />






### Producer Variables

<img src="analysis_files/figure-html/summary producer-1.png" width="1000px" />


### Time Series

<img src="analysis_files/figure-html/summary time series-1.png" width="1000px" />

# Miscellaneous

### Output

<img src="analysis_files/figure-html/output-1.png" width="1000px" /><img src="analysis_files/figure-html/output-2.png" width="1000px" /><img src="analysis_files/figure-html/output-3.png" width="1000px" />
**Mean output growth per time step is** 0.43%.

**Pseudo mean output growth per time step is** 1.92%.

### Market Behavior



**Market for defaulted firms**: Auction


Table: Cell Market

 mean_bankruptcy   mean_bankruptcy_per_time   share_sold   share_rebound
----------------  -------------------------  -----------  --------------
              37                  0.0616667    0.8918919       0.1081081

<img src="analysis_files/figure-html/market II-1.png" width="1000px" /><img src="analysis_files/figure-html/market II-2.png" width="1000px" /><img src="analysis_files/figure-html/market II-3.png" width="1000px" /><img src="analysis_files/figure-html/market II-4.png" width="1000px" />


### Agricolture

<img src="analysis_files/figure-html/agriculture-1.png" width="1000px" /><img src="analysis_files/figure-html/agriculture-2.png" width="1000px" /><img src="analysis_files/figure-html/agriculture-3.png" width="1000px" />
**Mean agriculture switching per time step is**
0.1033333

Conventional Lock-In Probability: NA

Sustainable Lock-In Probability: NA


# Detailed Graphs (Each Farmer -Single Run)

<img src="analysis_files/figure-html/farm level-1.png" width="1000px" />

# Spatial Graphs

<img src="analysis_files/figure-html/spatial graphs-1.png" width="1000px" /><img src="analysis_files/figure-html/spatial graphs-2.png" width="1000px" /><img src="analysis_files/figure-html/spatial graphs-3.png" width="1000px" />
Cell Correlation with Initial Land Productivity 0.4171691.

