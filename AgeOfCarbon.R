AgeOfCarbon <- function(AgeParameters,Inits,Days,RunDynamics){
  # AgeParameters from GetAgeParameters
  # Inits is a data frame of initial values for the state variable vectors
  # Days = number of days over which to run dynamics
  # RunDynamics = boolean, whether to run dynamics or just steady state solution
  # Age of water and Carbon, R version, July 2017
  # Adapted from Duffy, CJ, HA Dugan, and PC Hanson, 2018, 
  #     The age of water and carbon in lake catchments: a simple dynamical model, 
  #     Limnology & Oceanography Letters
  
  # The following are conventions used in equations in T 1.5
  # and adjusted for correct units
  A1 = AgeParameters$A1  # Da, Catchment area
  A1A2 = AgeParameters$A1A2
  A2 = AgeParameters$A2 # La, Lake area
  d1 = AgeParameters$d1   # Db (5,20), Catchment average storage depth
  d2 = AgeParameters$d2   # Mean lake depth
  
  Q1P = AgeParameters$Q1P  # MaP, Catchment precipitation, m/d
  Q2P = AgeParameters$Q2P  # MaP, Lake precipitation, m/d
  QPe = AgeParameters$QPe # Mean annual potential ET: m/d
  Qe1 = AgeParameters$Qe1  # 0.00207, Catchment evapotranspiration, m/d
  Qe2 = AgeParameters$Qe2  # Lake evapotranspiration, m/d
  Q1 = AgeParameters$Q1  # Outflow, m/d
  Q2 = AgeParameters$Q2  # Outflow, m/d
  
  C1P = AgeParameters$C1P # Precipitation concentration, g/m3
  C2P = AgeParameters$C2P  # Precipitation concentration on lake, g/m3
  s = AgeParameters$s    # Equivalent concentration of labile pool
#   C1e = AgeParameters$C1e  # Evaporation concentration over lake
#   C2e = AgeParameters$C2e  # Evaporation concentration over lake
  n = AgeParameters$n  # porosity
  kS =  AgeParameters$kS   # rate constant for desorption of soil labile carbon
  Kr1 = AgeParameters$Kr1 # 0.0004, rate constant for catchment carbon respiration
  Kr2 = AgeParameters$Kr2  # rate constant for lake carbon respiration
  Kb2 = AgeParameters$Kb2  # rate constant for lake carbon burial
  m = AgeParameters$m       # constant of recalcitrant carbon fraction
  # f = AgeParameters$f       # volume fraction of the soil carbon pool
  
  # Initialize dynamics
  D = data.frame(C1=rep(NA,Days),C2=rep(NA,Days),s=rep(NA,Days),Alpha1=rep(NA,Days),Alpha2=rep(NA,Days),
                        Beta=rep(NA,Days),Age1=rep(NA,Days),Age2=rep(NA,Days),AgesS=rep(NA,Days))

  if (RunDynamics){
    D$C1[1] = Inits$C1
    D$C2[1] = Inits$C2
    D$s[1] =  Inits$s
    D$Alpha1[1] = Inits$Alpha1
    D$Alpha2[1] = Inits$Alpha2
    D$Beta[1] = Inits$Beta
    D$Age1[1] = Inits$Age1
    D$Age2[1] = Inits$Age2
    D$AgeS[1] = Inits$AgeS
  }

  # Initialize steady-state vars
  SS = data.frame(RunoffRatio=NA,EvapRatio = NA,C1ave = NA,SSCave = NA,
                           C2ave = NA,Tau1 = NA,Tau2System = NA,TauS = NA,TauLake = NA)
  
  ###################################
  # Begin model
  if (RunDynamics){
    for (i in 2:Days){
      # Dynamics for catchment carbon
      D$C1[i] = D$C1[i-1] + Q1P/(n*d1)*(C1P-D$C1[i-1]) + Qe1/(n*d1)*D$C1[i-1] - kS*(D$C1[i-1]-m*D$s[i-1]) - Kr1*D$C1[i-1]
      
      # Dynamics of solid state in catchment
      D$s[i] = D$s[i-1] + kS*(D$C1[i-1] - m*D$s[i-1])
      
      # Dynamics for lake carbon
      D$C2[i] = D$C2[i-1] + Q2P/d2*(C2P-D$C2[i-1]) + Qe2/d2*D$C2[i-1] - 
        (A1*Q1)/(A2*d2)*(D$C2[i-1]-D$C1[i-1]) - Kr2*D$C2[i-1] - Kb2*D$C2[i-1]
      
      # Dynamics of catchment age concentration
      D$Alpha1[i] = D$Alpha1[i-1] + D$C1[i-1] - Q1P/(n*d1) * D$Alpha1[i-1] + Qe1/(n*d1)*D$Alpha1[i-1] - 
        kS*(D$Alpha1[i-1] - m*D$Beta[i-1]) - Kr1*D$Alpha1[i-1]
      
      D$Alpha2[i] = D$Alpha2[i-1] + D$C2[i-1] - Q1P/d2 * D$Alpha2[i-1] + Qe2/d2 * D$Alpha2[i-1] - 
        (A1*Q1)/(A2*d2) * (D$Alpha2[i-1]-D$Alpha1[i-1]) - Kr2 * D$Alpha2[i-1] - Kb2 * D$Alpha2[i-1]
      
      # Dynamics of beta
      D$Beta[i] = D$Beta[i-1] + m * s + kS*(D$Alpha1[i-1] - m * D$Beta[i-1])
      
      # Age at time t
      D$Age1[i] = D$Alpha1[i]/D$C1[i] # Catchment C
      D$Age2[i] = D$Alpha2[i]/D$C2[i] # Lake C
      D$AgeS[i] = D$Beta[i]/D$s[i] # Soil C
    }
  }
  # Steady state water balance
  SS$RunoffRatio = Q1P / QPe
  SS$EvapRatio = Qe1 / QPe
  
  # Steady state DOC for catchment
  SS$C1ave = C1P / (1 - Qe1 / Q1P + ((Kr1 * n * d1) / Q1P))
  
  # Steady-state carbon pool for catchment?
  SS$SSCave = SS$C1ave / m # g/m3
  
  # Steady state DOC for lake
  SS$C2ave = (Q1P / d2 * C1P + ((A1 * Q1) / (A2 * d2)) * SS$C1ave) /
    (Q1P / d2 - Qe2 / d2 + (A1 * Q1) / (A2 * d2) + Kr2 + Kb2)
  
  # Residence time for catchment, steady-state age
  SS$Tau1 = 2 / (Q1P / (n * d1) - Qe1 / (n * d1) + Kr1) # days
  
  # Steady-state age equation for the lake ***********check these
  SS$TauLake = 1/(Q1P/d2 - Qe2/d2 + (A1*Q1)/(A2*d2) + Kr2 + Kb2)
  
  # Steady state age equation for the system
  SS$Tau2System = 1/(Q1P/d2 - Qe2/d2 + (A1*Q1)/(A2*d2) + Kr2 + Kb2) + 
    (2*(A1*Q1)/(A2*d2)) /
    ((n*d1)/d2 * (Q1P/(n*d1) - Qe1/(n*d1) + Kr1)^2 + (A1*Q1)/(A2*d2)*(Q1P/(n*d1) - Qe1/(n*d1)+Kr1))
  
  # Steady-state age equation for the sediments
  SS$TauS = 1 / kS + 2 / (Q1P / (n * d1) - Qe1 / (n * d1) + Kr1) # days
  
  # End model
  ###################################
  
  AgeResults = list(D,SS)
  return(AgeResults)
}
