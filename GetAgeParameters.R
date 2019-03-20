GetAgeParameters <- function(LakeID){

  # LakeID, 1=Mendota, 2=Sunapee
  if (LakeID==1){ 
    # This one setup for phosphorus cycling and P-cycling meeting at L Sunapee
    # Assumed climate conditions to be same as Mendota
    # Age of water and Carbon, R version, July 2017
    # Adapted from Duffy, CJ, HA Dugan, and PC Hanson, In review, The age of water and carbon in lake catchments: a simple dynamical model, Limnology & Oceanography Letters
    AgeParameters = data.frame(A1=NA,A1A2=NA,d1=NA,d2=NA,Q1P=NA,QPe=NA,Qe1=NA,Qe2=NA,Q1=NA,Q2=NA,C1P=NA,
                               C2P=NA,s=NA,C1e=NA,C2e=NA,n=NA,V1=NA,V2=NA,kS=NA,Kr1=NA,Kr2=NA,Kb2=NA,m=NA,f=NA)
    # The following are conventions used in equations in T 1.5
    # and adjusted for correct units
    AgeParameters$A1 = 604 * 10^6  # Da, Catchment area (###Duffy )
    AgeParameters$A1A2 = NA #
    AgeParameters$A2 = 39.6 * 10^6 # La, Lake area
    AgeParameters$d1 = 5           # Db (5,20), Catchment average storage depth
    AgeParameters$d2 = 12.8        # Mean lake depth
    
    AgeParameters$Q1P = 0.00262005  # MaP, Catchment precipitation, m/d
    AgeParameters$Q2P = 0.00262005  # MaP, Lake precipitation, m/d
    AgeParameters$QPe = 0.0023862 # Mean annual potential ET: m/d
    AgeParameters$Qe1 = 0.00177772  # 0.00207, Catchment evapotranspiration, m/d
    AgeParameters$Qe2 = 0.00177772  # Lake evapotranspiration, m/d
    AgeParameters$Q1 = AgeParameters$Q1P-AgeParameters$Qe1  # Outflow, m/d
    AgeParameters$Q2 = AgeParameters$Q2P-AgeParameters$Qe2  # Outflow, m/d
    
    AgeParameters$C1P = 2  # 4, Precipitation concentration, g/m3 (###Duffy 2)
    #AgeParameters$C2P = 5  # Precipitation concentration on lake, g/m3
    AgeParameters$C2P = AgeParameters$C1P # for now, these must be set equal
    AgeParameters$s = 100    # Equivalent concentration of labile pool
    AgeParameters$C1e = 0  # Evaporation concentration over lake
    AgeParameters$C2e = 0  # Evaporation concentration over lake
    AgeParameters$n = 0.1  # porosity
    
    AgeParameters$kS =  0.001   # rate constant for desportion of soil labile carbon
    AgeParameters$Kr1 = 0.0004 # 0.0004, rate constant for catchment carbon respiration
    AgeParameters$Kr2 = 0.0008  # 0.0008, rate constant for lake carbon respiration
    AgeParameters$Kb2 = 0.00026  # 0.00026, rate constant for lake carbon burial
    AgeParameters$m = 0.1       # constant of recalcitrant carbon fraction
  }  
  if (LakeID==2){ #Sunapee
    # Age of water and Carbon, R version, July 2017
    # Adapted from Duffy, CJ, HA Dugan, and PC Hanson, In review, The age of water and carbon in lake catchments: a simple dynamical model, Limnology & Oceanography Letters
    AgeParameters = data.frame(A1=NA,A1A2=NA,d1=NA,d2=NA,Q1P=NA,QPe=NA,Qe1=NA,Qe2=NA,Q1=NA,Q2=NA,C1P=NA,
                               C2P=NA,s=NA,C1e=NA,C2e=NA,n=NA,V1=NA,V2=NA,kS=NA,Kr1=NA,Kr2=NA,Kb2=NA,m=NA,f=NA)
    # The following are conventions used in equations in T 1.5
    # and adjusted for correct units
    AgeParameters$A1 = 142 * 10^6  # Da, Catchment area (###Duffy )
    AgeParameters$A1A2 = NA #
    AgeParameters$A2 = 16.8 * 10^6 # La, Lake area
    AgeParameters$d1 = 3           # Db (5,20), Catchment average storage depth
    AgeParameters$d2 = 11.8        # Mean lake depth
    
    AgeParameters$Q1P = 0.00262005  # MaP, Catchment precipitation, m/d
    AgeParameters$Q2P = 0.00262005  # MaP, Lake precipitation, m/d
    AgeParameters$QPe = 0.0023862   # Mean annual potential ET: m/d
    AgeParameters$Qe1 = 0.00177772  # 0.00207, Catchment evapotranspiration, m/d
    AgeParameters$Qe2 = 0.00177772  # Lake evapotranspiration, m/d
    AgeParameters$Q1 = AgeParameters$Q1P-AgeParameters$Qe1  # Outflow, m/d
    AgeParameters$Q2 = AgeParameters$Q2P-AgeParameters$Qe2  # Outflow, m/d
    
    AgeParameters$C1P = 6 #, using 6 and 15 and 32 for sunapee, 3.5 #2  # 4, Precipitation concentration, g/m3 (###Duffy 2)
    #AgeParameters$C2P = 5  # Precipitation concentration on lake, g/m3
    AgeParameters$C2P = AgeParameters$C1P # for now, these must be set equal
    AgeParameters$s = 100    # 100, Equivalent concentration of labile pool
    AgeParameters$C1e = 0  # Evaporation concentration over lake
    AgeParameters$C2e = 0  # Evaporation concentration over lake
    AgeParameters$n = 0.1  # porosity
    
    AgeParameters$kS =  0.000   # 0.001, rate constant for desorption of soil labile carbon
    AgeParameters$Kr1 = 0.000 # 0.0004, rate constant for catchment carbon respiration
    AgeParameters$Kr2 = 0.000  # 0.0008, rate constant for lake carbon respiration
    AgeParameters$Kb2 = 0.002  # 0.00026, rate constant for lake carbon burial
    AgeParameters$m = 0.1       # constant of recalcitrant carbon fraction
  }

  return(AgeParameters)
}