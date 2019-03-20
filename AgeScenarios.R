# Age of water and Carbon, R version, July 2017
# Adapted from Duffy, CJ, HA Dugan, and PC Hanson, In review, The age of water and carbon in lake catchments: a simple dynamical model, Limnology & Oceanography Letters

source('./AgeOfCarbon.R')
source('./GetAgeParameters.R')
source('./Budyko.R')
#######################
# User parameters
# Scenarios: 1=single run; 2=Current, wet, dry; 2.5, same as 2 but Budyko
#            3=La:Wa gradient; 4=rain gradient; 5=sampling parameters; 
#            6=single run with conservative tracer; 7=Across Budyko gradient
myLakeID = 2 # 1=Mendota, 2=Sunapee
Scenario = 6 #2.5
Days = 3650 #10000 # Number of days to run dynamics
AxesCex = 0.9 # Try 0.6 for png, 0.9 for non png
Save2File = FALSE
LegendOn = FALSE
LegendCex = 0.7 # Try 0.7 for non png
# LOCCalVal = 5 # Lake Mendota OC calibration value, mg/L
LabelLake = "Lake OC (mgC/L)" # "Lake P (ug/L)"  #
LabelCatchment = "Catchment OC (mgC/L)" # "Catchment P (ug/L)" #

# End user parameters
#######################

# Initial values for state variables
Inits = data.frame(C1=NA,C2=NA,s=NA,Alpha1=NA,Alpha2=NA,Beta=NA,Age1=NA,Age2=NA,AgeS=NA)
Inits$C1 = 15, Duffy 50, 19 is a good starting concentration for Sunapee P
Inits$C2 = 7, Duffy 10, 5 is a good starting concentration for Sunapee P
Inits$s = 100 # Duffy 100
Inits$Alpha1 = 0
Inits$Alpha2 = 0
Inits$Beta = 0
Inits$Age1 = 0
Inits$Age2 = 0
Inits$AgeS = 0

###############################
# Scenario section
# Each scenario has setup and
# actually runs the model

myR = list() # All scenario results go in this list

if (Scenario==1){
  # Run once
ScenarioLabels = c('Current')
  RunDynamics = TRUE # Whether to run time dynamics
  # Set y limitis for figures
  SYLim = c(0,200) # Sediment
  CYLim = c(0,20) # Catchment
  LYLim = c(0,20) # Lake
  aSYLim = c(0,5) # Same, but for age
  aCYLim = c(0,5)
  aLYLim = c(0,5)
  # Run this scenario
  print('Run 1...')
  myParams = GetAgeParameters(myLakeID)
  myR[[1]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
}

if (Scenario==2){
  # Run Current, wet, dry scenarios
  ScenarioLabels = c('Current          ','Wet','Dry')
  RunDynamics = TRUE # Whether to run time dynamics
  # Set y limitis for figures
  SYLim = c(50,200)
  CYLim = c(5,50)
  LYLim = c(0,25)
  aSYLim = c(0,8)
  aCYLim = c(0,8)
  aLYLim = c(0,8)
  
  # Run Current's scenario
  print('Run 1...')
  myParams = GetAgeParameters(myLakeID)
  myR[[1]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
  # Run wet scenario
  myParams = GetAgeParameters(myLakeID)
  myParams$Q1P = 0.0038537 
  myParams$Q2P = 0.0038537
  myParams$Qe1 = 0.00203543
  myParams$Qe2 = 0.00203543
  myParams$Q1 = myParams$Q1P - myParams$Qe1
  myParams$Q2 = myParams$Q2P - myParams$Qe2
  myParams$QPe = 0.0023862
  print('Run 2...')
  myR[[2]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
  # Run dry scenario
  myParams = GetAgeParameters(myLakeID)
  myParams$Q1P = 0.00158921
  myParams$Q2P = 0.00158921
  myParams$Qe1 = 0.00133389
  myParams$Qe2 = 0.00133389
  myParams$QPe = 0.0023862
  myParams$Q1 = myParams$Q1P - myParams$Qe1
  myParams$Q2 = myParams$Q2P - myParams$Qe2
  print('Run 3...')
  myR[[3]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
}
if (Scenario==2.5){
  # Just like 2, but use Budyko function to get Qe
  # Run Current, wet, dry scenarios
  ScenarioLabels = c('Current          ','Wet','Dry')
  RunDynamics = TRUE # Whether to run time dynamics
  # Set y limitis for figures
  SYLim = c(50,200)
  CYLim = c(5,15)
  LYLim = c(0,10)
  aSYLim = c(0,8)
  aCYLim = c(0,8)
  aLYLim = c(0,8)
  
  myParams = GetAgeParameters(myLakeID)
  myBudyko = Budyko(myParams$QPe,c(myParams$Q1P,0.0038537,0.00158921),2,TRUE)
  # Run Current's scenario
  print('Run 1...')
  myParams = GetAgeParameters(myLakeID)
  myR[[1]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
  # Run wet scenario
  myParams = GetAgeParameters(myLakeID)
  myParams$Q1P = 0.0038537 
  myParams$Q2P = 0.0038537
  myParams$Qe1 = myBudyko$Qe[2]
  myParams$Qe2 = myBudyko$Qe[2]
  myParams$Q1 = myParams$Q1P - myParams$Qe1
  myParams$Q2 = myParams$Q2P - myParams$Qe2
  myParams$QPe = 0.0023862
  print('Run 2...')
  myR[[2]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
  # Run dry scenario
  myParams = GetAgeParameters(myLakeID)
  myParams$Q1P = 0.00158921
  myParams$Q2P = 0.00158921
  myParams$Qe1 = myBudyko$Qe[3]
  myParams$Qe2 = myBudyko$Qe[3]
  myParams$QPe = 0.0023862
  myParams$Q1 = myParams$Q1P - myParams$Qe1
  myParams$Q2 = myParams$Q2P - myParams$Qe2
  print('Run 3...')
  myR[[3]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
}

if (Scenario==3){
  # Run Current, but with gradient of lake areas
  A1A2 = seq(0.01,10,0.01) # La = Wa * A1A2
  ScenarioLabels = c('La:Wa gradient')
  RunDynamics = FALSE # Whether to run time dynamics
  Days = 0 # Days
  # Set y limitis for figures

  # Run Current's scenario
  print('Running area gradient...')
  myParams = GetAgeParameters(myLakeID)
  for (iA in 1:length(A1A2)){
    myParams$A2 = A1A2[iA] * myParams$A1
    myR[[iA]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
  }
}

if (Scenario==4){
  # Run Current, but with Rain OC concentration gradient
  CRain = seq(0.1,100,0.1)
  ScenarioLabels = c('C Rain gradient')
  RunDynamics = FALSE # Whether to run time dynamics
  Days = 0 # Days
  # Set y limitis for figures

    # Run Current's scenario
  print('Running area gradient...')
  myParams = GetAgeParameters(myLakeID)
  for (iA in 1:length(CRain)){
    myParams$C1P = CRain[iA]
    myR[[iA]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
  }
}

if (Scenario==5){
  # Sample two or more parameters
  nSamples = 3000
  #P2S = c(5,21) # parameters to sample, Q1P=5,Kr2=21
  P2S = c(5,16)
  LowerUpper = list(c(0.8,1.5),c(0.8,1.5)) # multiplicative of the default for respective parameter

  myR = list()
  myParams = GetAgeParameters(myLakeID)
  newParams = matrix(data=NA,nrow=length(P2S),ncol=nSamples)
  ParamNames = colnames(myParams)[P2S]
  for (iP2S in 1:length(P2S)){
    Lbound = myParams[P2S[iP2S]][1,1]*LowerUpper[[iP2S]][1]
    Ubound = myParams[P2S[iP2S]][1,1]*LowerUpper[[iP2S]][2]
    newParams[iP2S,] = runif(nSamples, min=Lbound, max=Ubound) 
  }
  ScenarioLabels = c('Parameter sampling')
  RunDynamics = FALSE # Whether to run time dynamics
  Days = 0 # Days
  # Set y limitis for figures

  print('Running parameter sampling...')
  myParams = GetAgeParameters(myLakeID)
  for (iA in 1:nSamples){
    for (iP in 1:length(P2S)){
      myParams[P2S[iP]] = newParams[iP,iA]
    }
    myParams$Q1 = myParams$Q1P - myParams$Qe1
    myParams$Q2 = myParams$Q2P - myParams$Qe2
    myR[[iA]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
  }
}

if (Scenario==6){
  # Run Current, along with conservative tracer
  ScenarioLabels = c('Current          ','Tracer')
  RunDynamics = TRUE # Whether to run time dynamics
  # Set y limitis for figures
  SYLim = c(50,150)
  CYLim = c(0,100)
  LYLim = c(0,25)
  aSYLim = c(0,8)
  aCYLim = c(0,8)
  aLYLim = c(0,8)
  
  # Run Current's scenario
  print('Run 1...')
  myParams = GetAgeParameters(myLakeID)
  myR[[1]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
  
  # Run tracer
  myParams = GetAgeParameters(myLakeID)
  #myParams$kS  = 0
  myParams$Kr1 = 0
  myParams$Kr2 = 0
  myParams$Kb2 = 0
  print('Run 2...')
  myR[[2]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
}

if (Scenario == 7){
  # Run over gradient of precip, using Budyko
  myParams = GetAgeParameters(myLakeID)
  # Define precipitation range
  QP = seq(0.001,0.004,0.0001)
  nScale = 3 # scaling parameter for Budyko
  QPe = myParams$QPe
  myBudyko = Budyko(QPe,QP,nScale,TRUE)
  Qe = myBudyko$Qe
  
  ScenarioLabels = c('Precip Gradient via Budyko')
  RunDynamics = FALSE # Whether to run time dynamics
  Days = 0 # Days
  # Set y limitis for figures
  
  # Run scenarios
  print('Running Budyko gradient...')
  for (iA in 1:length(QP)){
    myParams$Q1P = QP[iA] 
    myParams$Q2P = QP[iA]
    myParams$Qe1 = Qe[iA]
    myParams$Qe2 = Qe[iA]
    myParams$Q1 = myParams$Q1P - myParams$Qe1
    myParams$Q2 = myParams$Q2P - myParams$Qe2
    
    myR[[iA]] = AgeOfCarbon(myParams,Inits,Days,RunDynamics)
  }
}

###########################
# Process scenario results

# Print steady state values
# All scenarios
nRuns = length(myR)
if (nRuns < 20){
  for (iR in 1:nRuns){
    mySS = myR[[iR]][[2]]
    print('=========================')
    print(paste('Scenarion:',ScenarioLabels[iR]))
    print(paste('Steady-state DOC for catchment, lake (g/m3):',format(mySS$C1ave,digits=3),',',format(mySS$C2ave,digits=3)))
    print(paste('Steady-state Age for catchment, lake (y):',format(mySS$Tau1/365,digits=3),',',format(mySS$Tau2System/365,digits=3)))
  }
}

# All scenarios that run dynamics
if (RunDynamics) {
    # Determine number of runs
    nRuns = length(myR)
    myCols = c('black','red','blue','green')
    if (Scenario==6){
      myCols = c('black','grey','blue','green')
    }
    #############################
    # Plot concentration results
    par(
      mfrow = c(2,1),
      oma = c(1, 1, 1, 1), # four rows of text at the outer left and bottom margin
      mar = c(3.7, 3.7, 0.5, 0.0), # 0.5, 3.7, 0.5, 0.0 space for one row of text at ticks and to separate plots
      mgp = c(2.2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
      xpd = FALSE,
      cex = AxesCex
    )            # allow content to protrude into outer margin (and beyond)
    
#     for (iR in 1:nRuns) {
#       #Panel one, sediments
#       myD = myR[[iR]][[1]]
#       if (iR == 1) {
#         plot(1:Days / 365,myD$s,ylim = SYLim,type = 'l',lwd = 2,col = myCols[iR],xaxt = 'n',xlab = "",ylab = "Soil OC (mgC/L)",main = "")
#         abline(h=myR[[iR]][2][[1]]$SSave,lty=2,col=myCols[iR])
#       }else{
#         lines(1:Days / 365,myD$s,type = 'l',lwd = 2,col = myCols[iR])
#         abline(h=myR[[iR]][2][[1]]$SSave,lty=2,col=myCols[iR])
#       }
#     }
#     #myLegend = c(expression('probe, r'[p]*'/r'[w]*' = 5.0'),expression('probe, r'[p]*'/r'[w]*' = 0.5'))
#     legend("topright",y = NULL,ScenarioLabels,lty = rep(1,nRuns),col = myCols[1:nRuns],cex=LegendCex)
    
    for (iR in 1:nRuns) {
      #Panel 2, catchment
      myD = myR[[iR]][[1]]
      if (iR == 1) {
        plot(1:Days / 365,myD$C1,ylim = CYLim,type = 'l',lwd = 2,col = myCols[iR],xaxt = 'n',xlab ="",ylab = LabelCatchment,main = "")
        abline(h=myR[[iR]][2][[1]]$C1ave,lty=2,col=myCols[iR])
      }else{
        lines(1:Days / 365,myD$C1,type = 'l',lwd = 2,col = myCols[iR])
        abline(h=myR[[iR]][2][[1]]$C1ave,lty=2,col=myCols[iR])
      }
    }
    if (LegendOn){legend("topright",y = NULL,ScenarioLabels,lty = rep(1,nRuns),col = myCols[1:nRuns],cex=LegendCex)}
    for (iR in 1:nRuns) {
      #Panel 3, lake
      myD = myR[[iR]][[1]]
      if (iR == 1) {
        plot(1:Days / 365,myD$C2,ylim = LYLim,type = 'l',lwd = 2,col = myCols[iR],xlab = 'Years', ylab =LabelLake,main = "")
        abline(h=myR[[iR]][2][[1]]$C2ave,lty=2,col=myCols[iR])
      }else{
        lines(1:Days / 365,myD$C2,type = 'l',lwd = 2,col = myCols[iR])
        abline(h=myR[[iR]][2][[1]]$C2ave,lty=2,col=myCols[iR])
      }
      abline(h=12,lty=2,col='blue')
      abline(h=24,lty=2,col='green')
    }
    if (LegendOn){legend("topright",y = NULL,ScenarioLabels,lty = rep(1,nRuns),col = myCols[1:nRuns],cex=LegendCex)}

    if (Save2File){
      fName = paste('Concentration_','Scenario',Scenario,'.png',sep="")
      dev.copy(png,fName,width=3,height=4.0,units="in",res=300)
      dev.off()
    }
    
    #############################
    # Plot System Age results
    par(
      mfrow = c(2,1),
      oma = c(1, 1, 1, 1), # four rows of text at the outer left and bottom margin
      mar = c(3.7, 3.7, 0.5, 0.0), # 0.5, 3.7, 0.5, 0.0 space for one row of text at ticks and to separate plots
      mgp = c(2.2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
      xpd = FALSE,
      cex = AxesCex
    )            # allow content to protrude into outer margin (and beyond)
    
#     for (iR in 1:nRuns) {
#       #Panel one, sediments
#       myD = myR[[iR]][[1]]
#       if (iR == 1) {
#         plot(1:Days / 365,myD$AgeS / 365,ylim = aSYLim,type = 'l',col = myCols[iR],xaxt ='n',xlab = "",ylab = "Sediment age (y)",main = "")
#         abline(h=myR[[iR]][2][[1]]$TauS/365,lty=2,col=myCols[iR])
#       }else{
#         lines(1:Days / 365,myD$AgeS / 365,type = 'l',col = myCols[iR])
#         abline(h=myR[[iR]][2][[1]]$TauS/365,lty=2,col=myCols[iR])
#       }
#     }
#     legend("topright",y = NULL,ScenarioLabels,lty = rep(1,nRuns),col = myCols[1:nRuns],cex=LegendCex)
    
    for (iR in 1:nRuns) {
      #Panel 2, catchment
      myD = myR[[iR]][[1]]
      if (iR == 1) {
        plot(1:Days / 365,myD$Age1 / 365,ylim = aCYLim,type = 'l',col = myCols[iR],xaxt ='n',xlab = "",ylab = "Catchment OC age (y) ",main = "")
        abline(h=myR[[iR]][2][[1]]$Tau1/365,lty=2,col=myCols[iR])
      }else{
        lines(1:Days / 365,myD$Age1 / 365,type = 'l',col = myCols[iR])
        abline(h=myR[[iR]][2][[1]]$Tau1/365,lty=2,col=myCols[iR])
      }
    }
    if (LegendOn){legend("topright",y = NULL,ScenarioLabels,lty = rep(1,nRuns),col = myCols[1:nRuns],cex=LegendCex)}
    
    for (iR in 1:nRuns) {
      #Panel 3, lake
      myD = myR[[iR]][[1]]
      if (iR == 1) {
        plot(1:Days / 365,myD$Age2 / 365,ylim = aLYLim,type = 'l',col = myCols[iR],xlab ='Years', ylab = "System OC age (y)",main = "")
        abline(h=myR[[iR]][2][[1]]$Tau2System/365,lty=2,col=myCols[iR])
      }else{
        lines(1:Days / 365,myD$Age2 / 365,type = 'l',col = myCols[iR])
        abline(h=myR[[iR]][2][[1]]$Tau2System/365,lty=2,col=myCols[iR])
      }
    }
    if (LegendOn){legend("topright",y = NULL,ScenarioLabels,lty = rep(1,nRuns),col = myCols[1:nRuns],cex=LegendCex)}
    
    if (Save2File){
      fName = paste('Age_','Scenario',Scenario,'.png',sep="")
      dev.copy(png,fName,width=3,height=4.0,units="in",res=300)
      dev.off()
    }
    
    #############################
    # Plot Lake Age results
    par(
      mfrow = c(2,1),
      oma = c(1, 1, 1, 1), # four rows of text at the outer left and bottom margin
      mar = c(3.7, 3.7, 0.5, 0.0), # 0.5, 3.7, 0.5, 0.0 space for one row of text at ticks and to separate plots
      mgp = c(2.2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
      xpd = FALSE,
      cex = AxesCex
    )            # allow content to protrude into outer margin (and beyond)
  
    for (iR in 1:nRuns) {
      #Panel 2, catchment
      myD = myR[[iR]][[1]]
      if (iR == 1) {
        plot(1:Days / 365,myD$Age1 / 365,ylim = aCYLim,type = 'l',col = myCols[iR],xaxt ='n',xlab = "",ylab = "Catchment OC age (y) ",main = "")
        abline(h=myR[[iR]][2][[1]]$Tau1/365,lty=2,col=myCols[iR])
      }else{
        lines(1:Days / 365,myD$Age1 / 365,type = 'l',col = myCols[iR])
        abline(h=myR[[iR]][2][[1]]$Tau1/365,lty=2,col=myCols[iR])
      }
    }
    if (LegendOn){legend("topright",y = NULL,ScenarioLabels,lty = rep(1,nRuns),col = myCols[1:nRuns],cex=LegendCex)}
    
    for (iR in 1:nRuns) {
      #Panel 3, lake
      myD = myR[[iR]][[1]]
      if (iR == 1) {
        plot(1:Days / 365,myD$Age2 / 365,ylim = aLYLim,type = 'l',col = myCols[iR],xlab ='Years', ylab = "Lake OC age (y)",main = "")
        abline(h=myR[[iR]][2][[1]]$TauLake/365,lty=2,col=myCols[iR])
      }else{
        lines(1:Days / 365,myD$Age2 / 365,type = 'l',col = myCols[iR])
        abline(h=myR[[iR]][2][[1]]$TauLake/365,lty=2,col=myCols[iR])
      }
    }
    if (LegendOn){legend("topright",y = NULL,ScenarioLabels,lty = rep(1,nRuns),col = myCols[1:nRuns],cex=LegendCex)}
    
    if (Save2File){
      fName = paste('Age_Lake_','Scenario',Scenario,'.png',sep="")
      dev.copy(png,fName,width=3,height=4.0,units="in",res=300)
      dev.off()
    }
    
    
} # End of plotting dynamics

if (Scenario ==3){
  # Move list to vectors
  nRuns = length(myR)
  newSS = data.frame(Tau2System=rep(NA,nRuns),C2ave=rep(NA,nRuns))
  for (i in 1:nRuns){
    newSS$Tau2System[i] = myR[[i]][[2]]$Tau2System
    newSS$A1A2[i] = myR[[i]][[2]]$A1A2
    newSS$C2ave[i] = myR[[i]][[2]]$C2ave
  }
  par(
    mfrow = c(2,1),
    oma = c(1, 1, 1, 1), # four rows of text at the outer left and bottom margin
    mar = c(3.7, 3.7, 0.5, 0.0), # 0.5, 3.7, 0.5, 0.0 space for one row of text at ticks and to separate plots
    mgp = c(2.2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = FALSE,
    cex = AxesCex
  )            # allow content to protrude into outer margin (and beyond)
  plot(newSS$Tau2System/365,newSS$C2ave,type='l',xlab='Lake OC Age (y)',ylab='Lake OC (g/m3)')
  plot(A1A2,newSS$Tau2System/365,type='l',xlab='La:Ca',ylab='Lake OC Age (y)')
  if (Save2File){
    fName = paste('LA_WA_ratio_System_','Scenario',Scenario,'.png',sep="")
    dev.copy(png,fName,width=3,height=5,units="in",res=300)
    dev.off()
  }
}

if (Scenario ==4){
  # Move list to vectors
  nRuns = length(myR)
  newSS = data.frame(Tau2System=rep(NA,nRuns),C2ave=rep(NA,nRuns))
  for (i in 1:nRuns){
    newSS$Tau2System[i] = myR[[i]][[2]]$Tau2System
    newSS$A1A2[i] = myR[[i]][[2]]$A1A2
    newSS$C2ave[i] = myR[[i]][[2]]$C2ave
  }
  par(
    mfrow = c(3,1),
    oma = c(1, 1, 1, 1), # four rows of text at the outer left and bottom margin
    mar = c(3.7, 3.7, 0.5, 0.0), # 0.5, 3.7, 0.5, 0.0 space for one row of text at ticks and to separate plots
    mgp = c(2.2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = FALSE,
    cex = AxesCex
  )            # allow content to protrude into outer margin (and beyond)
  plot(CRain,newSS$C2ave,type='l',   xlab='C in Rain (mg/L)', ylab = 'Lake C (mg/L)')
  plot(CRain,newSS$Tau2System/365,type='l',xlab='C in Rain (mg/L)', ylab = 'Age (y)')
  plot(newSS$Tau2System/365,newSS$C2ave,type='l', xlab='Age (y)', ylab = 'Lake C (mg/L)')
  abline(a=0,b=1,lty=2)
  if (Save2File){
    fName = paste('NotSure_','Scenario',Scenario,'.png',sep="")
    dev.copy(png,fName,width=3,height=4.0,units="in",res=300)
    dev.off()
  }
}

if (Scenario ==5){
  # Create a matrix to hold the results
  nRuns = length(myR)
  nParams = length(P2S)
  # newParams = matrix(nrow = nParams,ncol = nSamples)
  # P2S = c(5,21) # parameters to sample, Q1P=5,Kr2=21
  # ParamNames = colnames(myParams)[P2S]
  
  newSS = data.frame(Tau2System=rep(NA,nRuns),C2ave=rep(NA,nRuns))
  for (i in 1:nRuns){
    newSS$Tau2System[i] = myR[[i]][[2]]$Tau2System
    #newSS$A1A2[i] = myR[[i]][[2]]$A1A2
    newSS$C2ave[i] = myR[[i]][[2]]$C2ave
  }
  # Plot lake C as a function of parameters
  par(
    mfrow = c(nParams,1),
    oma = c(1, 1, 1, 1), # four rows of text at the outer left and bottom margin
    mar = c(3.7, 3.7, 0.5, 0.0), # 0.5, 3.7, 0.5, 0.0 space for one row of text at ticks and to separate plots
    mgp = c(2.2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = FALSE,
    cex = AxesCex
  )            # allow content to protrude into outer margin (and beyond)
  for (i in 1:nParams){
    plot(newParams[i,],newSS$C2ave,xlab=ParamNames[i],ylab='Lake C (mg/L)')
         #ylim=c(-20,20))
  }
  # Plot lake C age as a function of parameters
  for (i in 1:nParams){
    plot(newParams[i,],log10(newSS$Tau2System/365),xlab=ParamNames[i],ylab='Lake C Age (y)')
         #ylim=c(0,5))
  }
  
}

if (Scenario==7){
  # Create a matrix to hold the results
  nRuns = length(myR)
  
  newSS = data.frame(Tau2System=rep(NA,nRuns),C2ave=rep(NA,nRuns))
  for (i in 1:nRuns){
    newSS$Tau2System[i] = myR[[i]][[2]]$Tau2System
    newSS$C2ave[i] = myR[[i]][[2]]$C2ave
  }
  # Plot lake C as a function of parameters
  par(
    mfrow = c(2,1),
    oma = c(1, 1, 1, 1), # four rows of text at the outer left and bottom margin
    mar = c(3.7, 3.7, 0.5, 0.0), # 0.5, 3.7, 0.5, 0.0 space for one row of text at ticks and to separate plots
    mgp = c(2.2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = FALSE,
    cex = AxesCex
  )            # allow content to protrude into outer margin (and beyond)

  plot(QP,newSS$C2ave,type='l',xlab='QP (m/d)',ylab='Lake C (mg/L)')
  
  # Plot lake C age as a function of parameters
  plot(QP,log10(newSS$Tau2System/365),type='l',xlab='QP',ylab='Lake C Age (y)')
  
}

  