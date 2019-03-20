Budyko <-function(Qpe=NULL,QP=NULL,nScale=NULL,PlotIt=NULL){
  # Budyko application described in Duffy et al. 2017, L&O Letters
  # Budyko implementation after Turc and Pike
  
  # If parameters are null, then set to defaults
  if (is.null(nScale)){
    nScale=2 # scaling parameter in the equation
  }
  if (is.null(Qpe)){
    Qpe = rep(0.0023862,3) # Mean annual potential ET, m/d
  }
  if (is.null(QP)){
    QP = c(0.00158921, 0.00262005, 0.00385371) # Mean annual precipitation, m/d
    # Three scenarios for L Mendota: dry, today, wet
    # yChris = c(0.559,0.745,0.853) # From Chris' notebook, just for reference
    # Qe = c(0.00133389, 0.00177772, 0.00203543) # Mean annual ET, m/d
  }
  if (is.null(PlotIt)){
    PlotIt = TRUE
  }

  x = QP/Qpe # this is in Chris' workbook
  #x2 = QP/Qe # this is what the paper says
  y = x/((1+x^nScale)^(1/nScale))
  
  if (PlotIt){
    # Now run the model over a gradient of the P,Ep ratio
    x2 = seq(0,4,0.1)
    y2 = x2/((1+x2^nScale)^(1/nScale))
    
    plot(x2,y2,type='l',xlim=c(0,4),ylim=c(0,1.2),
         xlab='Humidity index, P/Ep',ylab='Evaporative index, E/Ep')
    
    points(x,y)
    abline(h=1,lty=2)
    abline(a=0,b=1,lty=2)
  }
  Qe = y*Qpe
  Result = data.frame(QP=QP,Qe=Qe,n=nScale,HumidityIndex=x,EvaporativeIndex=y)
  return(Result)
}