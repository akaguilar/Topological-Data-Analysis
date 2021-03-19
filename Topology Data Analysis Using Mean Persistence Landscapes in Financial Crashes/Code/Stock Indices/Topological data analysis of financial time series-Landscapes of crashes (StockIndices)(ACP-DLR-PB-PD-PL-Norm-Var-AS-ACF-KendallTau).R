  library(PerformanceAnalytics)
  library(PortfolioAnalytics)
  library(timeSeries)
  library(astsa)
  library(TTR)
  library(tidyquant)
  library(tidyverse)
  library(quantmod)
  library(xts)
  library(fOptions)
  library(RQuantLib)
  library(ggplot2)
  library(dygraphs)
  library(shiny)
  library(TDA)
  library(TDAstats)
  library('moments') 
  library('Kendall') 
  library('KernSmooth') 
  library(deldir) 
  library(Matrix) # package for sparse matrice0s
  library(kernlab) 

  ### Stock Tickers
  
    tickers <- c("^DJI", "^GSPC", "^IXIC", "^RUT")
    
    tickers2 <- substr(tickers,2,length(tickers)+1)
    
    tickers3 <- c("Dow Jones", "S&P 500", "NASDAQ", "Russell 2000")
  
  ### Stock Prices
  
    for (i in 1:length(tickers))
    
    {
    
      dataSI1<-getSymbols(tickers[i], src="yahoo",from="2010-01-01", to="2020-07-01",periodcity="daily")
    
    
    }
  
  ### Stock Adjusted Closing Prices
  
    stocks1 = list()
    
    j1SI1 = 0
    
    for (aSI1 in tickers2)
      
    {
      
      j1SI1 = j1SI1 + 1
      
      stock_symbol1 = noquote(aSI1)
      
      rowssSI1 = data.frame(get(stock_symbol1)[ , 6 ])
      
      rowssSI1$dt = row.names(rowssSI1)
      
      stocks1[[j1SI1]] = rowssSI1
      
    }
    
    stock_table1 = stocks1[[1]]
    
    for (j in 2 : length(stocks1))
      
    {
      
      stock_table1 = merge(stock_table1 , stocks1[[j]], by="dt" )
      
    }
  
  ### Stock Adjusted Closing Prices Plots (2 rows by 2 columns)
    
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Adjusted Closing Prices/AdjustedClosingPrices(",
                    
      format(as.Date(stock_table1[1 , 1 ]),"%m%d%Y"), " - ",
                    
      format(as.Date(stock_table1[length(stock_table1[ , 1 ]) , 1 ]),"%m%d%Y"),
                    
      ")(AllStockIndices)1.png"),height=835,width=1100)
    
    par(mfrow=c(2,2))
  
    for (j in 1:length(tickers)) 
      
    {
      
      plot (as.Date(stock_table1[ , 1 ]) , stock_table1[ , j +1] , type="l" , xlab="Year", ylab="Adjusted Closing Price" , 
            
            cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
            
            col=rainbow(length(tickers))[j], main=paste0(tickers3[j],"\n Adjusted Closing Prices") )
    }
    
    dev.off()
        
  ### Stock Adjusted Closing Prices Plots (All the Same Plot)
    
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Adjusted Closing Prices/AdjustedClosingPrices(",
                    
      format(as.Date(stock_table1[1 , 1 ]),"%m%d%Y"), " - ",
                    
      format(as.Date(stock_table1[length(stock_table1[ , 1 ]) , 1 ]),"%m%d%Y"),
                    
      ")(AllStockIndices)2.png"),height=640,width=857)
    
    plot (as.Date(stock_table1[ , 1 ]) , stock_table1[ , 2] , type="l" , xlab="Year", ylab="Adjusted Closing Prices" , 
          
      cex.lab=1.1, cex.main=1.17,cex.axis=1.05, ylim=c(min(stock_table1[,2:(length(tickers)+1)]),max(stock_table1[,2:(length(tickers)+1)])),
          
      col=1, main=paste0("Adjusted Closing Prices \n Stock Indices") )
    
    for (j in 3:(length(tickers)+1)) 
      
    {
      
      lines(as.Date(stock_table1[ , 1 ]),stock_table1[ , j] , type="l", 
            
      cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
            
      col=(j-1))
      
    } 
    
    legend(x = "topleft", legend = c(tickers3), lty = 1, lwd = 3, col = 1:length(tickers),cex=0.91,bty='n')
    
    dev.off()
  
  ### Stock Daily Log Returns  
    
    nSI1 = length(stock_table1 [ , 1 ])
    
    dailyreturnsSI1 = stock_table1[ , 2 : (length (tickers)+1 ) ]
    
    for ( j in 1:length(tickers)) 
      
    {
      
      dailyreturnsSI1[2:nSI1, j] = diff(log(dailyreturnsSI1[ , j ]))
      
    }
    
    dailyreturnsSI1$dt = stock_table1$dt
    
    dailyreturnsSI1 = dailyreturnsSI1[2:nSI1 , ] 
    
    dailyreturnsSI1 = na.omit(dailyreturnsSI1)
    
  ### Stock Daily Log Returns Plots (2 rows by 2 columns)
    
    colnames(dailyreturnsSI1) <-c(tickers3,"Date")
    
    panelsSI1<-c("A","B","C","D")
    
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Daily Log Returns/DailyLogReturns(",
                    
      format(as.Date(stock_table1[2, 1 ]),"%m%d%Y"), " - ",
                    
      format(as.Date(stock_table1[nSI1 , 1 ]),"%m%d%Y"),
                    
      ")(AllStockIndices)1.png"),height=835,width=1100)
    
    par(mfrow=c(2,2))
    
    for (j in 1:length(tickers3)) 
      
      {
        
        plot (as.Date(stock_table1[2:nSI1, 1 ]) , dailyreturnsSI1[,j] , type="l" , xlab="Year", ylab="Daily Log Returns" ,
              
          cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
              
          col=j, main=paste0("Panel ",panelsSI1[j], ": Daily Log Returns-",tickers3[j]) )
        
        abline(h=0,col="black")
        
      }
    
      dev.off()
      
  ### Stock Daily Log Returns Plots (All Stock Indices plotted on the same graph)
      
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Daily Log Returns/DailyLogReturns(",
                      
      format(as.Date(dailyreturnsSI1[1, length(tickers)+1 ]),"%m%d%Y"), " - ",
                      
      format(as.Date(dailyreturnsSI1[length(dailyreturnsSI1[,1]),length(tickers)+1]),"%m%d%Y"),
                      
      ")(AllStockIndices)2.png"),height=835,width=1100)
    
    plot (as.Date(dailyreturnsSI1[, length(tickers)+1 ]) , dailyreturnsSI1[,1] , type="l" , lwd=3, xlab="Year", ylab="Daily Log Returns" ,
          
      cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
          
      col="blue", main=paste0("Daily Log Returns-Stock Indices \n ",
                                  
      format(as.Date(dailyreturnsSI1[1,length(tickers)+1]),"%B %d, %Y")," - ",
                                  
      format(as.Date(dailyreturnsSI1[length(dailyreturnsSI1[,1]),length(tickers)+1]),"%B %d, %Y")))
    
    abline(h=0,col="black")
    
    for(i in 2:length(tickers))
      
      {
        
        lines(as.Date(dailyreturnsSI1[,length(tickers)+1]),dailyreturnsSI1[,i],type="l",lwd=3,col=i)
        
      }
    
    dev.off()
  
  ### Stock Daily Log Returns Plots (4 rows by 1 column)
  
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Daily Log Returns/DailyLogReturns(",
                    
      format(as.Date(dailyreturnsSI1[, length(tickers)+1 ]),"%m%d%Y"), " - ",
                    
      format(as.Date(dailyreturnsSI1[length(dailyreturnsSI1[,1]),length(tickers)+1]),"%m%d%Y"),
                    
      ")(AllStockIndices)3.png"),height=672,width=900)
    
    par(mfrow=c(4,1), mar=c(0,6,0,1), oma=c(6,6,2,0), mgp=c(3.5,0.6,0), tcl=-.3, las=1)
    
    
    for (i in 1:((length(tickers))-1))
      
      {
        
        plot(as.Date(dailyreturnsSI1[,length(tickers)+1]),dailyreturnsSI1[,i],type="l",lwd=3,col=i, xaxt="no", ylab="",
             
          cex.axis=1.85,
             
          ylim=c(min(dailyreturnsSI1[,1:length(tickers)]),max(dailyreturnsSI1[,1:length(tickers)])))
        
        abline(h=0,col="black",lwd=3)
        
        legend(x = "topleft", legend = tickers3[i], cex=2,bty='n')
        
      }
    
    plot(as.Date(dailyreturnsSI1[,length(tickers)+1]),dailyreturnsSI1[,length(tickers)],type="l",lwd=3,col=length(tickers), ylab="",
         
      cex.axis=1.85,
         
      ylim=c(min(dailyreturnsSI1[,1:length(tickers)]),max(dailyreturnsSI1[,1:length(tickers)])))
    
    abline(h=0,col="black",lwd=3)
    
    legend(x = "topleft", legend = tickers3[length(tickers)], cex=2,bty='n')
    
    title(main=paste0("Daily Log Returns-Stock Indices"),xlab="Year",ylab="Daily Log Returns",outer=TRUE,cex.main=2,cex.lab=2,cex.axis=2)
    
    dev.off()
    
  ### Maximum and Minimum Daily Log Returns
    
      maxdrSI1 <-0.05
      
      mindrSI1 <- -0.05
      
      dtt1SI1  <- as.Date('2009-01-01')
      
      dtt2SI1   <- as.Date('2020-04-20')
      
      maxdlrSI1 <-which(dailyreturnsSI1[,1] >=maxdrSI1 & dailyreturnsSI1[,2] >=maxdrSI1 & dailyreturnsSI1[,3] >=maxdrSI1 & dailyreturnsSI1[,4] >=maxdrSI1 & dailyreturnsSI1$Date >= dtt1SI1& dailyreturnsSI1$Date<=dtt2SI1 )
      
      mindlrSI1 <-which(dailyreturnsSI1[,1] <=mindrSI1 & dailyreturnsSI1[,2] <=mindrSI1 & dailyreturnsSI1[,3] <=mindrSI1 & dailyreturnsSI1[,4] <=mindrSI1 & dailyreturnsSI1$Date >= dtt1SI1& dailyreturnsSI1$Date<=dtt2SI1)
    
    # kable(dailyreturnsSI1[maxdlrSI1,c(5,1,2,3,4)])%>%
    #   kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    # 
    # kable(dailyreturnsSI1[mindlrSI1,c(5,1,2,3,4)]) %>%
    #   kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    
    lSI1 <- 1000
    
    wSI1<- 50
    
    ### Adjusted Closing Prices (1000 days prior to 03-16-2020)
    
      par(mfrow=c(2,2))
      
      for (j in 1:length(tickers)) 
        
      {
        
        plot (as.Date(stock_table1[ ((mindlrSI1[4])-(lSI1-wSI1)):mindlrSI1[4],1 ]) 
              
              , stock_table1[ ((mindlrSI1[4])-(lSI1-wSI1)):mindlrSI1[4] , j +1] , type="l" , xlab="Year", ylab="Adjusted Closing Price" ,
              
              col="blue", main=paste0(tickers3[j],"\n Adjusted Closing Prices") )
      }
      
      nSI1 = length(stock_table1 [ , 1 ])
      
      dailyreturnsSI1 = stock_table1[ , 2 : (length (tickers)+1 ) ]
      
      for ( j in 1:length(tickers)) 
        
      {
        
        dailyreturnsSI1[2:nSI1, j] = diff(log(dailyreturnsSI1[ , j ]))
        
      }
      
    ### Daily Log Returns (1000 days prior to 03-16-2020)
    
      dailyreturnsSI1$dt = stock_table1$dt
      
      dailyreturnsSI1 = dailyreturnsSI1[2:nSI1 , ] 
      
      dailyreturnsSI1 = na.omit(dailyreturnsSI1)
      
      colnames(dailyreturnsSI1) <-c(tickers3,"Date")
      
      for (j in 1:length(tickers3)) 
        
      {
        
        plot (as.Date(stock_table1[((mindlrSI1[4])-(lSI1-wSI1)):mindlrSI1[4], 1 ]) , dailyreturnsSI1[ ((mindlrSI1[4])-(lSI1-wSI1)):mindlrSI1[4],j] , type="l" , xlab="Year", ylab="Daily Log Return" ,
              
              col="blue", main=paste0(tickers3[j],"\n Daily Log Returns") )
        
        abline(h=0,col="black")
        
      }
    
    
  ### Rips Filtration Parameters
  
    wSI1<- 50
    
    pc1SI1<- seq(1,length(dailyreturnsSI1[,1])-wSI1+1,1)
    
    pc2SI1<- seq(wSI1,length(dailyreturnsSI1[,1]),1)
    
    max.filtrationSI1<- 0.055
    
    XSI1<-vector("list",(length(dailyreturnsSI1[,1])-wSI1+1))
    
    for (i in 1:(length(dailyreturnsSI1[,1])-wSI1+1))
      
    {
      
      XSI1[[i]]<-dailyreturnsSI1[pc1SI1[i]:pc2SI1[i],1:(length(tickers))]
      
    }

  ### Euclidean distance between vectors

    euclidean.distance <- function(u, v) sqrt(sum((u - v) ^ 2))

  ### Persistence Diagrams Lists

    DiagSI1 <- vector("list",(length(dailyreturnsSI1[,1])-wSI1+1))

    PD.listSI1 <- vector("list",(length(dailyreturnsSI1[,1])-wSI1+1))

  ### Persistence Diagrams Actual List

    for (i in 1:(length(dailyreturnsSI1[,1])-wSI1+1))
      
    {
      
      DiagSI1[[i]]    <- ripsDiag(XSI1[[i]], maxdimension = 1, maxscale = max.filtrationSI1)
      
      PD.listSI1[[i]] <- DiagSI1[[i]][["diagram"]]
      
    }
  
  ### Persistence Landscapes Lists

    PL.listSI1 <- vector("list", (length(dailyreturnsSI1[,1])-wSI1+1))
    
      for (i in 1:(length(dailyreturnsSI1[,1])-wSI1+1))
        
      { 
        

        PL.listSI1[[i]] <- t(landscape(PD.listSI1[[i]],dimension=1,KK=1,seq(0,max.filtrationSI1,length=length(PD.listSI1[[i]]))))
      
      }

  ### Norms
    
    norm1SI1 <- 0
    
    norm2SI1 <- 0
    
  ### 1st and 2nd Norms
    
    for (i in pc1SI1)
        
    {
        
      ### 1st Norm for 03-16-2020 
      
        norm1SI1[i] <-  norm(t(PL.listSI1[[i]]),type="1")
      
      ### 2nd Norm for 03-16-2020
      
        norm2SI1[i] <- norm(t(PL.listSI1[[i]]),type="2")  #sum(abs(PL.list[[i]])^pp)^(1/pp) 
      
    }

    lSI1 <- 1000
    
    rSI1<- 75
    
    dates1SI1 <- dailyreturnsSI1[((mindlrSI1[4])-(lSI1-wSI1+1)):mindlrSI1[4],length(tickers)+1]
    
    dates11SI1 <- dailyreturnsSI1[(length(norm1SI1)-(rSI1-1)):length(norm1SI1),length(tickers)+1]
    
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Norms/Norms for Persistence Landscape (AllStockIndices).png"),
                    
      height=837,width=1117)
    
    par(mfrow=c(1,2))
    
      plot(as.Date(dates1SI1),norm1SI1[((mindlrSI1[4])-(lSI1-wSI1+1)):mindlrSI1[4]],xlab="Year", cex.lab=1.32, cex.main=1.39,cex.axis=1.32,
           
        ylab="",main=paste0("Panel A: Norms for Persistence Landscape-Stock Indices \n ", format(as.Date(dates1SI1[1]),"%B %d, %Y"), " - ",
                    
        format(as.Date(dates1SI1[length(dates1SI1)]),"%B %d, %Y")), type="l",col="red",  lwd=3)
      
      lines(as.Date(dates1SI1),norm2SI1[((mindlrSI1[4])-(lSI1-wSI1+1)):mindlrSI1[4]],xlab="Year",ylab="",type="l",lty=2,col="blue",  lwd=3)
       
      legend(x = "topleft", legend = c("L1", "L2"), lty = c(1,2), lwd = 3, col = c("red", "blue"),cex=1.7,bty='n')
      
      plot(as.Date(dates11SI1),norm1SI1[(length(norm1SI1)-(rSI1-1)):length(norm1SI1)],xlab="Month", cex.lab=1.32, cex.main=1.39,cex.axis=1.32,
           
        ylab="",main=paste0("Panel B: Norms for Persistence Landscape-Stock Indices \n ", format(as.Date(dates11SI1[1]),"%B %d, %Y"), " - ",
                            
        format(as.Date(dailyreturnsSI1[length(norm1SI1),length(tickers)+1]),"%B %d, %Y")),type="l",col="red",  lwd=3)
      
      lines(as.Date(dates11SI1),norm2SI1[(length(norm2SI1)-(rSI1-1)):length(norm2SI1)],xlab="Month",ylab="",type="l",lty=2,col="blue",  lwd=3)
      
      legend(x = "topleft", legend = c("L1", "L2"), lty = c(1,2), lwd = 3, col = c("red", "blue"),cex=1.7,bty='n')  
      
    dev.off()
      
  ### 1st and 2nd Norm Variance, Average Spectrum, and ACF 1000 trading days prior 03-16-2020
    
    wSI1<- 50
      
    lSI1 <- 1000
      
    ### Smoothing Data for 03-16-2020
    
    smoothdata1SI1=ksmooth(1:(lSI1-wSI1+1),norm1SI1[((mindlrSI1[4])-(lSI1-wSI1)):(mindlrSI1[4])],kernel=c("box","normal"),bandwidth=lSI1-wSI1+1) # To calculate deterministic trends in time series of stock market
    
    smoothdata2SI1=ksmooth(1:(lSI1-wSI1+1),norm2SI1[((mindlrSI1[4])-(lSI1-wSI1)):(mindlrSI1[4])],kernel=c("box","normal"),bandwidth=lSI1-wSI1+1) # To calculate deterministic trends in time series of stock market
      
    smooth1SI1=smoothdata1SI1$y
      
    smooth2SI1=smoothdata2SI1$y
      
    residuals1SI1 = norm1SI1[((mindlrSI1[4])-(lSI1-wSI1)):mindlrSI1[4]]-smooth1SI1 # Residual data
    
    residuals2SI1 = norm2SI1[((mindlrSI1[4])-(lSI1-wSI1)):mindlrSI1[4]]-smooth2SI1 # Residual data
    
  ### Parameters for 03-16-2020
  
    l_rwSI1=500
    
    N1SI1=length(residuals1SI1)
    
    N2SI1=length(residuals2SI1)
    
    var_residuals1SI1=numeric(N1SI1)
    
    acf_residuals1SI1=numeric(N1SI1)
    
    avgspec_residuals1SI1=numeric(N1SI1)
    
    var_residuals2SI1=numeric(N2SI1)
    
    acf_residuals2SI1=numeric(N2SI1)
    
    avgspec_residuals2SI1=numeric(N2SI1)
    
  ### Apply rolling window to residuals.
  
  ### Residual data analysis.
  
    for (i in 1:(N1SI1-l_rwSI1+1))
      
    {
      
      rolldata1SI1 = residuals1SI1[i:(i+l_rwSI1-1)];
      
      var_residuals1SI1[i+l_rwSI1-1] = var(rolldata1SI1)
      
      spec_residuals1SI1 = spectrum(rolldata1SI1,plot=FALSE)$spec
      
      acf_residuals1SI1[i+l_rwSI1-1] = acf(rolldata1SI1,plot=FALSE)$acf[2]
      
      avgspec_residuals1SI1[i+l_rwSI1-1] = mean(spec_residuals1SI1[2:floor(l_rwSI1/8)])
      
    }
    
    
    for (i in 1:(N2SI1-l_rwSI1+1))
      
    {
      
      rolldata2SI1 = residuals2SI1[i:(i+l_rwSI1-1)];
      
      var_residuals2SI1[i+l_rwSI1-1] = var(rolldata2SI1)
      
      spec_residuals2SI1 = spectrum(rolldata2SI1,plot=FALSE)$spec
      
      acf_residuals2SI1[i+l_rwSI1-1] = acf(rolldata2SI1,plot=FALSE)$acf[2]
      
      avgspec_residuals2SI1[i+l_rwSI1-1] = mean(spec_residuals2SI1[2:floor(l_rwSI1/8)])
      
    }
    
    ### Parameters
      NSI1=1000
      
      l_kwSI1=250

    ### Kendall-Tau values

        kendaltau_var1SI1 = Kendall(1:l_kwSI1, var_residuals1SI1[(NSI1-l_kwSI1+1):NSI1])$tau[1];
      
        kendaltau_avgspec1SI1 = Kendall(1:l_kwSI1, avgspec_residuals1SI1[(NSI1-l_kwSI1+1):NSI1])$tau[1];
        
        kendaltau_acf1SI1 = Kendall(1:l_kwSI1, acf_residuals1SI1[(NSI1-l_kwSI1+1):NSI1])$tau[1];
      
        kendalls1SI1 = list(acf=kendaltau_acf1SI1,  var=kendaltau_var1SI1, spec=kendaltau_avgspec1SI1)
      
        kendaltau_var2SI1 = Kendall(1:l_kwSI1, var_residuals2SI1[(NSI1-l_kwSI1+1):NSI1])$tau[1];
        
        kendaltau_avgspec2SI1 = Kendall(1:l_kwSI1, avgspec_residuals2SI1[(NSI1-l_kwSI1+1):NSI1])$tau[1];
        
        kendaltau_acf2SI1 = Kendall(1:l_kwSI1, acf_residuals2SI1[(NSI1-l_kwSI1+1):NSI1])$tau[1];
        
        kendalls2SI1 = list(acf=kendaltau_acf2SI1,  var=kendaltau_var2SI1, spec=kendaltau_avgspec2SI1)
      
      # All the kendall-tau values equal to one have been made to be 0.999 because of the error
      # it produces while generating histograms
        
  ### VIX Stock Information
  
    VIX11<-getSymbols("^VIX", src="yahoo",from="2008-01-01", to="2020-06-30",periodcity="daily")

    VIX11 = as.data.frame(apply(VIX, 2, function(x) (x - min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))))
    
    VIX11 <- data.frame(date = row.names(VIX11), VIX11, row.names = NULL)
    
    VIX11[which(VIX11[((mindlrSI1[4])-(lSI1-wSI1)):(mindlrSI1[4]),7]==0),7] <-0.00001

    VIX11Log <- diff(log(VIX11[ , 7 ]))
    
    VIX11Log1 <- na.omit(VIX11Log)
    
    par(mfrow=c(1,2))
    
      plot(as.Date(VIX11[ ((mindlrSI1[4])-(lSI1-wSI1)):(mindlrSI1[4]) , 1 ]) , VIX11[ ((mindlrSI1[4])-(lSI1-wSI1)):mindlrSI1[4], 7], type="l",
           
           xlab="Year", ylab="Adjusted Closing Price" , col="gray",
           
           main="VIX \n Adjusted Closing Prices ",lwd=3)
      
      
      plot (as.Date(VIX11[((mindlrSI1[4])-(lSI1-wSI1)):(mindlrSI1[4]), 1 ]) , VIX11Log1[((mindlrSI1[4])-(lSI1-wSI1)):(mindlrSI1[4])] , type="l" , xlab="Year", ylab="Daily Log Return" ,
            
            col="blue", main=" VIX \n Daily Log Returns" )
      
      abline(h=0,col="black")
  
  ### VIX Variance, Average Spectrum, and ACF 1000 trading days before 03-16-2020
  
    wSI1<- 50
    
    lSI1 <- 1000
  
  ### Smoothing Data for 03-16-2020
    
    smoothdata3=ksmooth(1:(lSI1-wSI1+1),VIX11[((mindlrSI1[4])-(lSI1-wSI1)):(mindlrSI1[4]),7],kernel=c("box","normal"),bandwidth=lSI1-wSI1+1) # To calculate deterministic trends in time series of stock market
  
    smooth3=smoothdata3$y
    
    residuals3 = VIX11[((mindlrSI1[4])-(lSI1-wSI1)):(mindlrSI1[4]),7]-smooth3 # Residual data
    
    ### Parameters for 03-16-2020
    
    l_rwSI1=500
    
    N3SI1=length(residuals3)
    
    var_residuals3=numeric(N3SI1)
    
    acf_residuals3=numeric(N3SI1)
    
    avgspec_residuals3=numeric(N3SI1)
  
  ### Apply rolling window to residuals.
  
  ### Residual data analysis.
  
    for (i in 1:(N3SI1-l_rwSI1+1))
      
    {
      
      rolldata3 = residuals3[i:(i+l_rwSI1-1)];
      
      var_residuals3[i+l_rwSI1-1] = var(rolldata3)
      
      spec_residuals3 = spectrum(rolldata3,plot=FALSE)$spec
      
      acf_residuals3[i+l_rwSI1-1] = acf(rolldata3,plot=FALSE)$acf[2]
      
      avgspec_residuals3[i+l_rwSI1-1] = mean(spec_residuals3[2:floor(l_rwSI1/8)])
      
    }
    
  ### Parameters
    
    NSI1=1000
    
    l_kwSI1=250
    
  ### Kendall-Tau values
    
    kendaltau_var3 = Kendall(1:l_kwSI1, var_residuals3[(NSI1-l_kwSI1+1):NSI1])$tau[1];
    
    kendaltau_avgspec3 = Kendall(1:l_kwSI1, avgspec_residuals3[(NSI1-l_kwSI1+1):NSI1])$tau[1];
    
    kendaltau_acf3 = Kendall(1:l_kwSI1, acf_residuals3[(NSI1-l_kwSI1+1):NSI1])$tau[1];
    
    kendalls3 = list(acf=kendaltau_acf3,  var=kendaltau_var3, spec=kendaltau_avgspec3)

  ### 1st and 2nd Norms Variance, Average Spectrum, and ACF plots 250 trading days prior 03-16-2020  
  
    ### VIX Variance, Average Spectrum, and ACF plots 250 trading days prior 03-16-2020
  
    par(mfrow=c(2,3))
    
    tSI1<-250
    
    dates2SI1 <- dailyreturnsSI1[(mindlrSI1[4]-(lSI1-1)):(mindlrSI1[4]) ,length(tickers)+1]
    
    plot(as.Date(dates2SI1[(lSI1-tSI1+1):lSI1]),var_residuals1SI1[(lSI1-tSI1+1):lSI1],type='l',lwd = 1,xlab= 'Month',ylab = 'Variance',
         col='black', main='1st Norm \n Variance \n (Stock Indices)')
    
    plot(as.Date(dates2SI1[(lSI1-tSI1+1):lSI1]),avgspec_residuals1SI1[(lSI1-tSI1+1):lSI1],type='l',lwd = 1,xlab= 'Month',ylab = 'Average Spectrum',
         col='black', main='1st Norm \n Average Spectrum \n (Stock Indices)')
    
    plot(as.Date(dates2SI1[(lSI1-tSI1+1):lSI1]),acf_residuals1SI1[(lSI1-tSI1+1):lSI1],type='l',lwd = 1,xlab= 'Month',ylab = 'ACF',
         col='black', main='1st Norm \n ACF \n (Stock Indices)')
    
    plot(as.Date(dates2SI1[(lSI1-tSI1+1):lSI1]),var_residuals2SI1[(lSI1-tSI1+1):lSI1],type='l',lwd = 5,xlab= 'Month',ylab = 'Variance',
         col='blue', main='2nd Norm \n Variance \n (Stock Indices)')
    
    plot(as.Date(dates2SI1[(lSI1-tSI1+1):lSI1]),avgspec_residuals2SI1[(lSI1-tSI1+1):lSI1],type='l',lwd = 5,xlab= 'Month',ylab = 'Average Spectrum',
         col='red', main='2nd Norm \n Average Spectrum \n (Stock Indices)')
    
    plot(as.Date(dates2SI1[(lSI1-tSI1+1):lSI1]),acf_residuals2SI1[(lSI1-tSI1+1):lSI1],type='l',lwd = 5,xlab= 'Month',ylab = 'ACF',
         col='gray', main='2nd Norm \n ACF \n (Stock Indices)')  
    
    tSI1<-250
    
    dates3SI1 <- dailyreturnsSI1[(mindlrSI1[4]-(lSI1-1)):(mindlrSI1[4]) ,length(tickers)+1]
    
      plot(as.Date(dates3SI1[(lSI1-tSI1+1):lSI1]),var_residuals3[(lSI1-tSI1+1):lSI1],type='l',lwd = 5,xlab= 'Month',ylab = 'Variance',
           col='blue', main='VIX \n Variance \n (Stock Indices)')
      
      plot(as.Date(dates3SI1[(lSI1-tSI1+1):lSI1]),avgspec_residuals3[(lSI1-tSI1+1):lSI1],type='l',lwd = 5,xlab= 'Month',ylab = 'Average Spectrum',
           col='red', main='VIX \n Average Spectrum \n (Stock Indices) ')
      
      plot(as.Date(dates3SI1[(lSI1-tSI1+1):lSI1]),acf_residuals3[(lSI1-tSI1+1):lSI1],type='l',lwd = 5,xlab= 'Month',ylab = 'ACF',
           col='gray', main='VIX \n ACF \n (Stock Indices)')
  