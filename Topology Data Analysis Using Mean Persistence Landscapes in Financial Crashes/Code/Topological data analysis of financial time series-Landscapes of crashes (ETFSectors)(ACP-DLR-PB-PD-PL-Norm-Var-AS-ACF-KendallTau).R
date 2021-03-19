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
  library(Matrix) # package for sparse matrices
  library(kernlab) 
  # library(knitr)
  # library(kableExtra)

  ### ETF Tickers
  
    ETFtickers1 <- c("XLY", "XLP", "XLE",    "XLF", "XLV",   
               "XLI", "XLB", "XLK", "XLU", "SPY")  
  
  ### Accompanying ETF sector names.
  
    ETFsector <- c("Consumer Discretionary", "Consumer Staples", 
              "Energy", "Financials", "Health Care", "Industrials", 
              "Materials", "Information Technology", "Utilities", "Index")
  
  ### ETF Tickers and Sectors
    
    ETFtickers2<-paste0(ETFsector,"-(",ETFtickers1,")")
    
  ### ETF Prices
  
    for (i in 1:length(ETFtickers1))
    
    {
    
      data<-getSymbols(ETFtickers1[i], src="yahoo",from="2010-01-01", to="2020-07-01",periodcity="daily")
    
    
    }
  
  ### ETF Adjusted Closing Prices
  
    ETFs = list()
    
    j = 0
    
    for (a in ETFtickers1)
      
    {
      
      j = j + 1
      
      ETF_symbol = noquote(a)
      
      rowss = data.frame(get(ETF_symbol)[ , 6 ])
      
      rowss$dt = row.names(rowss)
      
      ETFs[[j]] = rowss
      
    }
    
    ETF_table = ETFs[[1]]
    
    for (j in 2 : length(ETFs))
      
    {
      
      ETF_table = merge(ETF_table , ETFs[[j]], by="dt" )
      
    }
  
  ### ETF Adjusted Closing Prices Plots
    
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Adjusted closing Prices/AdjustedClosingPrices(",
                    
    format(as.Date(ETF_table[1, 1 ]),"%m%d%Y"), " - ",
                    
    format(as.Date(ETF_table[length(ETF_table[ , 1 ]) , 1 ]),"%m%d%Y"),
                    
    ")(AllETFSectors)1.png"),height=835,width=1100)
  
    par(mfrow=c(5,2),las=1)
  
    for (j in 1:length(ETFtickers1)) 
      
    {
      
      plot (as.Date(ETF_table[ , 1 ]) , ETF_table[ , j +1] , type="l" , xlab="Year", ylab="Adjusted Closing Price" ,
            
            col=rainbow(length(ETFtickers1))[j], main=paste0(ETFtickers1[j],"\n",ETFsector[j],"\n Adjusted Closing Prices") )
    }
    
    dev.off()
    
    ### ETF Sectors Adjusted Closing Prices Plots (All the Same Plot)
    
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Adjusted Closing Prices/AdjustedClosingPrices(",
                    
      format(as.Date(ETF_table[1 , 1 ]),"%m%d%Y"), " - ",
                    
      format(as.Date(ETF_table[length(ETF_table[ , 1 ]) , 1 ]),"%m%d%Y"),
                    
      ")(AllETFSectors)2.png"),height=640,width=857)
    
    plot (as.Date(ETF_table[ , 1 ]) , ETF_table[ , 2] , type="l" , xlab="Year", ylab="Adjusted Closing Prices" , 
          
      cex.lab=1.1, cex.main=1.17,cex.axis=1.05, ylim=c(min(ETF_table[,2:(length(ETFtickers1)+1)]),max(ETF_table[,2:(length(ETFtickers1)+1)])),
          
      col=rainbow(length(ETFtickers1))[1], main=paste0("Adjusted Closing Prices \n ETF Sectors") )
    
    
    for (j in 3:(length(ETFtickers1)+1)) 
      
      {
      
        lines(as.Date(ETF_table[ , 1 ]),ETF_table[ , j] , type="l", 
            
        cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
            
        col=rainbow(length(ETFtickers1))[j])
      
      } 
    
    legend(x = "topleft", legend = c(ETFtickers1), lty = 1, lwd = 3, col = rainbow(10)[1:10],cex=0.91,bty='n')
    
    dev.off()
    
  ### ETF Daily Log Returns
  
    nETF1 = length(ETF_table [ , 1 ])
    
    dailyreturnsETF1 = ETF_table[ , 2 : (length (ETFtickers1)+1 ) ]
    
    for ( j in 1:length(ETFtickers1)) 
      
      {
        
        dailyreturnsETF1[2:nETF1, j] = diff(log(dailyreturnsETF1[ , j ]))
        
      }
    
    dailyreturnsETF1$dt = ETF_table$dt
    
    dailyreturnsETF1 = dailyreturnsETF1[2:nETF1 , ] 
    
    dailyreturnsETF1 = na.omit(dailyreturnsETF1)
    
  ### ETF Daily Log Returns Plots (2 rows by 2 columns)
    
    colnames(dailyreturnsETF1) <-c(ETFtickers1,"Date")
    
    panelsETF1<-c("A","B","C","D")
    
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Daily Log Returns/DailyLogReturns(",
                    
                    format(as.Date(ETF_table[1, 1 ]),"%m%d%Y"), " - ",
                    
                    format(as.Date(ETF_table[nETF1 , 1 ]),"%m%d%Y"),
                    
                    ")(AllETFSectors)1.png"),height=835,width=1100)
    
    par(mfrow=c(5,2),las=1)
    
    for (j in 1:length(ETFtickers1)) 
      
      {
      
        plot (as.Date(ETF_table[2:nETF1, 1 ]) , dailyreturnsETF1[,j] , type="l" , xlab="Year", ylab="Daily Log Returns" ,
              
          col=rainbow(length(ETFtickers1))[j], main=paste0(ETFtickers1[j],"- ",ETFsector[j]))
        
        abline(h=0,col="black")
        
      }
    
    dev.off()
   
   ### ETF Daily Log Returns Plots (All ETF Sectors plotted on the same graph)
  
      png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Daily Log Returns/DailyLogReturns(",
                  
        format(as.Date(dailyreturnsETF1[, length(ETFtickers1)+1 ]),"%m%d%Y"), " - ",
                  
        format(as.Date(dailyreturnsETF1[length(dailyreturnsETF1[,1]),length(ETFtickers1)+1]),"%m%d%Y"),
                  
        ")(AllETFSectors)2.png"),height=835,width=1100)

      plot (as.Date(dailyreturnsETF1[, length(ETFtickers1)+1 ]) , dailyreturnsETF1[,1] , type="l" , lwd=3, xlab="Year", ylab="Daily Log Returns" ,
            
        col="blue", main=paste0("Daily Log Returns \n ETF Sectors \n ",
                  
        format(as.Date(dailyreturnsETF1[1,length(ETFtickers1)+1]),"%B %d, %Y")," - ",
                  
        format(as.Date(dailyreturnsETF1[length(dailyreturnsETF1[,1]),length(ETFtickers1)+1]),"%B %d, %Y")))
      
      abline(h=0,col="black")
      
        for(i in 2:length(ETFtickers1))
        
          {
            
            lines(as.Date(dailyreturnsETF1[,length(ETFtickers1)+1]),dailyreturnsETF1[,i],type="l",lwd=3,col=i)
            
          }
      
    dev.off()
    
  ### ETF Daily Log Returns Plots (5 rows by 2 columns)
      
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Daily Log Returns/DailyLogReturns(",
                    
      format(as.Date(dailyreturnsETF1[, length(ETFtickers1)+1 ]),"%m%d%Y"), " - ",
                    
      format(as.Date(dailyreturnsETF1[length(dailyreturnsETF1[,1]),length(ETFtickers1)+1]),"%m%d%Y"),
                    
      ")(AllETFSectors)3.png"),height=672,width=900)
    
    par(mfrow=c(5,2),mar=c(0,6,0,1), oma=c(6,6,2,0), mgp=c(3.5,0.6,0), tcl=-.3, las=1)
    
    for (i in 1:(length(ETFtickers1)-2))
      
      {
        plot(dailyreturnsETF1[,i],type="l",lwd=3,col=rainbow(length(ETFtickers1)-2)[i],xaxt="no",cex.axis=1.85, ylab="",
             
          ylim=c(min(dailyreturnsETF1[,1:length(ETFtickers1)]),max(dailyreturnsETF1[,1:length(ETFtickers1)])))
        
        abline(h=0,col="black",lwd=3)
        
        legend(x = "topleft", legend = paste0(ETFtickers1[i], "-", ETFsector[i]), cex=1.9,bty='n')
        
      }
    
    plot(as.Date(dailyreturnsETF1[,length(ETFtickers1)+1]),dailyreturnsETF1[,length(ETFtickers1)-1],type="l",lwd=3, cex.axis=1.85,
         
      col=1, ylab="",
         
      ylim=c(min(dailyreturnsETF1[,1:length(ETFtickers1)]),max(dailyreturnsETF1[,1:length(ETFtickers1)])))
    
    abline(h=0,col="black",lwd=3)
    
    legend(x = "topleft", legend = paste0(ETFtickers1[length(ETFtickers1)-1], "-", ETFsector[length(ETFtickers1)-1]), cex=1.9,bty='n')
    
    plot(as.Date(dailyreturnsETF1[,length(ETFtickers1)+1]),dailyreturnsETF1[,length(ETFtickers1)],type="l",lwd=3, cex.axis=1.85,
         
      col=8, ylab="",
         
      ylim=c(min(dailyreturnsETF1[,1:length(ETFtickers1)]),max(dailyreturnsETF1[,1:length(ETFtickers1)])))
    
    abline(h=0,col="black",lwd=3)
    
    legend(x = "topleft", legend = paste0(ETFtickers1[length(ETFtickers1)], "-", ETFsector[length(ETFtickers1)]), cex=1.9,bty='n')
    
    title(main=paste0("Daily Log Returns-ETF Sectors"),xlab="Year",ylab="Daily Log Returns",outer=TRUE,cex.main=2,cex.lab=1.9,cex.axis=1.9)
    
    dev.off()
    
      
  ### Maximum and Minimum Daily Log Returns
    
      maxdrETF1 <-0.05
      
      mindrETF1 <- -0.05
      
      dtt1   <- as.Date('2009-01-01')
      
      dtt2   <- as.Date('2020-04-20')
      
      maxdlrETF1 <-which(dailyreturnsETF1[,1] >=maxdrETF1 & dailyreturnsETF1[,2] >=maxdrETF1 & dailyreturnsETF1[,3] >=maxdrETF1 & dailyreturnsETF1[,4] >=maxdrETF1 & dailyreturnsETF1[,5] >=maxdrETF1 & dailyreturnsETF1[,6] >=maxdrETF1 &  
                     dailyreturnsETF1[,7] >=maxdrETF1 & dailyreturnsETF1[,8] >=maxdrETF1 & dailyreturnsETF1[,9] >=maxdrETF1 & dailyreturnsETF1[,10]>=maxdrETF1 & dailyreturnsETF1$Date >= dtt1 & dailyreturnsETF1$Date<=dtt2 )
      
      mindlrETF1 <-which(dailyreturnsETF1[,1] <=mindrETF1 & dailyreturnsETF1[,2] <=mindrETF1 & dailyreturnsETF1[,3] <=mindrETF1 & dailyreturnsETF1[,4] <=mindrETF1 & dailyreturnsETF1[,5] <=mindrETF1 & dailyreturnsETF1[,6] <=mindrETF1 &  
                     dailyreturnsETF1[,7] <=mindrETF1 & dailyreturnsETF1[,8] <=mindrETF1 & dailyreturnsETF1[,9] <=mindrETF1 & dailyreturnsETF1[,10]<=mindrETF1 & dailyreturnsETF1$Date >= dtt1 & dailyreturnsETF1$Date<=dtt2 )
    
    # kable(dailyreturnsETF1[maxdlrETF1,c(5,1,2,3,4)])%>%
    #   kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    #  
    # kable(dailyreturnsETF1[mindlrETF1,c(5,1,2,3,4)]) %>%
    #   kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    
    l <- 1000
    
    w <- 50
    
    ### Adjusted Closing Prices (1000 days prior to 03-16-2020)
    
      par(mfrow=c(2,2))
      
      for (j in 1:length(ETFtickers1)) 
        
      {
        
        plot (as.Date(ETF_table[ ((mindlrETF1[2])-(l-w)):mindlrETF1[2],1 ]) 
              
              , ETF_table[ ((mindlrETF1[2])-(l-w)):mindlrETF1[2] , j +1] , type="l" , xlab="Year", ylab="Adjusted Closing Price" ,
              
              col="blue", main=paste0(ETFtickers1[j],"\n",ETFsector[j],"\n Adjusted Closing Prices") )
      }
      
      n = length(ETF_table [ , 1 ])
      
      dailyreturnsETF1 = ETF_table[ , 2 : (length (ETFtickers1)+1 ) ]
      
      for ( j in 1:length(ETFtickers1)) 
        
      {
        
        dailyreturnsETF1[2:n, j] = diff(log(dailyreturnsETF1[ , j ]))
        
      }
      
      
      dailyreturnsETF1$dt = ETF_table$dt
      
      dailyreturnsETF1 = dailyreturnsETF1[2:n , ] 
      
      dailyreturnsETF1 = na.omit(dailyreturnsETF1)
      
      colnames(dailyreturnsETF1) <-c(ETFtickers1,"Date")
      
    ### Daily Log Returns (1000 days prior to 03-16-2020)
      
      par(mfrow=c(2,2))
      
      for (j in 1:length(ETFtickers1)) 
        
      {
        
        plot (as.Date(ETF_table[((mindlrETF1[2])-(l-w)):mindlrETF1[2], 1 ]) , dailyreturnsETF1[ ((mindlrETF1[2])-(l-w)):mindlrETF1[2],j] , type="l" , xlab="Year", ylab="Daily Log Return" ,
              
              col="blue", main=paste0(ETFtickers1[j],"\n",ETFsector[j],"\n Daily Log Returns") )
        
        abline(h=0,col="black")
        
      }

    ### Rips-Filtration Parameters
      
      pc1<- seq(1,length(dailyreturnsETF1[,1])-w+1,1)
      
      pc2<- seq(w,length(dailyreturnsETF1[,1]),1)
      
      max.filtration<- 0.07
      
      X1<-vector("list",(length(dailyreturnsETF1[,1])-w+1))
      
      for (i in 1:(length(dailyreturnsETF1[,1])-w+1))
        
      {
        
        X1[[i]]<-dailyreturnsETF1[pc1[i]:pc2[i],1:(length(ETFtickers1))]
        
      }

  ### Euclidean distance between vectors

    euclidean.distance <- function(u, v) sqrt(sum((u - v) ^ 2))

  ### Plot the Vietoris-Rips complex up to some filtration value
    
    plot.rips <- function(X,max.filtration){
      plot(X, pch=20, col="blue", asp=1)
      num.points <- dim(X)[1]
      for(i in 1:num.points)
        for(j in 1:num.points)
          if (euclidean.distance(X[i,],X[j,]) < max.filtration)
            lines(rbind(X[i,],X[j,]))
    }

### Plot representative cycles for Vietoris-Rips complex
  
  plot.rips.cycle <- function(X){
    PH.output <- ripsDiag(X, maxdimension = 1, maxscale = max.filtration, 
                          library = c("GUDHI", "Dionysus"), location = TRUE)
    PD <- PH.output[["diagram"]]
    ones <- which(PD[, 1] == 1)
    persistence <- PD[ones,3] - PD[ones,2]
    cycles <- PH.output[["cycleLocation"]][ones[order(persistence)]]
    for (i in 1:length(cycles)){
      plot(X, pch=20, col="blue", asp=1)
      for (j in 1:dim(cycles[[i]])[1])
        lines(cycles[[i]][j,,])
    }
  }

  ### Persistence Diagrams Lists

    Diag1 <- vector("list",(length(dailyreturnsETF1[,1])-w+1))

    PD.list1 <- vector("list",(length(dailyreturnsETF1[,1])-w+1))

  ### Persistence Diagrams Actual List

    for (i in 1:(length(dailyreturnsETF1[,1])-w+1))
      
    {
      
      Diag1[[i]]    <- ripsDiag(X1[[i]], maxdimension = 1, maxscale = max.filtration)
      
      PD.list1[[i]] <- Diag1[[i]][["diagram"]]
      
    }
  
  ### Persistence Landscapes Lists
    
    PL.list1 <- vector("list", (length(dailyreturnsETF1[,1])-w+1))
    
      for (i in 1:(length(dailyreturnsETF1[,1])-w+1))
        
      { 
        

        PL.list1[[i]] <- t(landscape(PD.list1[[i]],dimension=1,KK=1,seq(0,max.filtration,length=length(PD.list1[[i]]))))
        
      }

  ### Norms
    
    norm1ETF1 <- 0
    
    norm2ETF1 <- 0
    
  ### 1st and 2nd Norms
    
    for (i in pc1)
        
    {
        
      ### 1st Norm for 03-16-2020 
      
        norm1ETF1[i] <-  norm(t(PL.list1[[i]]),type="1")
      
      ### 2nd Norm for 03-16-2020
      
        norm2ETF1[i] <- norm(t(PL.list1[[i]]),type="2")  #sum(abs(PL.list[[i]])^pp)^(1/pp) 
      
    }
    
    l<-1000
    
    r<- 75
    
    dates1ETF1 <- dailyreturnsETF1[((mindlrETF1[2])-(l-w+1)):mindlrETF1[2],length(ETFtickers1)+1]
    
    dates11ETF1 <- dailyreturnsETF1[(length(norm1ETF1)-(r-1)):length(norm1ETF1),length(ETFtickers1)+1]
    
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Norms/Norms for Persistence Landscape (AllETFs).png"),
        
        height=837,width=1117)
    
    par(mfrow=c(1,2))
  
      plot(as.Date(dates1ETF1),norm1ETF1[((mindlrETF1[2])-(l-w+1)):mindlrETF1[2]],xlab="Year",  cex.lab=1.32, cex.main=1.39,cex.axis=1.32,
           
        ylab="",main=paste0("Panel A: Norms for Persistence Landscape-ETF sectors \n", format(as.Date(dates1ETF1[1]),"%B %d, %Y"), " - ",
                               
        format(as.Date(dates1ETF1[length(dates1ETF1)]),"%B %d, %Y")),type="l",col="red",  lwd=3)
      
      lines(as.Date(dates1ETF1),norm2ETF1[((mindlrETF1[2])-(l-w+1)):mindlrETF1[2]],xlab="Year",ylab="",type="l",lty=2,col="blue",  lwd=3)
      
      legend(x = "topleft", legend = c("L1", "L2"), lty = c(1,2), lwd = 3, col = c("red", "blue"),cex=1.7,bty='n')
      
      plot(as.Date(dates11ETF1),norm1ETF1[(length(norm1ETF1)-(r-1)):length(norm1ETF1)],xlab="Month", cex.lab=1.32, cex.main=1.39,cex.axis=1.32,
           
        ylab="", main=paste0("Panel B: Norms for Persistence Landscape-ETF sectors \n", format(as.Date(dates11ETF1[1]),"%B %d, %Y"), " - ",
        
        format(as.Date(dailyreturnsETF1[length(norm1ETF1),length(ETFtickers1)+1]),"%B %d, %Y")), type="l",col="red",  lwd=3)
      
      lines(as.Date(dates11ETF1),norm2ETF1[(length(norm2ETF1)-(r-1)):length(norm2ETF1)],xlab="Month",ylab="",type="l",lty=2,col="blue",  lwd=3)
      
      legend(x = "topleft", legend = c("L1", "L2"), lty = c(1,2), lwd = 3, col = c("red", "blue"),cex=1.7,bty='n')    
      
      dev.off()
      
  ### 1st and 2nd Norm Variance, Average Spectrum, and ACF 1000 trading days prior 03-16-2020
    
    w <- 50
      
    l <- 1000
      
    ### Smoothing Data for 03-16-2020
    
    smoothdata1=ksmooth(1:(l-w+1),norm1ETF1[((mindlrETF1[2])-(l-w)):(mindlrETF1[2])],kernel=c("box","normal"),bandwidth=l-w+1) # To calculate deterministic trends in time series of ETF market
    
    smoothdata2=ksmooth(1:(l-w+1),norm2ETF1[((mindlrETF1[2])-(l-w)):(mindlrETF1[2])],kernel=c("box","normal"),bandwidth=l-w+1) # To calculate deterministic trends in time series of ETF market
      
    smooth1=smoothdata1$y
      
    smooth2=smoothdata2$y
      
    residuals1 = norm1ETF1[((mindlrETF1[2])-(l-w)):mindlrETF1[2]]-smooth1 # Residual data
    
    residuals2 = norm2ETF1[((mindlrETF1[2])-(l-w)):mindlrETF1[2]]-smooth2 # Residual data
    
  ### Parameters for 03-16-2020
  
    l_rw=500
    
    N1=length(residuals1)
    
    N2=length(residuals2)
    
    var_residuals1=numeric(N1)
    
    acf_residuals1=numeric(N1)
    
    avgspec_residuals1=numeric(N1)
    
    var_residuals2=numeric(N2)
    
    acf_residuals2=numeric(N2)
    
    avgspec_residuals2=numeric(N2)
    
  ### Apply rolling window to residuals.
  
  ### Residual data analysis.
  
    for (i in 1:(N1-l_rw+1))
      
    {
      
      rolldata1 = residuals1[i:(i+l_rw-1)];
      
      var_residuals1[i+l_rw-1] = var(rolldata1)
      
      spec_residuals1 = spectrum(rolldata1,plot=FALSE)$spec
      
      acf_residuals1[i+l_rw-1] = acf(rolldata1,plot=FALSE)$acf[2]
      
      avgspec_residuals1[i+l_rw-1] = mean(spec_residuals1[2:floor(l_rw/8)])
      
    }
    
    
    for (i in 1:(N2-l_rw+1))
      
    {
      
      rolldata2 = residuals2[i:(i+l_rw-1)];
      
      var_residuals2[i+l_rw-1] = var(rolldata2)
      
      spec_residuals2 = spectrum(rolldata2,plot=FALSE)$spec
      
      acf_residuals2[i+l_rw-1] = acf(rolldata2,plot=FALSE)$acf[2]
      
      avgspec_residuals2[i+l_rw-1] = mean(spec_residuals2[2:floor(l_rw/8)])
      
    }
    
    ### Parameters
      N=1000
      
      l_kw=250

    ### Kendall-Tau values

        kendaltau_var1 = Kendall(1:l_kw, var_residuals1[(N-l_kw+1):N])$tau[1];
      
        kendaltau_avgspec1 = Kendall(1:l_kw, avgspec_residuals1[(N-l_kw+1):N])$tau[1];
        
        kendaltau_acf1 = Kendall(1:l_kw, acf_residuals1[(N-l_kw+1):N])$tau[1];
      
        kendalls1 = list(acf=kendaltau_acf1,  var=kendaltau_var1, spec=kendaltau_avgspec1)
      
        kendaltau_var2 = Kendall(1:l_kw, var_residuals2[(N-l_kw+1):N])$tau[1];
        
        kendaltau_avgspec2 = Kendall(1:l_kw, avgspec_residuals2[(N-l_kw+1):N])$tau[1];
        
        kendaltau_acf2 = Kendall(1:l_kw, acf_residuals2[(N-l_kw+1):N])$tau[1];
        
        kendalls2 = list(acf=kendaltau_acf2,  var=kendaltau_var2, spec=kendaltau_avgspec2)
      
      # All the kendall-tau values equal to one have been made to be 0.999 because of the error
      # it produces while generating histograms
  
  ### 1st and 2nd Norms Variance, Average Spectrum, and ACF plots 250 trading days prior 03-16-2020
  
    par(mfrow=c(1,2))
    
    t <-250
    
    dates2 <- dailyreturnsETF1[(mindlrETF1[2]-(l-1)):(mindlrETF1[2]) ,length(ETFtickers1)+1]
    
    
      plot(as.Date(dates2[(l-t+1):l]),var_residuals1[(l-t+1):l],type='l',lwd = 5,xlab= 'Month',ylab = 'Variance',
         col='black', main='1st Norm \n Variance \n (ETFs)')
    
      plot(as.Date(dates2[(l-t+1):l]),avgspec_residuals1[(l-t+1):l],type='l',lwd = 1,xlab= 'Month',ylab = 'Average Spectrum',
           col='black', main='1st Norm \n Average Spectrum \n (ETFs)')
      
      plot(as.Date(dates2[(l-t+1):l]),acf_residuals1[(l-t+1):l],type='l',lwd = 1,xlab= 'Month',ylab = 'ACF',
           col='black', main='1st Norm \n ACF \n (ETFs)')
    
      plot(as.Date(dates2[(l-t+1):l]),var_residuals2[(l-t+1):l],type='l',lwd = 5,xlab= 'Month',ylab = 'Variance',
           col='blue', main='2nd Norm \n Variance \n (ETFs)')
      
      plot(as.Date(dates2[(l-t+1):l]),avgspec_residuals2[(l-t+1):l],type='l',lwd = 5,xlab= 'Month',ylab = 'Average Spectrum',
           col='red', main='2nd Norm \n Average Spectrum \n (ETFs)')
      
      plot(as.Date(dates2[(l-t+1):l]),acf_residuals2[(l-t+1):l],type='l',lwd = 5,xlab= 'Month',ylab = 'ACF',
           col='gray', main='2nd Norm \n ACF \n (ETFs)')