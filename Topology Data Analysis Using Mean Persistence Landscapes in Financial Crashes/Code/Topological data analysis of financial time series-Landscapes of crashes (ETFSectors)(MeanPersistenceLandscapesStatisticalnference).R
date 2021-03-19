  library(PerformanceAnalytics)
  library(PortfolioAnalytics)
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
  
    for (iETF2 in 1:length(ETFtickers1))
      
      {
        
        dataETF2<-getSymbols(ETFtickers1[iETF2], src="yahoo",from="2019-06-30", to="2020-07-01",periodcity="daily")
        
      }
  
  ### ETF Adjusted Closing Prices
  
    ETF2 = list()
    
    j1ETF2 = 0
    
      for (aETF2 in ETFtickers1)
        
      {
        
        j1ETF2 = j1ETF2 + 1
        
        ETF_symbol2 = noquote(aETF2)
        
        rowssETF2 = data.frame(get(ETF_symbol2)[ , 6 ])
        
        rowssETF2$dt = row.names(rowssETF2)
        
        ETF2[[j1ETF2]] = rowssETF2
        
      }
      
    ETF_table2 = ETF2[[1]]
    
      for (jETF2 in 2 : length(ETF2))
        
      {
        
        ETF_table2 = merge(ETF_table2 , ETF2[[jETF2]], by="dt" )
        
      }
    
  ### ETF Daily Log Returns
  
      nETF2 = length(ETF_table2[ , 1 ])
      
      dailyreturnsETF2 = ETF_table2[ , 2 : (length (ETFtickers1)+1 ) ]
    
      for ( jETF2 in 1:length(ETFtickers1)) 
        
        {
          
          dailyreturnsETF2[2:nETF2, jETF2] = diff(log(dailyreturnsETF2[ , jETF2 ]))
          
        }
    
    dailyreturnsETF2$dt = ETF_table2$dt
    
    dailyreturnsETF2 = dailyreturnsETF2[2:nETF2 , ] 
    
    dailyreturnsETF2 = na.omit(dailyreturnsETF2)
    
    colnames(dailyreturnsETF2) <-c(ETFtickers1,"Date")
    
  ### Daily Log Returns Summary Statistics
    
    statsETF2=cbind(apply(dailyreturnsETF2[,1:length(ETFtickers1)],2, mean),
                    
      apply(dailyreturnsETF2[,1:length(ETFtickers1)],2, var),
                    
      apply(dailyreturnsETF2[,1:length(ETFtickers1)],2, sd),
                    
      apply(dailyreturnsETF2[,1:length(ETFtickers1)],2, skewness),
                    
      apply(dailyreturnsETF2[,1:length(ETFtickers1)],2, kurtosis))
    
    round(statsETF2,4)
    
    colnames(statsETF2) = c("Mean", "Variance", "Std Dev","Skewness", "Excess Kurtosis")
    
    # kable(statsETF2,caption="<center>Summary Statistics for ETF Sectors</center>",
    #       
    #       align="c",format="latex")%>%
    #   
    #   kable_styling("striped", full_width = F)  

  ### Maximum and Minimum Daily Log Returns
    
    maxdrETF2 <-0.05
    
    mindrETF2 <- -0.05
    
    dttETF2   <- as.Date('2020-04-20')
    
    maxdlrETF2 <-which(dailyreturnsETF2[,1] >=maxdrETF2 & dailyreturnsETF2[,2] >=maxdrETF2 & dailyreturnsETF2[,3] >=maxdrETF2 & dailyreturnsETF2[,4] >=maxdrETF2 & dailyreturnsETF2[,5] >=maxdrETF2 & dailyreturnsETF2[,6] >=maxdrETF2 &  
                     dailyreturnsETF2[,7] >=maxdrETF2 & dailyreturnsETF2[,8] >=maxdrETF2 & dailyreturnsETF2[,9] >=maxdrETF2 & dailyreturnsETF2[,10]>=maxdrETF2 & dailyreturnsETF2$Date<=dttETF2 )
    
    mindlrETF2 <-which(dailyreturnsETF2[,1] <=mindrETF2 & dailyreturnsETF2[,2] <=mindrETF2 & dailyreturnsETF2[,3] <=mindrETF2 & dailyreturnsETF2[,4] <=mindrETF2 & dailyreturnsETF2[,5] <=mindrETF2 & dailyreturnsETF2[,6] <=mindrETF2 &  
                     dailyreturnsETF2[,7] <=mindrETF2 & dailyreturnsETF2[,8] <=mindrETF2 & dailyreturnsETF2[,9] <=mindrETF2 & dailyreturnsETF2[,10]<=mindrETF2 & dailyreturnsETF2$Date<=dttETF2 )

  ### Rips Filtration Parameters
  
    wETF2 <- 50
    
    pc1ETF2<- seq(1,length(dailyreturnsETF2[,1])-wETF2+1,1)
    
    pc2ETF2<- seq(wETF2,length(dailyreturnsETF2[,1]),1)
    
    max.filtrationETF2<- 0.08
    
    XETF2<-vector("list",(length(dailyreturnsETF2[,1])-wETF2+1))
    
    for (iETF2 in 1:(length(dailyreturnsETF2[,1])-wETF2+1))
      
    {
      
      XETF2[[iETF2]]<-dailyreturnsETF2[pc1ETF2[iETF2]:pc2ETF2[iETF2],1:(length(ETFtickers1))]
      
    }

  ### Euclidean distance between vectors

    euclidean.distance <- function(u, v) sqrt(sum((u - v) ^ 2))
    
    
  ### Persistence Diagrams Lists
    
    DiagETF2 <- vector("list",(length(dailyreturnsETF2[,1])-wETF2+1))
    
    PD.listETF2 <- vector("list",(length(dailyreturnsETF2[,1])-wETF2+1))
    
  ### Persistence Diagrams Actual List
    
    for (iETF2 in 1:(length(dailyreturnsETF2[,1])-wETF2+1))
      
      {
      
        DiagETF2[[iETF2]]    <- ripsDiag(XETF2[[iETF2]], maxdimension = 1, maxscale = max.filtrationETF2)
      
        PD.listETF2[[iETF2]] <- DiagETF2[[iETF2]][["diagram"]]
      
      }
    
    ### Persistence Landscapes Lists
    
    PL.listETF2 <- vector("list", (length(dailyreturnsETF2[,1])-wETF2+1))
    
    for (iETF2 in 1:(length(dailyreturnsETF2[,1])-wETF2+1))
      
      { 
      
        PL.listETF2[[iETF2]] <- t(landscape(PD.listETF2[[iETF2]],
                                        
        dimension=1,KK=1,seq(0,max.filtrationETF2,length=length(PD.listETF2[[iETF2]]))))
      
      }

  ### Matrix of persistence landscape row vectors from list of persistence landscapes
    
      landscape.matrix.from.list <- function(PL.list){
        n <- length(PL.list)
        m <- ncol(PL.list[[1]])
        max.depth <- integer(n)
        for (i in 1:n)
          max.depth[i] <- nrow(PL.list[[i]])
        K <- max(max.depth)
        PL.matrix <- Matrix(0, nrow = n, ncol = m*K, sparse = TRUE)
        for (i in 1:n)
          for (j in 1:max.depth[i])
            PL.matrix[i,(1+(j-1)*m):(j*m)] <- PL.list[[i]][j,]
        return(PL.matrix)
      }
    
  ### Convert a vector to a persistence landscape
    
    landscape.from.vector <- function(PL.vector, t.vals)
        
      {
        
        m <- length(t.vals)
        K <- length(PL.vector)/m
        PL <- Matrix(0, nrow = K, ncol=m, sparse = TRUE)
        
        for (i in 1:K)
          
          {
          
            PL[i,1:m] <- PL.vector[(1+(i-1)*m):(i*m)]
          
          }
        
        return(PL)
        
      }

  ### Mean Landscapes Two Samples
      
      num.repeatsETF2<-10
      
      APD.listETF2<-vector("list", (length(dailyreturnsETF2[,1])-wETF2+1))
      
      APH.outputETF2<-vector("list", (length(dailyreturnsETF2[,1])-wETF2+1))
      
      for (iETF2 in 1:(length(dailyreturnsETF2[,1])-wETF2+1))
        
        { 
          
          APD.listETF2[[iETF2]]<-vector("list", num.repeatsETF2)
          
          APH.outputETF2[[iETF2]]<-vector("list", num.repeatsETF2)
          
          APH.outputETF2[[iETF2]]<-ripsDiag(XETF2[[iETF2]], maxdimension = 1, maxscale = max.filtrationETF2)
          
        }
      
      for (iETF2 in 1:(length(dailyreturnsETF2[,1])-wETF2+1))
        
        { 
          APH.outputETF2[[iETF2]]<-ripsDiag(XETF2[[iETF2]], maxdimension = 1, maxscale = max.filtrationETF2)
          
          
          for (cETF2 in 1:num.repeatsETF2)
            
          {
          
            APD.listETF2[[iETF2]][[cETF2]]<-APH.outputETF2[[iETF2]][["diagram"]]
  
          }
        
      }
      
      bETF2 <-seq((mindlrETF2[2]-wETF2+1),(length(dailyreturnsETF2[,1])-wETF2+1),1)
      
      APL.listETF2 <- vector("list", (length(dailyreturnsETF2[,1])-wETF2+1))
      
      for (iETF2 in 1:(length(dailyreturnsETF2[,1])-wETF2+1))
        
        {
          
          APL.listETF2[[iETF2]]<-vector("list", num.repeatsETF2)
          
        }
      
      for (iETF2 in 1:(length(dailyreturnsETF2[,1])-wETF2+1))
        
        {
          
          for (cETF2 in 1:num.repeatsETF2)
            
            {
              
              APL.listETF2[[iETF2]][[cETF2]] <- t(landscape(APD.listETF2[[iETF2]][[cETF2]],dimension=1,KK=1:100,
                                                            
                tseq=seq(0,max.filtrationETF2,length=length(APD.listETF2[[iETF2]][[cETF2]]))))
              
            }
          
        }
      
      APL.matrixETF2 <- vector("list", (length(dailyreturnsETF2[,1])-wETF2+1))
      
      for (iETF2 in 1:(length(dailyreturnsETF2[,1])-wETF2+1))
        
        {
          
          APL.matrixETF2[[iETF2]] <- landscape.matrix.from.list(APL.listETF2[[iETF2]])  
          
        }
      
      average.PL.vectorETF2 <- vector("list", (length(dailyreturnsETF2[,1])-wETF2+1))
      
      average.PLETF2 <-vector("list", (length(dailyreturnsETF2[,1])-wETF2+1))
      
      for (iETF2 in 1:(length(dailyreturnsETF2[,1])-wETF2+1))
        
        {
          
          average.PL.vectorETF2[[iETF2]] <- colMeans(APL.matrixETF2[[iETF2]], sparseResult = TRUE)
          
          average.PLETF2[[iETF2]] <- landscape.from.vector(average.PL.vectorETF2[[iETF2]],seq(0,max.filtrationETF2,length=length(average.PL.vectorETF2[[iETF2]])))
          
        }
      
    ### Mean Landscape Parameters
      
      qu1ETF2 <- vector("list", (length(dailyreturnsETF2[,1])-wETF2+1))
      
      qu2ETF2 <- vector("list", (length(dailyreturnsETF2[,1])-wETF2+1))
      
    ### Mean Landscape Parameter Lists
      
      for (iETF2 in 1:length(average.PLETF2))
          
        {
          qu1ETF2[[iETF2]]<-seq(1,length(average.PLETF2[[iETF2]]), 
                                
            length(average.PLETF2[[iETF2]])/(num.repeatsETF2*10))
          
          qu2ETF2[[iETF2]]<-seq(length(average.PLETF2[[iETF2]])/(num.repeatsETF2*10),
                                
            length(average.PLETF2[[iETF2]]),length(average.PLETF2[[iETF2]])/(num.repeatsETF2*10))
        }
      
    ### Topological Summaries (Persistent Barcodes, Persistent Diagrams, Persistence Landscapes, Mean Landscapes)
    ### Daily Log Returns with Sliding Window (50 trading days)  
        
        for (iETF2 in ((mindlrETF2[2]-wETF2+1):(length(dailyreturnsETF2[,1])-wETF2+1)))
          
          {
          
            png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Topological Summaries/TSDLR",
                          
                format(as.Date(dailyreturnsETF2[iETF2,length(ETFtickers1)+1]),"%m%d%Y"), " - ",
                          
                format(as.Date(dailyreturnsETF2[(iETF2+wETF2-1),length(ETFtickers1)+1]),"%m%d%Y"),
                          
                "(AllETFs).png"),height=758,width=1100)
          
            par(mfrow=c(2,2))
          
            plot(as.Date(dailyreturnsETF2[iETF2:(iETF2+wETF2-1),length(ETFtickers1)+1]), ylim=c(-0.16,0.16),
               
                dailyreturnsETF2[iETF2:(iETF2+wETF2-1),1], xlab="Day", ylab="Daily Log Returns", cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
               
                main=paste0("Panel A: Daily Log Returns-ETF sectors \n ", 
                           
                format(as.Date(dailyreturnsETF2[iETF2,length(ETFtickers1)+1]),"%B %d, %Y")," - " ,
                           
                format(as.Date(dailyreturnsETF2[(iETF2+wETF2-1),length(ETFtickers1)+1]),"%B %d, %Y")),
               
                type="h",col="blue",lwd=3)
          
            abline(h=0,col="black",lwd=2)
          
            for (i in 2:(length(ETFtickers1)))
            
              {
                
                lines(as.Date(dailyreturnsETF2[iETF2:(iETF2+wETF2-1),length(ETFtickers1)+1]),  
                      
                      dailyreturnsETF2[iETF2:(iETF2+wETF2-1),i],
                      
                      type="h",col=i,lwd=3)
                
              }
          
          plot(PD.listETF2[[iETF2]], main=paste0("Panel B: Persistence Diagram-ETF sectors"), cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
                                               
            diagLim = c(min(PD.listETF2[[iETF2]]), max.filtrationETF2))
          
          plot(seq(0,max.filtrationETF2,length=length(PD.listETF2[[iETF2]])),PL.listETF2[[iETF2]],  cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
               
            xlab="(Birth+Death)/2", ylab="(Death-Birth)/2" , type="l", lwd=3,
               
            main=paste0("Panel C: Persistence Landscape-ETF sectors"))
          
          plot(average.PLETF2[[iETF2]][1,qu1ETF2[[iETF2]][1]:qu2ETF2[[iETF2]][1]], cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
               
            xlim=c(0,length(average.PLETF2[[iETF2]])/(num.repeatsETF2*10)),
               
            ylim=c(min(average.PLETF2[[iETF2]]),max(average.PLETF2[[iETF2]])),type="l",lwd=3,xlab="",
               
            ylab="", main=paste0("Panel D: Mean Landscape-ETF sectors"))
          
          for (jETF2 in 2:(num.repeatsETF2*10))
            
            {
              
              lines(average.PLETF2[[iETF2]][1,qu1ETF2[[iETF2]][jETF2]:qu2ETF2[[iETF2]][jETF2]],type="l",col=iETF2,lwd=2)
              
            }
          
          dev.off()
            
        }
      


    ### 1st and 2nd Norms for July 2, 2019-June 30,2020
        
      norm1ETF2 <- 0
        
      norm2ETF2 <- 0
        
    ### 1st and 2nd Norms lists
        
        for (i in 1:length(PL.listETF2))
          
        {
          
          ### 1st Norm 
          
          norm1ETF2[i] <-  norm(t(PL.listETF2[[i]]),type="1")
          
          ### 2nd Norm
          
          norm2ETF2[i] <- norm(t(PL.listETF2[[i]]),type="2")  #sum(abs(PL.list[[i]])^pp)^(1/pp) 
          
        }
        
        png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Norms/NormsDLR",
                        
          format(as.Date(dailyreturnsETF2[1,length(ETFtickers1)+1]),"%m%d%Y"), " - ",
                        
          format(as.Date(dailyreturnsETF2[length(dailyreturnsETF2[,1]),length(ETFtickers1)+1]),"%m%d%Y"),
                        
          "(AllETFs).png"),height=837,width=1117)
        
        par(mfrow=c(1,2))
        
        plot(as.Date(dailyreturnsETF2[1:length(norm1ETF2),length(ETFtickers1)+1]),norm1ETF2,lwd=3, ,xlab="Month", ylab="",
             
             main=paste0("Panel A: Norms for Persistence Landscape-ETF sectors"), cex.lab=1.32, cex.main=1.39,cex.axis=1.32,
             
             type="l",col="red")
        
        lines(as.Date(dailyreturnsETF2[1:length(norm1ETF2),length(ETFtickers1)+1]),norm2ETF2,type="l",lty=2,col="blue",  lwd=3)
        
        legend(x = "topleft", legend = c("L1", "L2"), lty = c(1,2), lwd = 3, col = c("red", "blue"),cex=1.7,bty='n')
        
        lines(as.Date(dailyreturnsETF2[1:length(norm1ETF2),length(ETFtickers1)+1]),norm2ETF2,type="l",lty=2,col="blue",  lwd=3)
        
        segments(as.Date(dailyreturnsETF2[1,length(ETFtickers1)+1]), norm1ETF2[1],
                 
          as.Date(dailyreturnsETF2[1,length(ETFtickers1)+1]), min(norm1ETF2[1],norm2ETF2[1]),
                 
          col=("black"),lwd=2.5,lty=3)
        
        segments(as.Date(dailyreturnsETF2[which.max(norm1ETF2),length(ETFtickers1)+1]), max(norm1ETF2),
                 
          as.Date(dailyreturnsETF2[which.max(norm1ETF2),length(ETFtickers1)+1]), max(norm2ETF2),
                 
          col=("black"),lwd=2.5, lty=3)
        
        plot(as.Date(dailyreturnsETF2[,length(ETFtickers1)+1]), ylim=c(min(dailyreturnsETF2[,1:length(ETFtickers1)]),
                                                                       
          -min(dailyreturnsETF2[,1:length(ETFtickers1)])), dailyreturnsETF2[,1], xlab="Month", ylab="Daily Log Returns",
             
          main=paste0("Panel B: Daily Log Returns-ETF sectors"), cex.lab=1.32, cex.main=1.39,cex.axis=1.32,
          
          type="l",col="black",lwd=3)
        
        for (i in 2:(length(ETFtickers1)))
          
        {
          
          lines(as.Date(dailyreturnsETF2[,length(ETFtickers1)+1]),
                
                dailyreturnsETF2[,i],
                
                type="l",col=i,lwd=3)
          
        }
        
        abline(h=0,col="black",lwd=2)
        
        segments(as.Date(dailyreturnsETF2[1,length(ETFtickers1)+1]), max(dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)],dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)]),
                 
          as.Date(dailyreturnsETF2[1,length(ETFtickers1)+1]), min(dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)],dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)]),
                 
          col=("black"),lwd=2)
        
        segments(as.Date(dailyreturnsETF2[wETF2,length(ETFtickers1)+1]), max(dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)],dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)]),
                 
          as.Date(dailyreturnsETF2[wETF2,length(ETFtickers1)+1]), min(dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)],dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)]),
                 
          col=("black"),lwd=2)
        
        segments(as.Date(dailyreturnsETF2[1,length(ETFtickers1)+1]), max(dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)],dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)]),
                 
          as.Date(dailyreturnsETF2[wETF2,length(ETFtickers1)+1]), max(dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)],dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)]),
                 
          col=("black"),lwd=2)
        
        segments(as.Date(dailyreturnsETF2[1,length(ETFtickers1)+1]), min(dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)],dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)]),
                 
          as.Date(dailyreturnsETF2[wETF2,length(ETFtickers1)+1]), min(dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)],dailyreturnsETF2[1:wETF2,1:length(ETFtickers1)]),
                 
          col=("black"),lwd=2)
        
        segments(as.Date(dailyreturnsETF2[which.max(norm1ETF2),length(ETFtickers1)+1]), max(dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)],dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)]),
                 
          as.Date(dailyreturnsETF2[which.max(norm1ETF2),length(ETFtickers1)+1]), min(dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)],dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)]),
                 
          col=("black"),lwd=2)
        
        segments(as.Date(dailyreturnsETF2[which.max(norm1ETF2)+wETF2-1,length(ETFtickers1)+1]), max(dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)],dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)]),
                 
          as.Date(dailyreturnsETF2[which.max(norm1ETF2)+wETF2-1,length(ETFtickers1)+1]), min(dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)],dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)]),
                 
          col=("black"),lwd=2)
        
        segments(as.Date(dailyreturnsETF2[which.max(norm1ETF2),length(ETFtickers1)+1]), max(dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)],dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)]),
                 
          as.Date(dailyreturnsETF2[which.max(norm1ETF2)+wETF2-1,length(ETFtickers1)+1]), max(dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)],dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)]),
                 
          col=("black"),lwd=2)
        
        segments(as.Date(dailyreturnsETF2[which.max(norm1ETF2),length(ETFtickers1)+1]), min(dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)],
                                                                                      
          dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)]),
                 
          as.Date(dailyreturnsETF2[which.max(norm1ETF2)+wETF2-1,length(ETFtickers1)+1]), min(dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)],
                                                                                             
          dailyreturnsETF2[which.max(norm1ETF2):(which.max(norm1ETF2)+wETF2-1),1:length(ETFtickers1)]), col=("black"),lwd=2)
        
        dev.off()

  ### Permutation test for two matrices consisting of row vectors (Bubenik's code)
    
      permutation.test <- function(M1 ,M2, num.repeats = 10000){
        # append zeros if necessary so that the matrices have the same number of columns
        num.columns <- max(ncol(M1),ncol(M2))
        M1 <- cbind(M1, Matrix(0,nrow=nrow(M1),ncol=num.columns-ncol(M1)))
        M2 <- cbind(M2, Matrix(0,nrow=nrow(M2),ncol=num.columns-ncol(M2)))
        t.obs <- euclidean.distance(colMeans(M1),colMeans(M2))
        k <- dim(M1)[1]
        M <- rbind(M1,M2)
        n <- dim(M)[1]
        count <- 0
        for (i in 1:num.repeats){
          permutation <- sample(1:n)
          t <- euclidean.distance(colMeans(M[permutation[1:k],]),colMeans(M[permutation[(k+1):n],]))
          if (t >= t.obs)
            count <- count + 1
        }
        
        return(count/num.repeats)  
        
      }
      
    ### Creating Two Matrices to use for the permutation tests 
      
      M1ETF2<-vector("list", (length(dailyreturnsETF2[,1])-wETF2+1)-1)
      
      M2ETF2<-vector("list", (length(dailyreturnsETF2[,1])-wETF2+1)-1)
      
        for (iETF2 in 1:((length(dailyreturnsETF2[,1])-wETF2+1)-1))
          
          {
            M1ETF2[[iETF2]]<-APL.matrixETF2[[iETF2]]
          
            M2ETF2[[iETF2]]<-APL.matrixETF2[[(iETF2+1)]]
            
          }
      
    ### Using the permutation test function to obtain the p-values
      
      M3ETF2<-0
  
          for (iETF2 in 1:((length(dailyreturnsETF2[,1])-wETF2+1)-1))
          
            {
            
              M3ETF2[iETF2]<-permutation.test(M1ETF2[[iETF2]],M2ETF2[[iETF2]],1000)
          
          }
  ### Sliding Window graphs split into two years, need to change ALPHA!!!, png file to lt or gt, 
        
  ### and chart title  to either >= or <  
    
    ### Determine significance value
      
      alphaETF2<-0.05
      
    ### Obtaining the p-values that are less than the significance value
      
      length(which(M3ETF2==0))
      
      length(which(M3ETF2>0 & M3ETF2<alphaETF2))
      
      length(which(M3ETF2>=alphaETF2))
      