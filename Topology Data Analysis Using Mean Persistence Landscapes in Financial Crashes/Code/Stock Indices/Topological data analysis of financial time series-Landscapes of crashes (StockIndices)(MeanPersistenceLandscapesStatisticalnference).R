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
  
  ### Stock Tickers
  
    tickers <- c("^DJI", "^GSPC", "^IXIC", "^RUT")
    
    tickers2 <- substr(tickers,2,length(tickers)+1)
    
    tickers3 <- c("Dow Jones", "S&P 500", "NASDAQ", "Russell 2000")
  
  ### Stock Prices
  
    for (iSI2 in 1:length(tickers))
      
      {
        
        dataSI2<-getSymbols(tickers[iSI2], src="yahoo",from="2019-06-30", to="2020-07-01",periodcity="daily")
        
      }
  
  ### Stock Adjusted Closing Prices
  
    stocks2 = list()
    
    j1SI2 = 0
    
    for (aSI2 in tickers2)
      
      {
        
        j1SI2 = j1SI2 + 1
        
        stock_symbol2 = noquote(aSI2)
        
        rowssSI2 = data.frame(get(stock_symbol2)[ , 6 ])
        
        rowssSI2$dt = row.names(rowssSI2)
        
        stocks2[[j1SI2]] = rowssSI2
        
      }
    
    stock_table2 = stocks2[[1]]
    
    for (jSI2 in 2 : length(stocks2))
      
      {
       
        stock_table2 = merge(stock_table2 , stocks2[[jSI2]], by="dt" )
       
      }
  
  ### Stock Daily Log Returns
  
    nSI2 = length(stock_table2 [ , 1 ])
    
    dailyreturnsSI2 = stock_table2[ , 2 : (length (tickers)+1 ) ]
    
    for ( jSI2 in 1:length(tickers)) 
      
      {
        
        dailyreturnsSI2[2:nSI2, jSI2] = diff(log(dailyreturnsSI2[ , jSI2 ]))
        
      }
    
    dailyreturnsSI2$dt = stock_table2$dt
    
    dailyreturnsSI2 = dailyreturnsSI2[2:nSI2 , ] 
    
    dailyreturnsSI2 = na.omit(dailyreturnsSI2)
    
    colnames(dailyreturnsSI2) <-c(tickers3,"Date")
  
  ### Daily Log Returns Summary Statistics
  
    statsSI2=cbind(apply(dailyreturnsSI2[,1:length(tickers)],2, mean),
                   
      apply(dailyreturnsSI2[,1:length(tickers)],2, var),
                     
      apply(dailyreturnsSI2[,1:length(tickers)],2, sd),
                     
      apply(dailyreturnsSI2[,1:length(tickers)],2, skewness),
                     
      apply(dailyreturnsSI2[,1:length(tickers)],2, kurtosis))
    
      round(statsSI2,4)
    
    colnames(statsSI2) = c("Mean", "Variance", "Std Dev","Skewness", "Excess Kurtosis")
  
  ### Maximum and Minimum Daily Log Returns
  
    maxdrSI2 <-0.05
    
    mindrSI2 <- -0.05
    
    dttSI2   <- as.Date('2020-04-20')
    
    maxdlrSI2 <-which(dailyreturnsSI2[,1] >=maxdrSI2 & dailyreturnsSI2[,2] >=maxdrSI2 & dailyreturnsSI2[,3] >=maxdrSI2 & dailyreturnsSI2[,4] >=maxdrSI2 & dailyreturnsSI2$Date<=dttSI2 )
    
    mindlrSI2 <-which(dailyreturnsSI2[,1] <=mindrSI2 & dailyreturnsSI2[,2] <=mindrSI2 & dailyreturnsSI2[,3] <=mindrSI2 & dailyreturnsSI2[,4] <=mindrSI2 & dailyreturnsSI2$Date<=dttSI2)
    
  ### Rips Filtration Parameters
  
    wSI2 <- 50
    
    pc1SI2<- seq(1,length(dailyreturnsSI2[,1])-wSI2+1,1)
    
    pc2SI2<- seq(wSI2,length(dailyreturnsSI2[,1]),1)
    
    max.filtrationSI2<- 0.055
    
    XSI2<-vector("list",(length(dailyreturnsSI2[,1])-wSI2+1))
    
    for (iSI2 in 1:(length(dailyreturnsSI2[,1])-wSI2+1))
      
      {
        
        XSI2[[iSI2]]<-dailyreturnsSI2[pc1SI2[iSI2]:pc2SI2[iSI2],1:(length(tickers))]
        
      }
  
  ### Euclidean distance between vectors
  
    euclidean.distance <- function(u, v) sqrt(sum((u - v) ^ 2))
  
  ### Persistence Diagrams Lists
  
    DiagSI2 <- vector("list",(length(dailyreturnsSI2[,1])-wSI2+1))
    
    PD.listSI2 <- vector("list",(length(dailyreturnsSI2[,1])-wSI2+1))
    
  ### Persistence Diagrams Actual List
  
    for (iSI2 in 1:(length(dailyreturnsSI2[,1])-wSI2+1))
      
    {
      
      DiagSI2[[iSI2]]    <- ripsDiag(XSI2[[iSI2]], maxdimension = 1, maxscale = max.filtrationSI2)
      
      PD.listSI2[[iSI2]] <- DiagSI2[[iSI2]][["diagram"]]
      
    }
  
  ### Persistence Landscapes Lists
  
    PL.listSI2 <- vector("list", (length(dailyreturnsSI2[,1])-wSI2+1))
    
    for (iSI2 in 1:(length(dailyreturnsSI2[,1])-wSI2+1))
      
      { 
        
        PL.listSI2[[iSI2]] <- t(landscape(PD.listSI2[[iSI2]],
                                          
        dimension=1,KK=1,seq(0,max.filtrationSI2,length=length(PD.listSI2[[iSI2]]))))
        
      }
  
  ### Matrix of persistence landscape row vectors from list of persistence landscapes (Bubenik Code)
  
    landscape.matrix.from.list <- function(PL.list)
      
    {
      
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
  
  ### Mean Persistence Landscapes Two Samples
  
    num.repeatsSI2<-10
    
    APD.listSI2<-vector("list", (length(dailyreturnsSI2[,1])-wSI2+1))
    
    APH.outputSI2<-vector("list", (length(dailyreturnsSI2[,1])-wSI2+1))
    
    for (iSI2 in 1:(length(dailyreturnsSI2[,1])-wSI2+1))
      
      { 
        
        APD.listSI2[[iSI2]]<-vector("list", num.repeatsSI2)
        
        APH.outputSI2[[iSI2]]<-vector("list", num.repeatsSI2)
        
        APH.outputSI2[[iSI2]]<-ripsDiag(XSI2[[iSI2]], maxdimension = 1, maxscale = max.filtrationSI2)
        
      }
    
    for (iSI2 in 1:(length(dailyreturnsSI2[,1])-wSI2+1))
      
      { 
        APH.outputSI2[[iSI2]]<-ripsDiag(XSI2[[iSI2]], maxdimension = 1, maxscale = max.filtrationSI2)
      
      
        for (cSI2 in 1:num.repeatsSI2)
        
          {
            
            APD.listSI2[[iSI2]][[cSI2]]<-APH.outputSI2[[iSI2]][["diagram"]]
            
          }
      
      }
    
    APL.listSI2 <- vector("list", (length(dailyreturnsSI2[,1])-wSI2+1))
    
    for (iSI2 in 1:(length(dailyreturnsSI2[,1])-wSI2+1))
      
      {
      
        APL.listSI2[[iSI2]]<-vector("list", num.repeatsSI2)
      
      }
    
    for (iSI2 in 1:(length(dailyreturnsSI2[,1])-wSI2+1))
      
      {
        
        for (cSI2 in 1:num.repeatsSI2)
          
          {
            
            APL.listSI2[[iSI2]][[cSI2]] <- t(landscape(APD.listSI2[[iSI2]][[cSI2]],dimension=1,KK=1:100,
                                                       
           tseq=seq(0,max.filtrationSI2,length=length(APD.listSI2[[iSI2]][[cSI2]]))))
            
          }
        
      }
      
    APL.matrixSI2 <- vector("list", (length(dailyreturnsSI2[,1])-wSI2+1))
    
    for (iSI2 in 1:(length(dailyreturnsSI2[,1])-wSI2+1))
      
      {
        
        APL.matrixSI2[[iSI2]] <- landscape.matrix.from.list(APL.listSI2[[iSI2]])  
        
      }
    
    average.PL.vectorSI2 <- vector("list", (length(dailyreturnsSI2[,1])-wSI2+1))
    
    average.PLSI2 <-vector("list", (length(dailyreturnsSI2[,1])-wSI2+1))
    
    for (iSI2 in 1:(length(dailyreturnsSI2[,1])-wSI2+1))
      
      {
        
        average.PL.vectorSI2[[iSI2]] <- colMeans(APL.matrixSI2[[iSI2]], sparseResult = TRUE)
        
        average.PLSI2[[iSI2]] <- landscape.from.vector(average.PL.vectorSI2[[iSI2]],seq(0,max.filtrationSI2,length=length(average.PL.vectorSI2[[iSI2]])))
        
      }
  
  ### Mean Landscape Parameters
  
  qu1SI2 <- vector("list", (length(dailyreturnsSI2[,1])-wSI2+1))
  
  qu2SI2 <- vector("list", (length(dailyreturnsSI2[,1])-wSI2+1))
  
  ### Mean Landscape Parameter lists
  
  for (iSI2 in 1:length(average.PLSI2))
    
    {
      qu1SI2[[iSI2]]<-seq(1,length(average.PLSI2[[iSI2]]), 
                          
                      length(average.PLSI2[[iSI2]])/(num.repeatsSI2*10))
      
      qu2SI2[[iSI2]]<-seq(length(average.PLSI2[[iSI2]])/(num.repeatsSI2*10),
                          
                      length(average.PLSI2[[iSI2]]),length(average.PLSI2[[iSI2]])/(num.repeatsSI2*10))
      
    }
  
  ### Topological Summaries (Persistent Barcodes, Persistent Diagrams, Persistence Landscapes, Mean Landscapes)
  ### Daily Log Returns with Sliding Window (50 trading days)      
  
  for (iSI2 in ((mindlrSI2[3]-wSI2+1):(length(dailyreturnsSI2[,1])-wSI2+1)))
    
  {
    
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Topological Summaries/TSDLR",
                    
      format(as.Date(dailyreturnsSI2[iSI2,length(tickers)+1]),"%m%d%Y"), " - ",
                    
      format(as.Date(dailyreturnsSI2[(iSI2+wSI2-1),length(tickers)+1]),"%m%d%Y"),
                    
      "(AllStockIndices).png"),height=758,width=1100)
    
    par(mfrow=c(2,2))
    
    plot(as.Date(dailyreturnsSI2[iSI2:(iSI2+wSI2-1),length(tickers)+1]), ylim=c(-0.15,0.15), cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
         
      dailyreturnsSI2[iSI2:(iSI2+wSI2-1),1], xlab="Day", ylab="Daily Log Returns",
         
      main=paste0("Panel A: Daily Log Returns-Stock Indices \n",
                     
      format(as.Date(dailyreturnsSI2[iSI2,length(tickers)+1]),"%B %d, %Y")," - " ,
                     
      format(as.Date(dailyreturnsSI2[(iSI2+wSI2-1),length(tickers)+1]),"%B %d, %Y")),
         
      type="h",col="blue",lwd=3)
    
    abline(h=0,col="black",lwd=2)
    
    for (i in 2:(length(tickers)))
      
      {
        
        lines(as.Date(dailyreturnsSI2[iSI2:(iSI2+wSI2-1),length(tickers)+1]),
              
        dailyreturnsSI2[iSI2:(iSI2+wSI2-1),i],
              
        type="h",col=i,lwd=3)
        
      }
    
    plot(PD.listSI2[[iSI2]], main=paste0("Panel B: Persistence Diagram-Stock Indices"), cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
         
      diagLim = c(min(PD.listSI2[[iSI2]]), max.filtrationSI2))
    
    plot(seq(0,max.filtrationSI2,length=length(PD.listSI2[[iSI2]])),PL.listSI2[[iSI2]] , cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
         
      xlab="(Birth+Death)/2", ylab="(Death-Birth)/2" , type="l", lwd=3, 
         
      main=paste0("Panel C: Persistence Landscape-Stock Indices"))
    
    plot(average.PLSI2[[iSI2]][1,qu1SI2[[iSI2]][1]:qu2SI2[[iSI2]][1]], cex.lab=1.1, cex.main=1.17,cex.axis=1.05,
         
      xlim=c(0,length(average.PLSI2[[iSI2]])/(num.repeatsSI2*10)), 
         
      xlab=" ", ylab=" ", ylim=c(min(average.PLSI2[[iSI2]]),max(average.PLSI2[[iSI2]])),type="l",lwd=3,
         
      main=paste0("Panel D: Mean Landscape-Stock Indices"))
    
    for (jSI2 in 2:(num.repeatsSI2*10))
      
      {
      
        lines(average.PLSI2[[iSI2]][1,qu1SI2[[iSI2]][jSI2]:qu2SI2[[iSI2]][jSI2]],type="l",col=iSI2,lwd=2)
      
      }
    
    dev.off()
  
      }
  
  ### 1st and 2nd Norms for July 2, 2019-June 30,2020
  
    norm1SI2 <- 0
    
    norm2SI2 <- 0
    
    ### 1st and 2nd Norms lists
    
    for (i in 1:length(PL.listSI2))
      
      {
        
        ### 1st Norm 
        
          norm1SI2[i] <-  norm(t(PL.listSI2[[i]]),type="1")
        
        ### 2nd Norm
        
          norm2SI2[i] <- norm(t(PL.listSI2[[i]]),type="2")  #sum(abs(PL.list[[i]])^pp)^(1/pp) 
        
      }
    
    png(file=paste0("C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Norms/NormsDLR",
                    
                    format(as.Date(dailyreturnsSI2[1,length(tickers)+1]),"%m%d%Y"), " - ",
                    
                    format(as.Date(dailyreturnsSI2[length(dailyreturnsSI2[,1]),length(tickers)+1]),"%m%d%Y"),
                    
                    "(AllStockIndices).png"),height=837,width=1117)
    
    par(mfrow=c(1,2))
    
    plot(as.Date(dailyreturnsSI2[1:length(norm1SI2),length(tickers)+1]),norm1SI2,lwd=3, ,xlab="Month", ylab="",
         
      main=paste0("Panel A: Norms for Persistence Landscape-Stock Indices"), cex.lab=1.32, cex.main=1.39,cex.axis=1.32,
         
      type="l",col="red")
    
    legend(x = "topleft", legend = c("L1", "L2"), lty = c(1,2), lwd = 3, col = c("red", "blue"),cex=1.7,bty='n')
    
    lines(as.Date(dailyreturnsSI2[1:length(norm1SI2),length(tickers)+1]),norm2SI2,type="l",lty=2,col="blue",  lwd=3)
    
    segments(as.Date(dailyreturnsSI2[1,length(tickers)+1]), norm1SI2[1],
             
      as.Date(dailyreturnsSI2[1,length(tickers)+1]), min(norm1SI2[1],norm2SI2[1]),
             
      col=("black"),lwd=3,lty=3)
    
    segments(as.Date(dailyreturnsSI2[which.max(norm1SI2),length(tickers)+1]), max(norm1SI2),
             
      as.Date(dailyreturnsSI2[which.max(norm1SI2),length(tickers)+1]), max(norm2SI2),
             
      col=("black"),lwd=3, lty=3)
    
    plot(as.Date(dailyreturnsSI2[,length(tickers)+1]), ylim=c(-0.15,0.15), cex.lab=1.32, cex.main=1.39,cex.axis=1.32,
         
      dailyreturnsSI2[,1], xlab="Month", ylab="Daily Log Returns",
         
      main=paste0("Panel B: Daily Log Returns-Stock Indices"),
         
      type="l",col="purple",lwd=3)
    
    for (i in 2:(length(tickers)))
      
      {
      
        lines(as.Date(dailyreturnsSI2[,length(tickers)+1]),
              
        dailyreturnsSI2[,i],
              
        type="l",col=(i-1),lwd=3)
      
      }
    
    abline(h=0,col="black",lwd=3)
    
    segments(as.Date(dailyreturnsSI2[1,length(tickers)+1]), max(dailyreturnsSI2[1:wSI2,1:length(tickers)],dailyreturnsSI2[1:wSI2,1:length(tickers)]),
             
      as.Date(dailyreturnsSI2[1,length(tickers)+1]), min(dailyreturnsSI2[1:wSI2,1:length(tickers)],dailyreturnsSI2[1:wSI2,1:length(tickers)]),
             
      col=("black"),lwd=2)
    
    segments(as.Date(dailyreturnsSI2[wSI2,length(tickers)+1]), max(dailyreturnsSI2[1:wSI2,1:length(tickers)],dailyreturnsSI2[1:wSI2,1:length(tickers)]),
             
      as.Date(dailyreturnsSI2[wSI2,length(tickers)+1]), min(dailyreturnsSI2[1:wSI2,1:length(tickers)],dailyreturnsSI2[1:wSI2,1:length(tickers)]),
             
      col=("black"),lwd=2)
    
    segments(as.Date(dailyreturnsSI2[1,length(tickers)+1]), max(dailyreturnsSI2[1:wSI2,1:length(tickers)],dailyreturnsSI2[1:wSI2,1:length(tickers)]),
             
      as.Date(dailyreturnsSI2[wSI2,length(tickers)+1]), max(dailyreturnsSI2[1:wSI2,1:length(tickers)],dailyreturnsSI2[1:wSI2,1:length(tickers)]),
             
      col=("black"),lwd=2)
    
    segments(as.Date(dailyreturnsSI2[1,length(tickers)+1]), min(dailyreturnsSI2[1:wSI2,1:length(tickers)],dailyreturnsSI2[1:wSI2,1:length(tickers)]),
             
      as.Date(dailyreturnsSI2[wSI2,length(tickers)+1]), min(dailyreturnsSI2[1:wSI2,1:length(tickers)],dailyreturnsSI2[1:wSI2,1:length(tickers)]),
             
      col=("black"),lwd=2)
    
    segments(as.Date(dailyreturnsSI2[which.max(norm1SI2),length(tickers)+1]), max(dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)],dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)]),
             
      as.Date(dailyreturnsSI2[which.max(norm1SI2),length(tickers)+1]), min(dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)],dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)]),
             
      col=("black"),lwd=2)
    
    segments(as.Date(dailyreturnsSI2[which.max(norm1SI2)+wSI2-1,length(tickers)+1]), max(dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)],dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)]),
             
      as.Date(dailyreturnsSI2[which.max(norm1SI2)+wSI2-1,length(tickers)+1]), min(dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)],dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)]),
             
      col=("black"),lwd=2)
    
    segments(as.Date(dailyreturnsSI2[which.max(norm1SI2),length(tickers)+1]), max(dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)],dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)]),
             
      as.Date(dailyreturnsSI2[which.max(norm1SI2)+wSI2-1,length(tickers)+1]), max(dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)],dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)]),
             
      col=("black"),lwd=2)
    
    segments(as.Date(dailyreturnsSI2[which.max(norm1SI2),length(tickers)+1]), min(dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)],
                                                                                  
      dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)]),
             
      as.Date(dailyreturnsSI2[which.max(norm1SI2)+wSI2-1,length(tickers)+1]), min(dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)],
                                                                                         
      dailyreturnsSI2[which.max(norm1SI2):(which.max(norm1SI2)+wSI2-1),1:length(tickers)]), col=("black"),lwd=2)
    
    dev.off()
  
  ### Permutation test for two matrices consisting of row vectors
  
    permutation.test <- function(M1 ,M2, num.repeatsSI2 = 10000){
      # append zeros if necessary so that the matrices have the same number of columns
      num.columns <- max(ncol(M1),ncol(M2))
      M1 <- cbind(M1, Matrix(0,nrow=nrow(M1),ncol=num.columns-ncol(M1)))
      M2 <- cbind(M2, Matrix(0,nrow=nrow(M2),ncol=num.columns-ncol(M2)))
      t.obs <- euclidean.distance(colMeans(M1),colMeans(M2))
      k <- dim(M1)[1]
      M <- rbind(M1,M2)
      n <- dim(M)[1]
      count <- 0
      for (i in 1:num.repeatsSI2){
        permutation <- sample(1:n)
        t <- euclidean.distance(colMeans(M[permutation[1:k],]),colMeans(M[permutation[(k+1):n],]))
        if (t >= t.obs)
          count <- count + 1
      }
      return(count/num.repeatsSI2)
    }
  
  ### Creating Two Matrices to use for the permutation tests 
  
    M1SI2<-vector("list", (length(dailyreturnsSI2[,1])-wSI2+1))
    
    M2SI2<-vector("list", (length(dailyreturnsSI2[,1])-wSI2+1))
    
    for (iSI2 in 1:((length(dailyreturnsSI2[,1])-wSI2+1)-1))
      
      {
        
        M1SI2[[iSI2]]<-APL.matrixSI2[[iSI2]]
        
        M2SI2[[iSI2]]<-APL.matrixSI2[[(iSI2+1)]]
        
      }
  
  ### Using the permutation test function to obtain the p-values
  
  M3SI2<-0
  
  for (iSI2 in 1:((length(dailyreturnsSI2[,1])-wSI2+1)))
    
    {
      
      M3SI2[iSI2]<-permutation.test(M1SI2[[iSI2]],M2SI2[[iSI2]],1000)
      
    }
  
  
  ### Sliding Window graphs split into two years, need to change ALPHA!!!, png file to lt or gt, 
  
  # and chart title  to either >= or <  
  
  ### Determine significance value
  
    alphaSI2<-0.05
  
  ### Obtaining the p-values that are less than the significance value
  
    length(which(M3SI2==0))
  
    length(which(M3SI2>0 & M3SI2<alphaSI2))
  
    length(which(M3SI2>=alphaSI2))
  
  
