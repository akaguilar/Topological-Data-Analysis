library(animation)

  ### Persistence Barcodes (Stock Indices)

    animation::saveGIF(
      
      expr = {
        
        
        for (iSI2 in ((mindlrSI2[3]-wSI2):(length(dailyreturnsSI2[,1])-wSI2)))
        
        {
    
          plot(PD.listSI2[[iSI2]], main=paste0("Persistence Barcode \n for (Stock Indices) \n", 
                                               
          format(as.Date(dailyreturnsSI2[iSI2,length(tickers)+1]),"%B %d, %Y")," - " ,
                                               
          format(as.Date(dailyreturnsSI2[(iSI2+wSI2-1),length(tickers)+1]),"%B %d, %Y")),             

          diagLim = c(min(PD.listSI2[[iSI2]]), max(PD.listSI2[[iSI2]])), barcode=TRUE)

        }
        
      },
      
        movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Animation Files/PersistenceBarcodes (Stock Indices).gif"
    
    )

  ### Persistence Diagrams (Stock Indices)
    
    animation::saveGIF(
      
      expr = {
      
      for (iSI2 in ((mindlrSI2[3]-wSI2):(length(dailyreturnsSI2[,1])-wSI2)))
      
        {
        
          plot(PD.listSI2[[iSI2]], 
             
            main=paste0("Persistence Diagram \n 1st Dimension (Stock Indices) \n ",
                         
            format(as.Date(dailyreturnsSI2[iSI2,length(tickers)+1]),"%B %d, %Y")," - " ,
                         
            format(as.Date(dailyreturnsSI2[(iSI2+wSI2-1),length(tickers)+1]),"%B %d, %Y")),
             
            diagLim = c(min(PD.listSI2[[iSI2]]), max.filtrationSI2))
        
        }
      
    },
    
      movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Animation Files/PersistenceDiagrams (Stock Indices).gif"
    
    )

  ### Peristence Landscapes (Stock Indices)
    
    animation::saveGIF(
      
      expr = {
 
        for (iSI2 in ((mindlrSI2[3]-wSI2):(length(dailyreturnsSI2[,1])-wSI2)))
        
        {
          
          plot(seq(0,max.filtrationSI2,length=length(PD.listSI2[[iSI2]])),PL.listSI2[[iSI2]] ,
               
            xlab="(Birth+Death)/2", ylab="(Death-Birth)/2" , type="l", lwd=3,
               
            main=paste0("Persistence Landscape \n 1st Dimension (Stock Indices) \n ",
                           
            format(as.Date(dailyreturnsSI2[iSI2,length(tickers)+1]),"%B %d, %Y")," - " ,
                           
            format(as.Date(dailyreturnsSI2[(iSI2+wSI2-1),length(tickers)+1]),"%B %d, %Y")))
        
        }
          
    },
    
      movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Animation Files/PersistenceLandscapes (Stock Indices).gif"
  
    )

  ### Mean Landscape Plots (Stock Indices)
    
    animation::saveGIF(
      
      expr = {
    
        for (iSI2 in ((mindlrSI2[3]-wSI2):(length(dailyreturnsSI2[,1])-wSI2)))
        
        {
        
          plot(average.PLSI2[[iSI2]][1,qu1SI2[[iSI2]][1]:qu2SI2[[iSI2]][1]],
               
            xlim=c(0,length(average.PLSI2[[iSI2]])/(num.repeatsSI2*10)), xlab=" ", ylab=" ",
               
            ylim=c(min(average.PLSI2[[iSI2]]),max(average.PLSI2[[iSI2]])),type="l",lwd=3,
               
            main=paste0("Mean Landscape \n 1st Dimension (Stock Indices) \n ",
                           
            format(as.Date(dailyreturnsSI2[iSI2,length(tickers)+1]),"%B %d, %Y")," - " ,
                           
            format(as.Date(dailyreturnsSI2[(iSI2+wSI2-1),length(tickers)+1]),"%B %d, %Y")))
          
          for (jSI2 in 2:(num.repeatsSI2*10))
            
            {
            
              lines(average.PLSI2[[iSI2]][1,qu1SI2[[iSI2]][jSI2]:qu2SI2[[iSI2]][jSI2]],type="l",col=iSI2,lwd=2)
            
            }
        }    

    },
  
      movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Animation Files/MeanLandscapes (Stock Indices).gif"

    )

  ### Daily Log Returns for Sliding Windows (50 trading days)
    
    animation::saveGIF(
      
      expr = {
        
      for (iSI2 in ((mindlrSI2[3]-wSI2):(length(dailyreturnsSI2[,1])-wSI2)))
          
        {
          
      
        plot(as.Date(dailyreturnsSI2[iSI2:(iSI2+wSI2-1),length(tickers)+1]), ylim=c(-0.15,0.15),
             
             dailyreturnsSI2[iSI2:(iSI2+wSI2-1),1], xlab="Day", ylab="Daily Log Returns",
             
             main=paste0("Daily Log Returns \n Stock Indices (50 trading days) \n ", 
                         
                         format(as.Date(dailyreturnsSI2[iSI2,length(tickers)+1]),"%B %d, %Y")," - " ,
                         
                         format(as.Date(dailyreturnsSI2[(iSI2+wSI2-1),length(tickers)+1]),"%B %d, %Y"))
             
             ,type="h",col="blue",lwd=3)
        
        abline(h=0,col="black",lwd=2)
        
        for (i in 2:(length(tickers)))
          
          {
          
            lines(as.Date(dailyreturnsSI2[iSI2:(iSI2+wSI2-1),length(tickers)+1]),
                
              dailyreturnsSI2[iSI2:(iSI2+wSI2-1),i],
                
              type="h",col=i,lwd=3)
          
          }
        
    
      }
    
      },
      
      movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Animation Files/Daily Log Returns Sliding Windows (Stock Indices).gif"
      
    )
    
  ### Topological Summaries (Persistent Diagrams, Persistence Landscapes, Mean Landscapes) and 
    
  ### Daily Log Returns for Stock Indices
    
    animation::saveGIF(
      
      expr = {
      
        for (iSI2 in ((mindlrSI2[3]-wSI2):(length(dailyreturnsSI2[,1])-wSI2)))
          
        {
          
          par(mfrow=c(2,2))
          
          plot(as.Date(dailyreturnsSI2[iSI2:(iSI2+wSI2-1),length(tickers)+1]), ylim=c(-0.15,0.15),
               
            dailyreturnsSI2[iSI2:(iSI2+wSI2-1),1], xlab="Day", ylab="Daily Log Returns",
               
            main=paste0("Daily Log Returns \n Stock Indices (50 trading days) \n ", 
                           
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
            
          plot(PD.listSI2[[iSI2]], 
               
            main=paste0("Persistence Diagram \n 1st Dimension (Stock Indices) \n ",
                           
            format(as.Date(dailyreturnsSI2[iSI2,length(tickers)+1]),"%B %d, %Y")," - " ,
                           
            format(as.Date(dailyreturnsSI2[(iSI2+wSI2-1),length(tickers)+1]),"%B %d, %Y")),
               
            diagLim = c(min(PD.listSI2[[iSI2]]), max.filtrationSI2))
          
          plot(seq(0,max.filtrationSI2,length=length(PD.listSI2[[iSI2]])),PL.listSI2[[iSI2]] ,
               
            xlab="(Birth+Death)/2", ylab="(Death-Birth)/2" , type="l", lwd=3,
               
            main=paste0("Persistence Landscape \n 1st Dimension (Stock Indices) \n ",
                           
            format(as.Date(dailyreturnsSI2[iSI2,length(tickers)+1]),"%B %d, %Y")," - " ,
                           
            format(as.Date(dailyreturnsSI2[(iSI2+wSI2-1),length(tickers)+1]),"%B %d, %Y")))
          
          plot(average.PLSI2[[iSI2]][1,qu1SI2[[iSI2]][1]:qu2SI2[[iSI2]][1]],
               
            xlim=c(0,length(average.PLSI2[[iSI2]])/(num.repeatsSI2*10)), xlab=" ", ylab=" ",
               
            ylim=c(min(average.PLSI2[[iSI2]]),max(average.PLSI2[[iSI2]])),type="l",lwd=3,
               
            main=paste0("Mean Landscape \n 1st Dimension (Stock Indices) \n ",
                           
            format(as.Date(dailyreturnsSI2[iSI2,length(tickers)+1]),"%B %d, %Y")," - " ,
                           
            format(as.Date(dailyreturnsSI2[(iSI2+wSI2-1),length(tickers)+1]),"%B %d, %Y")))
          
          for (jSI2 in 2:(num.repeatsSI2*10))
            
            {
              
              lines(average.PLSI2[[iSI2]][1,qu1SI2[[iSI2]][jSI2]:qu2SI2[[iSI2]][jSI2]],type="l",col=iSI2,lwd=2)
              
            }
            
        }
        
        
  },
  
    movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Animation Files/TopologicalSummaries (Stock Indices).gif"

  )
    
  ### 1st and 2nd Norms for July 2, 2019-June 29,2020  
    
    animation::saveGIF(
      
      expr = {
        
        
        for (iSI2 in 1:(length(dailyreturnsSI2[,1])-wSI2))
          
        {
          
          par(mfrow=c(1,2))
          
          plot(as.Date(dailyreturnsSI2[1:iSI2,length(tickers)+1]),norm1SI2[1:iSI2],lwd=3, ,xlab="Date", ylab="",
               
               main=paste0("Norms for Persistence Landscape \n (Stock Indices) \n " ,

               format(as.Date(dailyreturnsSI2[1,length(tickers)+1]),"%B %d, %Y")," - " ,
                           
               format(as.Date(dailyreturnsSI2[(iSI2+wSI2),length(tickers)+1]),"%B %d, %Y")),
               
               type="l",col="red")
          
          lines(norm2SI2[1:iSI2],type="l",lty=2,col="blue",  lwd=3)
          
          legend(x = "topleft", legend = c("L1", "L2"), lty = c(1,2), lwd = 3, col = c("red", "blue"),cex=0.75,bty='n')
          
          plot(as.Date(dailyreturnsSI2[1:(iSI2+wSI2),length(tickers)+1]), ylim=c(-0.15,0.15),
               
               dailyreturnsSI2[1:(iSI2+wSI2),1], xlab="Month", ylab="Daily Log Returns",
               
               main=paste0("Daily Log Returns \n Stock Indices \n ", 
                           
               format(as.Date(dailyreturnsSI2[iSI2,length(tickers)+1]),"%B %d, %Y")," - " ,
                           
               format(as.Date(dailyreturnsSI2[(iSI2+wSI2),length(tickers)+1]),"%B %d, %Y")),
               
               type="h",col="black",lwd=3)
          
          for (i in 2:(length(tickers)))
            
          {
            
            lines(as.Date(dailyreturnsSI2[1:(iSI2+wSI2),length(tickers)+1]),
                  
                  dailyreturnsSI2[1:(iSI2+wSI2),i],
                  
                  type="h",col=i,lwd=3)
            
          }
          
          abline(h=0,col="black",lwd=2)
          
        } 
        
        
      },
      
      movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/Stock Indices/Animation Files/NormsDLR (StockIndices).gif"
    )    
    
    