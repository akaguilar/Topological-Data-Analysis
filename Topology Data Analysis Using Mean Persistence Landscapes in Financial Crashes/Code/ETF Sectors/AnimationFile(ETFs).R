library(animation)

  ### Persistence Barcodes (ETFs)

    animation::saveGIF(

      expr = {

        for (iETF2 in ((mindlrETF2[2]-wETF2+1):(length(dailyreturnsETF2[,1])-wETF2+1)))

          {
    
              plot(PD.listETF2[[iETF2]], main=paste0("Persistence Barcode \n for (ETF sectors) \n", 
                                               
              format(as.Date(dailyreturnsETF2[iETF2,length(ETFtickers1)+1]),"%B %d, %Y")," - " ,
                                               
              format(as.Date(dailyreturnsETF2[(iETF2+wETF2-1),length(ETFtickers1)+1]),"%B %d, %Y")),             
               
              diagLim = c(min(PD.listETF2[[iETF2]]), max(PD.listETF2[[iETF2]])), barcode=TRUE)  

          }
        
      },
        
        movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Animation Files/PersistenceBarcodes(ETFs).gif"
    
      )

  ### Persistence Diagrams (ETFs)

    animation::saveGIF(
      
      expr = {
      
        for (iETF2 in ((mindlrETF2[2]-wETF2+1):(length(dailyreturnsETF2[,1])-wETF2+1)))
          
          {  
          
            plot(PD.listETF2[[iETF2]], main=paste0("Persistence Diagram \n 1st Dimension (ETF sectors) \n ",
                                                 
              format(as.Date(dailyreturnsETF2[iETF2,length(ETFtickers1)+1]),"%B %d, %Y")," - " ,
                                                 
              format(as.Date(dailyreturnsETF2[(iETF2+wETF2-1),length(ETFtickers1)+1]),"%B %d, %Y")),
               
              diagLim = c(min(PD.listETF2[[iETF2]]), max.filtrationETF2))
          
          }
        
      },
    
        movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Animation Files/PersistenceDiagrams(ETFs).gif"
      
      )
    
  ### Peristence Landscapes (ETFs)
    
    animation::saveGIF(
      
      expr = {
        
        for (iETF2 in ((mindlrETF2[2]-wETF2+1):(length(dailyreturnsETF2[,1])-wETF2+1)))
          
        {
          plot(seq(0,max.filtrationETF2,length=length(PD.listETF2[[iETF2]])),PL.listETF2[[iETF2]] ,
               
            xlab="(Birth+Death)/2", ylab="(Death-Birth)/2" , type="l", lwd=3,
               
            main=paste0("Persistence Landscape \n 1st Dimension (ETF sectors) \n ",
                           
            format(as.Date(dailyreturnsETF2[iETF2,length(ETFtickers1)+1]),"%B %d, %Y")," - " ,
                           
            format(as.Date(dailyreturnsETF2[(iETF2+wETF2-1),length(ETFtickers1)+1]),"%B %d, %Y")))
        
        }
      },
      
        movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Animation Files/PersistenceLandscapes(ETFs).gif"
      
      )
    
  ### Mean Landscape Plots (ETFs)

    animation::saveGIF(
      
      expr = {
        
        for (iETF2 in ((mindlrETF2[2]-wETF2+1):(length(dailyreturnsETF2[,1])-wETF2+1)))
          
          {      
            plot(average.PLETF2[[iETF2]][1,qu1ETF2[[iETF2]][1]:qu2ETF2[[iETF2]][1]],
               
              xlim=c(0,length(average.PLETF2[[iETF2]])/(num.repeatsETF2*10)),
                 
              ylim=c(min(average.PLETF2[[iETF2]]),max(average.PLETF2[[iETF2]])),type="l",lwd=3,xlab="",
                 
              ylab="", main=paste0("Mean Landscape \n 1st Dimension (ETF sectors) \n ",
                                      
              format(as.Date(dailyreturnsETF2[iETF2,length(ETFtickers1)+1]),"%B %d, %Y")," - " ,
                                      
              format(as.Date(dailyreturnsETF2[(iETF2+wETF2-1),length(ETFtickers1)+1]),"%B %d, %Y")))
          
          for (jETF2 in 2:(num.repeatsETF2*10))
            
            {
            
              lines(average.PLETF2[[iETF2]][1,qu1ETF2[[iETF2]][jETF2]:qu2ETF2[[iETF2]][jETF2]],type="l",col=iETF2,lwd=2)
            
            }
          }
        
      },
        
        movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Animation Files/MeanLandscapes(ETFs).gif"
    )

  ### Daily Log Returns for Sliding Windows (50 trading days)
    
    animation::saveGIF(
      
      expr = {

        for (iETF2 in ((mindlrETF2[2]-wETF2+1):(length(dailyreturnsETF2[,1])-wETF2+1)))
          
          {
      
            plot(as.Date(dailyreturnsETF2[iETF2:(iETF2+wETF2-1),length(ETFtickers1)+1]), ylim=c(-0.16,0.16),
               
            dailyreturnsETF2[iETF2:(iETF2+wETF2-1),1], xlab="Day", ylab="Daily Log Returns",
               
            main=paste0("Daily Log Returns \n ETF sectors (50 trading days) \n ", 
                           
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
        
          }
    
    },

movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Animation Files/Daily Log Returns Sliding Windows (ETFs).gif"

)

  ### Topological Summaries (Persistent Diagrams, Persistence Landscapes, Mean Landscapes) and 
    
  ### Daily Log Returns for ETF sectors
  
    animation::saveGIF(
      
      expr = {
    
      for (iETF2 in ((mindlrETF2[2]-wETF2+1):(length(dailyreturnsETF2[,1])-wETF2+1)))
          
        {
        par(mfrow=c(2,2))
        
        plot(as.Date(dailyreturnsETF2[iETF2:(iETF2+wETF2-1),length(ETFtickers1)+1]), ylim=c(-0.16,0.16),
             
          dailyreturnsETF2[iETF2:(iETF2+wETF2-1),1], xlab="Day", ylab="Daily Log Returns",
             
          main=paste0("Daily Log Returns \n ETF sectors (50 trading days) \n ", 
                         
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
        
        plot(PD.listETF2[[iETF2]], main=paste0("Persistence Diagram \n 1st Dimension (ETF sectors) \n ",
                                               
          format(as.Date(dailyreturnsETF2[iETF2,length(ETFtickers1)+1]),"%B %d, %Y")," - " ,
                                               
          format(as.Date(dailyreturnsETF2[(iETF2+wETF2-1),length(ETFtickers1)+1]),"%B %d, %Y")),
             
          diagLim = c(min(PD.listETF2[[iETF2]]), max.filtrationETF2))
        
        plot(seq(0,max.filtrationETF2,length=length(PD.listETF2[[iETF2]])),PL.listETF2[[iETF2]] ,
             
          xlab="(Birth+Death)/2", ylab="(Death-Birth)/2" , type="l", lwd=3,
             
          main=paste0("Persistence Landscape \n 1st Dimension (ETF sectors) \n ",
                         
          format(as.Date(dailyreturnsETF2[iETF2,length(ETFtickers1)+1]),"%B %d, %Y")," - " ,
                         
          format(as.Date(dailyreturnsETF2[(iETF2+wETF2-1),length(ETFtickers1)+1]),"%B %d, %Y")))
        
        plot(average.PLETF2[[iETF2]][1,qu1ETF2[[iETF2]][1]:qu2ETF2[[iETF2]][1]],
             
          xlim=c(0,length(average.PLETF2[[iETF2]])/(num.repeatsETF2*10)),
             
          ylim=c(min(average.PLETF2[[iETF2]]),max(average.PLETF2[[iETF2]])),type="l",lwd=3,xlab="",
             
          ylab="", main=paste0("Mean Landscape \n 1st Dimension (ETF sectors) \n ",
                                  
            format(as.Date(dailyreturnsETF2[iETF2,length(ETFtickers1)+1]),"%B %d, %Y")," - " ,
                                  
            format(as.Date(dailyreturnsETF2[(iETF2+wETF2-1),length(ETFtickers1)+1]),"%B %d, %Y")))
        
        for (jETF2 in 2:(num.repeatsETF2*10))
          
          {
          
            lines(average.PLETF2[[iETF2]][1,qu1ETF2[[iETF2]][jETF2]:qu2ETF2[[iETF2]][jETF2]],type="l",col=iETF2,lwd=2)
          
          }
        
        } 

      },
      
      movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Animation Files/TopologicalSummaries(ETFs).gif"
    )

  ### 1st and 2nd Norms for July 2, 2019-June 29,2020
    
    animation::saveGIF(
      
      expr = {
        
        
        for (iETF2 in 1:(length(dailyreturnsETF2[,1])-wETF2))
          
        {
          par(mfrow=c(1,2))
          
          plot(as.Date(dailyreturnsETF2[1:iETF2,length(ETFtickers1)+1]),norm1ETF2[1:iETF2],lwd=3, ,xlab="Date", ylab="",
               
            main=paste0("Norms for Persistence Landscape \n (ETF sectors) \n " ,
                        
            format(as.Date(dailyreturnsETF2[1,length(ETFtickers1)+1]),"%B %d, %Y")," - " ,
                        
            format(as.Date(dailyreturnsETF2[(iETF2+wETF2),length(ETFtickers1)+1]),"%B %d, %Y")),
               
            type="l",col="red")
          
          lines(norm2ETF2[1:iETF2],type="l",lty=2,col="blue",  lwd=3)
          
          legend(x = "topleft", legend = c("L1", "L2"), lty = c(1,2), lwd = 3, col = c("red", "blue"),cex=0.75,bty='n')
          
          plot(as.Date(dailyreturnsETF2[iETF2:(iETF2+wETF2-1),length(ETFtickers1)+1]), ylim=c(-0.16,0.16),
               
            dailyreturnsETF2[iETF2:(iETF2+wETF2-1),1], xlab="Day", ylab="Daily Log Returns",
               
            main=paste0("Daily Log Returns \n ETF sectors (50 trading days) \n ", 
                           
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
          
        } 
    
    
      },
    
    movie.name = "C:/Users/Alejandro.Aguilar/Desktop/Journal of Mathematical Finance/Code/ETF Sectors/Animation Files/NormsDLR(ETFs).gif"
    )