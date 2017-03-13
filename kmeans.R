library(png)
library(dplyr)
library(grid)
library(gridExtra)
Data = as.matrix(readPNG("/Users/mueric35/Desktop/STA-561D/hw5/raccoon.png")) 


KM = function(Data,k){
  current_center = NULL
  new_center = NULL
  # Data = matrix
  # k = cluster amount 
  # iteration = iteration amount
  
  ITER = 1 # record iteration
  
  #transfer to dataframe with 1 col
  AllData = data.frame(data = c(Data)) 
  
  # while loop 
  while(all(abs(current_center-new_center) > 0.0001)){
    
    #pick center from top k points
    if(ITER == 1){
      center = AllData[c(1:k),]
    }
    
    #record distance 
    Gdf = NULL
    for(i in 1:length(center)){
      
      # calculate distance based on different center
      tempcenter = center[i] 
      #distance function
      Dist = function(a){
        sqrt(sum((a-tempcenter)^2))
      }
      #calculate distance 
      group = apply(AllData,1,function(x) Dist(x))
      #record
      Gdf = cbind(Gdf,group)
      
    }
    
    Gdf = data.frame(Gdf)
    # name the variables
    colnames(Gdf) = paste("group", c(1:k))
    # calculate min
    Gdf$label = apply(Gdf,1,min)
    # select min
    Gdf$G = apply(Gdf,1,function(x){which(x[1:k] %in% x[k+1])[1]})
    current_center = center
    center = NULL
    # calculate new center
    for(i in 1:k){
      Each = AllData[which(Gdf$G == i),]
      #newcenter = Each[sample(length(Each),1)]
      newcenter = mean(Each)
      center = cbind(center,newcenter)
      Each = NULL
    }
    new_center = center
    ITER = ITER + 1
    print(ITER)
  }
  #record final group for each points 
  AllData$G = Gdf$G
  colnames(center) = paste("center",c(1:k))
  dff = data.frame(G = c(1:k),newdata = t(center))
  rownames(dff) = NULL
  
  
  AllData$order = 1:nrow(AllData)
  NEWdata = merge(AllData,dff)
  NEWdata = NEWdata[order(NEWdata$order),]
  rownames(NEWdata) = NULL
  colnames(NEWdata)[1] = "cluster"
  
  #return group and corresponding mean 
  return(list(center,NEWdata))
}


## kmeans with different cluster number
a2 = KM(Data,2)
a4 = KM(Data,4)
a6 = KM(Data,6)


plotgrid = function(a){
  data = a[[2]]
  newmat = matrix(data$newdata,nrow = nrow(Data),ncol= ncol(Data))
  ## transfer to raster file for plot
  rasterGrob(newmat)
  }

k1 = rasterGrob(Data)
k2 = plotgrid(a2)
k4 = plotgrid(a4)
k6 = plotgrid(a6)

grid.arrange(k1,k2,k4,k6, ncol=2,nrow = 2)



imagE = function(a){
  data = a[[2]]
  newmat = matrix(data$newdata,nrow = nrow(Data),ncol= ncol(Data))
  #transfer in to matrix, the original form 
  newmat = apply(newmat,2,rev)
  return(newmat)
}

i2 = imagE(a2)
i4 = imagE(a4)
i6 = imagE(a6)


image(t(Data),col = grey(seq(0,1,length = 256)),axes = F,main = "Original")
image(t(i2), col = grey(seq(0,1,length = 256)),axes = F,main = "K = 2")
image(t(i4), col = grey(seq(0,1,length = 256)),axes = F,main = "K = 4")
image(t(i6), col = grey(seq(0,1,length = 256)),axes = F,main = "K = 6")

#save(a2,file = "/Users/mueric35/Desktop/a2.Rdata")
#save(a4,file = "/Users/mueric35/Desktop/a4.Rdata")
#save(a6,file = "/Users/mueric35/Desktop/a6.Rdata")


