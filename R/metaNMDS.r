metaNMDS <- function(file, groupFile=NULL, matrix.transpose=TRUE, distance.method='bray', k.cluster=NULL, z=FALSE, try=20, trymax=20, 
                    stressplot=FALSE, MDSplot=!stressplot, ...){           

  # ... further dataPC.plot parameters.
  
  if(is.null(file)){
    return("Please enter abundance file!")
	stop()
  }else{
    Gr = NULL
    data <- NULL  
    if(is.null(groupFile)){
      if(!(is.matrix(file) || is.array(file) || is.data.frame(file))){
        return("Enter abundance file should be a matrix or array, alternatively data frame format!")
        stop()		
      }else{
        colN = colnames(file)
        if(! "Group" %in% colN || !"SampleID" %in% colN){
          return("Enter abundance file should have two columns: 'Group' and 'SampleID' !")
          stop()		  
        }
        da = as.data.frame(file)
        rownames(da) = da[,c("SampleID")]
        data = da
      }
    }else{
      require(data.table)
      Gr = as.data.frame(fread(groupFile))
      Gr.name = colnames(Gr)
      if(! 'Group' %in% Gr.name && ! 'SampleID' %in% Gr.name){
        return("Please enter columns 'Group' and 'SampleID' for group file!")
		stop()
      }
      if(! 'SampleID' %in% Gr.name){
        return("Please enter column 'SampleID' for group file!")
		stop()
      }
      if(! 'Group' %in% Gr.name){
        return("Please enter column 'Group'  for group file!")
		stop()
      } 
      if(!(is.matrix(file) || is.array(file) || is.data.frame(file))){
        da = as.data.frame(fread(file)) 
        colnames(da)[1] = 'SampleID'  
        rownames(da) = da[,1]
        da = da[,-1]
        # keep rownames were the samples names
        if(isTRUE(matrix.transpose)){
          da = as.data.frame(t(da))
        } 
        da$SampleID = rownames(da) 
        if('Group' %in% colnames(da)){
          da = da[,-which(colnames(da) == 'Group')]   
        }
              
        # merge ... 
        data = merge(da,Gr,by=c('SampleID'),all=F,sort=F)  
        if(is.null(data) || nrow(data)==0){
          return("Please confirm whether the sample names of all inputfiles were consistent!")
          stop()		  
        }
      }else{
        da = as.data.frame(file)
        rownames(da) = da[,c("SampleID")]
        da = da[,-which(colnames(da) == 'SampleID')] 
        da = da[,-which(colnames(da) == 'Group')]   
        da$SampleID = rownames(da) 
        # merge ... 
        data = merge(da,Gr,by=c('SampleID'),all=F,sort=F)  
        if(is.null(data) || nrow(data)==0){
          return("Please confirm whether the sample names of all inputfiles were consistent!")
          stop()		  
        }
      }      
    }
  }
  
  # Data processing ...
  require(vegan)  
  da = data[,-which(colnames(data) == 'SampleID')] # remove column SampleID  
  da = da[,-which(colnames(da) == 'Group')]
  if(is.null(k.cluster)){
    k.cluster = 2
  } 
  if(z && k.cluster < 3){
    k.cluster = 3
  }
  nmds1 = vegan::metaMDS(da, distance = distance.method, k = k.cluster, try = try, trymax = trymax)
  if(z){  
    NMDS = data.frame(SampleID=data$SampleID,HostID=data$Group,Group=data$Group,x=nmds1$point[,1],y=nmds1$point[,2],z=nmds1$point[,3]) 
  }else{
    NMDS = data.frame(SampleID=data$SampleID,HostID=data$Group,Group=data$Group,x=nmds1$point[,1],y=nmds1$point[,2])
  } 

  if(stressplot && MDSplot){
    MDSplot=!stressplot
  }
  if(MDSplot){
    if(!z){
	  dataPC.plot(NMDS,...)
	}  
  }
  if(stressplot){
    stressplot(nmds1, main = 'Shepard Figure') 
  }
  if(!stressplot && !MDSplot){
    return(nmds1) 
  }
}
