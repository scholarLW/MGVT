#Min-Max Normalization
minmax <-function(x){
  #return((x-min(x))/(max(x)-min(x)))
  return(x/sum(x))
}

veganPCoA <- function(file, groupFile=NULL, matrix.transpose=TRUE, vegdist.method='bray', k.cluster=NULL, na.rm=TRUE, z=FALSE)
{
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
        if(! "HostID" %in% colN || !"SampleID" %in% colN){
          return("Enter abundance file should have two columns: 'HostID' and 'SampleID' !")
          stop()		  
        }
        da = as.data.frame(file)
        rownames(da) = da[,c("SampleID")]
        data = da
        da = da[,-which(colnames(da) == 'SampleID')]
        da = da[,-which(colnames(da) == 'HostID')] 
        n = ncol(da) 
        da.name = colnames(da)        
        re.n = intersect(c('Group','Timeseries','Groupcolor'),da.name)
        n = n-length(re.n) 
      }
    }else{
      require(data.table)
      Gr = as.data.frame(fread(groupFile))
      Gr.name = colnames(Gr)
      if(! 'HostID' %in% Gr.name && ! 'SampleID' %in% Gr.name){
        return("Please enter columns 'HostID' and 'SampleID' for group file!")
        stop()
      }
      if(! 'SampleID' %in% Gr.name){
        return("Please enter column 'SampleID' for group file!")
        stop()
      }
      if(! 'HostID' %in% Gr.name){
        return("Please enter column 'HostID'  for group file!")
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
        n = ncol(da)  
        da$SampleID = rownames(da) 
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
        da = da[,-which(colnames(da) == 'HostID')] 
        n = ncol(da) 
        da.name = colnames(da)        
        re.n = intersect(Gr.name,da.name)
        n = n - length(re.n)  
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
  SampleID = data$SampleID
  HostID = data$HostID
  if(length(which(colnames(data) == 'SampleID'))>0){
    data = data[,-which(colnames(data) == 'SampleID')] # remove column SampleID  
  }
  colname = colnames(data)
  da = data[,1:n]   
  if('Group' %in% colname){
    da = cbind(da,data$Group)
    colnames(da)[ncol(da)] = 'Group'
  }
  if('Timeseries' %in% colname){
    da = cbind(da,data$Timeseries)
    colnames(da)[ncol(da)] = 'Timeseries'
  }
  if('Groupcolor' %in% colname){
    da = cbind(da,data$Groupcolor)
    colnames(da)[ncol(da)] = 'Groupcolor'
  }
  da = cbind(da,data$HostID)
  colnames(da)[ncol(da)] = 'HostID'   
  if(is.null(k.cluster)){
    k.cluster = length(SampleID)-1
  }  
  
  
  a <- list()
  tmp = as.matrix(da[,1:n])
  tmp = matrix(as.numeric(tmp),,n)
  rownames(tmp) = SampleID    
  tmp = t(apply(tmp,1,minmax))
  
  
  # Principal coordinate analysis and simple ordination plot
  distance <- vegdist(tmp,method=vegdist.method,na.rm=na.rm) 
  pcoa = cmdscale(distance, k=k.cluster, eig=TRUE) 
  
  kk = 2
  if(z){
    kk = 3
  }
  
  # pc1 and pc2
  if(ncol(da) > n+1){
    points = as.data.frame(cbind(SampleID,da[,(n+1):ncol(da)],pcoa$points[,1:kk]))
  }else{
    points = as.data.frame(cbind(SampleID,HostID,pcoa$points[,1:kk]))
  }
  if(z){
    colnames(points)[(ncol(points)-2):ncol(points)] = c('x','y','z')
  }else{
    colnames(points)[(ncol(points)-1):ncol(points)] = c('x','y')
  }  
  eig = pcoa$eig
  cons = round((eig/sum(eig)*100)[1:kk],1) # contribution
  
  ix = is.na(points[,ncol(points)-2]) | is.na(points[,ncol(points)-1]) | is.na(points[,ncol(points)])
  points = points[!ix,]    
  a = list('points'=points,'cons'=cons)
  return(a)
}
