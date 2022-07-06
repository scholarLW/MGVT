#Min-Max Normalization
minmax <-function(x){
 #return((x-min(x))/(max(x)-min(x)))
 return(x/sum(x))
}

alphadiversity = function(file, groupFile=NULL, matrix.transpose=TRUE, tab.readcounts=FALSE, 
                          plot=FALSE, select.plot=c('Shannon','Simpson','Chao1','S.obs','S.chao1','se.chao1','S.ACE','se.ACE'), ...){
  
  # ... further parameters of boxline.  
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
  
  
  if(length(select.plot)>1){
     select.plot = 'Shannon'
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
  da = cbind(da,data$HostID)
  colnames(da)[ncol(da)] = 'HostID'   
  
  tmp = as.matrix(da[,1:n])
  tmp = matrix(as.numeric(tmp),,n)
  rownames(tmp) = SampleID  
  if(!tab.readcounts){
    tmp = t(apply(tmp,1,minmax))
  }
  Shannon <- diversity(tmp, index = "shannon")
  Simpson <- diversity(tmp, index = "simpson")
  profile_ab_merge <- as.data.frame(cbind(Shannon,Simpson)) 
  profile_ab_merge$SampleID <- as.character(rownames(profile_ab_merge))
  da$SampleID = SampleID
  profile_ab_merge = merge(da,profile_ab_merge,by="SampleID",all=F,sort=F)
  rownames(profile_ab_merge) = as.character(profile_ab_merge$SampleID)
  index <- profile_ab_merge[,which(colnames(profile_ab_merge) != "SampleID")] 

  if(tab.readcounts){ 
    chao <- estimateR(tmp)
    tchao <- as.data.frame(t(chao))
    
    tchao$SampleID <- rownames(tchao)
    index$SampleID <- rownames(index)
    index2 <- merge(index[match(rownames(index), rownames(tchao)),],tchao,by="SampleID")
    rownames(index2) <- index2$SampleID
    index <- index2[,which(colnames(index2) != "SampleID")]
  }
  
  if(plot){
    if(!tab.readcounts){
      if(select.plot %in% c('S.obs','S.chao1','se.chao1','S.ACE','se.ACE')){
        return("InputMatrix must be readcounts!")
		stop()
      } 
    }
    if(length(select.plot)>1){
      return("select.plot must be length of 1!")
	  stop()
    }
    if('Group' %in% colnames(index)){
      Group = as.character(index$Group)
    }
    HostID = as.character(index$HostID)
    data = as.numeric(as.character(index[,which(colnames(index) == select.plot)]))
    data <- cbind(HostID,Group,data)
    boxline(data, ...)
  }else{
    return(index)
  }
}  