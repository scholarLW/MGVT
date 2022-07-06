#Min-Max Normalization
minmax <-function(x){
  #return((x-min(x))/(max(x)-min(x)))
  return(x/sum(x))
}

RDACCA.an = function(file, groupFile=NULL, matrix.transpose=TRUE, method=c("RDA","CCA"), personal=FALSE, trait.add=TRUE, envfit=TRUE, pvalue=0.05, p.adjust.method='fdr', qvalue=0.1, envfit.pvaluechoose=TRUE){
  
  if(is.null(file)){
    return("Please enter abundance file!")
	stop()
  }else{
    Gr = NULL
    data <- NULL  
    if(is.null(groupFile)){
      # file is a matrix, then its columns are in the form of SampleID,features 1 ~ n,HostID,[Group,] factors 1 ~ m 
      
      if(!(is.matrix(file) || is.array(file) || is.data.frame(file))){
        return("Enter abundance file should be a matrix or array, alternatively data frame format!")
        stop()		
      }else{
        colN = colnames(file)
        if(! "HostID" %in% colN || !"SampleID" %in% colN){
          return("Enter abundance file should have two columns: 'HostID' and 'SampleID' !")
          stop()		  
        }
        if(!"SampleID" %in% colN[1]){
          return("columns 1 must be 'SampleID' !")
          stop()		  
        }
        da = as.data.frame(file)
        rownames(da) = da[,1] # columns 1 must be 'SampleID'
        data = da
        index = which(colnames(da) == 'HostID')
        n = index - 2
      }
    }else{
	  if(!(is.matrix(groupFile) || is.array(groupFile) || is.data.frame(groupFile))){
      require(data.table)
      Gr = as.data.frame(fread(groupFile))
	  }else{
	   Gr = as.data.frame(groupFile)
	  }
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
        n = ncol(da) + 1  
        da$SampleID = rownames(da) 
        # merge ... 
        data = merge(da,Gr,by=c('SampleID'),all=F,sort=F)  
        if(is.null(data) || nrow(data)==0){
          return("Please confirm whether the sample names of all inputfiles were consistent!")
          stop()		  
        }
      }else{
        da = as.data.frame(file)
        if(!"SampleID" %in% colnames(da)){
          return("columns 1 must be 'SampleID' !")
          stop()		  
        }
        rownames(da) = da[,c("SampleID")]
		intersectN = intersect(colnames(da),Gr.name)
		intersectN = intersectN[-which(intersectN=='SampleID')]
		if(length(intersectN)>0){
		  for(i in 1:length(intersectN)){
		    da = da[,-which(colnames(da)==intersectN[i])]
		  }
		}
		n = ncol(da) - 1
        # merge ... 
        data = merge(da,Gr,by=c('SampleID'),all=F,sort=F)  
        if(is.null(data) || nrow(data)==0){
          return("Please confirm whether the sample names of all inputfiles were consistent!")
          stop()		  
        }
      }      
    }
  }
  if(length(method)>1){
    method = 'CCA' 
  }
  # Data processing ...
  require(vegan) 
  SampleID = unique(as.character(data$SampleID))
  HostID = unique(as.character(data$HostID))
  envi = data[,(n+1):ncol(data)]
  if(personal && length(SampleID) > length(HostID)){
    vardata = unique(envi)
    vardata$SampleID = as.character(vardata$HostID)
    vardata = vardata[,-1]
    tmp.data = data[,2:(n+1)]
    tmp <- NULL
    for(i in 1:length(HostID)){
      ix = tmp.data$HostID == HostID[i]
      if(sum(ix)>1){
        tmp.d = tmp.data[ix,]
        tmp = rbind(tmp,as.data.frame(cbind(t(apply(tmp.d[,-ncol(tmp.d)],2,median)),'HostID'=HostID[i])))
      }else{
        tmp = rbind(tmp,tmp.data[ix,])
      }      
    }
    tmp$SampleID = as.character(tmp$HostID)
    data = merge(tmp,vardata,by='SampleID',all=F)
    SampleID = unique(as.character(data$SampleID))
  }
  
  tmp = as.matrix(data[,2:n])
  tmp = matrix(as.numeric(tmp),,n-1)
  rownames(tmp) = SampleID  
  tmp = t(apply(tmp,1,minmax))  
  colnames(tmp) = colnames(data)[2:n]
  dca <- decorana(tmp)
  # If the length of first DCA axis > 4, RDA is recommended. If the length < 3, 
  # CCA is recommended. If the length between 3 and 4, both RDA and CCA are recommended.
  if(max(dca$rproj[,1]) - min(dca$rproj[,1]) > 4){
    method = 'CCA'
  }
  if(max(dca$rproj[,1]) - min(dca$rproj[,1]) < 3){
    method = 'RDA'
  }  
  
  a = list()
  if(trait.add){
    n.diff = setdiff(colnames(tmp),intersect(colnames(tmp),colnames(envi)))
    envi = cbind(envi,tmp[,n.diff])
  }
  
  if(method == "RDA"){
    sp1 <- rda(tmp~.,envi[,-c(1,2)])
  }
  if(method == "CCA"){
    sp1 <- cca(tmp~.,envi[,-c(1,2)])
  }
  
  a$data = data
  a$ef = NULL
  if(envfit){
    set.seed(1234)
    ef = envfit(sp1,envi[,-c(1,2)],permu=999)
    a$ef = ef
    envlist <- as.data.frame(cbind(ef$vectors[2]$r,ef$vectors[4]$pvals,p.adjust(ef$vectors[4]$pvals,method = p.adjust.method)))
    colnames(envlist) <- c("R2","pvals","qvals")    
    envlist$env = rownames(envlist)
    envlist_0.05 = NULL
    envlist_0.05 <- subset(envlist,envlist$qvals < qvalue)	
    if(is.null(envlist_0.05)||nrow(envlist_0.05)<1){
      envlist_0.05 <- subset(envlist,envlist$pvals < pvalue)
    }   
    envlist_0.05 <- envlist_0.05[order(as.numeric(as.character(envlist_0.05$R2)),decreasing = F),]
    envlist_0.05$env <- factor(as.vector(envlist_0.05$env),levels=as.vector(envlist_0.05$env))
    if(envfit.pvaluechoose){
      myenvi <- envi[,which(colnames(envi) %in% envlist_0.05$env)]
      if(method == "RDA"){
        sp1 <- rda(tmp~.,myenvi)
      }
      if(method == "CCA"){
        sp1 <- cca(tmp~.,myenvi)
      }
      ef = envfit(sp1,myenvi,permu=999)
      a$ef = ef
    }
  }
  a$sp1 = sp1
  return(a)
}

metaCCA = function(file, groupFile=NULL, matrix.transpose=TRUE, method=c("RDA","CCA"), personal=FALSE, trait.add=TRUE, envfit=TRUE, pvalue=0.05, p.adjust.method='fdr', qvalue=0.1, envfit.pvaluechoose=TRUE, plot=FALSE, ...){
     res = RDACCA.an(file, groupFile, matrix.transpose, method, personal, trait.add, envfit, pvalue, p.adjust.method, qvalue, envfit.pvaluechoose)
	 if(plot){ 
	    metaCCA.plot(res,...)
	 }
	 return(res)
}