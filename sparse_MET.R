CV_sparse_MET = function(.pheno=NULL,f=.10,gids=NULL,envs=NULL,ngids=NULL,nenvs=NULL,rep=10,seed=9812981){
  
  library(plyr)
  
  if(is.null(ngids))      ngids  = nlevels(gids)
  if(is.null(nenvs))      nenvs  = nlevels(envs)
  output = vector('list', length = rep)

  
  gidsN = unique(gids)
  envN = unique(envs)
(  set = paste0(gids,'_by_',envs))
  

  for(j in 1:rep){
     set.seed(seed)
    (setList = suppressWarnings(split(sample(gidsN,replace = F),1:nenvs)))
    names(setList) = levels(envs)
    
    tr = melt(setList)
   (tr$set = paste0(tr$value,'_by_',tr$L1))
    tsGE = which(set %in% tr$set)
    if(f > .005){
     ( tsG = sample(gidsN,size = ngids*f,replace = F))
      if(j == 1) cat(paste0('number of genotypes evaluated in all environments: ',length(tsG),'\n'))
      tsGE  = c(tsGE, which(set %in% paste0(rep(tsG,each=nenvs),'_by_',envN)))
    }
   
   output[[j]] =  tsGE
    seed = seed+j*2
  }
  names(output) = paste0('rep',1:rep)  
  return(output)
}

