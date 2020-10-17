CV_sparse_MET = function(.pheno=NULL,gids=NULL,envs=NULL,ngids=NULL,nenvs=NULL,rep=10,seed=9812981){
  
  library(plyr)
  
  if(is.null(ngids))      ngids  = nlevels(gids)
  if(is.null(nenvs))      nenvs  = nlevels(envs)
  output = vector('list', length = rep)

  
  gidsN = unique(gids)
  envN = unique(envs)
(  set = paste0(gids,'_by_',envs))
  
  for(j in 1:rep){
     set.seed(seed)
    (gidC = 1:(ngids))
    (setList = suppressWarnings(split(sample(gids,replace = F),1:nenvs)))
    names(setList) = levels(envs)
    tr = melt(setList)
   (tr$set = paste0(tr$value,'_by_',tr$L1))
   output[[j]] =     which(set %in% tr$set)
    seed = seed+j*2
  }
  names(output) = paste0('rep',1:rep)  
  return(output)
}
