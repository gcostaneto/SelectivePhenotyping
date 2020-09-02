# Author: G Costa-Neto (August, 2020) germano.cneto@usp.br


#--------------------------------------------------------------------------------------------------------------------
# CV1 modified for specific traits. Each genotype is phenotyped for one trait. The MET is splited in equal size samples
#--------------------------------------------------------------------------------------------------------------------
# each genotype is phenotyped for one single trait across all environments.
# There is a correlation among phenotypic records for the same trait across environments (GxE for trait is computed)
# this scheme aims to check if the multi-trait structure is capable of predict the missing phenotypic records of the remaining unphenotyped traits.
#--------------------------------------------------------------------------------------------------------------------

# gids    = vector of gid in the data.frame of phenotypic records
# traits  = names of the traits
# ngids   = number of genotypes. If is NULL, we use nlevels(gids)
# ntraits = number of traits. If is NULL, we use lenght(traits)
# rep     = number of sampling repetitions
# seed    = seed for sampling


CV_single_trait_per_gid_env = function(gids,traits,ngids=NULL,ntraits=NULL,rep=10,seed=9187281){

  library(plyr)
  
  if(is.null(ntraits))   ntraits = length(traits)
  if(is.null(ngids))      ngids  = nlevels(gids)
  output = vector('list', length = rep)
  names(output) = paste0('rep',1:rep)
  
  gidsN = unique(gids)
  
  for(j in 1:rep){
    set.seed(seed)
    (gidC = 1:ngids)
    (setList = suppressWarnings(split(sample(gidC,replace = F),1:ntraits)))
    (traitsT = traits[(sample(1:ntraits,size = ntraits,replace = F))])
    
    out = llply(setList,function(x) which(gids %in% gidsN[x]))
    names(out) = traitsT
    output[[j]] = out
   # names(output[[j]]) = traitsT
    seed+j^2
  }
 
  return(output)
}

#--------------------------------------------------------------------------------------------------------------------
# CV2 modified for specific traits. We split the data.frame into a sparse MET for traits and genotypes across environments
#--------------------------------------------------------------------------------------------------------------------
# each genotype may be phenotyped for one or more trait across one or more environments.
#--------------------------------------------------------------------------------------------------------------------

# gids    = vector of gid in the data.frame of phenotypic records
# envs    = vector of env in the data.frame of phenotypic records
# traits  = names of the traits
# ngids   = number of genotypes. If is NULL, we use nlevels(gids)
# ntraits = number of traits. If is NULL, we use lenght(traits)
# nenvs   = number of environments. If is NULL, we use nlevles(envs)
# rep     = number of sampling repetitions
# seed    = seed for sampling


CV_sparse_MET_trait = function(gids=NULL,envs=NULL,traits,ngids=NULL,ntraits=NULL,nenvs=NULL,rep=10,seed=9812981){
  
  library(plyr)
  
  if(is.null(ntraits))   ntraits = length(traits)
  if(is.null(ngids))      ngids  = nlevels(gids)
  if(is.null(nenvs))      nenvs  = nlevels(envs)
  output = vector('list', length = rep)
  names(output) = paste0('rep',1:rep)
  
  gidsN = unique(gids)
  
  for(j in 1:rep){
    set.seed(seed)
    (gidC = 1:(ngids*ntraits))
    (setList = suppressWarnings(split(sample(gidC,replace = F),1:nenvs)))
    (traitsT = traits[(sample(1:ntraits,size = ntraits,replace = F))])

    out = setList
    names(out) = traitsT
    output[[j]] = out
    seed = seed*sqrt(j+1)
  }
  
  return(output)
}


#--------------------------------------------------------------------------------------------------------------------
# Additional function for adjusting the training set in the CV. Returns the training set using NA for testing observations
#--------------------------------------------------------------------------------------------------------------------
# Ytraits = data.frame of the traits observations
# TRset   = a output from the previous functions

Pos_NA_ordering = function(Ytraits,TRset){
  nT = names(Ytraits)
  for(j in 1:length(nT)){
      index = which(names(TRset) %in% nT[j])
      Ytraits[-TRset[[index]] , j] = NA
  }
  return(Ytraits)
}
