CV.random <- function(gids,ngids,f.train=.7, df, rep=20, seed=NULL){
  CV1<-Sampling.CV1(gids = gids,ngids = ngids,f = f.train,seed = seed,rep = rep)
  CV2<-Sampling.CV2(f = f.train,Y = df,seed = seed,rep = rep)
  return(list(CV1=CV1,CV2=CV2))
}

Sampling.CV1 <- function(gids,ngids=NULL,f=NULL,seed=NULL,rep=NULL){
  if(is.null(seed)) seed <-1010231
  if(is.null(rep)) rep<-10
  if(is.null(f)) f <-.7
  if(is.null(ngids)) ngids <- length(unique(gids))
  n <- f*ngids
  set1 <- matrix(NA,nrow=n,ncol=rep)
  set.seed(seed)
  for(s in 1:rep) set1[,s] <- sort(sample(1:ngids, size = n , replace = FALSE));set.seed(seed+s)
  return(set1)
}

Sampling.CV2 <- function(f,Y,seed=NULL,rep=NULL){
  if(is.null(seed)) seed <-1010231
  if(is.null(rep)) rep<-10
  if(is.null(f)) f <-.7
  n <- nrow(Y)
  set1 <- matrix(NA,nrow=n*f,ncol=rep)
  set.seed(seed)
  for(s in 1:rep) set1[,s] <- sort(sample(1:n, size = n*f , replace = FALSE));set.seed(seed+s)
  return(set1)
}

Sampling.CV0 <- function (gids, envs, ngids, f, out.env, seed, rep) 
{
  envs <- as.factor(envs)
  gids <- as.factor(gids)
  (env <- levels(envs))
  (gid <- levels(gids))
  set.seed(seed)
  cida <- seed
  out <- list()
  for (s in 1:rep) {
    set.seed(cida)
    (trai.env <- env[sample(1:length(env), size = length(env) - 
                              out.env, replace = F)])
    cida<-cida+1
    cat('training environments \n')
    cat(paste0(trai.env,'\n'))
    (trai.gid <- gid[Sampling.CV1(gids = gids, ngids = ngids, 
                                  f = f, seed = seed, rep = 1)])

    (newG <- which(!gids %in% trai.gid))
    (newE <- which(!envs %in% trai.env))
    newGE <- which(!envs %in% trai.env & !gids %in% trai.gid)
    training <- which(envs %in% trai.env & gids %in% trai.gid)
    out[[s]] <- list(newG = newG, newE = newE, newGE = newGE, 
                     training = training)
  }
  names(out) <- paste0("Rep", 1:rep)
  return(out)
}
