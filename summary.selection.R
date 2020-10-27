# .y vector of phenotypic observations
# .yhat vector of predicted phenotypes
# .gid vector of genotype ID
# .selint selection intensity (from 0 to 1)
# .decreasing boolean: TRUE select the best, FALSe the worst

summary.selection = function(.y,
                             .yhat,
                             .gid,
                             .selint,
                             .decreasing =T){
  
  # number of genotypes
  n = nlevels(.gid)
  
  # selected genotypes
  y.sel    = .gid[order(.y,decreasing = .decreasing)   ][1:(n*.selint)]
  yhat.sel = .gid[order(.yhat,decreasing = .decreasing)][1:(n*.selint)]
  
  # selection coincidence
  cs = sum(y.sel %in% yhat.sel)/(n*.selint)
  
  # selection differential (observed)
  dif.sel  = mean(.y[.gid %in% y.sel]) - mean(.y[!.gid %in% y.sel])
  gain.sel = 1-(dif.sel/mean(.y))
  
  # selection differential (predicted)
  dif.sel2  = mean(.yhat[.gid %in% yhat.sel]) - mean(.yhat[!.gid %in% yhat.sel])
  gain.sel2 = 1-(dif.sel2/mean(.yhat))
  
  
  return(list(y.sel = y.sel,yhat.sel = yhat.sel,
              y.difSel = gain.sel,yhat.difSel = dif.sel2,
              gain.y = gain.sel, gain.yhat = gain.sel2,
              cs=cs))
}
