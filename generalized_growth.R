reticulate::source_python('generalized_growth.py')
confint <- function(Ptrue, cases, IC, inits, bounds, timevect, flag, nsim, method ='trf'){
  Phats <- matrix(, nrow=length(Ptrue), ncol = nsim)
  bestfit = solvedSimpleGrowth(Ptrue, IC, timevect, flag)
  resid = bestfit - cases
  variance = var(resid)
 
  for (sim in 1:nsim) {
    yirData = bestfit[1,1]
    for (t in 2:nrow(bestfit)){
      yirData[t] = rpois(1, bestfit[t,1])
    }
    P = fittingSimpleGrowth(yirData, IC, inits, bounds, timevect, flag)
    Phats[,sim] = P
  }
  if (flag==1){
    rownames(Phats) <- 'r'
  } 
  else if (flag==2){
    rownames(Phats) <- c('r', 'p')
  }
  else if (flag==3){
    rownames(Phats) <- c('r', 'K')
  }
  else if (flag==4){
    rownames(Phats) <- c('r', 'p', 'K')
  }
  else if (flag==5){
    rownames(Phats) <- c('r', 'p','a', 'K')
  }
  return(Phats)
}


predict_fun <- function(Phats, IC, timevect, flag){

  nsim = ncol(Phats)
  curvesfc = data.frame(matrix( ,nrow=0, ncol=length(timevect)))
  for (sim in 1:nsim){
    P = Phats[,sim]
    pred = solvedSimpleGrowth(P, IC, timevect, flag)
    curvesfc = rbind(curvesfc, t(pred))
  }
  return(curvesfc)
}
  
predint <- function(Ptrue, Phats, cases, IC, timevect, flag, datevect){
  curvesfc <- predict_fun(Phats, IC, timevect, flag)
  p25 <- apply(curvesfc,2, function(x) quantile(x, 0.025))
  p975 <- apply(curvesfc,2, function(x) quantile(x, 0.975))
  
  pfit <- solvedSimpleGrowth(Ptrue, IC, timevect, flag)
  
  cases[(length(cases)+1): (length(timevect))] <- NA
  fc <- cbind(datevect, pfit, p25, p975, cases)
  rownames(fc) <- NULL
  colnames(fc) <- c('date','fitted', "p25", "p975", 'observed')
  return (fc)
}


##############################################
# Evalutaion metrics
evaluation <- function(observed, pred){

  good_fit <- data.frame(Metric = c('MAE', 'RMSE'), Value = c(colMeans(abs(pred-observed)), sqrt(colMeans((pred-observed)^2))))

  return(good_fit)
}

MIS <- function(observed, lb, ub){
  I_lb = observed < lb
  I_ub = observed > ub
  MIS = mean((ub-lb)) + 40*I_lb*(lb-observed) + 40*I_ub*(observed-ub)
  return(MIS)
}


# Misc
date_convertible <- function(x) {!all(is.na(as.Date(as.character(x),format = c("%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%Y-%m-%d"))))}
date_converted <- function(x){as.Date(as.character(x),tryFormats = c("%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%Y-%m-%d"))}

###################################################
confint2 <- function(Ptrue,npatch,onset_thr, cases, IC, inits, bounds, timevect, nsim, method ='trf'){
  Phats <- matrix(, nrow=length(Ptrue), ncol = nsim)
  bestfit = solvedModifiedLogisticGrowthPatch(Ptrue,IC,timevect,npatch,onset_thr)
  
  resid = bestfit - cases
  variance = var(resid)
  
  for (sim in 1:nsim) {
    yirData = bestfit[1,1]
    for (t in 2:nrow(bestfit)){
      yirData[t] = rpois(1, bestfit[t,1])
    }
    
    I0 = yirData[1]
    P = fittingModifiedLogisticGrowth(yirData, I0, inits, bounds, timevect, npatch, onset_thr, method)
    
    Phats[,sim] = P
    rownames(Phats) <- c('r', 'p', 'a', 'q', 'K')
  }
  return(Phats)
}

#predict_fun <- function(Ptrue, cases, IC, inits, bounds, timefit, timefc, flag, nsim, method ='trf'){
predict_fun2 <- function(Phats, IC, timevect, npatch,onset_thr){
  
  # timevect = timefit + timefc
  nsim = ncol(Phats)
  curvesfc = data.frame(matrix( ,nrow=0, ncol=length(timevect)))
  for (sim in 1:nsim){
    P = Phats[,sim]
    pred = solvedModifiedLogisticGrowthPatch(P,IC,timevect,npatch,onset_thr)
    
    curvesfc = rbind(curvesfc, t(pred))
  }
  return(curvesfc)
}

predint2 <- function(Ptrue, Phats, cases, IC, timevect,npatch,onset_thr, datevect){
  curvesfc <- predict_fun2(Phats, IC, timevect,npatch,onset_thr)
  p25 <- apply(curvesfc, 2, function(x) quantile(x, 0.025))
  p975 <- apply(curvesfc, 2, function(x) quantile(x, 0.975))
  
  pfit <- solvedModifiedLogisticGrowthPatch(Ptrue,IC,timevect,npatch,onset_thr)
  
  cases[(length(cases)+1): (length(timevect))] <- NA
  fc <- cbind(zoo::as.Date(datevect), pfit, p25, p975, cases)
  rownames(fc) <- NULL
  colnames(fc) <- c('date','fitted', "p25", "p975", 'observed')
  return (fc)
}

###############################################################################
plot_hist <- function(Ptrue, Phats){
  Phats_25 <- apply(Phats, 1, function(x) quantile(x, 0.025))
  Phats_975 <- apply(Phats, 1, function(x) quantile(x, 0.975))
  P <- rbind(Ptrue, Phats_25, Phats_975)
  # annotations = lapply(rownames(Phats), function(i){
  #   list(
  #     text = paste0(i, "=", signif(P[1,i],3), " (95%CI: ", signif(P[2,i],3), ", ", signif(P[3,i],3), ")" ),
  #     x = 0.8,  
  #     y = 1,  
  #     xref = 'x domain',  
  #     yref = 'x domain',  
  #     xanchor = "center",  
  #     yanchor = "top",
  #     showarrow = FALSE 
  #   )
  # })
  
  lapply(rownames(Phats), function(i){
    plot_ly(x=~Phats[i, ], type = "histogram") %>%layout(xaxis = list(title = paste0(i, "=", signif(P[1,i],3), " (95%CI: ", signif(P[2,i],3), ", ", signif(P[3,i],3), ")" )), showlegend = FALSE)
  }) %>% 
    subplot(
      nrows = ceiling(NROW(.)/2),
      shareX = FALSE,
      shareY = FALSE,
      titleX = TRUE,
      titleY = FALSE,
      margin = 0.1,
      which_layout = 1
    ) 
  
}

