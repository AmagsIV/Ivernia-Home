modconstrOptim <- function (theta, f,constr_f, grad, ui, ci, mu = 1e-04, control = list(), 
          method = if (is.null(grad)) "Nelder-Mead" else "BFGS", 
          outer.iterations = 100, outer.eps = 1e-05, ..., hessian = FALSE) 
{
  if (!is.null(control$fnscale) && control$fnscale < 0) 
    mu <- -mu
  R <- function(theta, theta.old, ...) {
    constraints_satisfied <- constr_f(theta, ...)
    if (!constraints_satisfied) 
      return(NaN)
    f(theta, ...)
  }
  constraints_satisfied <- constr_f(theta, ...)
  if (!constraints_satisfied) 
    stop("initial value is not in the interior of the feasible region")
  obj <- f(theta, ...)
  r <- R(theta, theta, ...)
  fun <- function(theta, ...) R(theta, theta.old, ...)
  totCounts <- 0
  s.mu <- sign(mu)
  for (i in seq_len(outer.iterations)) {
    obj.old <- obj
    r.old <- r
    theta.old <- theta
    a <- optim(theta.old, fun, gr=NULL, control = control, 
               method = method, hessian = hessian, ...)
    r <- a$value
    if (is.finite(r) && is.finite(r.old) && abs(r - r.old) < 
        (0.001 + abs(r)) * outer.eps) 
      break
    theta <- a$par
    totCounts <- totCounts + a$counts
    obj <- f(theta, ...)
    if (s.mu * obj > s.mu * obj.old) 
      break
  }
  if (i == outer.iterations) {
    a$convergence <- 7
    a$message <- gettext("Barrier algorithm ran out of iterations and did not converge")
  }
  if (mu > 0 && obj > obj.old) {
    a$convergence <- 11
    a$message <- gettextf("Objective function increased at outer iteration %d", 
                          i)
  }
  if (mu < 0 && obj < obj.old) {
    a$convergence <- 11
    a$message <- gettextf("Objective function decreased at outer iteration %d", 
                          i)
  }
  a$outer.iterations <- i
  a$counts <- totCounts
  a$barrier.value <- a$value
  a$value <- f(a$par, ...)
  a$barrier.value <- a$barrier.value - a$value
  a
}

constraints <- function(Rating_Vector){
  constraints_met <- TRUE
  global <- check_global_constraint_precalc(Rating_Vector)
  if(global<(1-RATE_REDUCTION)){
    constraints_met <- FALSE
  }
  if(any(Rating_Vector[-1]<(1-MAX_FACTOR_MOVE))){#exclude the intercept from the factor check as want to be able to hold any "spare" discount here
    constraints_met <- FALSE
  }
  if(any(Rating_Vector[-1]>(1+MAX_FACTOR_MOVE))){#exclude the intercept from the factor check as want to be able to hold any "spare" discount here
    constraints_met <- FALSE
  }
  return(constraints_met)
}

calc_prem <- function(Rating_Vector,Base_Adj=1){
  backup <- mdl$coefficients
  mdl$coefficients <- Rating_Vector*mdl$coefficients
  prem <- exp(predict(mdl,type="response",newdata = dt))*Base_Adj
  minprem <- rep(0,times=nrow(dt))
  minprem[dt$cover_type=="B"] <- 100
  minprem[dt$cover_type=="C"] <- 70
  minprem[dt$cover_type=="B+C"] <- 125
  maxprem <- 5000
  prem <- pmax(prem,minprem)
  prem <- pmin(prem,maxprem)
  mdl$coefficients <- backup
  return(prem)
}

calc_effectiveness <- function(Rating_Vector,Mnth=NA,Base_Adj=1){
  prem <- calc_prem(Rating_Vector,Base_Adj)
  if(is.na(Mnth)){
    effectiveness <- sum(ifelse(prem<=dt$min_prem,1,0))/nrow(dt)
  }else{
    effectiveness <- sum(ifelse(prem[dt$timeIdx==Mnth]<=dt$min_prem[dt$timeIdx==Mnth],1,0))/sum(dt$timeIdx==Mnth)
  }
  return(effectiveness)
}

calc_BSI <- function(Rating_Vector,Mnth=NA,Base_Adj=1){
  prem <- calc_prem(Rating_Vector,Base_Adj)
  if(is.na(Mnth)){
    tmp <- sum(dt$BuildingSumInsuredRequired[prem<=dt$min_prem])/sum(ifelse(prem<=dt$min_prem,1,0))
  }else{
    tmp <- 1
    warning("Invalid path")
  }
  return(tmp)
}

calc_CSI <- function(Rating_Vector,Mnth=NA,Base_Adj=1){
  prem <- calc_prem(Rating_Vector,Base_Adj)
  if(is.na(Mnth)){
    tmp <- sum(dt$ContentsSumInsuredRequired[prem<=dt$min_prem])/sum(ifelse(prem<=dt$min_prem,1,0))
  }else{
    tmp <- 1
    warning("Invalid path")
  }
  return(tmp)
}

calc_NCD <- function(Rating_Vector,Mnth=NA,Base_Adj=1){
  prem <- calc_prem(Rating_Vector,Base_Adj)
  if(is.na(Mnth)){
    tmp <- sum(dt$ClaimedYearsNCD[prem<=dt$min_prem])/sum(ifelse(prem<=dt$min_prem,1,0))
  }else{
    tmp <- 1
    warning("Invalid path")
  }
  return(tmp)
}

calc_NmBed <- function(Rating_Vector,Mnth=NA,Base_Adj=1){
  prem <- calc_prem(Rating_Vector,Base_Adj)
  if(is.na(Mnth)){
    tmp <- sum(dt$NumberofBedrooms[prem<=dt$min_prem])/sum(ifelse(prem<=dt$min_prem,1,0))
  }else{
    tmp <- 1
    warning("Invalid path")
  }
  return(tmp)
}

calc_NmBth <- function(Rating_Vector,Mnth=NA,Base_Adj=1){
  prem <- calc_prem(Rating_Vector,Base_Adj)
  if(is.na(Mnth)){
    tmp <- sum(dt$NumberofBathrooms[prem<=dt$min_prem])/sum(ifelse(prem<=dt$min_prem,1,0))
  }else{
    tmp <- 1
    warning("Invalid path")
  }
  return(tmp)
}

check_global_constraint <- function(Rating_Vector){
  prem_orig <- calc_prem(rep(1,times=length(Rating_Vector)))
  prem_new <- calc_prem(Rating_Vector)
  return(sum(prem_new)/sum(prem_orig))
}

check_global_constraint_precalc <- function(Rating_Vector){
  prem_new <- calc_prem(Rating_Vector)
  return(sum(prem_new)/sum(precalc_prems))
}

calc_AWP <- function(Rating_Vector,Mnth=NA,Base_Adj=1){
  prem <- calc_prem(Rating_Vector,Base_Adj)
  if(is.na(Mnth)){
    AWP <- sum(ifelse(prem<=dt$min_prem,1,0)*prem)/sum(ifelse(prem<=dt$min_prem,1,0))
  }else{
    AWP <- sum(ifelse(prem[dt$timeIdx==Mnth]<=dt$min_prem[dt$timeIdx==Mnth],1,0)*prem[dt$timeIdx==Mnth])/sum(ifelse(prem[dt$timeIdx==Mnth]<=dt$min_prem[dt$timeIdx==Mnth],1,0))
  }
  return(AWP)
}

print_stats <- function(Rating_Vector,Mnth=NA,Base_Adj=1,Full=FALSE){
  base_rating_vector <- rep(1,times=length(Rating_Vector))
  effectiveness_orig <- calc_effectiveness(base_rating_vector,Mnth)
  effectiveness_new <- calc_effectiveness(Rating_Vector,Mnth,Base_Adj)
  effectiveness_movement <- (effectiveness_new/effectiveness_orig)-1
  AWP_orig <- calc_AWP(base_rating_vector,Mnth)
  AWP_new <- calc_AWP(Rating_Vector,Mnth,Base_Adj)
  AWP_movement <- (AWP_new/AWP_orig)-1
  if(Full){
    print(paste0("Effectiveness Pre: ",round(effectiveness_orig,3)))
    print(paste0("AWP Pre: ",round(AWP_orig,2)))
    print(paste0("Effectiveness Post: ",round(effectiveness_new,3)))
    print(paste0("AWP Post: ",round(AWP_new,2)))
    print(paste0("Effectiveness Move: ",round(effectiveness_movement,3)))
    print(paste0("AWP Move: ",round(AWP_movement,2)))
    print(paste0("BSI: ",round(calc_BSI(Rating_Vector,Mnth,Base_Adj),2)))
    print(paste0("CSI: ",round(calc_CSI(Rating_Vector,Mnth,Base_Adj),2)))
    print(paste0("NCD: ",round(calc_NCD(Rating_Vector,Mnth,Base_Adj),2)))
    print(paste0("Bed: ",round(calc_NmBed(Rating_Vector,Mnth,Base_Adj),2)))
    print(paste0("Bath: ",round(calc_NmBth(Rating_Vector,Mnth,Base_Adj),2)))
  }else{
    print(paste0("Effectiveness Post: ",round(effectiveness_new,3)))
    print(paste0("AWP Post: ",round(AWP_new,2)))
  }
}

results <- function(){
  base_pred <- mean(dt$avr_prem)
  pred <- exp(predict(mdl,type="response"))
  minprem <- dt$avr_prem*0
  minprem[dt$cover_type=="B"] <- 100
  minprem[dt$cover_type=="C"] <- 70
  minprem[dt$cover_type=="B+C"] <- 125
  maxprem <- 5000
  pred <- pmax(pred,minprem)
  pred <- pmin(pred,maxprem)
  print(GiniV2(dt$avr_prem,pred,NULL,TRUE))
  print(1-(RMSE(dt$avr_prem,pred)/RMSE(dt$avr_prem,base_pred)))
  print(MAE(dt$avr_prem,pred))
}



