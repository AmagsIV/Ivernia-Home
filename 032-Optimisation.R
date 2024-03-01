FLDR_OUT <- paste0("C:/TIA/Home/Scenarios/",FL_MDL)
RATE_REDUCTION <- 0.05
MAX_FACTOR_MOVE <- 0.01

mdl$coefficients[1] <- 1000
head(predict(mdl,type="response",newdata = dt))#needs newdata arg to force recalc using altered coefficients

constraints_ui <- rbind(sensitivity,diag(length(base_rating_vector)),-diag(length(base_rating_vector)))
constraints_ci <- c(sum(sensitivity)-RATE_REDUCTION,rep(1-MAX_FACTOR_MOVE,times=length(base_rating_vector)),-rep(1+MAX_FACTOR_MOVE,times=length(base_rating_vector)))
new_rates <- constrOptim(theta=base_rating_vector,f=calc_effectiveness,grad=NULL,ui=constraints_ui,ci=constraints_ci,control=list(trace=2,fnscale=-1))
rating_vector <- new_rates$par

base_rating_vector <- rep(1,length(mdl$coefficients))
precalc_prems <- calc_prem(base_rating_vector)
new_rates <- modconstrOptim(theta=base_rating_vector,f=calc_effectiveness,constr_f = constraints,grad=NULL,control=list(trace=2,fnscale=-1))
rating_vector <- new_rates$par

print("###OVERALL STATS###")
print_stats(rating_vector)
print("###APRIL STATS###")
print_stats(rating_vector,Mnth=24268) #(2022*12)+4
print("###MAY STATS###")
print_stats(rating_vector,Mnth=24269)

write.csv(rating_vector,file.path(FLDR_OUT,paste0("RR",RATE_REDUCTION,"_","F",MAX_FACTOR_MOVE,".csv")))


#loop over multiple scenarios
for(RATE_REDUCTION in seq(from=0, to=0.1,by=0.025)){
  for(MAX_FACTOR_MOVE in seq(from=0.01,to=0.05,by=0.02)){
    print(paste0("Rate Reduction: ",RATE_REDUCTION))
    print(paste0("Factor: ",MAX_FACTOR_MOVE))
    print("==============================")
   new_rates <- modconstrOptim(theta=base_rating_vector,f=calc_effectiveness,constr_f = constraints,grad=NULL,control=list(fnscale=-1))
   rating_vector <- new_rates$par
   names(rating_vector) <- names(mdl$coefficients)
   write.csv(rating_vector,file.path(FLDR_OUT,paste0("RR",RATE_REDUCTION,"_","F",MAX_FACTOR_MOVE,".csv")))
  }
}

