calc_prem <- function(Rating_Vector,Base_Adj=1){
  browser()
  fac_list <- unique(sapply(strsplit(names(Rating_Vector),"__"),FUN=function(x)x[1]))
  prem <- dt$IVAPrice*Base_Adj
  for(fac in fac_list){
    tmp <- Rating_Vector[sapply(strsplit(names(Rating_Vector),"__"),FUN=function(x)x[1])==fac]
    names(tmp) <- sapply(strsplit(names(tmp),"__"),FUN=function(x)x[2])
    prem <- prem*tmp[as.character(dt[,fac])]
  }
  return(prem)
}

calc_effectiveness <- function(Rating_Vector,Base_Adj=1){
  # browser()
  prem <- calc_prem(Rating_Vector=Rating_Vector,Base_Adj = Base_Adj)
  effectiveness <- sum(ifelse(prem<=dt$market_price,1,0),na.rm = TRUE)/nrow(dt)
  return(effectiveness)
}

check_global_constraint <- function(Rating_Vector,Base_Adj=1){
  prem <- calc_prem(Rating_Vector=Rating_Vector,Base_Adj = Base_Adj)
  return(sum(prem)/sum(dt$IVAPrice))
}

check_prem_sold_delta <- function(Rating_Vector,Base_Adj=1){
  fac_list <- unique(sapply(strsplit(names(Rating_Vector),"__"),FUN=function(x)x[1]))
  orig_prem <- dt$SalesGWP*dt$SalesCount*Base_Adj
  prem <- orig_prem
  for(fac in fac_list){
    tmp <- Rating_Vector[sapply(strsplit(names(Rating_Vector),"__"),FUN=function(x)x[1])==fac]
    names(tmp) <- sapply(strsplit(names(tmp),"__"),FUN=function(x)x[2])
    prem <- prem*tmp[dt[,fac]]
  }
  return(sum(prem)/sum(orig_prem))
}

calc_risk <- function(Rating_Vector,Base_Adj=1,Predictions){
  prem <- calc_prem(Rating_Vector=Rating_Vector,Base_Adj = Base_Adj)
  risk <- sum(ifelse(prem<=dt$market_price,1,0)*Predictions,na.rm = TRUE)/sum(ifelse(prem<=dt$market_price,1,0)*prem,na.rm = TRUE)
  return(risk)
}

calc_AWP <- function(Rating_Vector,Base_Adj=1,Single_Num=TRUE){
  prem <- calc_prem(Rating_Vector=Rating_Vector,Base_Adj = Base_Adj)
  if(Single_Num){
    AWP <- sum(ifelse(prem<=dt$market_price,1,0)*prem,na.rm = TRUE)/sum(ifelse(prem<=dt$market_price,1,0),na.rm = TRUE)
  }else{
    AWP <- prem
  }
  return(AWP)
}

calc_scheme_share <- function(Rating_Vector,Base_Adj=1,Scheme="2A"){
  prem <- calc_prem(Rating_Vector=Rating_Vector,Base_Adj = Base_Adj)
  risk <- sum(ifelse(prem<=dt$market_price,1,0)*ifelse(dt$SchemeProfile==Scheme,1,0),na.rm = TRUE)/sum(ifelse(prem<=dt$market_price,1,0),na.rm = TRUE)
  return(risk)  
}

print_stats <- function(Rating_Vector,Base_Adj=1,Incl_Risk=TRUE,Predictions=NULL){
  if(Incl_Risk==TRUE && is.null(Predictions)){
    warning("Risk stats were requested, but no risk parameters were set")
    Incl_Risk=FALSE
  }
  base_rating_vector <- rep(1,times=length(Rating_Vector))
  names(base_rating_vector) <- names(Rating_Vector)
  print(paste0("Effectiveness Improvement: ",(calc_effectiveness(Rating_Vector,Base_Adj)/calc_effectiveness(base_rating_vector))-1))
  if(Incl_Risk){
    print(paste0("Risk Movement: ",(calc_risk(Rating_Vector,Base_Adj,Predictions=Predictions)/calc_risk(base_rating_vector,Predictions=Predictions))-1))
  }
  print(paste0("AWP Post: ",calc_AWP(Rating_Vector,Base_Adj)))
  print(paste0("AWP Movement: ",(calc_AWP(Rating_Vector,Base_Adj)/calc_AWP(base_rating_vector))-1))
}


returnstats <- function(Rating_Vector,Base_Adj=1,Incl_Risk=TRUE,Predictions=NULL){
  if(Incl_Risk==TRUE && is.null(Predictions)){
    warning("Risk stats were requested, but no risk parameters were set")
    Incl_Risk=FALSE
  }
  results <- rep(NA,times=4)
  base_rating_vector <- rep(1,times=length(Rating_Vector))
  names(base_rating_vector) <- names(Rating_Vector)
  results[1] <- (calc_effectiveness(Rating_Vector,Base_Adj)/calc_effectiveness(base_rating_vector))-1
  if(Incl_Risk){
    results[2] <- (calc_risk(Rating_Vector,Base_Adj,Predictions=Predictions)/calc_risk(base_rating_vector,Predictions=Predictions))-1
  }
  results[3] <- calc_AWP(Rating_Vector,Base_Adj)
  results[4] <- (calc_AWP(Rating_Vector,Base_Adj)/calc_AWP(base_rating_vector))-1
  names(results) <- c("Effectiveness","Risk","Post AWP","AWP Movement")
  return(results)

  
  }


print_stats_post <- function(Rating_Vector,Base_Adj=1,Incl_Risk=TRUE,Predictions=NULL){
  if(Incl_Risk==TRUE && is.null(Predictions)){
    warning("Risk stats were requested, but no risk parameters were set")
    Incl_Risk=FALSE
  }
  base_rating_vector <- rep(1,times=length(Rating_Vector))
  names(base_rating_vector) <- names(Rating_Vector)
  eff <- calc_effectiveness(Rating_Vector,Base_Adj)
  print(paste0("Effectiveness Post: ",eff))
  
  if(Incl_Risk){
    rsk <- calc_risk(Rating_Vector,Base_Adj,Predictions=Predictions)
    print(paste0("Risk Post: ",rsk))
  }
  awp <- calc_AWP(Rating_Vector,Base_Adj)
  print(paste0("AWP Post: ",awp))
  shr <- calc_scheme_share(Rating_Vector,Base_Adj)
  print(paste0("2A Post: ",shr))
  output <- c(eff,rsk,awp,shr)
  names(output) <- c("Effectiveness","Risk","AWP","2A Share")
  return(output)
}


print_partial_changes <- function(Rating_Vector,Base_Adj=1,Predictions=NULL){
  fac_list <- unique(sapply(strsplit(names(Rating_Vector),"__"),FUN=function(x)x[1]))
  base_rating_vector <- rep(1,times=length(Rating_Vector))
  names(base_rating_vector) <- names(Rating_Vector)
  print("Base Adjustment")
  if(is.null(Predictions)){
    print_stats(base_rating_vector,Base_Adj=1,Incl_Risk = FALSE)
  }else{
    print_stats(base_rating_vector,Base_Adj=1,Predictions=Predictions)
  }
  for(fac in fac_list){
    partial_rating_vector <- base_rating_vector
    partial_rating_vector[sapply(strsplit(names(Rating_Vector),"__"),FUN=function(x)x[1])==fac] <- Rating_Vector[sapply(strsplit(names(Rating_Vector),"__"),FUN=function(x)x[1])==fac]
    cat("",sep="\n\n")
    print(fac)
    if(is.null(Predictions)){
      print_stats(partial_rating_vector,Base_Adj=1,Incl_Risk = FALSE)
    }else{
      print_stats(partial_rating_vector,Base_Adj=1,Predictions=Predictions)
    }
  }
}

create_modelling_dataset_NQDB <- function(Data){
  browser()
  dt_mdl <- data.frame(QuoteEventID=Data$QuoteEventID)
  dt_mdl$mdl_cc <- 1398
  dt_mdl$mdl_cc[Data$RFCCBand=="a) 0-1000"] <- 950
  dt_mdl$mdl_cc[Data$RFCCBand=="b) 1001-1100"] <- 1050
  dt_mdl$mdl_cc[Data$RFCCBand=="c) 1101-1200"] <- 1150
  dt_mdl$mdl_cc[Data$RFCCBand=="d) 1201-1300"] <- 1250
  dt_mdl$mdl_cc[Data$RFCCBand=="e) 1301-1400"] <- 1350
  dt_mdl$mdl_cc[Data$RFCCBand=="f) 1401-1500"] <- 1450
  dt_mdl$mdl_cc[Data$RFCCBand=="g) 1501-1600"] <- 1550
  dt_mdl$mdl_cc[Data$RFCCBand=="h) 1601-1700"] <- 1650
  dt_mdl$mdl_cc[Data$RFCCBand=="i) 1701-1800"] <- 1750
  dt_mdl$mdl_cc[Data$RFCCBand=="j) 1801-1900"] <- 1850
  dt_mdl$mdl_cc[Data$RFCCBand=="k) 1901-2000"] <- 1950
  dt_mdl$mdl_cc[Data$RFCCBand=="l) 2001-2500"] <- 2250
  dt_mdl$mdl_cc[Data$RFCCBand=="m) 2501-3000"] <- 2750
  
  dt_mdl$bonus <- 9
  dt_mdl$bonus[Data$RFNCDYears=="d) 3 years"] <- 3
  dt_mdl$bonus[Data$RFNCDYears=="e) 4 years"] <- 4
  dt_mdl$bonus[Data$RFNCDYears=="f) 5 years"] <- 5
  dt_mdl$bonus[Data$RFNCDYears=="g) 6 years"] <- 6
  dt_mdl$bonus[Data$RFNCDYears=="h) 7 years"] <- 7
  dt_mdl$bonus[Data$RFNCDYears=="i) 8 years"] <- 8
  dt_mdl$bonus[Data$RFNCDYears=="j) 9+ years"] <- 9
  
  dt_mdl$transaction_type <- Data$ReportingTxnType #TODO - check levels
  dt_mdl$num_drivers <- 1
  dt_mdl$num_drivers[Data$RFDrivingCategory %in% c("b) Insured+Spouse","c) Insured+1Driver","e) Insured+Parent")] <- 2
  dt_mdl$num_drivers[Data$RFDrivingCategory %in% c("d) Insured+2Drivers","f) Other Category")] <- 3
  
  dt_mdl$driving_category <- Data$RFDrivingCategory
  dt_mdl$driving_category[dt_mdl$driving_category=="a) Insured Only"] <- "Insured Only"
  dt_mdl$driving_category[dt_mdl$driving_category=="b) Insured+Spouse"] <- "Insured + Spouse" 
  dt_mdl$driving_category[dt_mdl$driving_category=="c) Insured+1Driver"] <- "Insured and 1 Named driver"
  dt_mdl$driving_category[dt_mdl$driving_category=="d) Insured+2Drivers"] <- "Insured and 2 Named drivers" 
  dt_mdl$driving_category[dt_mdl$driving_category=="e) Insured+Parent"] <- "Insured and 1 Named driver"
  dt_mdl$driving_category[dt_mdl$driving_category=="f) Other Category"] <- "Open Driving no age limit" 
  
  policyStartDate <- Data$PolicyStartDate
  dt_mdl$time_factor <- (year(policyStartDate)*12)+month(policyStartDate)
  
  dt_mdl$licence_type <- dt$RFLicenceType
  
  dt_mdl$mdl_ownership <- 999
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="a) 0 years"] <- 0
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="b) 1 year"] <- 1
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="b) 2 years"] <- 2
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="d) 3 years"] <- 3
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="e) 4 years"] <- 4
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="f) 5 years"] <- 5
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="g) 6 years"] <- 6
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="h) 7 years"] <- 7
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="i) 8 years"] <- 8
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="j) 9 years"] <- 9
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="k) 10 years"] <- 10
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="l) 11 years"] <- 11
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="m) 12 years"] <- 12
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="n) 13 years"] <- 13
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="o) 14 years"] <- 14
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="p) 15 years"] <- 15
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="q) 16 years"] <- 16
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="r) 17 years"] <- 17
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="s) 18 years"] <- 18
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="t) 19 years"] <- 19
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="u) 20 years"] <- 20
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="v) 21 years"] <- 21
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="w) 22+ years"] <- 22
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="z) Other"] <- 0
  
  dt_mdl$mdl_vehage <- 999
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="a) Pre 2003"] <- 19
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="b) 2003"] <- 18
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="c) 2004"] <- 17
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="d) 2005"] <- 16
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="e) 2006"] <- 15
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="f) 2007"] <- 14
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="g) 2008"] <- 13
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="h) 2009"] <- 12
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="i) 2010"] <- 11
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="j) 2011"] <- 10
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="k) 2012"] <- 9
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="l) 2013"] <- 8
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="m) 2014"] <- 7
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="n) 2015"] <- 6
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="o) 2016"] <- 5
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="p) 2017"] <- 4
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="q) 2018"] <- 3
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="r) 2019"] <- 2
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="s) 2020"] <- 1
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="t) 2021"] <- 0
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="z) Other"] <- 0
  
  dt_mdl$fuel_type <- "Petrol"
  dt_mdl$fuel_type[Data$RFVehicleFuelType=="D"] <- "Diesel"
  dt_mdl$fuel_type[Data$RFVehicleFuelType=="E"] <- "Electricity"
  
  dt_mdl$body_type <- Data$RFBodyType
  dt_mdl$body_type[dt_mdl$body_type==""] <- "Hatchback"
  
  dt_mdl$mdl_buying_behaviour <- 999
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="a) 0 or less"] <- 0
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="b) 1-2 days out"] <- 1
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="b1) 1 day out"] <- 1
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="b2) 2 days out"] <- 2
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="c) 3-6 days out"] <- 4
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="d) 7-13days out"] <- 10
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="e) 14-20 days out"] <- 16
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="f) 21-27 days out"] <- 25
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="g) 28-34 days out"] <- 31
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="h) 35+ days"] <- 40
  
  dt_mdl$imported <- "N"
  
  dt_mdl$mdl_resident_years <- 999
  
  dt_mdl$mdl_age <- 999
  dt_mdl$mdl_age[Data$RFProposerAge=="c) 25-29"] <- 27
  dt_mdl$mdl_age[Data$RFProposerAge=="d) 30-34"] <- 32
  dt_mdl$mdl_age[Data$RFProposerAge=="e) 35-39"] <- 37
  dt_mdl$mdl_age[Data$RFProposerAge=="f) 40-44"] <- 42
  dt_mdl$mdl_age[Data$RFProposerAge=="g) 45-49"] <- 47
  dt_mdl$mdl_age[Data$RFProposerAge=="h) 50-54"] <- 52
  dt_mdl$mdl_age[Data$RFProposerAge=="i) 55-59"] <- 57
  dt_mdl$mdl_age[Data$RFProposerAge=="j) 60-64"] <- 62
  dt_mdl$mdl_age[Data$RFProposerAge=="k) 65-69"] <- 67
  dt_mdl$mdl_age[Data$RFProposerAge=="l) 70-74"] <- 72
  
  dt_mdl$ProposerPenaltyPoints <- 999
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="a) 0"] <- 0
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="b) 1"] <- 1
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="c) 2"] <- 2
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="d) 3"] <- 3
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="e) 4"] <- 4
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="f) 5"] <- 5
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="g) 6+"] <- 6
  
  
  dt_mdl$bonus_protection <- Data$RFNCDType
  dt_mdl$bonus_protection[dt_mdl$bonus_protection=="S"] <- "Step Back"
  dt_mdl$bonus_protection[dt_mdl$bonus_protection=="F"] <- "Full"
  
  dt_mdl$employment_status <- Data$RFEmploymentType
  dt_mdl$homeowner <- Data$RFHomeOwner
  
  dt_mdl$mdl_resident_years <- dt_mdl$mdl_age
  
  dt_mdl$VehicleGroup <- 999
  dt_mdl$VehicleGroup[Data$RFCCBand=="a) 0-1000"] <- 7
  dt_mdl$VehicleGroup[Data$RFCCBand=="b) 1001-1100"] <- 8
  dt_mdl$VehicleGroup[Data$RFCCBand=="c) 1101-1200"] <- 10
  dt_mdl$VehicleGroup[Data$RFCCBand=="d) 1201-1300"] <- 12
  dt_mdl$VehicleGroup[Data$RFCCBand=="e) 1301-1400"] <- 13
  dt_mdl$VehicleGroup[Data$RFCCBand=="f) 1401-1500"] <- 15
  dt_mdl$VehicleGroup[Data$RFCCBand=="g) 1501-1600"] <- 15
  dt_mdl$VehicleGroup[Data$RFCCBand=="h) 1601-1700"] <- 15
  dt_mdl$VehicleGroup[Data$RFCCBand=="i) 1701-1800"] <- 19
  dt_mdl$VehicleGroup[Data$RFCCBand=="j) 1801-1900"] <- 19
  dt_mdl$VehicleGroup[Data$RFCCBand=="k) 1901-2000"] <- 25
  dt_mdl$VehicleGroup[Data$RFCCBand=="l) 2001-2500"] <- 30
  dt_mdl$VehicleGroup[Data$RFCCBand=="m) 2501-3000"] <- 37
  
  dt_mdl$mdl_driving_experience <- dt_mdl$mdl_age-22
  dt_mdl$ProposerClaimFaultYearsAgo <- ifelse(dt$RFClaimsFlag=="a) Yes",2,5)
  dt_mdl$ProposerClaimWSYearsAgo <- 5
  dt_mdl$ProposerClaimNonFaultYearsAgo <- 5
  
  dt_mdl$mdl_voluntary_excess <- 0
  dt_mdl$mdl_voluntary_excess[Data$RFVoluntaryExcess=="a) 0-100"] <- 100
  dt_mdl$mdl_voluntary_excess[Data$RFVoluntaryExcess=="C) 201-400"] <- 250
  dt_mdl$mdl_voluntary_excess[Data$RFVoluntaryExcess=="d) 401-600"] <- 500
  
  
  dt_mdl$Creativity <- Data$Creativity
  dt_mdl$Physical <- Data$Physical
  dt_mdl$Autonomy <- Data$Autonomy
  dt_mdl$Educated <- Data$Educated
  dt_mdl$Art.Science <- Data$Art.Science
  dt_mdl$Public.Facing <- Data$Public.Facing
  dt_mdl$Seniority <- Data$Seniority
  
  return(dt_mdl)
}

create_modelling_dataset <- function(Data){
  dt_mdl <- data.frame(QuoteEventID=Data$QuoteEventID)
  dt_mdl$mdl_cc <- 1398
  dt_mdl$mdl_cc[Data$RFCCBand=="a) 0-1000"] <- 950
  dt_mdl$mdl_cc[Data$RFCCBand=="b) 1001-1100"] <- 1050
  dt_mdl$mdl_cc[Data$RFCCBand=="c) 1101-1200"] <- 1150
  dt_mdl$mdl_cc[Data$RFCCBand=="d) 1201-1300"] <- 1250
  dt_mdl$mdl_cc[Data$RFCCBand=="e) 1301-1400"] <- 1350
  dt_mdl$mdl_cc[Data$RFCCBand=="f) 1401-1500"] <- 1450
  dt_mdl$mdl_cc[Data$RFCCBand=="g) 1501-1600"] <- 1550
  dt_mdl$mdl_cc[Data$RFCCBand=="h) 1601-1700"] <- 1650
  dt_mdl$mdl_cc[Data$RFCCBand=="i) 1701-1800"] <- 1750
  dt_mdl$mdl_cc[Data$RFCCBand=="j) 1801-1900"] <- 1850
  dt_mdl$mdl_cc[Data$RFCCBand=="k) 1901-2000"] <- 1950
  dt_mdl$mdl_cc[Data$RFCCBand=="l) 2001-2500"] <- 2250
  dt_mdl$mdl_cc[Data$RFCCBand=="m) 2501-3000"] <- 2750
  
  dt_mdl$bonus <- 9
  dt_mdl$bonus[Data$RFNCDYears=="d) 3 years"] <- 3
  dt_mdl$bonus[Data$RFNCDYears=="e) 4 years"] <- 4
  dt_mdl$bonus[Data$RFNCDYears=="f) 5 years"] <- 5
  dt_mdl$bonus[Data$RFNCDYears=="g) 6 years"] <- 6
  dt_mdl$bonus[Data$RFNCDYears=="h) 7 years"] <- 7
  dt_mdl$bonus[Data$RFNCDYears=="i) 8 years"] <- 8
  dt_mdl$bonus[Data$RFNCDYears=="j) 9+ years"] <- 9
  
  dt_mdl$transaction_type <- Data$ReportingTxnType #TODO - check levels
  dt_mdl$num_drivers <- 1
  dt_mdl$num_drivers[Data$RFDrivingCategory %in% c("b) Insured+Spouse","c) Insured+1Driver","e) Insured+Parent")] <- 2
  dt_mdl$num_drivers[Data$RFDrivingCategory %in% c("d) Insured+2Drivers","f) Other Category")] <- 3
  
  dt_mdl$driving_category <- Data$RFDrivingCategory
  dt_mdl$driving_category[dt_mdl$driving_category=="a) Insured Only"] <- "Insured Only"
  dt_mdl$driving_category[dt_mdl$driving_category=="b) Insured+Spouse"] <- "Insured + Spouse" 
  dt_mdl$driving_category[dt_mdl$driving_category=="c) Insured+1Driver"] <- "Insured and 1 Named driver"
  dt_mdl$driving_category[dt_mdl$driving_category=="d) Insured+2Drivers"] <- "Insured and 2 Named drivers" 
  dt_mdl$driving_category[dt_mdl$driving_category=="e) Insured+Parent"] <- "Insured and 1 Named driver"
  dt_mdl$driving_category[dt_mdl$driving_category=="f) Other Category"] <- "Open Driving no age limit" 
  
  policyStartDate <- Data$PolicyStartDate
  dt_mdl$time_factor <- (year(policyStartDate)*12)+month(policyStartDate)
  
  dt_mdl$licence_type <- dt$RFLicenceType
  
  dt_mdl$mdl_ownership <- 999
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="a) 0 years"] <- 0
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="b) 1 year"] <- 1
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="b) 2 years"] <- 2
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="d) 3 years"] <- 3
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="e) 4 years"] <- 4
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="f) 5 years"] <- 5
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="g) 6 years"] <- 6
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="h) 7 years"] <- 7
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="i) 8 years"] <- 8
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="j) 9 years"] <- 9
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="k) 10 years"] <- 10
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="l) 11 years"] <- 11
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="m) 12 years"] <- 12
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="n) 13 years"] <- 13
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="o) 14 years"] <- 14
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="p) 15 years"] <- 15
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="q) 16 years"] <- 16
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="r) 17 years"] <- 17
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="s) 18 years"] <- 18
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="t) 19 years"] <- 19
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="u) 20 years"] <- 20
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="v) 21 years"] <- 21
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="w) 22+ years"] <- 22
  dt_mdl$mdl_ownership[Data$RFCarOwnership=="z) Other"] <- 0
  
  dt_mdl$mdl_vehage <- 999
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="a) Pre 2003"] <- 19
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="b) 2003"] <- 18
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="c) 2004"] <- 17
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="d) 2005"] <- 16
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="e) 2006"] <- 15
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="f) 2007"] <- 14
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="g) 2008"] <- 13
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="h) 2009"] <- 12
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="i) 2010"] <- 11
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="j) 2011"] <- 10
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="k) 2012"] <- 9
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="l) 2013"] <- 8
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="m) 2014"] <- 7
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="n) 2015"] <- 6
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="o) 2016"] <- 5
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="p) 2017"] <- 4
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="q) 2018"] <- 3
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="r) 2019"] <- 2
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="s) 2020"] <- 1
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="t) 2021"] <- 0
  dt_mdl$mdl_vehage[Data$RFYearOfMake=="z) Other"] <- 0
  
  dt_mdl$fuel_type <- "Petrol"
  dt_mdl$fuel_type[Data$RFVehicleFuelType=="D"] <- "Diesel"
  dt_mdl$fuel_type[Data$RFVehicleFuelType=="E"] <- "Electricity"
  
  dt_mdl$body_type <- Data$RFBodyType
  dt_mdl$body_type[dt_mdl$body_type==""] <- "Hatchback"
  
  dt_mdl$mdl_buying_behaviour <- 999
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="a) 0 or less"] <- 0
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="b) 1-2 days out"] <- 1
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="b1) 1 day out"] <- 1
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="b2) 2 days out"] <- 2
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="c) 3-6 days out"] <- 4
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="d) 7-13days out"] <- 10
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="e) 14-20 days out"] <- 16
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="f) 21-27 days out"] <- 25
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="g) 28-34 days out"] <- 31
  dt_mdl$mdl_buying_behaviour[Data$RFBuyBehaviour=="h) 35+ days"] <- 40
  
  dt_mdl$imported <- "N"
  
  dt_mdl$mdl_resident_years <- 999
  
  dt_mdl$mdl_age <- 999
  dt_mdl$mdl_age[Data$RFProposerAge=="c) 25-29"] <- 27
  dt_mdl$mdl_age[Data$RFProposerAge=="d) 30-34"] <- 32
  dt_mdl$mdl_age[Data$RFProposerAge=="e) 35-39"] <- 37
  dt_mdl$mdl_age[Data$RFProposerAge=="f) 40-44"] <- 42
  dt_mdl$mdl_age[Data$RFProposerAge=="g) 45-49"] <- 47
  dt_mdl$mdl_age[Data$RFProposerAge=="h) 50-54"] <- 52
  dt_mdl$mdl_age[Data$RFProposerAge=="i) 55-59"] <- 57
  dt_mdl$mdl_age[Data$RFProposerAge=="j) 60-64"] <- 62
  dt_mdl$mdl_age[Data$RFProposerAge=="k) 65-69"] <- 67
  dt_mdl$mdl_age[Data$RFProposerAge=="l) 70-74"] <- 72
  
  dt_mdl$ProposerPenaltyPoints <- 999
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="a) 0"] <- 0
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="b) 1"] <- 1
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="c) 2"] <- 2
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="d) 3"] <- 3
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="e) 4"] <- 4
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="f) 5"] <- 5
  dt_mdl$ProposerPenaltyPoints[Data$RFPenaltyPoints=="g) 6+"] <- 6
  
  
  dt_mdl$bonus_protection <- Data$RFNCDType
  dt_mdl$bonus_protection[dt_mdl$bonus_protection=="S"] <- "Step Back"
  dt_mdl$bonus_protection[dt_mdl$bonus_protection=="F"] <- "Full"
  
  dt_mdl$employment_status <- Data$RFEmploymentType
  dt_mdl$homeowner <- Data$RFHomeOwner
  
  dt_mdl$mdl_resident_years <- dt_mdl$mdl_age
  
  dt_mdl$VehicleGroup <- 999
  dt_mdl$VehicleGroup[Data$RFCCBand=="a) 0-1000"] <- 7
  dt_mdl$VehicleGroup[Data$RFCCBand=="b) 1001-1100"] <- 8
  dt_mdl$VehicleGroup[Data$RFCCBand=="c) 1101-1200"] <- 10
  dt_mdl$VehicleGroup[Data$RFCCBand=="d) 1201-1300"] <- 12
  dt_mdl$VehicleGroup[Data$RFCCBand=="e) 1301-1400"] <- 13
  dt_mdl$VehicleGroup[Data$RFCCBand=="f) 1401-1500"] <- 15
  dt_mdl$VehicleGroup[Data$RFCCBand=="g) 1501-1600"] <- 15
  dt_mdl$VehicleGroup[Data$RFCCBand=="h) 1601-1700"] <- 15
  dt_mdl$VehicleGroup[Data$RFCCBand=="i) 1701-1800"] <- 19
  dt_mdl$VehicleGroup[Data$RFCCBand=="j) 1801-1900"] <- 19
  dt_mdl$VehicleGroup[Data$RFCCBand=="k) 1901-2000"] <- 25
  dt_mdl$VehicleGroup[Data$RFCCBand=="l) 2001-2500"] <- 30
  dt_mdl$VehicleGroup[Data$RFCCBand=="m) 2501-3000"] <- 37
  
  dt_mdl$mdl_driving_experience <- dt_mdl$mdl_age-22
  dt_mdl$ProposerClaimFaultYearsAgo <- ifelse(dt$RFClaimsFlag=="a) Yes",2,5)
  dt_mdl$ProposerClaimWSYearsAgo <- 5
  dt_mdl$ProposerClaimNonFaultYearsAgo <- 5
  
  dt_mdl$mdl_voluntary_excess <- 0
  dt_mdl$mdl_voluntary_excess[Data$RFVoluntaryExcess=="a) 0-100"] <- 100
  dt_mdl$mdl_voluntary_excess[Data$RFVoluntaryExcess=="C) 201-400"] <- 250
  dt_mdl$mdl_voluntary_excess[Data$RFVoluntaryExcess=="d) 401-600"] <- 500
  
  
  dt_mdl$Creativity <- Data$Creativity
  dt_mdl$Physical <- Data$Physical
  dt_mdl$Autonomy <- Data$Autonomy
  dt_mdl$Educated <- Data$Educated
  dt_mdl$Art.Science <- Data$Art.Science
  dt_mdl$Public.Facing <- Data$Public.Facing
  dt_mdl$Seniority <- Data$Seniority
  
  return(dt_mdl)
}

predict_GBM <- function(Dump_Path,Link_Type,Base_Score,Data){
  mdl_GBM <- read.csv(Dump_Path)
  lnk <- make.link(Link_Type)
  
  features <- unique(mdl_GBM$Feature)
  
  #Sort out categorical features (in XGBoost model dumps, the feature name and feature level are
  #joined together with no delimiter so we will check against the column names in the input
  #dataset to find out what the names could be. This code will likely fail if there are column names that are
  #extensions of each other. This does not occur at present but handling code should be added to manage this
  cat_features <- features[!(features %in% c("Leaf",colnames(Data)))]
  cat_features_formatted <- rep("",times=length(cat_features))
  cat_levels_formatted <- rep("",times=length(cat_features))
  
  mdl_GBM$formatted_features <- ""
  mdl_GBM$formatted_levels <- ""
  for(col in colnames(Data)){
    matches <- substr(cat_features,1,nchar(col))==col
    if(sum(matches)>0){
      cat_features_formatted[matches] <- col
      cat_levels_formatted[matches] <- substr(cat_features[matches],nchar(col)+1,nchar(cat_features[matches]))
    }
  }
  mappings <- data.frame(orig=cat_features,new_feat=cat_features_formatted,new_level=cat_levels_formatted)
  
  for (mapping_idx in 1:nrow(mappings)){
    mdl_GBM$formatted_features[mdl_GBM$Feature==mappings$orig[mapping_idx]] <- mappings$new_feat[mapping_idx]
    mdl_GBM$formatted_levels[mdl_GBM$Feature==mappings$orig[mapping_idx]] <- mappings$new_level[mapping_idx]
    mdl_GBM$Feature[mdl_GBM$Feature==mappings$orig[mapping_idx]] <- "LEVEL"
  }
  
  curr_node <- rep("",time=nrow(Data))
  eta <- rep(lnk$linkfun(Base_Score),times=nrow(Data))
  
  for(node_idx in 1:nrow(mdl_GBM)){
    if(mdl_GBM$Node[node_idx]==0){#starting a new tree
      curr_node <- mdl_GBM$ID[node_idx]
    }
    if(mdl_GBM$Feature[node_idx]=="Leaf"){
      eta[curr_node==mdl_GBM$ID[node_idx]] <- eta[curr_node==mdl_GBM$ID[node_idx]]+mdl_GBM$Quality[node_idx]
    }else if(mdl_GBM$Feature[node_idx]=="LEVEL"){
      yes_logic <- (curr_node==mdl_GBM$ID[node_idx]) & (Data[,mdl_GBM$formatted_features[node_idx]]!=mdl_GBM$formatted_levels[node_idx])
      no_logic <- (curr_node==mdl_GBM$ID[node_idx]) & (Data[,mdl_GBM$formatted_features[node_idx]]==mdl_GBM$formatted_levels[node_idx])
      curr_node[yes_logic] <- mdl_GBM$Yes[node_idx]
      curr_node[no_logic] <- mdl_GBM$No[node_idx]
    }else{
      yes_logic <- (curr_node==mdl_GBM$ID[node_idx]) & (Data[,mdl_GBM$Feature[node_idx]]<mdl_GBM$Split[node_idx])
      no_logic <- (curr_node==mdl_GBM$ID[node_idx]) & (Data[,mdl_GBM$Feature[node_idx]]>=mdl_GBM$Split[node_idx])
      curr_node[yes_logic] <- mdl_GBM$Yes[node_idx]
      curr_node[no_logic] <- mdl_GBM$No[node_idx]
    }
  }
  pred <- lnk$linkinv(eta)
  return(pred)
}


load_run <- function(Run_Path,Data){
  dt_run <- read.csv(Run_Path)
  colnames(dt_run) <- c("Full_Desc","Rating")
  rating_vector <- dt_run$Rating
  names(rating_vector) <- dt_run$Full_Desc
  rm(dt_run)
  return(rating_vector)
}

clean_data <- function(Rating_Vector,Data){
  #tidy any columns in the data that will be needed by the run
  for(fac in unique(sapply(strsplit(names(Rating_Vector),"__"),FUN=function(x)x[1]))){
    Data[Data[,fac]=="",fac] <- "BLANK"
    Data[is.na(Data[,fac]),fac] <- "NULL"
  }
  return(Data)
}

apply_occupation <- function(Rating_Vector){
  fac_list <- unique(sapply(strsplit(names(Rating_Vector),"__"),FUN=function(x)x[1]))
  output <- dt_lookup[c("Relay.Code","Description")]
  output$Rating_Adj <- 1
  for(fac in fac_list){
    tmp <- Rating_Vector[sapply(strsplit(names(Rating_Vector),"__"),FUN=function(x)x[1])==fac]
    names(tmp) <- sapply(strsplit(names(tmp),"__"),FUN=function(x)x[2])
    output$Rating_Adj <- output$Rating_Adj*tmp[as.character(dt_lookup[,fac])]
  }
  return(output)
}

load_occupation_file <- function(filepath){
  occ_file <- read.csv(filepath)
  
}

objective <- function(Rating_Vector,Base_Adj=1,Predictions,Fix_Factor=1){
  Predictions=Predictions
  prem <- calc_prem(Rating_Vector=Rating_Vector,Base_Adj = Base_Adj)
  profit <- sum(ifelse(prem<=dt$market_price,1,0)*prem,na.rm = TRUE)-sum(ifelse(prem<=dt$market_price,1,0)*Predictions*Fix_Factor,na.rm = TRUE)
  return(profit)
} 