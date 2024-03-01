rm(list=ls())
tryCatch({
  dev.off()
}, error = function(e) {
  # Handle the error if needed
  cat("An error occurred:", conditionMessage(e), "\n")
})
options(error = traceback)
options(scipen=999)
options(warn=-1)

# Set Working Directory ####
setwd('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/')
wd=getwd()

# Import Source Files and libraries ####
source('P:/Underwriting/Pricing Pipelines/IV-Motor-Pricing-Engine2/000-Functions.R')
source('P:/Underwriting/Pricing Pipelines/IV-Motor-Pricing-Engine2/011-Cleaning Functions.R')
# Include packages in this function if required in pipeline. This will load
# and/or install any packages required
InstallAndLoadMissingPackages()
require(data.table)
library(tidyverse)
source('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/000-General_Functions.R')

# Load Data ####
load('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/02-CleanData.RDS')
SlidingScaleCommission=read_excel('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Commission slider scale.xlsx')
Areas=read_excel('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Areas.xlsx',skip = 0)
Occupation=read.csv('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/OccupationList.csv',skip = 0)
Loads=read_excel('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Chromosomedesign.xlsx')
SlidingScaleCommission=SlidingScaleCommission%>%mutate(minLossRatio=lag(`Loss Ratio`,1),
                                                       minLossRatio=dplyr::if_else(is.na(minLossRatio),0,minLossRatio),
                                                       maxLossRatio=`Loss Ratio`)
SlidingScaleCommission$maxLossRatio[dim(SlidingScaleCommission)[1]]=1000
SlidingScaleCommission$`Loss Ratio`=NULL

FLDR_MDL <- "P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Model"
FL_MDL <- "Model1"
# source('000 - Optimisation_Functions.R')

FLDR_OUT <- paste0("P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Model",FL_MDL)
mdl <- readRDS(file=file.path(FLDR_MDL,paste0(FL_MDL,".bin")))

MAX_FACTOR_MOVE <- 0.01
Segments=10
noOfChr=50
iter=10
EffectivenessTarget=0.8
Conversion=0.5
RenewalPremiumAdjustment=0.97
Retention=0
IverniaFeeAddons=50
IverniaFeeAddonsAdjust=1
BurningCostTarget=220
NBCanc=0.05
Avg_Premium_Target=400
LossRatioTarget=0.535
GWPTarget=13000000
AverageBuildingsTarget=260000
AverageContentsTarget=35000
BrokerCommission=0.15
AltInsurerAddons=10
minimumPremiumAlternativeAdjustment=1
OccupationLoadinMax=1.15
OccupationLoadinMin=0.85

# Grouping based on average market premium
# Grouping into pods ####
# MaxAddOn=1
predictionssplit=as.data.frame(dt$avr_prem)%>%arrange(dt$avr_prem)
# predictionssplit=rbind(predictionssplit,MaxAddOn)
names(predictionssplit)='AveragePrem'

# Transforms ####
dt=dt%>%mutate(BurningCosts=avr_prem*LossRatioTarget)
# mdl$coefficients[1] <- 1000
dt=dt[,-which(names(dt)%in%c('PostalAddressTown','PostalAddressTown','PostalAddressCounty','RiskAddressPerilResponse','RiskAddressTown'))]
dt$PropertyAge=sapply(1:dim(dt)[1],function(x){
  PropertyAge=floor(as.numeric(dt$CoverStartDate[x]-dt$ConstructionDate[x])/365)
})

SummaryCom=dt%>%summarise(Volume=n(),
                          # Sc1=sum(dplyr::if_else(HasLocks=='Yes' & !AlarmType%in%c('Unspecified','Other') & ContentsSumInsuredRequired<=100000,1,0),na.rm=T)/Volume,
                          # Sc1a=sum(dplyr::if_else(HasLocks=='Yes' & !AlarmType%in%c('Unspecified','Other') & ContentsSumInsuredRequired<=100000,1,0),na.rm=T)/Volume,
                          Sc2=sum(dplyr::if_else(HasLocks=='No' & !AlarmType%in%c('Unspecified','Other') & ContentsSumInsuredRequired<=100000,1,0),na.rm=T)/Volume,
                          Sc2a=sum(dplyr::if_else(HasLocks=='No' & !AlarmType%in%c('Unspecified','Other') & ContentsSumInsuredRequired<=60000,1,0),na.rm=T)/Volume,
                          Sc3=sum(dplyr::if_else(HasLocks=='Yes' & AlarmType%in%c('Unspecified','Other') & ContentsSumInsuredRequired<=100000,1,0),na.rm=T)/Volume-
                          sum(dplyr::if_else(HasLocks=='Yes' & AlarmType%in%c('Unspecified','Other') & ContentsSumInsuredRequired<=60000,1,0),na.rm=T)/Volume,
                          # Sc4=sum(dplyr::if_else(HasLocks=='No' & AlarmType%in%c('Unspecified','Other') & ContentsSumInsuredRequired<=100000,1,0),na.rm=T)/Volume,
                          Sc4=sum(dplyr::if_else(HasLocks=='No' & AlarmType%in%c('Unspecified','Other') & ContentsSumInsuredRequired<=35000,1,0),na.rm=T)/Volume)

SummaryBathrooms=dt%>%group_by(NumberofBathrooms)%>%summarise(volume=n(),QuoteCount=mean(qcount,na.rm = T),AvgPrem=mean(avr_prem,na.rm = T),minPrem=mean(min_prem,na.rm = T))
SummaryLocks=dt%>%filter(ContentsSumInsuredRequired>15000)%>%mutate(higherthan30K=dplyr::if_else(ContentsSumInsuredRequired>=65000,1,0),
                         Alarm=dplyr::if_else(AlarmType%in%c('Unspecified','Other'),'No Alarm','Alarm'))%>%group_by(HasLocks,Alarm,higherthan30K)%>%
  summarise(volume=n(),QuoteCount=mean(qcount,na.rm = T),AvgPrem=mean(avr_prem,na.rm = T),minPrem=mean(min_prem,na.rm = T),contents=mean(ContentsSumInsuredRequired,na.rm = T))%>%filter(volume>100)
# Filtering ####
dt=dt%>%filter(dplyr::if_else(NumberofBedrooms<=5 &
                                NumberofBathrooms<=3 &
                                ResidenceType=='Owner Occupied' &
                                RoofNonStandardpercentage<=20 &
                                ListedBuilding=="No" &
                                RoofConstructionType=="Standard" &
                                RiskAddressMatchLevel<=700 &
                                BuildingSumInsuredRequired<=1000000 &
                                (BuildingSumInsuredRequired==0 | BuildingSumInsuredRequired>=175000) &
                                ContentsSumInsuredRequired<=100000 &
                                ContentsSumInsuredRequired>=15000 &
                                SpecifiedItemsSumInsuredRequired<=ContentsSumInsuredRequired*0.3 &
                                (BuildingSumInsuredRequired>0 & ContentsSumInsuredRequired>0) &
                                PropertyAge<90
                              ,1,0)==1)

dt=dt%>%filter(CoverEffectiveFrom>=as.Date('2023-03-01',format='%Y-%m-%d') & CoverEffectiveFrom<as.Date('2023-11-01',format='%Y-%m-%d'))

library(Hmisc) # cut2
SegmentSplit=split(predictionssplit, cut2(predictionssplit$AveragePrem, g=Segments))


GroupingMaxMin=data.frame(Min=sapply(names(SegmentSplit),function(x){as.numeric(substr(x,2,gregexpr(',',x)[[1]][1]-1))}),
                          Max=sapply(names(SegmentSplit),function(x){as.numeric(substr(x,gregexpr(',',x)[[1]][1]+1,gregexpr(')',x)[[1]][1]-1))}),
                          Group=seq(1,Segments,by=1))


rownames(GroupingMaxMin)=c(1:Segments)
GroupingMaxMin$Group=rownames(GroupingMaxMin)
colnames(GroupingMaxMin)<-c('Min','Max','Group')

GroupingMaxMin$Max[1]=GroupingMaxMin$Min[2]
GroupingMaxMin$Max[which(GroupingMaxMin$Group==max(as.numeric(GroupingMaxMin$Group))[1])]=10000000

dt$Group=sapply(dt$avr_prem,function(x){
  output=as.numeric(GroupingMaxMin$Group[which(x>=as.numeric(GroupingMaxMin$Min) & x<=as.numeric(GroupingMaxMin$Max))[1]])
  return(output)
})



convert_numeric_to_labels3 <- function(df,Segments=20) {
  # browser()
  numeric_columns <- df %>%
    select(where(is.numeric)) %>%
    names()
  # browser()
  for (col in numeric_columns) {
    # Use quantile to find breakpoints for equal volumes
    
    print(col)
    df[which(is.na(df[,col])),col]=0
    breakpoints <- quantile(df[,col] , probs = seq(0, 1, length.out = Segments + 1))
    breakpoints=unique(breakpoints)
    # Use cut to split the array into equal volumes based on breakpoints
    volume_labels <- cut(df[,col], breaks = breakpoints, labels = FALSE, ordered_result = TRUE)
    
    
    
    df$Group=volume_labels
    eqn=paste0("label=df%>%group_by(Group)%>%mutate(label=paste0(as.character(min(",col,")),'-',as.character(max(",col,"))))%>%ungroup()")
    eval(parse(text = eqn))
    eqn=paste0("df$",col,"=label$label")
    eval(parse(text = eqn))
    # label=df%>%group_by(Group)%>%mutate(label=paste0(as.character(min(VehicleValue)),'-',as.character(max(VehicleValue))))
    # df[col]=label$label
    # df$Group=NULL
    # # Identify factor columns
    # factor_columns <- sapply(df, is.factor)
    # # Convert factor columns to character
    # df[factor_columns] <- lapply(df[factor_columns], as.character)
  }
  
  return(df)
}

monitor <- function(obj) {
  # browser()
  library(plotly)
  evals<<-cbind(evals,c(mean(obj$evaluations),min(obj$evaluations)))
  meanEval=which(sqrt((obj$evaluations-mean(obj$evaluations))*(obj$evaluations-mean(obj$evaluations)))
                 ==min(sqrt((obj$evaluations-mean(obj$evaluations))*(obj$evaluations-mean(obj$evaluations)))))
  minEval=which(obj$evaluations==min(obj$evaluations))
  
  # plot observations
  plotdata=as.data.frame(t(evals))
  plotdata=plotdata[-1,]
  plotdata$Iteration=seq(1:(dim(plotdata)[1]))
  names(plotdata)=c('Mean','Best','Iteration')
  plotdata=melt(plotdata,id.vars = 'Iteration')
  
  if (length(dev.list()!=0)) {dev.off()}
  dev.new()
  
  ggp<-ggplot(plotdata) +
    aes(x = Iteration, y = value, colour = variable) +
    geom_point(shape = "circle", size = 3.4) +
    geom_smooth(span = 0.75) +
    scale_color_hue(direction = 1) +
    labs(title = "Objective performance - Best/Average") +
    theme_minimal() +
    xlim(max(plotdata$Iteration), 1)+
    ylim(0,10000)
  print(ggp)
  # browser()
  return(evals)
}

QuotesData=read_xlsx('Chill Quotes.xlsx')




Optim_eval_function<-function(chromosome,Data,QuotesData,EffectivenessTarget=0.08,SummaryOutput=FALSE){
  # browser()
  # chromosome[15]=1
  IverniaFeeAddonsAdjust=1#chromosome[16]
  Base=chromosome[15]
  # Attach Area ####
  Areas$Load=chromosome[21:68]
  temparea=dt%>%select(RiskAddressCounty)
  temparea=left_join(temparea,Areas,by='RiskAddressCounty')
  # Attach Occupations ####
  tempOcc=dt%>%select(OccupationType)
  Occupation=Occupation%>%select(Level,Loading)
  Occupation$Loading=Occupation$Loading*chromosome[69]
  Occupation=Occupation%>%mutate(Loading=dplyr::if_else(Loading>OccupationLoadinMax,OccupationLoadinMax,
                                                        dplyr::if_else(Loading<OccupationLoadinMin,OccupationLoadinMin,Loading)))
  names(Occupation)[1]='OccupationType'
  tempOcc=left_join(tempOcc,Occupation,by='OccupationType')
  
  dt$IVAPrice=(exp(predict(mdl,type="response",newdata = dt)))
  dt$IVAPrice=dt$IVAPrice+IverniaFeeAddons*IverniaFeeAddonsAdjust
  dt=dt%>%mutate(IverniaQuote=dplyr::if_else(NumberofBedrooms<=5 & 
                                               NumberofBathrooms<=3 & 
                                               ResidenceType=='Owner Occupied' & 
                                               RoofNonStandardpercentage<=20 &
                                               ListedBuilding=="No" &
                                               RoofConstructionType=="Standard" &
                                               RiskAddressMatchLevel<=700 &
                                               BuildingSumInsuredRequired<=1000000 &
                                               (BuildingSumInsuredRequired==0 | BuildingSumInsuredRequired>=175000) &
                                               ContentsSumInsuredRequired<=100000 &
                                               ContentsSumInsuredRequired>=15000 &
                                               SpecifiedItemsSumInsuredRequired<=ContentsSumInsuredRequired*0.3 &
                                               (BuildingSumInsuredRequired>0 & ContentsSumInsuredRequired>0) &
                                               PropertyAge<90
                                             ,1,0)
                 )
  

  
  SummaryGroups=dt%>%filter(IverniaQuote==1)%>%group_by(Group)%>%summarise(Volume=n(),
                                                                           IverniaSales=sum(dplyr::if_else(IVAPrice<=min_prem,1,NA),na.rm = T),
                                                 AlternativeQuotes=mean(qcount,na.rm = T),
                                                 AlternativeMinPrem=mean(min_prem,na.rm = T),
                                                 AlternativeAvgPrem=mean(avr_prem,na.rm = T),
                                                 AvgIverniaPremium=mean(IVAPrice,na.rm = T),
                                                 AvgIverniaPremiumWin=mean(dplyr::if_else(IVAPrice<=min_prem,IVAPrice,NA),na.rm = T),
                                                 IverniaCheapest=mean(dplyr::if_else(IVAPrice<=min_prem,1,0),na.rm = T),
                                                 BuildingsADPortion=mean(dplyr::if_else(BuildingAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                                 BuildingSI=mean(BuildingSumInsuredRequired,na.rm = T),
                                                 BuildingSIIVA=mean(dplyr::if_else(IVAPrice<=min_prem,BuildingSumInsuredRequired,NA),na.rm = T),
                                                 ContentsADPortion=mean(dplyr::if_else(ContentsAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                                 ContentsSI=mean(ContentsSumInsuredRequired,na.rm = T),
                                                 ContentsSIIVA=mean(dplyr::if_else(IVAPrice<=min_prem,ContentsSumInsuredRequired,NA),na.rm = T),
                                                 SpecifiedItemsPortion=mean(dplyr::if_else(SpecifiedItemsSumInsuredRequired>0,1,0),na.rm = T),
                                                 SpecifiedItemsSI=mean(SpecifiedItemsSumInsuredRequired,na.rm = T),
                                                 NumberofBathrooms=mean(NumberofBathrooms,na.rm = T),
                                                 NumberofBedrooms=mean(NumberofBedrooms,na.rm = T),
                                                 RoofConstructionStandardPortion=mean(dplyr::if_else(RoofConstructionType=='Standard',1,0),na.rm = T),
                                                 ResidentOwnerType=mean(dplyr::if_else(ResidenceType=='Owner Occupied',1,0),na.rm = T),
                                                 AverageIverniaWinPrem=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1,IVAPrice,NA),na.rm = T),
                                                 AverageIverniaWinBSI=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1,BuildingSumInsuredRequired,NA),na.rm = T),
                                                 ContentSContentsIFlg=mean(dplyr::if_else(ContentsSumInsuredRequired>0 & BuildingSumInsuredRequired>0,1,0),na.rm = T),
                                                 BuildingsAndContentsAvgPrem=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1 & ContentsSumInsuredRequired>0 & BuildingSumInsuredRequired>0,IVAPrice,NA),na.rm = T))%>%
    mutate(Group=as.character(Group))
  SummaryOverall=dt%>%filter(IverniaQuote==1)%>%summarise(Group='Overall',
                                Volume=n(),
                                IverniaSales=sum(dplyr::if_else(IVAPrice<=min_prem,1,NA),na.rm = T),
                                AlternativeQuotes=mean(qcount,na.rm = T),
                                AlternativeMinPrem=mean(min_prem,na.rm = T),
                                AlternativeAvgPrem=mean(avr_prem,na.rm = T),
                                AvgIverniaPremium=mean(IVAPrice,na.rm = T),
                                AvgIverniaPremiumWin=mean(dplyr::if_else(IVAPrice<=min_prem,IVAPrice,NA),na.rm = T),
                                IverniaCheapest=mean(dplyr::if_else(IVAPrice<=min_prem,1,0),na.rm = T),
                                BuildingsADPortion=mean(dplyr::if_else(BuildingAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                BuildingSI=mean(BuildingSumInsuredRequired,na.rm = T),
                                BuildingSIIVA=mean(dplyr::if_else(IVAPrice<=min_prem,BuildingSumInsuredRequired,NA),na.rm = T),
                                ContentsADPortion=mean(dplyr::if_else(ContentsAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                ContentsSI=mean(ContentsSumInsuredRequired,na.rm = T),
                                ContentsSIIVA=mean(dplyr::if_else(IVAPrice<=min_prem,ContentsSumInsuredRequired,NA),na.rm = T),
                                SpecifiedItemsPortion=mean(dplyr::if_else(SpecifiedItemsSumInsuredRequired>0,1,0),na.rm = T),
                                SpecifiedItemsSI=mean(SpecifiedItemsSumInsuredRequired,na.rm = T),
                                
                                NumberofBathrooms=mean(NumberofBathrooms,na.rm = T),
                                NumberofBedrooms=mean(NumberofBedrooms,na.rm = T),
                                RoofConstructionStandardPortion=mean(dplyr::if_else(RoofConstructionType=='Standard',1,0),na.rm = T),
                                ResidentOwnerType=mean(dplyr::if_else(ResidenceType=='Owner Occupied',1,0),na.rm = T),
                                AverageIverniaWinPrem=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1,IVAPrice,NA),na.rm = T),
                                AverageIverniaWinBSI=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1,BuildingSumInsuredRequired,NA),na.rm = T),
                                ContentSContentsIFlg=mean(dplyr::if_else(ContentsSumInsuredRequired>0 & BuildingSumInsuredRequired>0,1,0),na.rm = T),
                                BuildingsAndContentsAvgPrem=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1 & ContentsSumInsuredRequired>0 & BuildingSumInsuredRequired>0,IVAPrice,NA),na.rm = T))
  summary1=rbind(SummaryGroups,SummaryOverall)
  summary1$Option='Base 1'
  
  # Adjust Base ####
  Base=chromosome[15]

  mdl[["coefficients"]][["ResidenceYears"]]=0
  mdl[["coefficients"]][["num_ph"]]=mdl[["coefficients"]][["num_ph"]]*chromosome[1]
  # Coefficient adjustment ####
  mdl[["coefficients"]][["SocialWelfareYes"]]=mdl[["coefficients"]][["SocialWelfareYes"]]*0
  mdl[["coefficients"]][["SpecifiedItemsSumInsuredRequired"]]=mdl[["coefficients"]][["SpecifiedItemsSumInsuredRequired"]]*chromosome[2]
  mdl[["coefficients"]][["BuildingSumInsuredRequired"]]=mdl[["coefficients"]][["BuildingSumInsuredRequired"]]*chromosome[3]
  mdl[["coefficients"]][["ContentsSumInsuredRequired"]]=mdl[["coefficients"]][["ContentsSumInsuredRequired"]]*chromosome[4]
  mdl[["coefficients"]][["NumberofBedrooms"]]=mdl[["coefficients"]][["NumberofBedrooms"]]*chromosome[5]
  mdl[["coefficients"]][["NumberofBathrooms"]]=mdl[["coefficients"]][["NumberofBathrooms"]]*chromosome[6]
  mdl[["coefficients"]][["PHAge"]]=mdl[["coefficients"]][["PHAge"]]*chromosome[7]
  mdl[["coefficients"]][["I(PHAge^2)"]]=mdl[["coefficients"]][["I(PHAge^2)"]]*chromosome[8]
  mdl[["coefficients"]][["cover_typeB+C"]]=mdl[["coefficients"]][["cover_typeB+C"]]*chromosome[9]
  mdl[["coefficients"]][["cover_typeC"]]=mdl[["coefficients"]][["cover_typeC"]]*chromosome[10]
  mdl[["coefficients"]][["ClaimedYearsNCD"]]=mdl[["coefficients"]][["ClaimedYearsNCD"]]*chromosome[11]
  mdl[["coefficients"]][["OccupiedDuringDateYes"]]=mdl[["coefficients"]][["OccupiedDuringDateYes"]]*chromosome[12]
  mdl[["coefficients"]][["PropertyAge"]]=mdl[["coefficients"]][["PropertyAge"]]*chromosome[13]
  mdl[["coefficients"]][["I(PropertyAge^2)"]]=mdl[["coefficients"]][["I(PropertyAge^2)"]]*chromosome[14]
  # mdl[["coefficients"]][["(Intercept)"]]=mdl[["coefficients"]][["(Intercept)"]]*chromosome[15]
  mdl[["coefficients"]][["BuildingAccidentalDamageRequiredYes"]]=mdl[["coefficients"]][["BuildingAccidentalDamageRequiredYes"]]*chromosome[16]
  mdl[["coefficients"]][["ContentsAccidentalDamageRequiredYes"]]=mdl[["coefficients"]][["ContentsAccidentalDamageRequiredYes"]]*chromosome[17]
  mdl[["coefficients"]][["EmploymentTypeRetired"]]=mdl[["coefficients"]][["EmploymentTypeRetired"]]*chromosome[18]
  mdl[["coefficients"]][["EmploymentTypeSelf Employed"]]=mdl[["coefficients"]][["EmploymentTypeSelf Employed"]]*chromosome[19]
  mdl[["coefficients"]][["EmploymentTypeUnemployed"]]=mdl[["coefficients"]][["EmploymentTypeUnemployed"]]*chromosome[20]
  # mdl[["coefficients"]][["ResidenceYears"]]=1

  
  # Premium Calculation ####
  BuildingsAndContentsMin=125
  BuildingsMin=100
  ContentsMin=70
  dt$IVAPrice=(exp(predict(mdl,type="response",newdata = dt)))*temparea$Load*Base*tempOcc$Loading
  dt$IVAPrice=dt$IVAPrice+IverniaFeeAddons*IverniaFeeAddonsAdjust
  dt=dt%>%mutate(IVAPrice=dplyr::if_else(cover_type=='B+C' & IVAPrice<(BuildingsAndContentsMin+IverniaFeeAddons*IverniaFeeAddonsAdjust),(BuildingsAndContentsMin+IverniaFeeAddons*IverniaFeeAddonsAdjust),
                                         dplyr::if_else(cover_type=='B' & IVAPrice<(BuildingsMin+IverniaFeeAddons*IverniaFeeAddonsAdjust),(BuildingsMin+IverniaFeeAddons*IverniaFeeAddonsAdjust),
                                                        dplyr::if_else(cover_type=='C' & IVAPrice<(ContentsMin+IverniaFeeAddons*IverniaFeeAddonsAdjust),(ContentsMin+IverniaFeeAddons*IverniaFeeAddonsAdjust),IVAPrice)
                                         )))
  # Summary new ####
  SummaryGroups=dt%>%filter(IverniaQuote==1)%>%group_by(Group)%>%summarise(Volume=n(),
                                                                           IverniaSales=sum(dplyr::if_else(IVAPrice<=min_prem,1,NA),na.rm = T),
                                                                           AlternativeQuotes=mean(qcount,na.rm = T),
                                                                           AlternativeMinPrem=mean(min_prem,na.rm = T),
                                                                           AlternativeAvgPrem=mean(avr_prem,na.rm = T),
                                                                           AvgIverniaPremium=mean(IVAPrice,na.rm = T),
                                                                           AvgIverniaPremiumWin=mean(dplyr::if_else(IVAPrice<=min_prem,IVAPrice,NA),na.rm = T),
                                                                           IverniaCheapest=mean(dplyr::if_else(IVAPrice<=min_prem,1,0),na.rm = T),
                                                                           BuildingsADPortion=mean(dplyr::if_else(BuildingAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                                                           BuildingSI=mean(BuildingSumInsuredRequired,na.rm = T),
                                                                           BuildingSIIVA=mean(dplyr::if_else(IVAPrice<=min_prem,BuildingSumInsuredRequired,NA),na.rm = T),
                                                                           ContentsADPortion=mean(dplyr::if_else(ContentsAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                                                           ContentsSI=mean(ContentsSumInsuredRequired,na.rm = T),
                                                                           ContentsSIIVA=mean(dplyr::if_else(IVAPrice<=min_prem,ContentsSumInsuredRequired,NA),na.rm = T),
                                                                           SpecifiedItemsPortion=mean(dplyr::if_else(SpecifiedItemsSumInsuredRequired>0,1,0),na.rm = T),
                                                                           SpecifiedItemsSI=mean(SpecifiedItemsSumInsuredRequired,na.rm = T),
                                                                           NumberofBathrooms=mean(NumberofBathrooms,na.rm = T),
                                                                           NumberofBedrooms=mean(NumberofBedrooms,na.rm = T),
                                                                           RoofConstructionStandardPortion=mean(dplyr::if_else(RoofConstructionType=='Standard',1,0),na.rm = T),
                                                                           ResidentOwnerType=mean(dplyr::if_else(ResidenceType=='Owner Occupied',1,0),na.rm = T),
                                                                           AverageIverniaWinPrem=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1,IVAPrice,NA),na.rm = T),
                                                                           AverageIverniaWinBSI=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1,BuildingSumInsuredRequired,NA),na.rm = T),
                                                                           ContentSContentsIFlg=mean(dplyr::if_else(ContentsSumInsuredRequired>0 & BuildingSumInsuredRequired>0,1,0),na.rm = T),
                                                                           BuildingsAndContentsAvgPrem=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1 & ContentsSumInsuredRequired>0 & BuildingSumInsuredRequired>0,IVAPrice,NA),na.rm = T)
  )%>%
    mutate(Group=as.character(Group))
  SummaryOverall=dt%>%filter(IverniaQuote==1)%>%summarise(Group='Overall',
                                                          Volume=n(),
                                                          IverniaSales=sum(dplyr::if_else(IVAPrice<=min_prem,1,NA),na.rm = T),
                                                          AlternativeQuotes=mean(qcount,na.rm = T),
                                                          AlternativeMinPrem=mean(min_prem,na.rm = T),
                                                          AlternativeAvgPrem=mean(avr_prem,na.rm = T),
                                                          AvgIverniaPremium=mean(IVAPrice,na.rm = T),
                                                          AvgIverniaPremiumWin=mean(dplyr::if_else(IVAPrice<=min_prem,IVAPrice,NA),na.rm = T),
                                                          IverniaCheapest=mean(dplyr::if_else(IVAPrice<=min_prem,1,0),na.rm = T),
                                                          BuildingsADPortion=mean(dplyr::if_else(BuildingAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                                          BuildingSI=mean(BuildingSumInsuredRequired,na.rm = T),
                                                          BuildingSIIVA=mean(dplyr::if_else(IVAPrice<=min_prem,BuildingSumInsuredRequired,NA),na.rm = T),
                                                          ContentsADPortion=mean(dplyr::if_else(ContentsAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                                          ContentsSI=mean(ContentsSumInsuredRequired,na.rm = T),
                                                          ContentsSIIVA=mean(dplyr::if_else(IVAPrice<=min_prem,ContentsSumInsuredRequired,NA),na.rm = T),
                                                          SpecifiedItemsPortion=mean(dplyr::if_else(SpecifiedItemsSumInsuredRequired>0,1,0),na.rm = T),
                                                          SpecifiedItemsSI=mean(SpecifiedItemsSumInsuredRequired,na.rm = T),
                                                          NumberofBathrooms=mean(NumberofBathrooms,na.rm = T),
                                                          NumberofBedrooms=mean(NumberofBedrooms,na.rm = T),
                                                          RoofConstructionStandardPortion=mean(dplyr::if_else(RoofConstructionType=='Standard',1,0),na.rm = T),
                                                          ResidentOwnerType=mean(dplyr::if_else(ResidenceType=='Owner Occupied',1,0),na.rm = T),
                                                          AverageIverniaWinPrem=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1,IVAPrice,NA),na.rm = T),
                                                          AverageIverniaWinBSI=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1,BuildingSumInsuredRequired,NA),na.rm = T),
                                                          ContentSContentsIFlg=mean(dplyr::if_else(ContentsSumInsuredRequired>0 & BuildingSumInsuredRequired>0,1,0),na.rm = T),
                                                          BuildingsAndContentsAvgPrem=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1 & ContentsSumInsuredRequired>0 & BuildingSumInsuredRequired>0,IVAPrice,NA),na.rm = T))
  # SummaryGroups$IverniaCheapest[which(SummaryGroups$Group=='10')]= SummaryGroups$IverniaCheapest[which(SummaryGroups$Group=='10')]*2
  summary2=rbind(SummaryGroups,SummaryOverall)
  summary2$Option=paste0('Contents ',as.character(chromosome[1]),
                         'Contents and Buildings ',as.character(chromosome[9]),
                         'Buildings',as.character(chromosome[10]))
  
  summary=rbind(summary1,summary2)
  Quoteability=mean(dt$IverniaQuote,na.rm = TRUE)
  Effectiveness=dt%>%
    summarise(Eff=sum(dplyr::if_else(IverniaQuote==1 & IVAPrice<=min_prem,1,0),na.rm = TRUE)/
                sum(dplyr::if_else(IverniaQuote==1,1,0),na.rm = TRUE))
  Effectiveness=Effectiveness$Eff
  Avg_Premium=mean(dt$IVAPrice[which(dt$IverniaQuote==1 & dt$IVAPrice<=dt$min_prem)],na.rm=TRUE)
  Alt_Avg_Premium=mean(dt$avr_prem[which(dt$IverniaQuote==1 & dt$IVAPrice<=dt$min_prem)],na.rm=TRUE)
  BurningCost=mean(dt$BurningCosts[which(dt$IverniaQuote==1 & dt$IVAPrice<=dt$min_prem)],na.rm=TRUE)
  if(is.na(Avg_Premium)){Avg_Premium=10000}
  Avg_PremiumBC=mean(dt$IVAPrice[which(dt$IverniaQuote==1 & dt$IVAPrice<=dt$min_prem &
                                         dt$BuildingSumInsuredRequired>0 & dt$ContentsSumInsuredRequired>0)],na.rm=TRUE)
  
  # Forecast Cacluation ####
  QuotesData$IverniaQuotes=QuotesData$`Chill Monthly Quotes`*Quoteability
  QuotesData$Effectiveness=Effectiveness
  QuotesData$IverniaEffectiveQuotes=QuotesData$IverniaQuotes*Effectiveness
  QuotesData$`Ivernia Transfers`=QuotesData$`Chill Invites`*Effectiveness*0
  QuotesData$IverniaSales=QuotesData$IverniaEffectiveQuotes*Conversion
  QuotesData$IverniaCancels=QuotesData$IverniaSales*NBCanc
  QuotesData=QuotesData%>%mutate(NBPolicyCount=IverniaSales-IverniaCancels)
  QuotesData$Premium=Avg_Premium
  QuotesData$CumBase=cumsum(QuotesData$Base)
  QuotesData$Premium=QuotesData$Premium*(1+QuotesData$CumBase)
  QuotesData=QuotesData%>%mutate(InsurerNBPremium=Premium-IverniaFeeAddons)
  QuotesData=QuotesData%>%mutate(InsurerNBPremiumTot=InsurerNBPremium*NBPolicyCount,
                                 TotalNBClaims=NBPolicyCount*BurningCost)
  # browser()
  QuotesData$TransferPremium=QuotesData$Premium*RenewalPremiumAdjustment
  QuotesData$InsurerTranPremium=QuotesData$TransferPremium-IverniaFeeAddons
  QuotesData$InsurerTranPremiumTot=QuotesData$InsurerTranPremium*QuotesData$`Ivernia Transfers`
  QuotesData=QuotesData%>%mutate(TotalTranCost=`Ivernia Transfers`*BurningCost)
  
  
  QuotesData$Retention=Retention
  QuotesData=QuotesData%>%mutate(RenewalPremium=lag(Premium,12),
                                 RenewalPremium=RenewalPremium*RenewalPremiumAdjustment+RenewalPremium*CumBase,
                                 InsurerRenewalPremium=RenewalPremium-IverniaFeeAddons,
                                 RenewalPremium=dplyr::if_else(is.na(RenewalPremium),0,RenewalPremium))
  QuotesData=QuotesData%>%mutate(IverniaInvites=lag(NBPolicyCount,12)+lag(`Ivernia Transfers`,12),
                                 IverniaRenewals=IverniaInvites*Retention,
                                 InsurerRenewalPremiumTot=InsurerRenewalPremium*IverniaRenewals,
                                 TotalRenCost=IverniaRenewals*BurningCost,
                                 TotalInsurerCost=dplyr::if_else(is.na(TotalNBClaims),0,TotalNBClaims)+
                                   dplyr::if_else(is.na(TotalTranCost),0,TotalTranCost)+
                                   dplyr::if_else(is.na(TotalRenCost),0,TotalRenCost),
                                 TotalInsurerTot=dplyr::if_else(is.na(InsurerNBPremiumTot),0,InsurerNBPremiumTot)+
                                   dplyr::if_else(is.na(InsurerTranPremiumTot),0,InsurerTranPremiumTot)+
                                   dplyr::if_else(is.na(InsurerRenewalPremiumTot),0,InsurerRenewalPremiumTot),
                                 LossRatio=TotalInsurerCost/TotalInsurerTot)
  LossRatio=sum(QuotesData$TotalInsurerCost)/sum(QuotesData$TotalInsurerTot)
  # print(LossRatio)
  # Sliding Scale ####
  # browser()
  if(length(which(LossRatio>=SlidingScaleCommission$minLossRatio & LossRatio< SlidingScaleCommission$maxLossRatio))!=0){
    Commission=SlidingScaleCommission$Commission[which(LossRatio>=SlidingScaleCommission$minLossRatio & LossRatio< SlidingScaleCommission$maxLossRatio)]  
  }else{
    Commission=SlidingScaleCommission$Commission[dim(SlidingScaleCommission)[1]]
  }
  
  QuotesData$Commission=Commission
  # browser()
  QuotesData=QuotesData%>%mutate(
                                 IverniaIncome=
                                   # Fees and addon Commissions
                                   NBPolicyCount*IverniaFeeAddons*IverniaFeeAddonsAdjust+
                                   `Ivernia Transfers`*IverniaFeeAddons*IverniaFeeAddonsAdjust+
                                   IverniaRenewals*IverniaFeeAddons*IverniaFeeAddonsAdjust+
                                   # Premium Commission
                                   (TotalInsurerTot*Commission-TotalInsurerTot*BrokerCommission)
                                 )
  
  AverageBuildings=dt%>%summarise(avg=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1,BuildingSumInsuredRequired,NA),na.rm = TRUE))
  AverageContents=dt%>%summarise(avg=mean(dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1,ContentsSumInsuredRequired,NA),na.rm = TRUE))
  GWP=sum(QuotesData$TotalInsurerTot,na.rm = TRUE)
  
  LossRatioObj=abs((LossRatio-LossRatioTarget)/LossRatioTarget)
  # Avg_Premium_Obj=abs((Avg_Premium-Avg_Premium_Target)/Avg_Premium_Target)
  Avg_Premium_Obj=sum(abs(dt$IVAPrice[which(dt$IverniaQuote==1 & dt$IVAPrice<=dt$min_prem)]-dt$avr_prem[which(dt$IverniaQuote==1 & dt$IVAPrice<=dt$min_prem)]))
  GWP_Obj=abs((GWP-GWPTarget)/GWPTarget)
  
  AverageBuildingsObj=abs((AverageBuildings$avg-AverageBuildingsTarget)/AverageBuildingsTarget)
  AverageContentsObj=abs((AverageContents$avg-AverageContentsTarget)/AverageContentsTarget)

  
  SummaryGroups=SummaryGroups%>%mutate(Penalty=abs(as.numeric(Group)-5.5)*as.numeric(Group))
  SummaryGroups$Penalty=SummaryGroups$Penalty/sum(SummaryGroups$Penalty)
  SummaryGroups$Cheapest=abs((abs(SummaryGroups$IverniaCheapest-
                                    SummaryOverall$IverniaCheapest)/SummaryOverall$IverniaCheapest)*2)
  SummaryGroups=SummaryGroups%>%mutate(Penalty2=Cheapest*Penalty)
  Penalty=sum(abs(SummaryGroups$Penalty2))
  if(is.na(Penalty)){Penalty=10000}
  RMSEOfDistrib=abs(sum(abs(SummaryGroups$IverniaCheapest-
                              SummaryOverall$IverniaCheapest)/SummaryOverall$IverniaCheapest))
  if(is.nan(RMSEOfDistrib)){RMSEOfDistrib=10000}
  # browser()
  SummaryGroups$AvgIverniaPremiumWin[is.na(SummaryGroups$AvgIverniaPremiumWin)]=0
  RMSEOfPrice=abs(sum(abs(SummaryGroups$AvgIverniaPremium-SummaryGroups$AlternativeAvgPrem)/SummaryGroups$AlternativeAvgPrem))
  RMSEOfPriceMin=abs(sum(abs(SummaryGroups$AvgIverniaPremiumWin-SummaryGroups$AlternativeMinPrem*minimumPremiumAlternativeAdjustment)/
                           SummaryGroups$AlternativeMinPrem*minimumPremiumAlternativeAdjustment))
  if(is.nan(RMSEOfPrice)){RMSEOfPrice=10000}
  EffectivenessObj=abs(EffectivenessTarget*100-SummaryOverall$IverniaCheapest*100)/(EffectivenessTarget*100)
  BurningCostObj=((BurningCost-BurningCostTarget)/BurningCostTarget)
  # Objective=Effectiveness*10+RMSEOfDistrib*0+RMSEOfPrice*3+Penalty
  
  # Objective ####
  Objective=EffectivenessObj*5+RMSEOfPrice*5+RMSEOfPriceMin*3+RMSEOfDistrib*1+Penalty*5
  if(is.na(Objective)){Objective=10000000}
    
  print(paste0('LR:',as.character(round(LossRatio,2)),
               ' GWP:',as.character(round(GWP,0)),
               ' Avg_Prem:',as.character(round(Avg_Premium,2)),
               ' Ivernia Fee/Addons: ',as.character(round(IverniaFeeAddons*IverniaFeeAddonsAdjust,2)),
               ' Obj:',round(Objective,1)))
  if(SummaryOutput==FALSE){
    return(Objective)
  }else{
    output=list()
    output$GroupSummary=summary
    output$summary=QuotesData
    
    output$Price=dt$IVAPrice
    output$results=list(GWP=GWP,Avg_Premium=Avg_Premium,LR=LossRatio,
                        AverageBuildings=AverageBuildings$avg,
                        AverageContents=AverageContents$avg,
                        FeeAddons=IverniaFeeAddons*IverniaFeeAddonsAdjust,
                        BurningCost=BurningCost,
                        Effectiveness=Effectiveness)
    return(output)
  }
}

# Chromosome Builder ####
set.seed(0)

for(c in 1:dim(Loads)[1]){
  if(c==1){
    chromosomeInput=matrix(runif(noOfChr,Loads$Min[c],Loads$Max[c]))
  }else{
    chromosomeInput=cbind(chromosomeInput,matrix(runif(noOfChr,Loads$Min[c],Loads$Max[c])))
  }
}
chromosomeInput=t(chromosomeInput)


evals=matrix(0,2)



# optimise rates ####
GAmodelCloseReg <- rbga(stringMin=Loads$Min, stringMax=Loads$Max,
                        suggestions=t(chromosomeInput), iters=iter,popSize=noOfChr+1,monitorFunc = monitor,
                        mutationChance = 1/round((c(dim(chromosomeInput)[1]))*0.5,0),showSettings=FALSE,
                        evalFunc=function(chromosomeInput) Optim_eval_function(chromosome=chromosomeInput,Data=dt,QuotesData=QuotesData,EffectivenessTarget=EffectivenessTarget,SummaryOutput=FALSE)
                        ,verbose = TRUE)
# Best Solution ####
bestSolutionReg<-GAmodelCloseReg$population[which.min(GAmodelCloseReg$evaluations),]
names(bestSolutionReg)=c(Loads$Chromosome)

# bestSolutionReg[c(15)]=c(1.0047)

Output=Optim_eval_function(chromosome=bestSolutionReg,Data=dt,QuotesData=QuotesData,EffectivenessTarget=EffectivenessTarget,SummaryOutput=TRUE)
dt$IVAPrice=Output$Price
dt=dt%>%mutate(IverniaQuote=dplyr::if_else(NumberofBedrooms<=5 &
                                             NumberofBathrooms<=3 &
                                             ResidenceType=='Owner Occupied' &
                                             RoofNonStandardpercentage<=20 &
                                             ListedBuilding=="No" &
                                             RoofConstructionType=="Standard" &
                                             RiskAddressMatchLevel<=700 &
                                             BuildingSumInsuredRequired<=1000000 &
                                             (BuildingSumInsuredRequired==0 | BuildingSumInsuredRequired>=175000) &
                                             ContentsSumInsuredRequired<=100000 &
                                             SpecifiedItemsSumInsuredRequired<=ContentsSumInsuredRequired*0.3 &
                                             PropertyAge<90
                                             ,1,0),
               IverniaQuoteWin=dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1,1,0))%>%
  mutate(IverniaQuoteWin=dplyr::if_else(is.na(IverniaQuoteWin),0,IverniaQuoteWin))

write_clip(Output$GroupSummary)
bestSolutionReg=data.frame(bestSolutionReg,stringsAsFactors = FALSE)
write.csv(bestSolutionReg,'BestSolution.csv')
print(Output$results)
View(Output$summary)

# Interogate Coefficients ####
FactorsToAssess=names(dt)[1:57]
FactorsToAssess=FactorsToAssess[-which(FactorsToAssess%in%c("CoverEndDate","HomeCoverId","CoverStartDate","ResidenceYears",
                                                            "timeIdx","x","HomeRiskId","ConstructionDate","SocialWelfare",
                                                            "min_prem","max_prem","avr_prem","BirthDate","IsFurnished",
                                                            "qcount","ResidenceUnk","AreaRating","ResidenceType","ResidenceDate",
                                                            "RoofNonStandardpercentage","CoverEffectiveFrom",
                                                            "NumberofTimesLetinaYear"))]
dt_banded=dt[,which(names(dt)%in%FactorsToAssess)]
dt_banded=convert_numeric_to_labels3(dt_banded,10)
dt=dt%>%mutate(IVAPriceWin=dplyr::if_else(IVAPrice<=min_prem & IverniaQuote==1,IVAPrice,NA))
dt_banded$IVAPriceWin=dt$IVAPriceWin
for(f in 1:length(FactorsToAssess)){
  print(paste0(f,' ',FactorsToAssess[f]))
  if(f==1){
    levels=length(unique(dt_banded[,which(names(dt_banded)==FactorsToAssess[f])]))
    FullFactorLevel=list()
    FullFactorLevel$Factor=matrix(unique(FactorsToAssess)[f],levels)
    for(l in 1:levels){
      FullFactorLevel$Level[l]=unique(dt_banded[,which(names(dt_banded)==FactorsToAssess[f])])[l]
      FullFactorLevel=as.data.frame(FullFactorLevel, stringsAsFactors = FALSE)
    }
  }else{
    levels=length(unique(dt_banded[,which(names(dt_banded)==FactorsToAssess[f])]))
    FullFactorLeveladd=list()
    FullFactorLeveladd$Factor=matrix(unique(FactorsToAssess)[f],levels)
    for(l in 1:levels){
      FullFactorLeveladd$Level[l]=unique(dt_banded[,which(names(dt_banded)==FactorsToAssess[f])])[l]
      FullFactorLeveladd=as.data.frame(FullFactorLeveladd, stringsAsFactors = FALSE)
    }
    FullFactorLevel=rbind(FullFactorLevel,FullFactorLeveladd)
  }
}

for(l in 1:dim(FullFactorLevel)[1]){
  idx=which(dt[,FullFactorLevel$Factor[l]]==FullFactorLevel$Level[l])
  FullFactorLevel$Vol[l]=length(idx)
  FullFactorLevel$PerVol[l]=length(idx)/dim(dt)[1]
  FullFactorLevel$AlternativeAvg[l]=mean(dt$avr_prem[idx],na.rm=T)
  FullFactorLevel$AlternativeMin[l]=mean(dt$min_prem[idx],na.rm=T)
  FullFactorLevel$IverniaAvg[l]=mean(dt$IVAPrice[idx],na.rm=T)
  FullFactorLevel$IverniaAvgWin[l]=mean(dt$IVAPriceWin[idx],na.rm=T)
}
FullFactorLevel=FullFactorLevel%>%
  mutate(DifferencetoMin=AlternativeMin-IverniaAvgWin,
         DifferencetoAvg=AlternativeAvg-IverniaAvg)%>%
  arrange(desc(DifferencetoMin))%>%
  mutate(AltLoading=DifferencetoMin/AlternativeMin,
         AltLoading2=DifferencetoAvg/AlternativeAvg)%>%
  # filter(Factor!='OccupationType')%>%
  filter(PerVol>0.001)%>%
  mutate(Loading=AltLoading*0.5+1)

View(FullFactorLevel)
# write.csv(FullFactorLevel,'OccupationList.csv')
# Output Input for rates file #### 
# Occupations
OccupationOut=Occupation%>%select(Level,Loading)%>%mutate(Loading=dplyr::if_else(is.na(Loading),1,Loading))
names(OccupationOut)[1]='Factor'
BSnames=row.names(bestSolutionReg)
temp=data.frame(Factor=BSnames,Loading=bestSolutionReg$bestSolutionReg)
temp=temp[-dim(temp)[1],]
OutputForExcelModel=rbind(temp[,-dim(temp)[1]],
                          OccupationOut)
write.csv(OutputForExcelModel,'RatingBookOutput.csv')


# Summary Tables ####
HeatingType=dt%>%group_by(HeatingType)%>%summarise(Vol=n(),
                                                   AvgAltPremium=mean(avr_prem,na.rm = T),
                                                   AvgAltPremiumMin=mean(min_prem,na.rm = T),
                                                   AvgIverniaPremium=mean(IVAPrice,na.rm=T),
                                                   AvgIverniaPremiumWin=mean(dplyr::if_else(IVAPrice<min_prem,IVAPrice,NA),na.rm = T),
                                                   WinPortinon=mean(dplyr::if_else(IVAPrice<min_prem,1,0),na.rm = T))
dt$BuildingSumInsuredRequiredBanded='NULL'
dt[,c("HeatingType","BuildingSumInsuredRequiredBanded")]=convert_numeric_to_labels3(dt[,c("HeatingType","BuildingSumInsuredRequired")],Segments=10)
BuildingsSI=dt%>%group_by(BuildingSumInsuredRequiredBanded)%>%summarise(Vol=n(),
                                                   AvgAltPremium=mean(avr_prem,na.rm = T),
                                                   AvgAltPremiumMin=mean(min_prem,na.rm = T),
                                                   AvgIverniaPremium=mean(IVAPrice,na.rm=T),
                                                   AvgIverniaPremiumWin=mean(dplyr::if_else(IVAPrice<min_prem,IVAPrice,NA),na.rm = T),
                                                   WinPortinon=mean(dplyr::if_else(IVAPrice<min_prem,1,0),na.rm = T))
dt$ContentsSumInsuredRequiredBanded='NULL'
dt[,c("HeatingType","ContentsSumInsuredRequiredBanded")]=convert_numeric_to_labels3(dt[,c("HeatingType","ContentsSumInsuredRequired")],Segments=10)
ContentsSI=dt%>%group_by(ContentsSumInsuredRequiredBanded)%>%summarise(Vol=n(),
                                                                        AvgAltPremium=mean(avr_prem,na.rm = T),
                                                                        AvgAltPremiumMin=mean(dplyr::if_else(IVAPrice<min_prem,min_prem,NA),na.rm = T),
                                                                        AvgIverniaPremium=mean(IVAPrice,na.rm=T),
                                                                        AvgIverniaPremiumWin=mean(dplyr::if_else(IVAPrice<min_prem,IVAPrice,NA),na.rm = T),
                                                                        WinPortinon=mean(dplyr::if_else(IVAPrice<min_prem,1,0),na.rm = T))
dt=dt%>%mutate(delta=(IVAPrice-min_prem)/min_prem)
dt$deltabanded=dt$delta
dt[,c("HeatingType","deltabanded")]=convert_numeric_to_labels3(dt[,c("HeatingType","delta")],Segments=10)
distrib=ggplot(dt) +
  aes(x = delta) +
  geom_histogram(bins = 200L, fill = "#112446") +
  theme_minimal()
ggplotly(distrib)

write_csv(dt,'HomePrice.csv')
