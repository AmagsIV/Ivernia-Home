rm(list=ls())
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

FLDR_MDL <- "P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Model"
FL_MDL <- "Model1"
# source('000 - Optimisation_Functions.R')

FLDR_OUT <- paste0("P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Model",FL_MDL)
mdl <- readRDS(file=file.path(FLDR_MDL,paste0(FL_MDL,".bin")))

MAX_FACTOR_MOVE <- 0.01
Segments=10
noOfChr=15
iter=10
TargetEffectiveness=0.12


# Grouping based on average market premium
# Grouping into pods ####
# MaxAddOn=1
predictionssplit=as.data.frame(dt$avr_prem)%>%arrange(dt$avr_prem)
# predictionssplit=rbind(predictionssplit,MaxAddOn)
names(predictionssplit)='AveragePrem'

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

# mdl$coefficients[1] <- 1000



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
    labs(title = "Train Test Performance") +
    theme_minimal() +
    xlim(max(plotdata$Iteration), 1)
  print(ggp)
  # browser()
  return(evals)
}

Optim_eval_function<-function(chromosome,Data,TargetEffectiveness=0.08,SummaryOutput=FALSE){
  # browser()

  dt$IVAPrice=(exp(predict(mdl,type="response",newdata = dt))+50)*1.05
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
                                             ,1,0)
                 )
  

  
  SummaryGroups=dt%>%filter(IverniaQuote==1)%>%group_by(Group)%>%summarise(Volume=n(),
                                                 AlternativeQuotes=mean(qcount,na.rm = T),
                                                 AlternativeMinPrem=mean(min_prem,na.rm = T),
                                                 AlternativeAvgPrem=mean(avr_prem,na.rm = T),
                                                 AvgIverniaPremium=mean(IVAPrice,na.rm = T),
                                                 IverniaCheapest=mean(dplyr::if_else(IVAPrice<=min_prem,1,0),na.rm = T),
                                                 BuildingsADPortion=mean(dplyr::if_else(BuildingAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                                 BuildingSI=mean(BuildingSumInsuredRequired,na.rm = T),
                                                 ContentsADPortion=mean(dplyr::if_else(ContentsAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                                 ContentsSI=mean(ContentsSumInsuredRequired,na.rm = T),
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
                                AlternativeQuotes=mean(qcount,na.rm = T),
                                AlternativeMinPrem=mean(min_prem,na.rm = T),
                                AlternativeAvgPrem=mean(avr_prem,na.rm = T),
                                AvgIverniaPremium=mean(IVAPrice,na.rm = T),
                                IverniaCheapest=mean(dplyr::if_else(IVAPrice<=min_prem,1,0),na.rm = T),
                                BuildingsADPortion=mean(dplyr::if_else(BuildingAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                BuildingSI=mean(BuildingSumInsuredRequired,na.rm = T),
                                ContentsADPortion=mean(dplyr::if_else(ContentsAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                ContentsSI=mean(ContentsSumInsuredRequired,na.rm = T),
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
  # Base=chromosome[1]
  mdl[["coefficients"]][["BuildingAccidentalDamageRequiredYes"]]=0.03
  mdl[["coefficients"]][["ContentsAccidentalDamageRequiredYes"]]=0.07
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
  mdl[["coefficients"]][["(Intercept)"]]=mdl[["coefficients"]][["(Intercept)"]]*chromosome[15]

  
  
  # dt$IVAPrice=exp(predict(mdl,type="response",newdata = dt))*Base
  
  BuildingsAndContentsMin=150
  BuildingsMin=125
  ContentsMin=80
  dt$IVAPrice=(exp(predict(mdl,type="response",newdata = dt))+50)
  dt=dt%>%mutate(IVAPrice=dplyr::if_else(cover_type=='B+C' & IVAPrice<(BuildingsAndContentsMin+50),(BuildingsAndContentsMin+50),
                                         dplyr::if_else(cover_type=='B' & IVAPrice<(BuildingsMin+50*1.05),(BuildingsMin+50),
                                                        dplyr::if_else(cover_type=='C' & IVAPrice<(ContentsMin+50),(ContentsMin+50),IVAPrice)
                                         )))
  
  SummaryGroups=dt%>%filter(IverniaQuote==1)%>%group_by(Group)%>%summarise(Volume=n(),
                                                 AlternativeQuotes=mean(qcount,na.rm = T),
                                                 AlternativeMinPrem=mean(min_prem,na.rm = T),
                                                 AlternativeAvgPrem=mean(avr_prem,na.rm = T),
                                                 AvgIverniaPremium=mean(IVAPrice,na.rm = T),
                                                 IverniaCheapest=mean(dplyr::if_else(IVAPrice<=min_prem,1,0),na.rm = T),
                                                 BuildingsADPortion=mean(dplyr::if_else(BuildingAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                                 BuildingSI=mean(BuildingSumInsuredRequired,na.rm = T),
                                                 ContentsADPortion=mean(dplyr::if_else(ContentsAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                                 ContentsSI=mean(ContentsSumInsuredRequired,na.rm = T),
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
                                AlternativeQuotes=mean(qcount,na.rm = T),
                                AlternativeMinPrem=mean(min_prem,na.rm = T),
                                AlternativeAvgPrem=mean(avr_prem,na.rm = T),
                                AvgIverniaPremium=mean(IVAPrice,na.rm = T),
                                IverniaCheapest=mean(dplyr::if_else(IVAPrice<=min_prem,1,0),na.rm = T),
                                BuildingsADPortion=mean(dplyr::if_else(BuildingAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                BuildingSI=mean(BuildingSumInsuredRequired,na.rm = T),
                                ContentsADPortion=mean(dplyr::if_else(ContentsAccidentalDamageRequired=='Yes',1,0),na.rm = T),
                                ContentsSI=mean(ContentsSumInsuredRequired,na.rm = T),
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
  SummaryGroups=SummaryGroups%>%mutate(Penalty=abs(as.numeric(Group)-5.5))
  SummaryGroups$Cheapest=abs((abs(SummaryGroups$IverniaCheapest-
                        SummaryOverall$IverniaCheapest)/SummaryOverall$IverniaCheapest))
  SummaryGroups=SummaryGroups%>%mutate(Penalty2=Cheapest/Penalty)

  # Objective ####
  Penalty=sum(abs(SummaryGroups$Penalty2))
  if(is.na(Penalty)){Penalty=10000}
  RMSEOfDistrib=abs(sum(abs(SummaryGroups$IverniaCheapest-
                           SummaryOverall$IverniaCheapest)/SummaryOverall$IverniaCheapest))
  if(is.nan(RMSEOfDistrib)){RMSEOfDistrib=10000}
  RMSEOfPrice=abs(sum(abs(SummaryGroups$AvgIverniaPremium-SummaryOverall$AlternativeAvgPrem)/SummaryOverall$AlternativeAvgPrem))
  if(is.nan(RMSEOfPrice)){RMSEOfPrice=10000}
  Effectiveness=abs(TargetEffectiveness*100-SummaryOverall$IverniaCheapest*100)/(TargetEffectiveness*100)
  Objective=Effectiveness*10+RMSEOfDistrib*0+RMSEOfPrice*3+Penalty

    
  print(paste0('Effectiveness:',as.character(SummaryOverall$IverniaCheapest),' RMSEOfDistrib:',
               as.character(RMSEOfDistrib),' RMSEOfPrice:',
               as.character(RMSEOfPrice),' Obj:',Objective))
  if(SummaryOutput==FALSE){
    return(Objective)
  }else{
    output=list()
    output$summary=summary
    output$Price=dt$IVAPrice
    return(output)
  }
}

# Chromosome Builder ####
set.seed(0)
CoverTypeContents=matrix(runif(noOfChr,0.3,1.5))
BuildingsAdj=matrix(runif(noOfChr,0.01,5))
contentsAdj=matrix(runif(noOfChr,0.01,5))
SpecItemsAdj=matrix(runif(noOfChr,0.01,5))
NumBedAdj=matrix(runif(noOfChr,0.3,2))
NumBathAdj=matrix(runif(noOfChr,0.3,2))
AgeC1hAdj=matrix(runif(noOfChr,0.8,2))
AgeC2hAdj=matrix(runif(noOfChr,0.8,2))
cover_typeBC=matrix(runif(noOfChr,0.3,2))
cover_typeB=matrix(runif(noOfChr,0.3,2))
ClaimedYearsNCD=matrix(runif(noOfChr,0.3,2))
OccupiedDuringDateYes=matrix(runif(noOfChr,0.7,2))
PropertyAge=matrix(runif(noOfChr,0.8,2))
PropertyAgeSqrd=matrix(runif(noOfChr,0.8,2))
Intercept=matrix(runif(noOfChr,0.95,1.05))

chromosomeInput=t(cbind(CoverTypeContents,SpecItemsAdj,BuildingsAdj,contentsAdj,NumBedAdj,NumBathAdj,
                        AgeC1hAdj,AgeC2hAdj,cover_typeBC,
                        cover_typeB,PropertyAge,OccupiedDuringDateYes,
                        PropertyAge,PropertyAgeSqrd,Intercept))
evals=matrix(0,2)

# optimise rates ####
GAmodelCloseReg <- rbga(stringMin=c(0.3,0.05,0.1,0.1,0.8,0.8,0.8,0.8,0.4,0.4,0.4,0.7,0.8,0.8,0.95), stringMax=c(1.1,5,5,5,2,2,2,2,2,2,2,2,2,2,1.05),
                        suggestions=t(chromosomeInput), iters=iter,popSize=noOfChr+1,monitorFunc = monitor,
                        mutationChance = 1/round((c(dim(chromosomeInput)[1]))*0.5,0),showSettings=FALSE,
                        evalFunc=function(chromosomeInput) Optim_eval_function(chromosome=chromosomeInput,Data=dt,TargetEffectiveness=TargetEffectiveness,SummaryOutput=FALSE)
                        ,verbose = TRUE)

bestSolutionReg<-GAmodelCloseReg$population[which.min(GAmodelCloseReg$evaluations),]
names(bestSolutionReg)=c('num_ph','SpecifiedItemsSumInsuredRequired','BuildingSumInsuredRequired',
                         'ContentsSumInsuredRequired',
                         'NumberofBedrooms','NumberofBathrooms',
                        'PHAge','I(PHAge^2)','cover_typeB+C',
                        'cover_typeC','ClaimedYearsNCD','OccupiedDuringDateYes','PropertyAge','I(PropertyAge^2)','Intercept')

# bestSolutionReg[c(15)]=c(1.0047)

Output=Optim_eval_function(chromosome=bestSolutionReg,Data=dt,TargetEffectiveness=TargetEffectiveness,SummaryOutput=TRUE)
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

write_clip(Output$summary)
bestSolutionReg=data.frame(bestSolutionReg)
