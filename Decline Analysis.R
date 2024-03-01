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
options(tidyverse.quiet = TRUE)
load('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/02-CleanData.RDS')

# Review Acceptance Rules ####
############
# Decline Max Bedrooms greater than	6
# Decline non-owner occupied	
# Decline any listed property	
# Decline non- standard construction type	
# Decline non-standard roof material	
# Decline flat roof percentage greater than	20
# Decline previous claims greater than	2
# Decline risks with non-settled claims	
# Decline non-ROI addresses	
# Decline total sports equipment  greater than	2500
# Decline pedal bike value greater than	1000
# Decline pedal bike number greater than	2
# Decline caravan cover (structure or contents) greater than	0
# Decline RiskAddressMatchLevel greater than 	700
# Decline Buildings Sum Insured greater than	1000000
# Decline Contents Sum Insured greater than	100000
# Decline specified items total greater than % of contents sum insured	30%
# Decline specified item individually greater than % of contents sum insured	10%
# Decline working at home if any access to public	
# Decline property age greater than	90
#########################

dt=dt%>%mutate(
              TargetSegment=dplyr::if_else(BuildingSumInsuredRequired>=150000 & BuildingSumInsuredRequired<=450000 &
                                             ContentsSumInsuredRequired>=0 & ContentsSumInsuredRequired<=75000 &
                                             ClaimedYearsNCD>=3 &
                                             NumberofBedrooms>=2 & NumberofBedrooms<=5 &
                                             NumberofBathrooms>=1 & NumberofBathrooms<=3,1,0),
              DeclineMaxBedroom=dplyr::if_else(NumberofBedrooms>6,1,0),
              DeclineOwnerShip=dplyr::if_else(ResidenceType=='Owner Occupied',0,1),
              DeclineListed=dplyr::if_else(ListedBuilding!='No',1,0),
              DeclineNSConstruction=0,
              DeclineNSRoof=dplyr::if_else(RoofConstructionType!='Standard',1,0),
              DeclineFlatRoof=dplyr::if_else(RoofNonStandardpercentage>20,1,0),
              DeclineClaims=dplyr::if_else(clm_count>2,1,0),
              DeclineClaimsUnsettled=0,
              DeclineNonROI=0,
              DeclineSportsEquipment=0,
              DeclinePedalBikeValue=0,
              DeclinePedalBikeCount=0,
              DeclineCaravan=dplyr::if_else(!PropertyType%in%c("Semi Detached House","Bungalow","Terraced House","Detached House","Flat"),1,0),
              DeclineAddressMatch=dplyr::if_else(RiskAddressMatchLevel>700,1,0),
              DeclineBuildings=dplyr::if_else(BuildingSumInsuredRequired>1000000,1,0),
              DeclineContents=dplyr::if_else(ContentsSumInsuredRequired>100000,1,0),
              DeclineSpecified=dplyr::if_else(SpecifiedItemsSumInsuredRequired>ContentsSumInsuredRequired*0.3,1,0),
              DeclineSpecifiedSingle=0,
              DeclineWorkingAtHomeAccesPublic=0,
              DeclinePropertyAge=dplyr::if_else(PropertyAge>90,1,0),
              DeclineOverall=DeclineMaxBedroom+
                              DeclineOwnerShip+
                              DeclineListed+
                              DeclineNSConstruction,DeclineNSRoof+
                              DeclineFlatRoof+
                              DeclineClaims+
                              DeclineClaimsUnsettled+
                              DeclineNonROI+
                              DeclineSportsEquipment+
                              DeclinePedalBikeValue+
                              DeclinePedalBikeCount+
                              DeclineCaravan+
                              DeclineAddressMatch+
                              DeclineBuildings+
                              DeclineContents+
                              DeclineSpecified+
                              DeclineSpecifiedSingle+
                              DeclineWorkingAtHomeAccesPublic+
                              DeclinePropertyAge
              )
dt_agg=dt%>%
  select(TargetSegment,
                   DeclineMaxBedroom,
                   DeclineOwnerShip,
                   DeclineListed,
                   DeclineNSConstruction,DeclineNSRoof,
                   DeclineFlatRoof,
                   DeclineClaims,
                   DeclineClaimsUnsettled,
                   DeclineNonROI,
                   DeclineSportsEquipment,
                   DeclinePedalBikeValue,
                   DeclinePedalBikeCount,
                   DeclineCaravan,
                   DeclineAddressMatch,
                   DeclineBuildings,
                   DeclineContents,
                   DeclineSpecified,
                   DeclineSpecifiedSingle,
                   DeclineWorkingAtHomeAccesPublic,
                   DeclinePropertyAge,
                   DeclineOverall)%>%
  group_by()%>%
  summarise(Volume=n(),
            TargetSegment=sum(TargetSegment)/Volume,
             DeclineMaxBedroom=sum(DeclineMaxBedroom)/Volume,
             DeclineOwnerShip=sum(DeclineOwnerShip)/Volume,
             DeclineListed=sum(DeclineListed)/Volume,
             DeclineNSConstruction,DeclineNSRoof=sum(DeclineNSConstruction)/Volume,
             DeclineFlatRoof=sum(DeclineFlatRoof)/Volume,
             DeclineClaims=sum(DeclineClaims)/Volume,
             DeclineClaimsUnsettled=sum(DeclineClaimsUnsettled)/Volume,
             DeclineNonROI=sum(DeclineNonROI)/Volume,
             DeclineSportsEquipment=sum(DeclineSportsEquipment)/Volume,
             DeclinePedalBikeValue=sum(DeclinePedalBikeValue)/Volume,
             DeclinePedalBikeCount=sum(DeclinePedalBikeCount)/Volume,
             DeclineCaravan=sum(DeclineCaravan)/Volume,
             DeclineAddressMatch=sum(DeclineAddressMatch)/Volume,
             DeclineBuildings=sum(DeclineBuildings)/Volume,
             DeclineContents=sum(DeclineContents)/Volume,
             DeclineSpecified=sum(DeclineSpecified)/Volume,
             DeclineSpecifiedSingle=sum(DeclineSpecifiedSingle)/Volume,
             DeclineWorkingAtHomeAccesPublic=sum(DeclineWorkingAtHomeAccesPublic)/Volume,
             DeclinePropertyAge=sum(DeclinePropertyAge)/Volume,
             DeclineOverall=sum(DeclineOverall)/Volume)
dt_agg=data.frame(DeclinePercent=t(dt_agg[1,]))
