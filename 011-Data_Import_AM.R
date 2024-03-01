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

FLDR_IN <- "P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Home Quote Data/July Refresh/"
FL_CLAIM <- "MI2155 - EXT_Home_QS_ClaimDetails 20230711.csv"
FL_COVER <- "MI2155 - EXT_Home_QS_CoverDetails 20230711.csv"
FL_INTERESTEDPARTY <- "MI2155 - EXT_Home_QS_InterestedPartyDetails 20230711.csv"
FL_POLICY <- "MI2155 - EXT_Home_QS_PolicyDetails 20230711.csv"
FL_POLHOLDER <- "MI2155 - EXT_Home_QS_PolicyHolderDetails 20230717.csv"
FL_QUOTE <- "MI2155 - EXT_Home_QS_QuoteDetails 20230717.csv"
FL_RISK <- "MI2155 - EXT_Home_QS_RiskDetails 20230714.csv"
FL_SPECITEMS <- "MI2155 - EXT_Home_QS_SpecifiedItemsDetails 20230714.csv"


dt_claim <- fread(file.path(FLDR_IN,FL_CLAIM),sep=",",data.table=FALSE)
dt_claim_count <- aggregate(dt_claim$HomeRiskId,by=list(HomeRiskId=dt_claim$HomeRiskId),FUN=function(x) length(x))
colnames(dt_claim_count)[colnames(dt_claim_count)=="x"] <- "clm_count"
rm(dt_claim)

dt_cover <- fread(file.path(FLDR_IN,FL_COVER),sep=",",data.table=FALSE)
to_keep <- c("HomeCoverId","CoverStartDate","CoverEndDate","BuildingSumInsuredRequired","BuildingAccidentalDamageRequired",
             "ContentsSumInsuredRequired","ContentsAccidentalDamageRequired","SpecifiedItemsSumInsuredRequired")
dt_cover <- dt_cover[,to_keep]

#dt_interestedparty <- fread(file.path(FLDR_IN,FL_INTERESTEDPARTY),sep=",",data.table=FALSE)
dt_policy <- fread(file.path(FLDR_IN,FL_POLICY),sep=",",data.table=FALSE)
dt_policy$PolicyId <- toupper(dt_policy$PolicyId)
dt_policy$HomeRiskId <- toupper(dt_policy$RiskId)
to_keep <- c("PolicyId","HomeRiskId")
dt_policy <- dt_policy[,to_keep]

dt_polholder <- fread(file.path(FLDR_IN,FL_POLHOLDER),sep=",",data.table=FALSE)
dt_polholder_count <- aggregate(dt_polholder$PolicyHolderId,by=list(PolicyId=dt_polholder$PolicyId),FUN=function(x) length(x))
colnames(dt_polholder_count)[colnames(dt_polholder_count)=="x"] <- "num_ph"
dt_polholder$IsSmokerFlag <- 0
dt_polholder$IsSmokerFlag[dt_polholder$IsSmoker=="Yes"] <- 1
dt_polholder_smoker <- aggregate(dt_polholder$IsSmokerFlag,by=list(PolicyId=dt_polholder$PolicyId),FUN=function(x) max(x))
colnames(dt_polholder_smoker)[colnames(dt_polholder_smoker)=="x"] <- "smoker_present"
#TODO - consider youngest PH, consider highest risk occ
dt_polholder <- merge(x=dt_polholder,y=dt_polholder_count,by="PolicyId")
dt_polholder <- merge(x=dt_polholder,y=dt_polholder_smoker,by="PolicyId")
dt_polholder <- merge(x=dt_polholder,y=dt_policy,by="PolicyId")
rm(dt_policy,dt_polholder_count,dt_polholder_smoker)
dt_polholder <- dt_polholder[dt_polholder$PrimaryPolicyHolder=="Yes",]
to_keep <- c("FirstTimeBuyer","BirthDate","MaritalStatus","OccupationType","EmploymentType",
             "num_ph","smoker_present","HomeRiskId")
dt_polholder <- dt_polholder[,to_keep]

dt_quote <- fread(file.path(FLDR_IN,FL_QUOTE),sep=",",data.table=FALSE)
to_drop <- c("DataSource","$ExtractDateTime","SchemeCode","InsuranceCompany","CoverEffectiveFrom",
             "CoverEffectiveTo","SchemeOutcome","TotalPayable","QuoteReference","OverrideAmount",
             "OverrideProvider","OverrideAuthCode","Levy","Commission")
dt_quote <- dt_quote[,!(names(dt_quote)%in%to_drop)]
gc()
dt_quote <- dt_quote[dt_quote$BusinessProcess=="New Business",]
dt_quote <- dt_quote[dt_quote$EventType=="Quotation Provided",]
to_drop <- c("QuoteId","BusinessProcess","EventType")
dt_quote <- dt_quote[,!(names(dt_quote)%in%to_drop)]
gc()
dt_quote <- dt_quote[dt_quote$Outcome=="Premium Returned",]
# dt_quote <- dt_quote[dt_quote$NetPremium>0,]
dt_quote_min <- aggregate(dt_quote$NetPremium,by=list(HomeRiskId=dt_quote$HomeRiskId),FUN=function(x) min(x))
dt_quote_max <- aggregate(dt_quote$NetPremium,by=list(HomeRiskId=dt_quote$HomeRiskId),FUN=function(x) max(x))
dt_quote_mean <- aggregate(dt_quote$NetPremium,by=list(HomeRiskId=dt_quote$HomeRiskId),FUN=function(x) mean(x))
dt_quote_count <- aggregate(dt_quote$NetPremium,by=list(HomeRiskId=dt_quote$HomeRiskId),FUN=function(x) length(x))
colnames(dt_quote_min)[colnames(dt_quote_min)=="x"] <- "min_prem"
dt_quote_max <- aggregate(dt_quote$NetPremium,by=list(HomeRiskId=dt_quote$HomeRiskId),FUN=function(x) max(x))
colnames(dt_quote_max)[colnames(dt_quote_max)=="x"] <- "max_prem"
dt_quote_mean <- aggregate(dt_quote$NetPremium,by=list(HomeRiskId=dt_quote$HomeRiskId),FUN=function(x) mean(x))
colnames(dt_quote_mean)[colnames(dt_quote_mean)=="x"] <- "avr_prem"
dt_quote_count <- aggregate(dt_quote$NetPremium,by=list(HomeRiskId=dt_quote$HomeRiskId),FUN=function(x) length(x))
colnames(dt_quote_count)[colnames(dt_quote_count)=="x"] <- "qcount"
dt_quote <- merge(x=dt_quote_min,y=dt_quote_max)
dt_quote <- merge(x=dt_quote,y=dt_quote_mean)
dt_quote <- merge(x=dt_quote,y=dt_quote_count)
rm(dt_quote_min,dt_quote_max,dt_quote_mean,dt_quote_count)
gc()

dt_risk <- fread(file.path(FLDR_IN,FL_RISK),sep=",",data.table=FALSE)
to_drop <- c("DataSource","$ExtractDateTime","ProductType","PreviousPolicyInsurer","PreviousPolicyNumber",
             "PreviousPolicyEndDate","PostalAddressBuildingName","PostalAddressStreetName","PostalAddressCity","PostalAddressCountry",         
             "RiskAddressBuildingName","RiskAddressStreetName","RiskAddressGeoCode","PropertySubType","BuildingSize",
             "BuildingSizeMeasurement","GarageSize","GarageSizeMeasurement","LockAdditionalDetails","AlarmInstalledBy")
dt_risk<- dt_risk[,!(names(dt_risk)%in%to_drop)]

dt_specitems <- fread(file.path(FLDR_IN,FL_SPECITEMS),sep=",",data.table=FALSE)

dt <- merge(x=dt_cover,y=dt_risk,by="HomeCoverId")
dt <- merge(x=dt,y=dt_quote,by="HomeRiskId")
dt <- merge(x=dt,y=dt_polholder,by="HomeRiskId")
dt <- merge(x=dt,y=dt_claim_count,by="HomeRiskId",all.x=TRUE)
dt$clm_count[is.na(dt$clm_count)] <- 0

# rm(FLDR_IN,FL_CLAIM,FL_COVER,FL_INTERESTEDPARTY,FL_POLHOLDER,FL_POLICY,FL_QUOTE,FL_RISK,FL_SPECITEMS)
# # rm(dt_cover,dt_quote,dt_risk,dt_polholder,dt_claim_count)
# rm(to_drop,to_keep)
# gc()
# save(dt,file='P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/00-ImportedData.RDS')