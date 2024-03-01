rm(list=ls())
options(error = traceback)
options(scipen=999)
options(warn=-1)
# Controls ####
BaseAdjustment=0.95
ReadInNewData=TRUE
AdjustPremium=TRUE

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

# StartDate=Sys.Date()-day(Sys.Date())+1-months(3)


if(ReadInNewData==TRUE){
  # SQL #### 
  ## SQL Paramaters and login info #####
  dbServer <- "IV-SQL-02\\IVERNIAREPORTING"
  dbName <- "OP"
  dbSchema<-"OP"
  conn_str = paste("driver={ODBC Driver 17 for SQL Server};server=", dbServer, ";trusted_connection=yes;Integrated Security=SSPI;", sep="")
  
  # Run Queries
  Query_claims=paste0("SELECT *  FROM [DataFramework].[ODS].[EXT_Home_QS_ClaimDetails]")
  dt_claim=run_query(conn_str,Query_claims)
  save(dt_claim,file='00_Claims_Extract_Raw.Rdata')
  print('Claims Data Loaded from SQL')
  
  Query_Cover=paste0("SELECT *  FROM [DataFramework].[ODS].[EXT_Home_QS_CoverDetails]")
  dt_cover=run_query(conn_str,Query_Cover)
  save(dt_cover,file='00_Cover_Extract_Raw.Rdata')
  print('Cover Data Loaded from SQL')
  
  Query_Policy=paste0("SELECT *  FROM [DataFramework].[ODS].[EXT_Home_QS_PolicyDetails]")
  dt_policy=run_query(conn_str,Query_Policy)
  save(dt_policy,file='00_Policy_Extract_Raw.Rdata')
  print('Policy Data Loaded from SQL')
  
  Query_PolicyHolder=paste0("SELECT *  FROM [DataFramework].[ODS].[EXT_Home_QS_PolicyHolderDetails] ")
  dt_polholder=run_query(conn_str,Query_PolicyHolder)
  save(dt_polholder,file='00_PolicyHolder_Extract_Raw.Rdata')
  print('PolicyHolder Data Loaded from SQL')
  
  Query_QuoteDetails=paste0("SELECT *  FROM [DataFramework].[ODS].[EXT_Home_QS_QuoteDetails]")
  dt_quote=run_query(conn_str,Query_QuoteDetails)
  # dt_quote=read.csv('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Home Quote Data/Aug Refresh/Home_QS_QuoteDetails_20230816.txt')
  save(dt_quote,file='00_QuoteDetails_Extract_Raw.Rdata')
  print('QuoteDetails Data Loaded from SQL')
  
  Query_RiskDetails=paste0("SELECT *  FROM [DataFramework].[ODS].[EXT_Home_QS_RiskDetails]")
  dt_risk=run_query(conn_str,Query_RiskDetails)
  save(dt_risk,file='00_RiskDetails_Extract_Raw.Rdata')
  print('RiskDetails Data Loaded from SQL')
  
  Query_RiskDetails=paste0("SELECT *  FROM [DataFramework].[ODS].[EXT_Home_QS_SpecifiedItemsDetails]")
  dt_specitems=run_query(conn_str,Query_RiskDetails)
  save(dt_specitems,file='00_SpecifiedItems_Extract_Raw.Rdata')
  print('SpecifiedItems Data Loaded from SQL')
  
  
}else{
  # FLDR_IN <- "P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Home Quote Data/July Refresh/"
  # FL_CLAIM <- "MI2155 - EXT_Home_QS_ClaimDetails 20230711.csv"
  # dt_claim <- fread(file.path(FLDR_IN,FL_CLAIM),sep=",",data.table=FALSE)
  # FL_COVER <- "MI2155 - EXT_Home_QS_CoverDetails 20230711.csv"
  # dt_cover <- fread(file.path(FLDR_IN,FL_COVER),sep=",",data.table=FALSE)
  # FL_INTERESTEDPARTY <- "MI2155 - EXT_Home_QS_InterestedPartyDetails 20230711.csv"
  # FL_POLICY <- "MI2155 - EXT_Home_QS_PolicyDetails 20230711.csv"
  # dt_policy <- fread(file.path(FLDR_IN,FL_POLICY),sep=",",data.table=FALSE)
  # FL_POLHOLDER <- "MI2155 - EXT_Home_QS_PolicyHolderDetails 20230717.csv"
  # dt_polholder <- fread(file.path(FLDR_IN,FL_POLHOLDER),sep=",",data.table=FALSE)
  # FL_QUOTE <- "MI2155 - EXT_Home_QS_QuoteDetails 20230717.csv"
  # dt_quote <- fread(file.path(FLDR_IN,FL_QUOTE),sep=",",data.table=FALSE)
  # FL_RISK <- "MI2155 - EXT_Home_QS_RiskDetails 20230714.csv"
  # dt_risk <- fread(file.path(FLDR_IN,FL_RISK),sep=",",data.table=FALSE)
  # FL_SPECITEMS <- "MI2155 - EXT_Home_QS_SpecifiedItemsDetails 20230714.csv"
  # dt_specitems <- fread(file.path(FLDR_IN,FL_SPECITEMS),sep=",",data.table=FALSE)
  
  
  # load('00_Claims_Extract_Raw.Rdata')
  # print('Claims Data Loaded')
  # 
  load('00_Cover_Extract_Raw.Rdata')
  print('Cover Data Loaded')

  load('00_Policy_Extract_Raw.Rdata')
  print('Policy Data Loaded')
# 
#   load('00_PolicyHolder_Extract_Raw.Rdata')
#   print('PolicyHolder Data Loaded')
  
  load('00_QuoteDetails_Extract_Raw.Rdata')
  print('QuoteDetails Data Loaded')
  
  load('00_RiskDetails_Extract_Raw.Rdata')
  print('RiskDetails Data Loaded')
  # 
  # load('00_SpecifiedItems_Extract_Raw.Rdata')
  # print('SpecifiedItems Data Loaded')
  
}

# Read In Rates file ####
Area=read_excel('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Home Rates V9 Ivernia.xlsx',sheet='Area')

# Read in Rate Adjustment file ####
RateAdjustment=read_excel('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Home MI/Rate Adjustment.xlsx',sheet='Sheet1')
RateAdjustment=RateAdjustment%>%filter(Adjustment!='')
# Risk Join to Cover ####
Risk_Cover=merge(x=dt_cover,y=dt_risk,by="HomeCoverId")


# Quotes Transformation ####
Quotes=dt_quote%>%filter(`$ExtractDateTime`>=as.character('2024-01-02',format='%Y-%m-%d'))%>%
  filter(EventType!='New Business Policy Accepted')%>%
  mutate(QuoteDate=as.Date(`$ExtractDateTime`))


Quotes=merge(x=Risk_Cover,y=Quotes,by="HomeRiskId")

# Adjust Quotes Rates ####
if(AdjustPremium==TRUE){
  for(f in 1:dim(RateAdjustment)[1]){
    QuotesAffected=which(Quotes[,RateAdjustment$Variable[f]]==RateAdjustment$Value[f] & Quotes[,"InsuranceCompany"]=='Accredited Insurances Limited(RE9090)' & Quotes[,"Outcome"]=='Premium Returned')
    Quotes$NetPremium[QuotesAffected]=Quotes$NetPremium[QuotesAffected]*RateAdjustment$Adjustment
  }
}

Quotes=Quotes%>%filter(`$ExtractDateTime`>=as.character('2024-01-24',format='%Y-%m-%d'))%>%
  group_by(HomeRiskId,InsuranceCompany)%>%
  arrange(NetPremium)%>%
  mutate(ID=row_number())%>%
  filter((ID==1 & InsuranceCompany=='Accredited Insurances Limited(RE9090)') | InsuranceCompany!='Accredited Insurances Limited(RE9090)')%>%
  ungroup()
  
gc()
Quotes=Quotes%>%
  mutate(DaysToIncept=as.Date(`$ExtractDateTime`,format='%Y-%m-%d')-as.Date(CoverEffectiveFrom,format='%Y-%m-%d'),
         HomeRiskId=toupper(HomeRiskId))%>%
  group_by(HomeRiskId) %>%
  mutate(BusinessProcess = ifelse(any(grepl("Renewal Confirmation", BusinessProcess)), "Renewal Confirmation", BusinessProcess),
         InsurersQuoting=sum(dplyr::if_else(Outcome=='Premium Returned',1,0),na.rm=T),
         MinPrem=min(dplyr::if_else(Outcome=='Premium Returned',NetPremium,NA),na.rm = T))%>%# & InsuranceCompany != "Accredited Insurances Limited(RE9090)"
  arrange(NetPremium)%>%
  mutate(Rank=row_number())%>%
  filter(BusinessProcess=='New Business')%>%
  ungroup()


# Quotes_Risk=merge(x=Risk_Cover,y=Quotes,by="HomeRiskId")
# write_csv(Quotes_Risk,file='QuoteRisk.csv')

# check quote
# 007E87B5-DEB9-EE11-811E-005056896A35


# Latest Effectiveness Data ####
LatestEffectiveness=Quotes%>%mutate(DatePeriod=dplyr::if_else(`$ExtractDateTime`<as.character('2023-12-21',format='%Y-%m-%d'),'1-Pre-GoLive',
                                                              dplyr::if_else(`$ExtractDateTime`<as.character('2024-01-02',format='%Y-%m-%d'),'2-Back Office Only',
                                                                             dplyr::if_else(`$ExtractDateTime`<as.character('2024-01-16',format='%Y-%m-%d'),'3-BO & OL',
                                                                                            dplyr::if_else(`$ExtractDateTime`<as.character('2024-01-13',format='%Y-%m-%d'),'4-GAP 1','5-RateRed and Gap')))))%>%
  filter(DatePeriod=='5-RateRed and Gap')%>%
  select(QuoteDate,QuoteId,HomeRiskId,BusinessProcess,EventType,SchemeCode,InsuranceCompany,CoverEffectiveFrom,Outcome,TotalPayable,NetPremium,Levy,MinPrem,Rank,InsurersQuoting)%>%
  mutate(HomeRiskId=toupper(HomeRiskId),
         IVRefDec=dplyr::if_else(Outcome%in%c("Refer - Submit To Insurer","Refer - Quote Only","Decline","Refer") & InsuranceCompany == "Accredited Insurances Limited(RE9090)",1,0),
         IVQuotes=dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany == "Accredited Insurances Limited(RE9090)",1,0),
         IVEffectiveQuotes=dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany == "Accredited Insurances Limited(RE9090)" & NetPremium<=MinPrem,1,0),
         IVNetPremium=dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany == "Accredited Insurances Limited(RE9090)",NetPremium,NA),
         IVNetPremiumWin=dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany == "Accredited Insurances Limited(RE9090)" & NetPremium<=MinPrem,NetPremium,NA),
         AltNetPremium=dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany != "Accredited Insurances Limited(RE9090)",NetPremium,NA),
         AltBestNetPremium=dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany != "Accredited Insurances Limited(RE9090)",MinPrem,NA),
         IVOutcome=dplyr::if_else(InsuranceCompany == "Accredited Insurances Limited(RE9090)",Outcome,NA)
         )%>%
  arrange(HomeRiskId,IVOutcome)%>%
  # filter(EventType!='New Business Policy Accepted')%>%
  group_by(HomeRiskId,BusinessProcess,EventType,QuoteDate)%>%
  summarise(IVOutcome=first(IVOutcome),
            IVQuotes=sum(IVQuotes,na.rm = T),
            IVRefDec=sum(IVRefDec,na.rm = T),
             IVEffectiveQuotes=sum(IVEffectiveQuotes,na.rm = T),
             IVNetPremium=mean(IVNetPremium,na.rm = T),
            IVNetPremiumWin=mean(IVNetPremiumWin,na.rm = T),
             AltNetPremium=mean(AltNetPremium,na.rm = T),
             AltBestNetPremium=mean(AltBestNetPremium,na.rm = T),
            MinPrem=mean(dplyr::if_else(is.na(MinPrem),IVNetPremium,MinPrem),na.rm = T),
            
            Rank=mean(dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany == "Accredited Insurances Limited(RE9090)",Rank,NA),na.rm = T),
            InsurersQuoting=mean(InsurersQuoting,na.rm = T))%>%
  mutate(PercentGapToBest=(IVNetPremium-MinPrem)/IVNetPremium)


LatestEffectiveness_Risk=merge(x=Risk_Cover,y=LatestEffectiveness,by="HomeRiskId")





# Converting to Long Format ####

columns=names(dt_risk)[which(!names(dt_risk)%in%c("$ExtractDateTime",'HomeRiskId','HomeCoverId','PreviousPolicyNumber','PostalAddressStreetName','PreviousPolicyEndDate','PostalAddressBuildingName','PostalAddressStreetName','PostalAddressTown','RiskAddressBuildingName','RiskAddressStreetName','RiskAddressGeoCode'))]
variables=c('BusinessProcess','IVQuotes','IVEffectiveQuotes','IVNetPremium','AltNetPremium','AltBestNetPremium','InsurersQuoting','Rank','MinPrem','IVNetPremiumWin')
LatestEffectiveness_Risk_Long=LatestEffectiveness_Risk[,c(columns,variables)]
LatestEffectiveness_Risk_Long=LatestEffectiveness_Risk_Long%>%mutate_all(as.character)
LatestEffectiveness_Risk_Long=pivot_longer(LatestEffectiveness_Risk_Long,cols = c(columns),names_to = 'Variable',values_to = 'Value')
LatestEffectiveness_Risk_Long=LatestEffectiveness_Risk_Long%>%group_by(BusinessProcess,Variable,Value)%>%mutate(IVQuotes=as.numeric(IVQuotes),
                                                                                          IVEffectiveQuotes=as.numeric(IVEffectiveQuotes),
                                                                                          IVNetPremium=as.numeric(IVNetPremium),
                                                                                          IVNetPremiumWin=as.numeric(IVNetPremiumWin),
                                                                                          AltNetPremium=as.numeric(AltNetPremium),
                                                                                          AltBestNetPremium=as.numeric(AltBestNetPremium),
                                                                                          MinPrem=as.numeric(MinPrem),
                                                                                          Rank=as.numeric(Rank),
                                                                                          InsurersQuoting=as.numeric(InsurersQuoting))%>%
  summarise(TotalQuotes=dim(LatestEffectiveness_Risk)[1],
            # TotalQuotesRC=dim(LatestEffectiveness_Risk[which(LatestEffectiveness_Risk$BusinessProcess=='Renewal Confirmation'),])[1],
            Volume=n(),
            PortionOfQuotes=Volume/TotalQuotes,
            IVQuotes=sum(IVQuotes,na.rm = T),
            Quoteability=IVQuotes/Volume,
            IVEffectiveQuotes=sum(IVEffectiveQuotes,na.rm = T),
            Effectiveness=sum(IVEffectiveQuotes,na.rm = T)/IVQuotes,
            IVNetPremium=mean(IVNetPremium,na.rm = T),
            AltNetPremium=mean(AltNetPremium,na.rm = T),
            AltBestNetPremium=mean(AltBestNetPremium,na.rm = T),
            MinPrem=mean(MinPrem,na.rm = T),
            PercentGapToBest=(IVNetPremium-MinPrem)/IVNetPremium,
            Rank=mean(Rank,na.rm = T),
            InsurersQuoting=mean(InsurersQuoting,na.rm = T))%>%group_by(Variable)%>%mutate(VariableVolume=sum(Volume),VariableVolumePercent=Volume/VariableVolume)%>%ungroup()%>%
  # filter(!is.na(Value) & Volume!=dim(LatestEffectiveness_Risk)[1])%>%
  arrange(desc(Volume))%>%
  # filter(Variable!='PreviousPolicyInsurer')%>%
  # filter(Volume>50)%>%
  arrange(Variable,Quoteability,desc(Volume))
gc()
LatestEffectiveness_Risk_Long$VariableVolumePercent<-NULL
LatestEffectiveness_Risk_Long$PercentGapToBest <-NULL
LatestEffectiveness_Risk_Long$VariableVolume <-NULL
LatestEffectiveness_Risk_Long$InsurersQuoting<-NULL 
LatestEffectiveness_Risk_Long$MinPrem <-NULL


write_clip(LatestEffectiveness_Risk_Long)

# Summary ####
SummaryEffectiveness=LatestEffectiveness_Risk%>%
  group_by(BusinessProcess)%>%
  summarise(Volume=n(),
            IVQuotes=sum(IVQuotes,na.rm = T),
            IVRefDec=sum(IVRefDec,na.rm = T),
            Quoteability=IVQuotes/Volume,
            NoIverniaCall=(Volume-IVQuotes-IVRefDec),
            NoCallPerc=NoIverniaCall/Volume,
            IVEffectiveQuotes=sum(IVEffectiveQuotes,na.rm = T),
            Effectiveness=IVEffectiveQuotes/IVQuotes,
            IVNetPremium=mean(IVNetPremium,na.rm = T),
            IVNetPremiumWin=mean(IVNetPremiumWin,na.rm = T),
            AltNetPremium=mean(AltNetPremium,na.rm = T),
            AltBestNetPremium=mean(AltBestNetPremium,na.rm = T),
            Rank=mean(Rank,na.rm = T),
            InsurersQuoting=mean(InsurersQuoting,na.rm = T))
write_clip(SummaryEffectiveness)
