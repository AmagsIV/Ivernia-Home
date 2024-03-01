rm(list=ls())
options(error = traceback)
options(scipen=999)
options(warn=-1)
# Controls ####
BaseAdjustment=0.95
ReadInNewData=FALSE
ReadInRiskHandler=FALSE

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
library(base64enc)
library(jsonlite)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr) 
library(httr)
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
  print('Running DWH Queries')
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
  
  
  load('00_Claims_Extract_Raw.Rdata')
  print('Claims Data Loaded')
  # 
  load('00_Cover_Extract_Raw.Rdata')
  print('Cover Data Loaded')

  load('00_Policy_Extract_Raw.Rdata')
  print('Policy Data Loaded')
# 
  load('00_PolicyHolder_Extract_Raw.Rdata')
  print('PolicyHolder Data Loaded')
  
  load('00_QuoteDetails_Extract_Raw.Rdata')
  print('QuoteDetails Data Loaded')
  
  load('00_RiskDetails_Extract_Raw.Rdata')
  print('RiskDetails Data Loaded')
  # 
  # load('00_SpecifiedItems_Extract_Raw.Rdata')
  # print('SpecifiedItems Data Loaded')
  
}
# Risk Handler Data ####
if(ReadInRiskHandler==TRUE){
  # Import Source Files and libraries #
  source('P:/Underwriting/Pricing Pipelines/IV-Motor-Pricing-Engine2/000-Functions.R')
  # and/or install any packages required
  InstallAndLoadMissingPackages()
  
  ## Control ####
  fromDate <- "2024-01-01"
  toDate <- as.character(as.Date(Sys.Date()))
  
  ## API login and configuration #### 
  url <- "https://ivernia.riskhandler.com/api/products/1264e2f9-8a18-4b28-8d97-91b8836e3ae8/transactions?fromDate=2023-09-01&toDate=2023-11-17"
  
  # # Test Environment details
  # # Replace 'your_username' and 'your_password' with your actual credentials
  # username <- "48HRBX9861EFSIGNZPZ4FTZGVDY3Q7QX"
  # password <- "XQYBVGF8WYVN4ADRXHBIP31UCAXZQSLCXD47HHE4TNING5CKWS2FW8STOYP6PGSP"
  
  
  # Replace 'your_username' and 'your_password' with your actual credentials
  username <- "2K3L8RN7UPYU5L4YT1QVVNO2Z6LFKPX6"
  password <- "47P6T9FHKNCFY344B3ZRB5HDRZKEH3FPGP3PTT5RIBV3DW1C61MRXXJ5TDGEIENC"
  
  # Additional parameters
  
  page <- 1
  
  query_params <- list(
    fromDate = fromDate,
    toDate = toDate,
    page = page
  )
  
  
  # Construct Basic Authentication string
  auth_string <- paste(username, password, sep = ":")
  encoded_auth <- enc2utf8(base64encode(charToRaw(auth_string)))
  
  custom_headers <- c(
    "Authorization" = paste("Basic", encoded_auth),
    "Host"=c("iverniatest",
             "riskhandler",
             "com"),
    "User-Agent" = "PostmanRuntime/7.32.3",
    "Accept"="*/*",
    "Accept-Encoding"="gzip, deflate, br",
    "Connection"="keep-alive"
  )
  response <- GET(
    url,
    query = query_params,
    add_headers(
      custom_headers
      
    )
  )
  
  ## Initial API query for record details ####
  # check number of records
  records=fromJSON(response$headers$`x-pagination`)$TotalCount
  perPage=fromJSON(response$headers$`x-pagination`)$PageSize
  numberOfRequests=ceiling(records/perPage)
  
  ## Loop through API queries ####
  for(p in 1:numberOfRequests){
    query_params <- list(
      fromDate = fromDate,
      toDate = toDate,
      page = p
    )
    response <- GET(
      url,
      query = query_params,
      add_headers(
        custom_headers
        
      )
    )
    if(p==1){
      json_content <- content(response, "parsed")
    }else{
      json_content <- append(json_content,content(response, "parsed"))
    }
  }
  
  
  
  ## Deconstructing JSON files into DWH tables ####
  
  c=1
  sp=1
  ad=1
  ed=1
  e=1
  for(i in 1:length(json_content)){
    record=json_content[[i]]
    # record=unlist(record,recursive = FALSE)
    checklist=sapply(record,function(x){
      class(x)
    })
    
    # create policy table
    if(i ==1){
      PolicyTable=c(record[which(checklist!="list")],record$AccountsTransaction)
    }else{
      PolicyTable=rbind(PolicyTable,c(record[which(checklist!="list")],record$AccountsTransaction))
    }
    
    
    # Create Risk table
    if(i ==1){
      RiskTable=c(PolicyNumber=record$PolicyNumber,DateTransacted=record$DateTransacted,TransactionType=record$TransactionType,
                  unlist(record[which(checklist=="list" & names(record)=='RiskData')]$RiskData[[1]]$Properties))
      Tablenames=names(RiskTable)
      RiskTable=as.data.frame(t(RiskTable))
    }else{
      newRisk=c(PolicyNumber=record$PolicyNumber,DateTransacted=record$DateTransacted,TransactionType=record$TransactionType,
                unlist(record[which(checklist=="list" & names(record)=='RiskData')]$RiskData[[1]]$Properties))
      Tablenames=names(newRisk)
      newRisk=as.data.frame(t(newRisk))
      samenames=names(RiskTable)[which(names(RiskTable)%in%names(newRisk))]
      RiskTable=merge(RiskTable,newRisk
                      ,by=c(samenames),all.x=TRUE,all.y=TRUE)
    }
    # Create Claims table
    if(length(record[which(checklist=="list" & names(record)=="RiskData")]$RiskData)>1 ){
      # browser()
      temp=unlist(record[which(checklist=="list" & names(record)=="RiskData")]$RiskData)
      ind1=which(names(temp)=='TableName')
      ind2=which(temp=='CLAIMS')
      for(claims in 1:length(ind2)){
        claimsnode=which(ind1>=ind2[claims])[1]
        if(c==1){
          ClaimsTable=c(PolicyNumber=record$PolicyNumber,DateTransacted=record$DateTransacted,TransactionType=record$TransactionType,
                        unlist(record[which(checklist=="list" & names(record)=='RiskData')]$RiskData[[claimsnode]]$Properties))
          Tablenames=names(ClaimsTable)
          ClaimsTable=as.data.frame(t(ClaimsTable))
        }else{
          newRisk=c(PolicyNumber=record$PolicyNumber,DateTransacted=record$DateTransacted,TransactionType=record$TransactionType,
                    unlist(record[which(checklist=="list" & names(record)=='RiskData')]$RiskData[[claimsnode]]$Properties))
          Tablenames=names(newRisk)
          newRisk=as.data.frame(t(newRisk))
          samenames=names(ClaimsTable)[which(names(ClaimsTable)%in%names(newRisk))]
          ClaimsTable=merge(ClaimsTable,newRisk
                            ,by=c(samenames),all.x=TRUE,all.y=TRUE)
        }
        c=c+1
      }
      
    }
    # Specified Items table
    if(length(record[which(checklist=="list" & names(record)=="RiskData")]$RiskData)>1 ){
      # browser()
      temp=unlist(record[which(checklist=="list" & names(record)=="RiskData")]$RiskData)
      ind1=which(names(temp)=='TableName')
      ind2=which(temp=='SPECIFIED_ITEMS')
      for(claims in 1:length(ind2)){
        claimsnode=which(ind1>=ind2[claims])[1]
        if(sp==1){
          SpecifiedItems=c(PolicyNumber=record$PolicyNumber,DateTransacted=record$DateTransacted,TransactionType=record$TransactionType,
                           unlist(record[which(checklist=="list" & names(record)=='RiskData')]$RiskData[[claimsnode]]$Properties))
          Tablenames=names(SpecifiedItems)
          SpecifiedItems=as.data.frame(t(SpecifiedItems))
        }else{
          newRisk=c(PolicyNumber=record$PolicyNumber,DateTransacted=record$DateTransacted,TransactionType=record$TransactionType,
                    unlist(record[which(checklist=="list" & names(record)=='RiskData')]$RiskData[[claimsnode]]$Properties))
          Tablenames=names(newRisk)
          newRisk=as.data.frame(t(newRisk))
          samenames=names(SpecifiedItems)[which(names(SpecifiedItems)%in%names(newRisk))]
          SpecifiedItems=merge(SpecifiedItems,newRisk
                               ,by=c(samenames),all.x=TRUE,all.y=TRUE)
        }
        sp=sp+1
      }
      
    }
    # Additional Insurers table
    if(length(record[which(checklist=="list" & names(record)=="RiskData")]$RiskData)>1 ){
      # browser()
      temp=unlist(record[which(checklist=="list" & names(record)=="RiskData")]$RiskData)
      ind1=which(names(temp)=='TableName')
      ind2=which(temp=='ADDITIONAL_INSUREDS')
      if(length(ind2)!=0){
        for(claims in 1:length(ind2)){
          claimsnode=which(ind1>=ind2[claims])[1]
          if(ad==1){
            AddInsurers=c(PolicyNumber=record$PolicyNumber,DateTransacted=record$DateTransacted,TransactionType=record$TransactionType,
                          unlist(record[which(checklist=="list" & names(record)=='RiskData')]$RiskData[[claimsnode]]$Properties))
            Tablenames=names(AddInsurers)
            AddInsurers=as.data.frame(t(AddInsurers))
          }else{
            newRisk=c(PolicyNumber=record$PolicyNumber,DateTransacted=record$DateTransacted,TransactionType=record$TransactionType,
                      unlist(record[which(checklist=="list" & names(record)=='RiskData')]$RiskData[[claimsnode]]$Properties))
            Tablenames=names(newRisk)
            newRisk=as.data.frame(t(newRisk))
            samenames=names(AddInsurers)[which(names(AddInsurers)%in%names(newRisk))]
            AddInsurers=merge(AddInsurers,newRisk
                              ,by=c(samenames),all.x=TRUE,all.y=TRUE)
          }
          ad=ad+1
        }
      }
      
    }
    
  }
  RiskTable=RiskTable%>%mutate(Cover=dplyr::if_else(SI_Contents>0 & SI_Buildings >0,'Buildings and Contents',
                                                    dplyr::if_else(SI_Contents>0 & SI_Buildings<=0,'Contents Only',
                                                                   dplyr::if_else(SI_Contents<=0 & SI_Buildings >0,'Buildings Only',NA))))
  
  PolicyTable=as.data.frame(PolicyTable)

  PolicyTable_df <- as.data.frame(lapply(PolicyTable, unlist), stringsAsFactors = FALSE)
  saveRDS(list(RiskTable,PolicyTable,PolicyTable_df,ClaimsTable,AddInsurers), "RiskHanlderData.rds")
}else{
  RiskHandlerData <- readRDS("RiskHanlderData.rds")
  RiskTable=RiskHandlerData[[1]]
  PolicyTable=RiskHandlerData[[2]]
  PolicyTable_df=RiskHandlerData[[3]]
  ClaimsTable=RiskHandlerData[[4]]
  AddInsurers=RiskHandlerData[[5]]
  rm(RiskHandlerData)
}
print('Risk Handler Data imported')
# Read In Rates file ####
Area=read_excel('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Home Rates V9 Ivernia.xlsx',sheet='Area')

# Claims Data ####
ClaimsData=dt_claim%>%group_by(HomeRiskId)%>%arrange(Settled)%>%
  summarise(ClaimsCount=n(),
            ClaimsTotal=sum(HomeClaimAmount),
            OpenClaim=first(Settled)
            )%>%mutate(OpenClaim=dplyr::if_else(OpenClaim=='Yes',0,1))

# Risk Join to Cover ####
Risk_Cover=merge(x=dt_cover,y=dt_risk,by="HomeCoverId")
Risk_Cover=merge(x=Risk_Cover,y=ClaimsData,by="HomeRiskId",all.x = T)



# Sales ####
Sales=dt_quote%>%filter(`$ExtractDateTime`>=as.character('2024-01-01',format='%Y-%m-%d'))%>%
  filter(EventType%in%c('New Business Policy Accepted','Policy Cancelled','Policy Lapsed'
,'Permanently Adjusted Policy','Policy Renewal Accepted'))%>%
  mutate(QuoteDate=as.Date(`$ExtractDateTime`)-1)



# Quotes Transformation ####
Quotes=dt_quote%>%filter(`$ExtractDateTime`>=as.character('2024-01-01',format='%Y-%m-%d'))%>%
  filter(EventType!='New Business Policy Accepted')%>%
  mutate(QuoteDate=as.Date(`$ExtractDateTime`)-1)

Quotes=Quotes%>%#filter(`$ExtractDateTime`>=as.character('2024-01-24',format='%Y-%m-%d'))%>%
  group_by(HomeRiskId,InsuranceCompany)%>%
  arrange(NetPremium)%>%
  mutate(ID=row_number())%>%
  filter((ID==1 & InsuranceCompany=='Accredited Insurances Limited(RE9090)') | InsuranceCompany!='Accredited Insurances Limited(RE9090)')%>%
  ungroup()
  
gc()
Quotes=Quotes%>%
  mutate(DaysToIncept=as.Date(QuoteDate,format='%Y-%m-%d')-as.Date(CoverEffectiveFrom,format='%Y-%m-%d'),
         HomeRiskId=toupper(HomeRiskId))%>%
  group_by(HomeRiskId) %>%
  mutate(BusinessProcess = ifelse(any(grepl("Renewal Confirmation", BusinessProcess)), "Renewal Confirmation", BusinessProcess),
         InsurersQuoting=sum(dplyr::if_else(Outcome=='Premium Returned',1,0),na.rm=T),
         MinAltPrem=min(dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany != "Accredited Insurances Limited(RE9090)",NetPremium,9999),na.rm = T))%>%
  arrange(NetPremium)%>%
  mutate(Rank=row_number())%>%
  # filter(BusinessProcess=='New Business')%>%
  ungroup()


# Quotes_Risk=merge(x=Risk_Cover,y=Quotes,by="HomeRiskId")
# write_csv(Quotes_Risk,file='QuoteRisk.csv')


# Risk Data ####
LatestEffectiveness=Quotes%>%mutate(DatePeriod=dplyr::if_else(QuoteDate<as.character('2023-12-21',format='%Y-%m-%d'),'1-Pre-GoLive',
                                                              dplyr::if_else(QuoteDate<as.character('2024-01-02',format='%Y-%m-%d'),'2-Back Office Only',
                                                                             dplyr::if_else(QuoteDate<as.character('2024-01-16',format='%Y-%m-%d'),'3-BO & OL',
                                                                                            dplyr::if_else(QuoteDate<as.character('2024-01-13',format='%Y-%m-%d'),'4-GAP 1',
                                                                                                           dplyr::if_else(QuoteDate<as.character('2024-01-27',format='%Y-%m-%d'),'5-RateRed and Gap',
                                                                                                                          dplyr::if_else(QuoteDate<as.character('2024-01-31',format='%Y-%m-%d'),'6-Quoteability Changes',
                                                                                                                                         dplyr::if_else(QuoteDate<as.character('2024-02-03',format='%Y-%m-%d'),'7-QuoteArea Rate Change','8-Area Rate'))))))))%>%
  # filter(DatePeriod=='5-RateRed and Gap')%>%
  select(QuoteDate,QuoteId,HomeRiskId,BusinessProcess,EventType,SchemeCode,InsuranceCompany,CoverEffectiveFrom,Outcome,TotalPayable,NetPremium,Levy,MinAltPrem,Rank,InsurersQuoting)%>%
  mutate(HomeRiskId=toupper(HomeRiskId),
         IVRefDec=dplyr::if_else(Outcome%in%c("Refer - Submit To Insurer","Refer - Quote Only","Decline","Refer") & InsuranceCompany == "Accredited Insurances Limited(RE9090)",1,0),
         IVQuotes=dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany == "Accredited Insurances Limited(RE9090)",1,0),
         IVEffectiveQuotes=dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany == "Accredited Insurances Limited(RE9090)" & NetPremium<=MinAltPrem,1,0),
         IVNetPremium=dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany == "Accredited Insurances Limited(RE9090)",NetPremium,NA),
         AltNetPremium=dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany != "Accredited Insurances Limited(RE9090)",NetPremium,NA),
         AltBestNetPremium=dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany != "Accredited Insurances Limited(RE9090)" & MinAltPrem!=9999,MinAltPrem,NA),
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
             AltNetPremium=mean(AltNetPremium,na.rm = T),
             AltBestNetPremium=mean(AltBestNetPremium,na.rm = T),
            MinAltPrem=mean(MinAltPrem,na.rm = T),
            Rank=mean(dplyr::if_else(Outcome=='Premium Returned' & InsuranceCompany == "Accredited Insurances Limited(RE9090)",Rank,NA),na.rm = T),
            InsurersQuoting=mean(InsurersQuoting,na.rm = T))%>%
  filter(dplyr::if_else(InsurersQuoting==1 & BusinessProcess=='Renewal Confirmation',F,T))


LatestEffectiveness_Risk=merge(x=LatestEffectiveness,y=Risk_Cover,by="HomeRiskId",all.x = TRUE)
dt_polholder$PolicyId=trimws(toupper(dt_polholder$PolicyId))
dt_policy$PolicyId=trimws(toupper(dt_policy$PolicyId))
Polholder=merge(x=dt_polholder[which(dt_polholder$PrimaryPolicyHolder=='Yes'),],y=dt_policy[,c("RiskId","PolicyId")],by="PolicyId")
Polholder$HomeRiskId=Polholder$RiskId
Polholder$RiskId<-NULL

LatestEffectiveness_Risk$HomeRiskId=trimws(toupper(LatestEffectiveness_Risk$HomeRiskId))
Polholder$HomeRiskId=trimws(toupper(Polholder$HomeRiskId))
LatestEffectiveness_Risk=merge(x=LatestEffectiveness_Risk,y=Polholder,by="HomeRiskId")

LatestEffectiveness_Risk=LatestEffectiveness_Risk%>%mutate(QuoteID=paste0(as.character(CoverStartDate)
                                                                          ,as.character(round(as.numeric(BuildingSumInsuredRequired),0))
                                                                          ,as.character(round(as.numeric(ContentsSumInsuredRequired),0))
                                                                          ,as.character(EmploymentType)
                                                                          ,as.character(ClaimedYearsNCD)
                                                                          ,as.character(NumberofBedrooms)
                                                                          ,as.character(NumberofBathrooms)
                                                                          ,as.character(RiskAddressCounty)
                                                                          ,as.character(RoofConstructionType)
                                                                          ,as.character(BuildingAccidentalDamageRequired)
                                                                          ,as.character(ContentsAccidentalDamageRequired)
                                                                          ,as.character(as.Date(QuoteDate,format='%d/%m/%Y'))
                                                                          ,as.character(IVNetPremium)
                                                                          # ,PropertyType
                                                                          # ,as.character(round(as.numeric(SpecifiedItemsSumInsuredRequired),0))
                                                                          ))

LatestEffectiveness_Risk=LatestEffectiveness_Risk%>%group_by(QuoteID)%>%arrange(`$ExtractDateTime.x`)%>%mutate(QuoteIDrowNum=row_number(),count=n())%>%ungroup()#%>%filter(QuoteIDrowNum==1)
# RHPolicies=PolicyTable_df%>%group_by(PolicyNumber)%>%summarise(RHPremium=sum(as.numeric(TotalClientDebit)))
# RiskTableNew=merge(x=RiskTable,y=RHPolicies,by="PolicyNumber",all.x = TRUE)
# RiskTableNew=RiskTableNew%>%mutate(PropertyRoofType=gsub('100% ','',PropertyRoofType))%>%
#   filter(TransactionType=='NB')%>%mutate(QuoteID=paste0(as.character(as.Date(StartOfCover,format='%d/%m/%Y'))
#                                                                             ,as.character(SI_Buildings)
#                                                                             ,as.character(SI_Contents)
#                                                                             ,as.character(ProposerEmpStatus)
#                                                                             ,as.character(ClaimFreeYears)
#                                                                             ,as.character(PropertyBedrooms)
#                                                                             ,as.character(PropertyBathrooms)
#                                                                             ,CorrCounty
#                                                                             ,PropertyRoofType
#                                                                             ,PropertyADBuilding
#                                                                             ,PropertyADContents
#                                                                             ,as.character(as.Date(QuoteDate,format='%d/%m/%Y'))
#                                                                             # ,as.character(round(as.numeric(RHPremium),2))
#                                                         
#                                                                             # ,as.character(SpecifiedItems)
#                                                                             ))
# # LatestEffectiveness_Risk_J=LatestEffectiveness_Risk%>%select(HomeRiskId,BuildingSumInsuredRequired,QuoteID)



# RiskTableNew=RiskTableNew%>%select(PolicyNumber,QuoteID)
# RiskTableNew=merge(x=RiskTableNew,y=RHPolicies,by="PolicyNumber",all.x = TRUE)
# LatestEffectiveness_Risk_J=merge(x=LatestEffectiveness_Risk,y=RiskTableNew,by="QuoteID",all.x = TRUE)
# LatestEffectiveness_Risk_J=LatestEffectiveness_Risk_J%>%group_by(QuoteID)%>%mutate(QuoteIDrowNum=row_number(),count=n())%>%filter(!is.na(PolicyNumber))
# write_clip(LatestEffectiveness_Risk)

# Summary Effective ####
SummaryEffectiveness=LatestEffectiveness_Risk%>%  
  filter(EventType%in%c('Quotation Provided'))%>%
  filter(!BusinessProcess%in%c('Mid-term Cancellation','Permanent Mid Term Adjustment'))%>%
  # filter(!BusinessProcess%in%c('Renewal Confirmation') && InsurersQuoting!=1)%>%
  group_by(BusinessProcess,QuoteDate)%>%
  summarise(Volume=n(),
             IVQuotes=sum(IVQuotes,na.rm = T),
             IVRefDec=sum(IVRefDec,na.rm = T),
             Quoteability=IVQuotes/Volume,
             NoIverniaCall=(Volume-IVQuotes-IVRefDec),
             NoCallPerc=NoIverniaCall/Volume,
             IVEffectiveQuotes=sum(IVEffectiveQuotes,na.rm = T),
             Effectiveness=IVEffectiveQuotes/IVQuotes,
             IVNetPremium=mean(IVNetPremium,na.rm = T),
             AltNetPremium=mean(AltNetPremium,na.rm = T),
             AltBestNetPremium=mean(AltBestNetPremium,na.rm = T),
             MinAltPrem=mean(MinAltPrem,na.rm = T),
             Rank=mean(Rank,na.rm = T),
             InsurersQuoting=mean(InsurersQuoting,na.rm = T))%>%mutate(Weekday= format(QuoteDate, "%a"))%>%
  mutate(DatePeriod=dplyr::if_else(QuoteDate<as.character('2023-12-21',format='%Y-%m-%d'),'1-Pre-GoLive',
                                   dplyr::if_else(QuoteDate<as.character('2024-01-02',format='%Y-%m-%d'),'2-Back Office Only',
                                                  dplyr::if_else(QuoteDate<as.character('2024-01-16',format='%Y-%m-%d'),'3-BO & OL',
                                                                 dplyr::if_else(QuoteDate<as.character('2024-01-13',format='%Y-%m-%d'),'4-GAP 1',
                                                                                dplyr::if_else(QuoteDate<as.character('2024-01-27',format='%Y-%m-%d'),'5-RateRed and Gap',
                                                                                               dplyr::if_else(QuoteDate<as.character('2024-01-31',format='%Y-%m-%d'),'6-Quoteability Changes',
                                                                                                              dplyr::if_else(QuoteDate<as.character('2024-02-03',format='%Y-%m-%d'),'7-QuoteArea Rate Change','8-Area Rate'))))))))
write_clip(SummaryEffectiveness)


LatestEffectiveness_RiskNB=LatestEffectiveness_Risk%>%
  mutate(DatePeriod=dplyr::if_else(QuoteDate<as.character('2023-12-21',format='%Y-%m-%d'),'1-Pre-GoLive',
                                   dplyr::if_else(QuoteDate<as.character('2024-01-02',format='%Y-%m-%d'),'2-Back Office Only',
                                                  dplyr::if_else(QuoteDate<as.character('2024-01-16',format='%Y-%m-%d'),'3-BO & OL',
                                                                 dplyr::if_else(QuoteDate<as.character('2024-01-13',format='%Y-%m-%d'),'4-GAP 1',
                                                                                dplyr::if_else(QuoteDate<as.character('2024-01-27',format='%Y-%m-%d'),'5-RateRed and Gap',
                                                                                               dplyr::if_else(QuoteDate<as.character('2024-01-31',format='%Y-%m-%d'),'6-Quoteability Changes',
                                                                                                              dplyr::if_else(QuoteDate<as.character('2024-02-03',format='%Y-%m-%d'),'7-QuoteArea Rate Change','8-Area Rate'))))))))%>%
  filter(BusinessProcess=='New Business' & EventType=='Quotation Provided')%>%
  filter(DatePeriod%in%'8-Area Rate')
# %>%
#   filter(
#         as.numeric(BuildingSumInsuredRequired)<1000000 &
#         as.numeric(ContentsSumInsuredRequired)>=10000&
#         as.numeric(NumberofPayingGuests)==0 &
#         ResidenceType=='Owner Occupied' &
#         RoofConstructionType=='Standard' &
#         as.numeric(NumberofBedrooms<=5) &
#         as.numeric(NumberofBathrooms)<=3 &
#         as.numeric(RiskAddressMatchLevel)<=700
#   )
write_clip(LatestEffectiveness_RiskNB)

LatestEffectiveness_RiskRNL=LatestEffectiveness_Risk%>%
  mutate(DatePeriod=dplyr::if_else(QuoteDate<as.character('2023-12-21',format='%Y-%m-%d'),'1-Pre-GoLive',
                                   dplyr::if_else(QuoteDate<as.character('2024-01-02',format='%Y-%m-%d'),'2-Back Office Only',
                                                  dplyr::if_else(QuoteDate<as.character('2024-01-16',format='%Y-%m-%d'),'3-BO & OL',
                                                                 dplyr::if_else(QuoteDate<as.character('2024-01-13',format='%Y-%m-%d'),'4-GAP 1',
                                                                                dplyr::if_else(QuoteDate<as.character('2024-01-27',format='%Y-%m-%d'),'5-RateRed and Gap',
                                                                                               dplyr::if_else(QuoteDate<as.character('2024-01-31',format='%Y-%m-%d'),'6-Quoteability Changes',
                                                                                                              dplyr::if_else(QuoteDate<as.character('2024-02-03',format='%Y-%m-%d'),'7-QuoteArea Rate Change','8-Area Rate'))))))))%>%
  filter(BusinessProcess=='Renewal Confirmation' & EventType=='Quotation Provided')%>%
  filter(DatePeriod%in%c('8-Area Rate','7-QuoteArea Rate Change'))
  #        %>%
  # filter(
  #   as.numeric(BuildingSumInsuredRequired)<1000000 &
  #     as.numeric(ContentsSumInsuredRequired)>=10000&
  #     as.numeric(NumberofPayingGuests)==0 &
  #     ResidenceType=='Owner Occupied' &
  #     RoofConstructionType=='Standard' &
  #     as.numeric(NumberofBedrooms<=5) &
  #     as.numeric(NumberofBathrooms)<=3 &
  #     as.numeric(RiskAddressMatchLevel)<=700
  # )
write_clip(LatestEffectiveness_RiskRNL)

# rm(dt_cover,dt_policy,dt_risk,dt_quote)
gc()

# Factor Level View ####
columns=names(dt_risk)[which(!names(dt_risk)%in%c("$ExtractDateTime",'HomeRiskId','HomeCoverId','PreviousPolicyNumber','PostalAddressStreetName','PreviousPolicyEndDate','PostalAddressBuildingName','PostalAddressStreetName','PostalAddressTown','RiskAddressBuildingName','RiskAddressStreetName','RiskAddressGeoCode'))]
variables=c('BusinessProcess','IVQuotes','IVEffectiveQuotes','IVNetPremium','AltNetPremium','AltBestNetPremium','InsurersQuoting','Rank','ClaimsCount','ClaimsTotal','OpenClaim')
LatestEffectiveness_Risk_Long=LatestEffectiveness_RiskNB[,c(columns,variables)]
LatestEffectiveness_Risk_Long=LatestEffectiveness_Risk_Long%>%mutate_all(as.character)
LatestEffectiveness_Risk_Long=pivot_longer(LatestEffectiveness_Risk_Long,cols = c(columns),names_to = 'Variable',values_to = 'Value')
LatestEffectiveness_Risk_Long=LatestEffectiveness_Risk_Long%>%group_by(BusinessProcess,Variable,Value)%>%mutate(IVQuotes=as.numeric(IVQuotes),
                                                                                          IVEffectiveQuotes=as.numeric(IVEffectiveQuotes),
                                                                                          IVNetPremium=as.numeric(IVNetPremium),
                                                                                          AltNetPremium=as.numeric(AltNetPremium),
                                                                                          AltBestNetPremium=as.numeric(AltBestNetPremium),
                                                                                          Rank=as.numeric(Rank),
                                                                                          InsurersQuoting=as.numeric(InsurersQuoting))%>%
  summarise(TotalQuotes=dim(LatestEffectiveness_RiskNB)[1],
            # TotalQuotesRC=dim(LatestEffectiveness_Risk[which(LatestEffectiveness_Risk$BusinessProcess=='Renewal Confirmation'),])[1],
            Volume=n(),
            PortionOfQuotes=Volume/TotalQuotes,
            IVQuotes=sum(IVQuotes,na.rm = T),
            Quoteability=IVQuotes/Volume,
            IVEffectiveQuotes=sum(IVEffectiveQuotes,na.rm = T),
            Effectiveness=sum(IVEffectiveQuotes,na.rm = T)/IVQuotes,
            IVNetPremium=mean(IVNetPremium,na.rm = T),
            AltNetPremium=mean(AltNetPremium,na.rm = T),
            AltBestNetPremium=mean(AltBestNetPremium,na.rm = T)
            # Rank=mean(Rank,na.rm = T),
            # InsurersQuoting=mean(InsurersQuoting,na.rm = T)
            )%>%
  group_by(Variable)%>%#mutate(VariableVolume=sum(Volume),VariableVolumePercent=Volume/VariableVolume)%>%ungroup()%>%
  arrange(desc(Volume))%>%
  filter(PortionOfQuotes>0.01 & PortionOfQuotes!=1)%>%
  filter(!Variable%in%c('ConstructionDate','GarageSize','GarageSizeMeasurement','ResidenceDate',
                        'RiskAddressTown','RiskAddressPerilResponse','ProductType','PreviousPolicyInsurer',
                        'SocialWelfare'))%>%
  arrange(Variable,Quoteability,desc(Volume))
gc()
write_clip(LatestEffectiveness_Risk_Long)


# Quoteability Funnel ####
QuoteabilityFunnel=LatestEffectiveness_Risk%>%
  mutate(DatePeriod=dplyr::if_else(QuoteDate<as.character('2023-12-21',format='%Y-%m-%d'),'1-Pre-GoLive',
                                   dplyr::if_else(QuoteDate<as.character('2024-01-02',format='%Y-%m-%d'),'2-Back Office Only',
                                                  dplyr::if_else(QuoteDate<as.character('2024-01-16',format='%Y-%m-%d'),'3-BO & OL',
                                                                 dplyr::if_else(QuoteDate<as.character('2024-01-13',format='%Y-%m-%d'),'4-GAP 1',
                                                                                dplyr::if_else(QuoteDate<as.character('2024-01-27',format='%Y-%m-%d'),'5-RateRed and Gap',
                                                                                               dplyr::if_else(QuoteDate<as.character('2024-01-31',format='%Y-%m-%d'),'6-Quoteability Changes',
                                                                                                              dplyr::if_else(QuoteDate<as.character('2024-02-03',format='%Y-%m-%d'),'7-QuoteArea Rate Change','8-Area Rate'))))))))%>%
  filter(BusinessProcess%in%c('Renewal Confirmation','New Business'))%>%
  filter(DatePeriod%in%c('8-Area Rate'))%>%
  filter(dplyr::if_else(InsurersQuoting==1 & BusinessProcess=='Renewal Confirmation',1,0)!=1)%>%
  mutate(OpenClaim=dplyr::if_else(is.na(OpenClaim),0,OpenClaim),
         ClaimsCount=dplyr::if_else(is.na(ClaimsCount),0,ClaimsCount))%>%
  mutate(                                                   
        BuildingsSI=dplyr::if_else(as.numeric(BuildingSumInsuredRequired)<=1000000,1,0)
        ,ContentsSI=dplyr::if_else((as.numeric(ContentsSumInsuredRequired)>=10000 || ContentsSumInsuredRequired==0),1,0)
        ,PayingGuests=dplyr::if_else(as.numeric(NumberofPayingGuests)==0 ,1,0)
        ,ResidenceType=dplyr::if_else(ResidenceType=='Owner Occupied' ,1,0)
        ,RoofConstructionType=dplyr::if_else((RoofConstructionType=='Standard' || RoofNonStandardpercentage<=20) ,1,0)
        ,NumberofBedrooms_q=dplyr::if_else(as.numeric(NumberofBedrooms)<=5 ,1,0)
        ,NumberofBathrooms_q=dplyr::if_else(as.numeric(NumberofBathrooms)<=4 ,1,0)
        ,RiskAddressMatchLevel=dplyr::if_else(as.numeric(RiskAddressMatchLevel)<=700,1,0)
        ,NumberofSmokeAlarms_q=dplyr::if_else(as.numeric(NumberofSmokeAlarms)>0,1,0)
        ,GapInCover=dplyr::if_else(dplyr::if_else(PreviousPolicyEndDate=='0001-01-01',1,as.numeric(as.Date(CoverStartDate)-as.Date(PreviousPolicyEndDate,format='%Y-%m-%d')))>28,0,1)
        ,Listed=dplyr::if_else(ListedBuilding=='Yes',0,1)
        ,Basement=dplyr::if_else(HasBasement=='Yes',0,1)
        ,Boat=dplyr::if_else(as.numeric(SmallCraftSumInsuredRequired)>0,0,1)
        ,Caravan=dplyr::if_else(as.numeric(CaravanContentsSumInsuredRequired)>0,0,1)
        ,SISI=dplyr::if_else(as.numeric(SpecifiedItemsSumInsuredRequired)>(0.3*as.numeric(ContentsSumInsuredRequired)),0,1)
        ,Openclaims=dplyr::if_else(OpenClaim==1,0,1)
        ,ClaimsCount=dplyr::if_else(ClaimsCount>2,0,1)
        ,Plus20Insurers=dplyr::if_else(InsurersQuoting>=20,1,0)
        ,Plus16Insurers=dplyr::if_else(InsurersQuoting>=16,1,0)
        # 2 or more claims in last 3 years
        
        )%>%
  # filter(dplyr::if_else(SISI==1 & Caravan==1 & Boat==1 & Basement==1 & Listed==1 & GapInCover==1 & NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0)!=1 &
  #                    IVQuotes==1)%>%
  select(BusinessProcess,BuildingsSI,ContentsSI,PayingGuests,ResidenceType,RoofConstructionType,NumberofBedrooms_q,NumberofBathrooms_q,RiskAddressMatchLevel,NumberofSmokeAlarms_q,GapInCover,Listed,Basement,Boat,Caravan,SISI,count,IVQuotes
         ,Openclaims,ClaimsCount,Plus20Insurers,Plus16Insurers)%>%
  group_by(BusinessProcess)%>%summarise(Volume=n()
                         ,UniqueQuotes=round(Volume-(sum(dplyr::if_else(count>1,1/count,0))),2)
                         ,BuildingsSI_Quote=sum(dplyr::if_else(BuildingsSI==1,1,0),na.rm = T)/Volume
                         ,ContentsSI_Quote=sum(dplyr::if_else(BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,PayingGuests_Quote=sum(dplyr::if_else(PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,ResidenceType_Quote=sum(dplyr::if_else(ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,RoofConstructionType_q=sum( dplyr::if_else(RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,NumberofBedrooms_Quote=sum( dplyr::if_else(NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,NumberofBathrooms_Quote=sum( dplyr::if_else(NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,RiskAddressMatchLevel_Quote=sum( dplyr::if_else(RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,NumberofSmokeAlarms_Quote=sum( dplyr::if_else(NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,GapInCover_Quote=sum( dplyr::if_else(GapInCover==1 & NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,Listed_Quote=sum( dplyr::if_else(Listed==1 & GapInCover==1 & NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,Basement_Quote=sum( dplyr::if_else(Basement==1 & Listed==1 & GapInCover==1 & NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,Boat_Quote=sum( dplyr::if_else(Boat==1 & Basement==1 & Listed==1 & GapInCover==1 & NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,Caravan_Quote=sum( dplyr::if_else(Caravan==1 & Boat==1 & Basement==1 & Listed==1 & GapInCover==1 & NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,SISI_Quote=sum( dplyr::if_else(SISI==1 & Caravan==1 & Boat==1 & Basement==1 & Listed==1 & GapInCover==1 & NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,Openclaims_Quote=sum( dplyr::if_else(Openclaims==1 & SISI==1 & Caravan==1 & Boat==1 & Basement==1 & Listed==1 & GapInCover==1 & NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & RoofConstructionType==1 & NumberofBedrooms_q==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,ClaimsCount_Quote=sum( dplyr::if_else(ClaimsCount==1 & Openclaims==1 & SISI==1 & Caravan==1 & Boat==1 & Basement==1 & Listed==1 & GapInCover==1 & NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         
                         ,Plus16Insurers=sum( dplyr::if_else(Plus16Insurers==1 & ClaimsCount==1 & Openclaims==1 & SISI==1 & Caravan==1 & Boat==1 & Basement==1 & Listed==1 & GapInCover==1 & NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,Plus20Insurers=sum( dplyr::if_else(Plus20Insurers==1 &  ClaimsCount==1 & Openclaims==1 & SISI==1 & Caravan==1 & Boat==1 & Basement==1 & Listed==1 & GapInCover==1 & NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,Quoteability=sum( dplyr::if_else(IVQuotes==1 & ClaimsCount==1 & Openclaims==1 & SISI==1 & Caravan==1 & Boat==1 & Basement==1 & Listed==1 & GapInCover==1 & NumberofSmokeAlarms_q==1 & RiskAddressMatchLevel==1 & NumberofBathrooms_q==1 & NumberofBedrooms_q==1 & RoofConstructionType==1 & ResidenceType==1& PayingGuests==1 & BuildingsSI==1 & ContentsSI==1,1,0),na.rm = T)/Volume
                         ,ActualQuoteability=sum(IVQuotes==1,na.rm = T)/Volume
                         )
write_clip(QuoteabilityFunnel)


## Quoteability Funnel Long ####

QuoteabilityFunnelLong=LatestEffectiveness_Risk%>%
  mutate(DatePeriod=dplyr::if_else(QuoteDate<as.character('2023-12-21',format='%Y-%m-%d'),'1-Pre-GoLive',
                                   dplyr::if_else(QuoteDate<as.character('2024-01-02',format='%Y-%m-%d'),'2-Back Office Only',
                                                  dplyr::if_else(QuoteDate<as.character('2024-01-16',format='%Y-%m-%d'),'3-BO & OL',
                                                                 dplyr::if_else(QuoteDate<as.character('2024-01-13',format='%Y-%m-%d'),'4-GAP 1',
                                                                                dplyr::if_else(QuoteDate<as.character('2024-01-27',format='%Y-%m-%d'),'5-RateRed and Gap',
                                                                                               dplyr::if_else(QuoteDate<as.character('2024-01-31',format='%Y-%m-%d'),'6-Quoteability Changes',
                                                                                                              dplyr::if_else(QuoteDate<as.character('2024-02-03',format='%Y-%m-%d'),'7-QuoteArea Rate Change','8-Area Rate'))))))))%>%
  filter(BusinessProcess%in%c('Renewal Confirmation'))%>%#,'New Business'))%>%
  filter(DatePeriod%in%c('8-Area Rate'))%>%
  filter(dplyr::if_else(InsurersQuoting==1 & BusinessProcess=='Renewal Confirmation',1,0)!=1)%>%
  mutate(OpenClaim=dplyr::if_else(is.na(OpenClaim),0,OpenClaim),
         ClaimsCount=dplyr::if_else(is.na(ClaimsCount),0,ClaimsCount))%>%
  mutate(                                                   
    BuildingsSI=dplyr::if_else(as.numeric(BuildingSumInsuredRequired)<=1000000,1,0)
    ,ContentsSI=dplyr::if_else((as.numeric(ContentsSumInsuredRequired)>=10000 || ContentsSumInsuredRequired==0),1,0)
    ,PayingGuests=dplyr::if_else(as.numeric(NumberofPayingGuests)==0 ,1,0)
    ,ResidenceType=dplyr::if_else(ResidenceType=='Owner Occupied' ,1,0)
    ,RoofConstructionType_q=dplyr::if_else((RoofConstructionType=='Standard' || RoofNonStandardpercentage<=20) ,1,0)
    ,NumberofBedrooms_q=dplyr::if_else(as.numeric(NumberofBedrooms)<=5 ,1,0)
    ,NumberofBathrooms_q=dplyr::if_else(as.numeric(NumberofBathrooms)<=4 ,1,0)
    ,RiskAddressMatchLevel_q=dplyr::if_else(as.numeric(RiskAddressMatchLevel)<=700,1,0)
    ,NumberofSmokeAlarms_q=dplyr::if_else(as.numeric(NumberofSmokeAlarms)>0,1,0)
    ,GapInCover=dplyr::if_else(dplyr::if_else(PreviousPolicyEndDate=='0001-01-01',1,as.numeric(as.Date(CoverStartDate)-as.Date(PreviousPolicyEndDate,format='%Y-%m-%d')))>28,0,1)
    ,Listed=dplyr::if_else(ListedBuilding=='Yes',0,1)
    ,Basement=dplyr::if_else(HasBasement=='Yes',0,1)
    ,Boat=dplyr::if_else(as.numeric(SmallCraftSumInsuredRequired)>0,0,1)
    ,Caravan=dplyr::if_else(as.numeric(CaravanContentsSumInsuredRequired)>0,0,1)
    ,SISI=dplyr::if_else(as.numeric(SpecifiedItemsSumInsuredRequired)>(0.3*as.numeric(ContentsSumInsuredRequired)),0,1)
    ,Openclaims=dplyr::if_else(OpenClaim==1,0,1)
    ,ClaimsCount=dplyr::if_else(ClaimsCount>2,0,1)
    ,Plus20Insurers=dplyr::if_else(InsurersQuoting>=20,1,0)
    ,Plus16Insurers=dplyr::if_else(InsurersQuoting>=16,1,0)
    # 2 or more claims in last 3 years
    
  )%>%
  filter(dplyr::if_else(
    ClaimsCount==1 &
    Openclaims==1 &
      SISI==1 & 
      Caravan==1 & 
      Boat==1 & 
      Basement==1 & 
      Listed==1 & 
      GapInCover==1 & 
      NumberofSmokeAlarms_q==1 & 
      RiskAddressMatchLevel_q==1 & 
      NumberofBathrooms_q==1 & 
      RoofConstructionType_q==1 & 
      NumberofBedrooms_q==1 & 
      ResidenceType==1 & 
      PayingGuests==1 & 
      BuildingsSI==1 & 
      ContentsSI==1
    ,1,0)==1 & IVQuotes==0)

columns=names(dt_risk)[which(!names(dt_risk)%in%c("$ExtractDateTime",'HomeRiskId','HomeCoverId','PreviousPolicyNumber','PostalAddressStreetName','PreviousPolicyEndDate','PostalAddressBuildingName','PostalAddressStreetName','PostalAddressTown','RiskAddressBuildingName','RiskAddressStreetName','RiskAddressGeoCode'))]
variables=c('BusinessProcess','IVQuotes','IVEffectiveQuotes','IVNetPremium','AltNetPremium','AltBestNetPremium','InsurersQuoting','Rank','ClaimsCount','ClaimsTotal','OpenClaim')
LatestEffectiveness_Risk_Long=QuoteabilityFunnelLong[,c(columns,variables)]
LatestEffectiveness_Risk_Long=LatestEffectiveness_Risk_Long%>%mutate_all(as.character)
LatestEffectiveness_Risk_Long=pivot_longer(LatestEffectiveness_Risk_Long,cols = c(columns),names_to = 'Variable',values_to = 'Value')
LatestEffectiveness_Risk_Long=LatestEffectiveness_Risk_Long%>%group_by(BusinessProcess,Variable,Value)%>%mutate(IVQuotes=as.numeric(IVQuotes),
                                                                                                                IVEffectiveQuotes=as.numeric(IVEffectiveQuotes),
                                                                                                                IVNetPremium=as.numeric(IVNetPremium),
                                                                                                                AltNetPremium=as.numeric(AltNetPremium),
                                                                                                                AltBestNetPremium=as.numeric(AltBestNetPremium),
                                                                                                                Rank=as.numeric(Rank),
                                                                                                                InsurersQuoting=as.numeric(InsurersQuoting))%>%
  summarise(TotalQuotes=dim(QuoteabilityFunnelLong)[1],
            # TotalQuotesRC=dim(LatestEffectiveness_Risk[which(LatestEffectiveness_Risk$BusinessProcess=='Renewal Confirmation'),])[1],
            Volume=n(),
            PortionOfQuotes=Volume/TotalQuotes,
            IVQuotes=sum(IVQuotes,na.rm = T),
            Quoteability=IVQuotes/Volume,
            IVEffectiveQuotes=sum(IVEffectiveQuotes,na.rm = T),
            Effectiveness=sum(IVEffectiveQuotes,na.rm = T)/IVQuotes,
            IVNetPremium=mean(IVNetPremium,na.rm = T),
            AltNetPremium=mean(AltNetPremium,na.rm = T),
            AltBestNetPremium=mean(AltBestNetPremium,na.rm = T)
            # Rank=mean(Rank,na.rm = T),
            # InsurersQuoting=mean(InsurersQuoting,na.rm = T)
  )%>%
  group_by(Variable)%>%#mutate(VariableVolume=sum(Volume),VariableVolumePercent=Volume/VariableVolume)%>%ungroup()%>%
  arrange(desc(Volume))%>%
  # filter(PortionOfQuotes>0.015 & PortionOfQuotes<0.95)%>%
  filter(!Variable%in%c('ConstructionDate','GarageSize','GarageSizeMeasurement','ResidenceDate',
                        'RiskAddressTown','RiskAddressPerilResponse','ProductType','PreviousPolicyInsurer',
                        'SocialWelfare','BuildingSizeMeasurement','AlarmInstalledBy','BuildingSize'))%>%
  arrange(Variable,Quoteability,desc(Volume))
gc()
write_clip(LatestEffectiveness_Risk_Long)
  
# Buying behaviour ####
BuyingBehaviour=LatestEffectiveness_Risk%>%mutate(BusinessProcess=as.character(BusinessProcess))%>%
  filter(BusinessProcess%in%c('New Business','Renewal Confirmation'))%>%
  mutate(BuyingBehaviour=as.Date(CoverStartDate,format('%Y-%m-%d'))-as.Date(QuoteDate,format('%Y-%m-%d')))%>%
  mutate(BuyingBehaviour = case_when(
  (BuyingBehaviour) <= 0 ~ "01.0",
  (BuyingBehaviour) > 0 & BuyingBehaviour <= 2 ~ "02.1-2",
  (BuyingBehaviour) > 3 & BuyingBehaviour <= 4 ~ "03.3-4",
  (BuyingBehaviour) > 4 & BuyingBehaviour <= 6 ~ "04.5-6",
  (BuyingBehaviour) > 6 & BuyingBehaviour <= 10 ~ "05.7-10",
  (BuyingBehaviour) > 10 & BuyingBehaviour <= 15 ~ "06.11-15",
  (BuyingBehaviour) > 15 & BuyingBehaviour <= 20 ~ "07.16-20",
  (BuyingBehaviour) > 20 & BuyingBehaviour <= 25 ~ "08.21-25",
  (BuyingBehaviour) > 25 & BuyingBehaviour <= 30 ~ "09.26-30",
  (BuyingBehaviour) > 30 & BuyingBehaviour <= 35 ~ "10.31-35",
  (BuyingBehaviour) > 35 ~ "11.greater than 35",
  TRUE ~ "12.Other" # This line handles values outside the specified bins, such as negative values or NA
))%>%group_by(BusinessProcess,BuyingBehaviour)%>%summarise(Volume=n())%>%
  group_by(BusinessProcess)%>%
  mutate(PercentVolume=Volume/sum(Volume),Cumsum=cumsum(PercentVolume))

BuyingBehaviourPlot=ggplotly(ggplot(BuyingBehaviour) +
  aes(
    x = BuyingBehaviour,
    y = PercentVolume,
    fill = BusinessProcess,
    group = BusinessProcess,
  ) +
  geom_col(position = "dodge") +
  scale_fill_hue(direction = 1) +

  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
BuyingBehaviourPlot
