rm(list=ls())
# Load Libraries ####
library(httr)
library(base64enc)
library(jsonlite)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr) 
# Import Source Files and libraries #
source('P:/Underwriting/Pricing Pipelines/IV-Motor-Pricing-Engine2/000-Functions.R')
# and/or install any packages required
InstallAndLoadMissingPackages()

# Control ####
fromDate <- "2024-01-01"
toDate <-as.character(as.Date(Sys.Date()))



# # Test Environment details
# # Replace 'your_username' and 'your_password' with your actual credentials
# username <- "48HRBX9861EFSIGNZPZ4FTZGVDY3Q7QX"
# password <- "XQYBVGF8WYVN4ADRXHBIP31UCAXZQSLCXD47HHE4TNING5CKWS2FW8STOYP6PGSP"
# # API login and configuration #### 
# url <- "https://iverniatest.riskhandler.com/api/products/1264e2f9-8a18-4b28-8d97-91b8836e3ae8/transactions?fromDate=2023-09-01&toDate=2023-11-17"
# Prod Environment details
# Replace 'your_username' and 'your_password' with your actual credentials
username <- "2K3L8RN7UPYU5L4YT1QVVNO2Z6LFKPX6"
password <- "47P6T9FHKNCFY344B3ZRB5HDRZKEH3FPGP3PTT5RIBV3DW1C61MRXXJ5TDGEIENC"
# API login and configuration ####
url <- paste0("https://ivernia.riskhandler.com/api/products/1264e2f9-8a18-4b28-8d97-91b8836e3ae8/transactions?fromDate=",fromDate,"&toDate=",toDate)

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

# Initial API query for record details ####
# check number of records
records=fromJSON(response$headers$`x-pagination`)$TotalCount
perPage=fromJSON(response$headers$`x-pagination`)$PageSize
numberOfRequests=ceiling(records/perPage)

# Loop through API queries ####
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



# Deconstructing JSON files into DWH tables ####

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

# if(RiskTable$SI_Contents[p]>0 & SI_Buildings[p] >0){'Buildings and Contents'}else if(
#   RiskTable$SI_Contents[p]>0 & RiskTable$SI_Buildings[p]<=0){'Contents Only'}else if(
#     RiskTable$SI_Contents[p]<=0 & RiskTable$SI_Buildings[p] >0){'Buildings Only'}else{NA}
PolicyTable=as.data.frame(PolicyTable)
# ClaimsTable=as.data.frame(ClaimsTable)
# SpecifiedItems=as.data.frame(SpecifiedItems)
# AddInsurers=as.data.frame(AddInsurers)

# variables=ls()
# variables=variables[-which(variables%in%c('PolicyTable','ClaimsTable','SpecifiedItems','AddInsurers','RiskTable'))]
# eqn=paste0('rm(',variables,',eqn,variables)')
# eval(parse(text = eqn))

# Bordereaux generation ####
## Policy Bordereaux ####
### Read in Template Policy Bordereaux ####
PolicyBordereauxSample=read_xlsx('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/AIEL- Premium Accounts and Claims Bordereaux Specs-UK HH-Ivernia.xlsx',
                                 sheet='Policy Bordereaux Spec', skip = 1)
PolicyBordereauxSample=PolicyBordereauxSample%>%filter(!is.na(`Field Name`))

PolicyBordereaux=matrix(0,nrow=1,ncol = length(PolicyBordereauxSample$`Field Name`))
PolicyBordereaux=as.data.frame(PolicyBordereaux)
names(PolicyBordereaux)=PolicyBordereauxSample$`Field Name`

### Risk Handler Adjustments ####
# PolicyTable$GrossExLevy=as.numeric(PolicyTable$GrossExLevy)
PolicyTable$Levy=as.numeric(PolicyTable$Levy)+1.5
PolicyTable=PolicyTable%>%mutate(MgaFee=dplyr::if_else(as.numeric(MgaFee)!=0,as.numeric(MgaFee)-1.5,as.numeric(MgaFee)))
### Template implementation ####
for(p in 1:dim(PolicyTable)[1]){

  for(f in 1:length(PolicyBordereauxSample$`Field Name`)){
    # Default Values
    if(!is.na(PolicyBordereauxSample$Default[f])){
      if(is.na(PolicyBordereauxSample$Map[f])){
        PolicyBordereaux[p,f]=PolicyBordereauxSample[f,'Default']
      }
    }else if(!is.na(PolicyBordereauxSample$Map[f]) & PolicyBordereauxSample$Map[f]!='Calculation' & !is.na(PolicyBordereauxSample$Table[f])){
    # Mapped Values
      eqn=paste0('Value=',PolicyBordereauxSample$Table[f],'$',PolicyBordereauxSample$Map[f],'[',p,']')
      eval(parse(text = eqn))
      PolicyBordereaux[p,f]=Value
    }else if(!is.na(PolicyBordereauxSample$Table[f]) & PolicyBordereauxSample$Table[f]=='Calculation'){
      # Calculated Values
      eqn=paste0('Value=',paste0('paste0',gsub('\\[p\\]',paste0('[',p,']'),PolicyBordereauxSample$Calculation[f])))
      eval(parse(text = eqn))
      PolicyBordereaux[p,f]=Value
    }
    
      
    # PolicyBordereaux[p,f]=
      
  }
}
PolicyBordereaux=PolicyBordereaux%>%mutate(
  `Effective Date`=as.Date(`Effective Date`),
  `Term Start Date`=as.Date(`Term Start Date`,format='%d/%m/%Y'),
  `Term Expiry Date`=as.Date(`Term Expiry Date`,format='%d/%m/%Y')
)%>%filter(`Transaction Type`!='BLT'# & `Policy Number`=='IVH/24/10014'
           )%>%
  # Financial fields
  mutate(
    `IPT €`=round(GWP*as.numeric(`IPT %`)/100,2)+1.5,
    `GWP plus Admin Fee`=round(GWP+`Admin Fee`,2),
    `Broker Acquisition Cost €`=round(GWP*as.numeric(`Broker Acquisition Cost %`)/100,2),
    `Net Premium`=round(GWP-`Broker Acquisition Cost €`,2),
    `MGA Acquisition Cost €`=round(GWP*as.numeric(`MGA Acquisition Cost %`)/100,2),
    `Net Net Premium`=round(`Net Premium`-`MGA Acquisition Cost €`,2),
    `JBA Rating`=dplyr::if_else(is.na(`JBA Rating`),"0",`JBA Rating`)
  )%>%
  # Other fields
  mutate(
    `Transaction Type Code`=dplyr::if_else(`Transaction Type`=='NB','NB',
                                           dplyr::if_else(`Transaction Type`=='CAN','RP',
                                                          dplyr::if_else(`Transaction Type`=='MTA' & GWP>=0,'AP',
                                                                         dplyr::if_else(`Transaction Type`=='MTA' & GWP<0,'RP',`Transaction Type`)))),
    `Current Policy Status`=dplyr::if_else(as.Date(`Term Expiry Date`)<=as.Date(toDate) &
                                             !`Transaction Type`%in%c('CAN','NTU'),'Lapsed',
                                           dplyr::if_else(as.Date(`Term Expiry Date`)<=as.Date(toDate),`Transaction Type`,
                                                          'OnRisk')),
    `Transaction Type`=dplyr::if_else(`Transaction Type`=='NB','New Business',
                                      dplyr::if_else(`Transaction Type`=='CAN','Cancellation',
                                                     dplyr::if_else(`Transaction Type`=='MTA' & GWP>=0,'AP',
                                                                    dplyr::if_else(`Transaction Type`=='MTA' & GWP<0,'RP',`Transaction Type`)))),
    `Premium Payable to Insurer`=round(`Net Net Premium`+`IPT €`+`Stamp Duty`,2),
    `General Deductible`=0
    # Cover=dplyr::if_else(`Buildings Sum Insured`>0 & SI_Con)
  )
## Paid Bordereaux ####
### Read in Template Paid Bordereaux ####
PaidBordereauxSample=read_xlsx('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/AIEL- Premium Accounts and Claims Bordereaux Specs-UK HH-Ivernia.xlsx',
                                 sheet='Paid Bordereau Spec', skip = 1)
PaidBordereauxSample=PaidBordereauxSample%>%filter(!is.na(`Field Name`))
PaidBordereauxSamplenames=names(PaidBordereauxSample)
PaidBordereauxSample=sapply(PaidBordereauxSample,function(x){
  if(length(which(x==0))!=0){
    x[which(x==0)]=NA
  }
  return(x)
})
PaidBordereauxSample=as.data.frame(PaidBordereauxSample)
names(PaidBordereauxSample)=PaidBordereauxSamplenames
# names(PaidBordereauxSample)=gsub('.',' ',names(PaidBordereauxSample))
PaidBordereaux=matrix(0,nrow=1,ncol = length(PaidBordereauxSample$`Field Name`))
PaidBordereaux=as.data.frame(PaidBordereaux)
names(PaidBordereaux)=PaidBordereauxSample$`Field Name`

for(p in 1:dim(PolicyTable)[1]){
  
  for(f in 1:length(PaidBordereauxSample$`Field Name`)){
    # Default Values
    if(!is.na(PaidBordereauxSample$Default[f])){
      if(is.na(PaidBordereauxSample$Map[f])){
        PaidBordereaux[p,f]=PaidBordereauxSample[f,'Default']
      }
    }else if(!is.na(PaidBordereauxSample$Map[f]) & PaidBordereauxSample$Map[f]!='Calculation' & !is.na(PaidBordereauxSample$Table[f])){
      # Mapped Values
      eqn=paste0('Value=',PaidBordereauxSample$Table[f],'$',PaidBordereauxSample$Map[f],'[',p,']')
      eval(parse(text = eqn))
      PaidBordereaux[p,f]=Value
    }else if(!is.na(PaidBordereauxSample$Table[f]) & PaidBordereauxSample$Table[f]=='Calculation'){
      # Calculated Values
      eqn=paste0('Value=',paste0('paste0',gsub('\\[p\\]',paste0('[',p,']'),PaidBordereauxSample$Calculation[f])))
      eval(parse(text = eqn))
      PaidBordereaux[p,f]=Value
    }
    
    
    # PaidBordereaux[p,f]=
    
  }
}
PaidBordereaux=PaidBordereaux%>%mutate(
  `Effective Date`=as.Date(`Effective Date`),
  `Term Start Date`=as.Date(`Term Start Date`,format='%d/%m/%Y'),
  `Term Expiry Date`=as.Date(`Term Expiry Date`,format='%d/%m/%Y')
)%>%filter(`Transaction Type`!='BLT')%>%
  # Financial fields
  mutate(
    # `IPT €`=round(GWP*as.numeric(`IPT %`)/100,2),
    `GWP plus Admin Fee`=round(GWP+`Admin Fee`,2),
    `Broker Acquisition Cost €`=round(GWP*as.numeric(`Broker Acquisition Cost %`)/100,2),
    `Net Premium`=round(GWP-`Broker Acquisition Cost €`,2),
    `MGA Acquisition Cost €`=round(GWP*as.numeric(`MGA Acquisition Cost %`)/100,2),
    `Net Net Premium`=round(`Net Premium`-`MGA Acquisition Cost €`,2),
    `Current Policy Status`=dplyr::if_else(as.Date(`Term Expiry Date`)<=as.Date(toDate) &
                                             !`Transaction Type`%in%c('CAN','NTU'),'Lapsed',
                                           dplyr::if_else(as.Date(`Term Expiry Date`)<=as.Date(toDate),`Transaction Type`,
                                                          'OnRisk')),
    `Premium Payable to Insurer`=`Net Net Premium`+`IPT €`+`Stamp Duty`,
    `Paid / Not Paid`=dplyr::if_else(`Paid / Not Paid`=='SETTLED','Paid','Not Paid'),
    `Report Mth`=Sys.Date()-day(Sys.Date()),
    `Amount Paid By Broker`=(round(GWP-`Broker Acquisition Cost €`+`IPT €`,2)),
    `Amount Paid to Insurer`=(round(GWP-`Broker Acquisition Cost €`-`IPT €`,2)),
    `IPT Paid to Insurer`=(round(`IPT €`,2))
    
  )
PaidBordereaux$`Broker Acquisition Cost %`<-NULL
PaidBordereaux$`MGA Acquisition Cost %`<-NULL
PaidBordereaux$`Net Premium`<-NULL
PaidBordereaux$`Net Net Premium`<-NULL
#%>%
  # # Other fields
  # mutate(
  #   # `Transaction Type Code`=`Transaction Type`,
  #   `Transaction Type`=dplyr::if_else(`Transaction Type`=='NB','New Business',
  #                                     dplyr::if_else(`Transaction Type`=='CAN','Cancellation',`Transaction Type`))
  # )
# rm(Value,PaidBordereauxSample,PaidBordereauxSamplenames,PolicyBordereauxSample)

PolicyTable_df <- as.data.frame(lapply(PolicyTable, unlist), stringsAsFactors = FALSE)
library(openxlsx)
write.xlsx(PolicyBordereaux,file='P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/PolicyBordereaux.xlsx', fileEncoding = "UTF-8")
write.xlsx(PolicyTable_df,file='P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/PolicyTable.xlsx', fileEncoding = "UTF-8")
write.xlsx(PaidBordereaux,file='P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/PaidBordereaux.xlsx', fileEncoding = "UTF-8")
write.xlsx(RiskTable,file='P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/RiskTable.xlsx')
write.xlsx(SpecifiedItems,file='P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/SpecifiedItems.xlsx', fileEncoding = "UTF-8")
write.xlsx(AddInsurers,file='P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/AddInsurers.xlsx', fileEncoding = "UTF-8")
write.xlsx(ClaimsTable,file='P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/ClaimsTable.xlsx', fileEncoding = "UTF-8")

# write.csv(PolicyBordereaux,'P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/PolicyBordereaux.csv', fileEncoding = "UTF-8")
# write.csv(PolicyTable_df,'P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/PolicyTable.csv', fileEncoding = "UTF-8")
# write.csv(PaidBordereaux,'P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/PaidBordereaux.csv', fileEncoding = "UTF-8")
# write.csv(RiskTable,'P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/RiskTable.csv')
# write.csv(SpecifiedItems,'P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/SpecifiedItems.csv', fileEncoding = "UTF-8")
# write.csv(AddInsurers,'P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/AddInsurers.csv', fileEncoding = "UTF-8")
# write.csv(ClaimsTable,'P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Bordereaux files/Post Go Live/ClaimsTable.csv', fileEncoding = "UTF-8")
PolicyTable=PolicyTable_df%>%filter(TransactionType=='NB')%>%mutate(Date=as.Date(substr(DateTransacted,1,10)))
RiskTable=RiskTable%>%filter(TransactionType=='NB')
SpecifiedItems=SpecifiedItems%>%filter(TransactionType=='NB')
ClaimsTable=ClaimsTable%>%filter(TransactionType=='NB')
AddInsurers=AddInsurers%>%filter(TransactionType=='NB')



library(gridExtra)

# First plot
plot1 <- ggplot(PolicyTable) +
  aes(x = Date) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal() +
  labs(subtitle = "Volume")

# Second plot
plot2 <- ggplot(PolicyTable) +
  aes(x = Date, y = TotalClientDebit) +
  geom_point(
    shape = "circle",
    size = 1.15,
    colour = "#112446"
  ) +
  geom_smooth(span = 0.41) +
  theme_minimal() +
  labs(subtitle = "Average Accepted Premium")
RiskTable=RiskTable%>%
  mutate(CountyString=gsub("[0-9]", "",RiskAddress.StringCounty))%>%
  mutate(CountyString=gsub(" ", "",CountyString))%>%
  mutate(CountyString=gsub("Co.", "",CountyString))
plot3 <- RiskTable %>%
  ggplot() +
  aes(x = CountyString) +
  geom_bar(fill = "#112446") +
  theme_minimal()

ggplotly(plot2)
ggplotly(plot3)
ggplotly(plot1)
# # Combine plots into a single row with two columns
# combined_plot <- grid.arrange(
#   plot1,
#   plot2,
#   plot3,
#   ncol = 2
# )
summary=PolicyTable_df%>%mutate(Date=as.Date(substr(DateTransacted,1,10)))%>%
  group_by(Date)%>%
  summarise(Volume=sum(dplyr::if_else(InsurerName=='Accredited',1,0)),
            Fee=sum(dplyr::if_else(MgaFee!=0,(MgaFee-1.5)*(1-0.175),MgaFee)),
            Commission=sum(MgaCommission),
            Total=Fee+Commission,
            GrossExLevy=sum(GrossExLevy))%>%mutate(Weekday=format(Date, "%a"))%>%
  mutate(DatePeriod=dplyr::if_else(Date<as.character('2023-12-21',format='%Y-%m-%d'),'1-Pre-GoLive',
                                   dplyr::if_else(Date<as.character('2024-01-02',format='%Y-%m-%d'),'2-Back Office Only',
                                                  dplyr::if_else(Date<as.character('2024-01-16',format='%Y-%m-%d'),'3-BO & OL',
                                                                 dplyr::if_else(Date<as.character('2024-01-13',format='%Y-%m-%d'),'4-GAP 1',
                                                                                dplyr::if_else(Date<as.character('2024-01-27',format='%Y-%m-%d'),'5-RateRed and Gap',
                                                                                               dplyr::if_else(Date<as.character('2024-01-31',format='%Y-%m-%d'),'6-Quoteability Changes',
                                                                                                              dplyr::if_else(Date<as.character('2024-02-16',format='%Y-%m-%d'),'7-QuoteArea Rate Change',
                                                                                                                             dplyr::if_else(Date<as.character('2024-02-22',format='%Y-%m-%d'),'8-Buying Behaviour','9-Buying Behaviour')))))))))

SummaryTot=PolicyTable_df%>%mutate(Date=as.Date(substr(DateTransacted,1,10)))%>%
  summarise(Date=Sys.Date()
            ,Volume=sum(dplyr::if_else(InsurerName=='Accredited',1,0)),
            Fee=sum(dplyr::if_else(MgaFee!=0,(MgaFee-1.5)*(1-0.175),MgaFee)),
            Commission=sum(MgaCommission),
            Total=Fee+Commission,
            GrossExLevy=sum(GrossExLevy))%>%mutate(Weekday='NA',
                                                   DatePeriod='All')
summary=rbind(summary,SummaryTot)

summary=PolicyTable_df%>%mutate(Date=as.Date(substr(DateTransacted,1,10)))%>%
  group_by(Date)%>%
  summarise(Volume=sum(dplyr::if_else(InsurerName=='Accredited',1,0)),
            Fee=sum(dplyr::if_else(MgaFee!=0,(MgaFee-1.5)*(1-0.175),MgaFee)),
            Commission=sum(MgaCommission),
            Total=Fee+Commission,
            GrossExLevy=sum(GrossExLevy))%>%mutate(Weekday=format(Date, "%a"))%>%
  mutate(DatePeriod=dplyr::if_else(Date<as.character('2023-12-21',format='%Y-%m-%d'),'1-Pre-GoLive',
                                   dplyr::if_else(Date<as.character('2024-01-02',format='%Y-%m-%d'),'2-Back Office Only',
                                                  dplyr::if_else(Date<as.character('2024-01-16',format='%Y-%m-%d'),'3-BO & OL',
                                                                 dplyr::if_else(Date<as.character('2024-01-13',format='%Y-%m-%d'),'4-GAP 1',
                                                                                dplyr::if_else(Date<as.character('2024-01-27',format='%Y-%m-%d'),'5-RateRed and Gap',
                                                                                               dplyr::if_else(Date<as.character('2024-01-31',format='%Y-%m-%d'),'6-Quoteability Changes',
                                                                                                              dplyr::if_else(Date<as.character('2024-02-16',format='%Y-%m-%d'),'7-QuoteArea Rate Change',
                                                                                                                             dplyr::if_else(Date<as.character('2024-02-22',format='%Y-%m-%d'),'8-Buying Behaviour','9-Buying Behaviour')))))))))

SummaryPCTot=PolicyTable_df%>%
  mutate(Date=as.Date(substr(DateTransacted,1,10)))%>%
  group_by(PolicyNumber,Date)%>%
  summarise(Volume=sum(dplyr::if_else(InsurerName=='Accredited',1,0)),
            Fee=sum(dplyr::if_else(MgaFee!=0,(MgaFee-1.5)*(1-0.175),MgaFee)),
            Commission=sum(MgaCommission),
            Total=Fee+Commission,
            GrossExLevy=sum(GrossExLevy),
            TotalClientDebit=sum(TotalClientDebit))%>%mutate(Weekday='NA',
                                                   DatePeriod='All')
# summary=rbind(summary,SummaryTot)

