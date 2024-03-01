load('P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/02-CleanData.RDS')

# Removing NA's

dt$avr_prem=as.numeric(dt$avr_prem)
# FLDR_MDL <- "C:/TIA/Home/Scenarios"
FLDR_MDL <- "P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Model"
FL_MDL <- "Model1"


mdl_col_list <- c("cover_type","NumberofBedrooms","NumberofBathrooms","NeigbourhoodWatch","BuildingSumInsuredRequired",
                  "ContentsSumInsuredRequired","ClaimedYearsNCD","OccupiedDuringDate","BuildingSumInsuredRequired*ContentsSumInsuredRequired",
                  "BuildingAccidentalDamageRequired","ContentsAccidentalDamageRequired","PHAge","I(PHAge^2)","ResidenceYears","PropertyAge","I(PropertyAge^2)",
                  "num_ph","HeatingType","MaritalStatus","PropertyType","timeIdx",
                  "RequiredVoluntaryExcessAmount","HasLocks","OccupiedDuringDate",
                  "ResidenceType","SocialWelfare","FirstTimeBuyer","EmploymentType","AreaRating","SpecifiedItemsSumInsuredRequired",
                  "RiskAddressMatchLevel","shortalarm","clm_count")
Eqn=paste0("I(log(avr_prem))~",paste0(mdl_col_list,collapse="+"))
tmp_formula <- as.formula(Eqn)
tmp=dt[,which(names(dt)%in%c("avr_prem",mdl_col_list))]

mdl <- glm(formula=tmp_formula,data=tmp)
summary(mdl)
# results()

saveRDS(mdl,file=file.path(FLDR_MDL,paste0(FL_MDL,".bin")))
mdl <- readRDS(file=file.path(FLDR_MDL,paste0(FL_MDL,".bin")))

dt_out <- data.frame(Category=names(mdl$coefficients),Value=mdl$coefficients)
write.csv(dt_out,"P:/Underwriting/Pricing Pipelines/IV-Home Pricing-Engine/Coefficients.csv",row.names=FALSE)




pred <- exp(predict(mdl,type="response"))
minprem <- dt$avr_prem*0
minprem[dt$cover_type=="B"] <- 100
minprem[dt$cover_type=="C"] <- 70
minprem[dt$cover_type=="B+C"] <- 125
pred <- pmax(pred,minprem)
maxprem <- 5000
pred <- pmax(pred,minprem)
pred <- pmin(pred,maxprem)
tmp <- data.frame(pr=pred,act=dt$avr_prem,mn=dt$min_prem)
tmp$win <- ifelse(tmp$pr<tmp$mn,1,0)
tmp$resid <- tmp$act-tmp$pr
tmp$mnresid <- tmp$mn-tmp$p

print(sum(tmp$win)/nrow(tmp))

print(sum(ifelse((tmp$pr*0.9)<tmp$mn,1,0))/nrow(tmp))
print((sum(ifelse((tmp$pr*0.95)<tmp$mn,1,0))/nrow(tmp))/(sum(ifelse((tmp$pr)<tmp$mn,1,0))/nrow(tmp)))

View(dt[tmp$resid>500,])




# Grouping into pods ####
MaxAddOn=1
predictionssplit=as.data.frame(ModelData$PTC)%>%arrange(ModelData$PTC)
predictionssplit=rbind(predictionssplit,MaxAddOn)
names(predictionssplit)='PTC'

library(Hmisc) # cut2
SegmentSplit=split(predictionssplit, cut2(predictionssplit$PTC, g=Segments))


GroupingMaxMin=as.data.frame(t(sapply(SegmentSplit,function(x){
  c(x$PTC[1],x$PTC[length(x$PTC)])
})
)
)
rownames(GroupingMaxMin)=c(1:Segments)
GroupingMaxMin$Group=rownames(GroupingMaxMin)
colnames(GroupingMaxMin)<-c('Min','Max','Group')

GroupingMaxMin$Max[1]=GroupingMaxMin$Min[2]
GroupingMaxMin$Max[which(GroupingMaxMin$Group==max(as.numeric(GroupingMaxMin$Group))[1])]=10000000

ModelData$Group=sapply(ModelData$PTC,function(x){
  output=as.numeric(GroupingMaxMin$Group[which(x>=as.numeric(GroupingMaxMin$Min) & x<=as.numeric(GroupingMaxMin$Max))[1]])
  return(output)
})

# Groupings ####
ModelData$PolicyInceptDate=SingleLinePPViewKeep$PolicyInceptDate
ModelData$Claimants=SingleLinePPViewKeep$Claimants
ModelData$GWP=SingleLinePPViewKeep$GWP
ModelData$PAF=SingleLinePPViewKeep$PAF
ModelData$SchemeGroup=SingleLinePPViewKeep$Scheme
ModelData=ModelData%>%mutate(Scheme=dplyr::if_else(grepl('1',SchemeGroup),'1',dplyr::if_else(grepl('2',SchemeGroup),'2','NA')))
ModelData$PAF[is.na(ModelData$PAF)]=1

ModelData[,ClaimsCols]=SingleLinePPViewKeep[,ClaimsCols]
ModelData=ModelData%>%mutate(year_month=as.Date(paste0(as.character(year(as.Date(PolicyInceptDate))),"-",as.character(month(as.Date(PolicyInceptDate))),"-01")))




#Area
# Area <- tapply(tmp$resid,dt$RiskAddressCounty,mean)
# Area <- data.frame(Area=names(Area),AreaRating=Area)
# write.csv(Area,"C:/TIA/Home/Area.csv",row.names=FALSE)
# dt <- merge(x=dt,y=Area,by.x="RiskAddressCounty",by.y="Area")


# require(earth)
# mdl <- earth(formula=tmp_formula,data=dt,degree=2,thresh = 0.000000001)



