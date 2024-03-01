FLDR_MDL <- "C:/TIA/Home/Scenarios"
FL_MDL <- "Model1"


mdl_col_list <- c("cover_type","NumberofBedrooms","NumberofBathrooms","NeigbourhoodWatch","BuildingSumInsuredRequired",
                  "ContentsSumInsuredRequired","ClaimedYearsNCD","OccupiedDuringDate","BuildingSumInsuredRequired*ContentsSumInsuredRequired",
                  "BuildingAccidentalDamageRequired","ContentsAccidentalDamageRequired","PHAge","I(PHAge^2)","ResidenceYears","PropertyAge","I(PropertyAge^2)",
                  "num_ph","HeatingType","MaritalStatus","PropertyType","timeIdx",
                  "RequiredVoluntaryExcessAmount","HasLocks","OccupiedDuringDate",
                  "ResidenceType","SocialWelfare","FirstTimeBuyer","EmploymentType","AreaRating","SpecifiedItemsSumInsuredRequired",
                  "RiskAddressMatchLevel","shortalarm","clm_count")
tmp_formula <- as.formula(paste0("I(log(avr_prem))~",paste0(mdl_col_list,collapse="+")))
mdl <- glm(formula=tmp_formula,data=dt)
summary(mdl)
results()

saveRDS(mdl,file=file.path(FLDR_MDL,paste0(FL_MDL,".bin")))
mdl <- readRDS(file=file.path(FLDR_MDL,paste0(FL_MDL,".bin")))

dt_out <- data.frame(Category=names(mdl$coefficients),Value=mdl$coefficients)
write.csv(dt_out,"C:/TIA/Home/Coefficients.csv",row.names=FALSE)




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


#Area
# Area <- tapply(tmp$resid,dt$RiskAddressCounty,mean)
# Area <- data.frame(Area=names(Area),AreaRating=Area)
# write.csv(Area,"C:/TIA/Home/Area.csv",row.names=FALSE)
# dt <- merge(x=dt,y=Area,by.x="RiskAddressCounty",by.y="Area")


# require(earth)
# mdl <- earth(formula=tmp_formula,data=dt,degree=2,thresh = 0.000000001)



