dt$cover_type <- "Unknown"
dt$cover_type[dt$BuildingSumInsuredRequired>0] <- "B"
dt$cover_type[dt$ContentsSumInsuredRequired>0] <- "C"
dt$cover_type[dt$BuildingSumInsuredRequired>0 & dt$ContentsSumInsuredRequired>0] <- "B+C"
dt <- dt[dt$cover_type!="Unknown",]

dt$CoverStartDate <- as.Date(dt$CoverStartDate,format="%d/%m/%Y")
dt$CoverEndDate <- as.Date(dt$CoverEndDate,format="%d/%m/%Y")
dt$ConstructionDate <- as.Date(dt$ConstructionDate,format="%d/%m/%Y")
dt$BirthDate <- as.Date(dt$BirthDate,format="%d/%m/%Y")
dt$ResidenceDate <- as.Date(dt$ResidenceDate,format="%d/%m/%Y")
dt$ResidenceUnk <- ifelse(is.na(dt$ResidenceDate),1,0)
dt$ResidenceDate[is.na(dt$ResidenceDate)] <- mean(dt$ResidenceDate,na.rm=TRUE)

dt$PropertyAge <- year(dt$CoverStartDate)-year(dt$ConstructionDate)#using difference in years since construction date is only a year
dt$PropertyAge[is.na(dt$PropertyAge)] <- 0

dt$PHAge <- fullyeardiff(dt$BirthDate,dt$CoverStartDate)
###TODO - should not be needed
dt <- dt[!is.na(dt$PHAge),]

dt$ResidenceYears <- fullyeardiff(dt$ResidenceDate,dt$CoverStartDate)
dt$ResidenceYears[dt$ResidenceYears<0] <- 0

tmp <- dt$HeatingType
tmp[tmp=="Mixture Inc. Oil"] <- "Oil"
tmp[tmp=="NULL"] <- "Gas"
tmp[tmp=="Unknown"] <- "Gas"
tmp[tmp=="Solar/Wind"] <- "Other"
tmp[tmp=="Solid Fuel"] <- "Other"
tmp[tmp=="Woodchip"] <- "Other"
dt$HeatingType <- tmp

tmp <- dt$MaritalStatus
tmp[tmp=="Common Law Spouse"] <- "Married"
tmp[tmp=="Widowed"] <- "Married"
tmp[tmp!="Married"] <- "NotMarried"
dt$MaritalStatus <- tmp

# dt_occ <- fread("C:/TIA/Home/Motor_Occs.csv",sep=",",data.table=FALSE)
# tmp <- merge(x=dt,y=dt_occ,by.x="OccupationType",by.y="Description")

tmp <- dt$PropertyType
tmp[tmp=="Country Mansion"] <- "Detached House"
tmp[tmp=="Farm House"] <- "Detached House"
tmp[tmp=="Maisonette"] <- "Flat"
tmp[tmp=="Purpose Built Apartment"] <- "Detached House"
dt$PropertyType <- tmp

dt$timeIdx <- (year(dt$CoverStartDate)*12)+month(dt$CoverStartDate)

dt$RoofConstructionType[dt$RoofConstructionType!="Standard"] <- "NonStandard"
dt$ResidenceType[dt$ResidenceType!="Owner Occupied"] <- "NotOwnerOcc"

tmp <- dt$EmploymentType
tmp[tmp=="Company Director"] <- "Employed"
tmp[tmp=="Household Duties"] <- "Unemployed"
tmp[tmp=="Independent Means"] <- "Unemployed"
tmp[tmp=="Student"] <- "Unemployed"
tmp[tmp=="Unknown"] <- "Unemployed"
tmp[tmp=="Voluntary Worker"] <- "Unemployed"
dt$EmploymentType <- tmp

dt$RiskAddressMatchLevel[dt$RiskAddressMatchLevel=="NULL"] <- 900
dt$RiskAddressMatchLevel <- as.numeric(dt$RiskAddressMatchLevel)

dt_area <- read.csv("C:/TIA/Home/Area.csv")
dt <- merge(x=dt,y=dt_area,by.x="RiskAddressCounty",by.y="Area")
colnames(dt)[colnames(dt)=="Rating"] <- "AreaRating"

dt$shortalarm <- ifelse(dt$AlarmType=="Unspecified",1,0)
