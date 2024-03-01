FLDR_SCENARIOS <- file.path("C:/TIA/Home/Scenarios/",FL_MDL)

list.files(FLDR_SCENARIOS)

rating_vector <- rep(1,times=length(mdl$coefficients))
base_adj <- 0.9

mdl <- readRDS("C:/TIA/Home/Scenarios/Model1.bin")


print("###OVERALL STATS###")
print_stats(rating_vector,Base_Adj = base_adj,Full=TRUE)
print("###APRIL STATS###")
print_stats(rating_vector,Mnth=24268,Base_Adj = base_adj,Full=TRUE) #(2022*12)+4
print("###MAY STATS###")
print_stats(rating_vector,Mnth=24269,Base_Adj = base_adj,Full=TRUE)

tmp <- read.csv(file=file.path(FLDR_SCENARIOS,"RR0.1_F0.03.csv"))
rating_vector <- tmp$x
print("###OVERALL STATS###")
print_stats(rating_vector,Full=TRUE)
print("###APRIL STATS###")
print_stats(rating_vector,Mnth=24268,Full=TRUE) #(2022*12)+4
print("###MAY STATS###")
print_stats(rating_vector,Mnth=24269,Full=TRUE)


target <- 0.0354#quotes to sales
avr_conversion <- 0.5#from Chill HH team (approx figure)
((sum(tmp$win[dt$Year==2022])/nrow(tmp[dt$Year==2022,]))/(target/avr_conversion))-1

#####27/03
qts <- qapply(dt$RiskAddressCounty)
tmp <- calc_prem(Rating_Vector=rating_vector,Base_Adj=base_adj)
sls <- qapply(dt$RiskAddressCounty[tmp<=dt$min_prem])


