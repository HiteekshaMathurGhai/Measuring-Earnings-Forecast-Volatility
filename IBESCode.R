library(data.table)
library(foreign)
library(haven)
library(dplyr)
library(lubridate)
library(zoo)
library(RcppRoll)
library(scales)
setwd("~/Users/hiteekshamathur/Desktop/MFE/UCLA/ProfMarikoRA")
earnings=read_dta('EARNINGS1985to2015.dta')
write.csv(earnings,'IBES_data.csv')
spinoff=read_dta('CRSPDATA1985to2015.dta')
write.csv(spinoff,'CRSP_data.csv')

# quantile(earnings$VALUE, c(.005,.01,.05,.10,.25,.5,.75,.9,.95,.99,.995),na.rm = T)
#winsorize value and actual: above .995 and below .005
A=squish(earnings$ACTUAL, round(quantile(earnings$ACTUAL, c(.005, .995),na.rm = T)))
V=squish(earnings$VALUE, round(quantile(earnings$VALUE, c(.005, .995),na.rm = T)))
earnings$ACTUAL=A
earnings$VALUE=V
#frollapply

earnings$num_months=(as.yearmon(strptime(earnings$FPEDATS, format = "%Y-%m-%d"))-
    as.yearmon(strptime(earnings$ANNDATS, format = "%Y-%m-%d")))*12
earnings$error_abs=abs(earnings$ACTUAL- earnings$VALUE)
earnings$error=earnings$ACTUAL- earnings$VALUE
earnings=as.data.table(earnings)
earnings$ANNDATS_Year=as.numeric(format(as.Date(earnings$ANNDATS, format="%Y-%m-%d"),"%Y"))
earnings=earnings[,period:=ifelse(num_months<6,'short',
                                  ifelse(num_months>=6 & num_months<12,'medium',
                                         ifelse(num_months>=12 & num_months<=24,'long','verylong')))]

earnings=earnings[,Quarter:=quarters(as.Date(ANNDATS_ACT))] 
earnings=earnings[!is.na(earnings$ANNDATS_ACT),]#drop NAs

earnings=as.data.table(earnings)

##########################################################################

spinoff=spinoff[!is.na(spinoff$SICCD),]
spinoff=spinoff[spinoff$SICCD>0,]
pricedata=spinoff[,c('date','SICCD','CUSIP','TICKER','COMNAM','PRC')]
pricedata$Year=as.numeric(format(as.Date(pricedata$date, format="%Y-%m-%d"),"%Y"))
pricedata=as.data.table(pricedata)
pricedata=pricedata[, head(.SD, 1), by = c('CUSIP','Year')]

merged=merge(earnings,pricedata, by.x=c('CUSIP','ANNDATS_Year'),by.y = c('CUSIP','Year'))
merged$Year=as.numeric(format(as.Date(merged$date, format="%Y-%m-%d"),"%Y"))
merged[is.na(merged)] <- 0
merged$PRC=abs(merged$PRC)
merged=merged[merged$PRC!=0,]#drop prc=0
merged=merged[,c("avg_err", "std_err") := list(mean(error_abs/(PRC)), sd(error/(PRC))),
                  by=c('CUSIP','ANNDATS_Year','period')] #group by year of ANNDATS

merged=merged[,Avg_Earnings:=mean(ACTUAL),by=c('CUSIP','Quarter')]
merged=merged[,Detrended_Earnings:= (ACTUAL-Avg_Earnings)]
merged2=merged[merged[, .I[1], by=c('CUSIP','ANNDATS_Year','Quarter')]$V1, check:=1]
# merged2=merged[merged[, .I[1], by=c('CUSIP','ANALYS','ANNDATS_Year','Quarter')]$V1, check:=1]

merged2[, Detrended_Earnings_final := ifelse (check==1, Detrended_Earnings, 'NA')]

# merged2=merged2[, Earnings_Vol:= rollapplyr(merged2$Detrended_Earnings_final, 1:.N,
#                   function(x) sd(tail((x), 5)), fill = 'NA'),by = 'CUSIP']
merged2[,Earnings_Vol:=frollapply(Detrended_Earnings_final, 5, FUN=mean,na.rm=TRUE,align="right"),
        by = 'CUSIP']

merged2=merged2[,Normalized_Forecast_err := (avg_err/Earnings_Vol)] 
write.csv(merged2,'earnings.csv')
merged2=data.table(merged2)

#group to cusip year:
cusip_year=merged2[, lapply(.SD, mean, na.rm=TRUE), by=c('CUSIP','ANNDATS_Year','period'),
                   .SDcols=c("avg_err", "std_err", "Normalized_Forecast_err","SICCD") ]  
---#include SIC code
  write.csv(cusip_year,'cusip_year.csv')
write_dta(cusip_year,'cusip_year.dta')
cusip_year_dta=read_dta('cusip_year.dta')
write.csv(cusip_year_dta,'cusip_year.csv')

#group to sic year:
sic_year=cusip_year[, lapply(.SD, mean, na.rm=TRUE), by=c('SICCD','ANNDATS_Year','period'),
                    .SDcols=c("avg_err", "std_err", "Normalized_Forecast_err") ] 
write.csv(sic_year,'sic_year.csv')     

####################################### END OF CODE  #####################################

