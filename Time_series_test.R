#////////////////////////////Stationarity Lectore///////////////////////////

data=read.csv(file.choose(data),header = TRUE,sep = ";")
names(data)
attach(data)
print(head(data))

#******************Tronsforme the data into time series*****************************
require(tseries)
gdp_series = ts(data$GDP_Index, frequency = 365, start = c(2000,1))
print(head(GDP_Index))

plot(gdp_series,
     title("time serie of gdp_index"),
     col='blue',
     lwd=2
     )

#***********Stationarity test*****************************************
adf_test=adf.test(gdp_series)
print(adf_test)

#//////////////////////////////how tronsforme the sereis into Stationary seris//

log_gdp_index=log(gdp_series)
plot(log_gdp_index)

adf_log_gdp=adf.test(log_gdp_index)
adf_log_gdp

#*******************diff method**********************
diff_gdp=diff(gdp_series)
plot(dif_gdp)
adf.test(diff_gdp)

#///////////////////////////Philip Perron test/////////////////////////////
philip_test=pp.test(diff_gdp)
philip_test

#///////////////////////////KPSS test//////////////////////////////////////


kpss_test=kpss.test(gdp_series)
kpss_test



#******************Second series******************************************
Inflation_Rate = ts(data$Inflation_Rate, frequency = 1, start = c(2000,1))
print(head(Inflation_Rate))

plot(Inflation_Rate,
     title("time serie of Inflation_Rate"),
     col='blue',
     lwd=2
)

adf_test_2=adf.test(Inflation_Rate)
adf_test_2


