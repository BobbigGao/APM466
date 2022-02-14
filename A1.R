
install.packages("jrvFinance")
install.packages("lubridate")
library(jrvFinance)
library(readxl)

bonds_data <- read_excel("/Users/yutenggao/Desktop/APM466/A1/dataset.xlsx", sheet = "close_price")
View(bonds_data)
coupon_rate <- c(0.005,0.0275,0.0175,0.015,0.0225,0.025,0.0125,0.0225,0.0025,0.015)
maturity_date <- c("2022-03-01","2022-06-01","2023-03-01","2023-06-01","2024-03-01","2024-06-01","2025-03-01","2025-06-01","2026-03-01","2026-06-01")
recorded_date <- c("2022-01-11","2022-01-12","2022-01-13","2022-01-14","2022-01-17","2022-01-18","2022-01-19","2022-01-20","2022-01-21","2022-01-24")
close_price_table <- matrix(c(bonds_data$`CAN 0.5 Mar 1 2022`, bonds_data$`CAN 2.75 Jun 1 2022`, bonds_data$`CAN 1.75 Mar 1 2023`, bonds_data$`CAN 1.5 Jun 1 2023`,
                              bonds_data$`CAN 2.25 Mar 1 2024`, bonds_data$`CAN 2.5 Jun 1 2024`, bonds_data$`CAN 1.25 Mar 1 2025`, bonds_data$`CAN 2.25 Jun 1 2025`,
                              bonds_data$`CAN 0.25 Mar 1 2026`, bonds_data$`CAN 1.5 Jun 1 2026`), nrow = 10, ncol = 10, byrow = FALSE)

ytm <- matrix('numeric',nrow = 10, ncol = 10)
for (i in c(1:10)) {
  close_price <- close_price_table[,i]
  for (j in c(1:10)){
    ytm[j,i] <- bond.yield(settle = recorded_date[j], mature = maturity_date[i], coupon = coupon_rate[i], freq = 2, close_price[j], 
                           convention = c("30/360","ACT/ACT","ACT/360","30/360E"), comp.freq = 2, redemption_value = 100)
  }
}


remaining_years <- matrix('numeric', nrow = 10, ncol = 10)
for (i in c(1:10)){
  for (j in c(1:10)){
    remaining_years[i,j] <- yearFraction(recorded_date[i], maturity_date[j], freq = 2, convention = c("30/360","ACT/ACT","ACT/360","30/360E"))
  }
}

year <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
plot(year, ytm[1,], main = "5-Year YTM Curve", xlab = "Remaining Years", ylab = "YTM", ylim = c(0,0.018))
lines(year, ytm[2,],type = 'o',lwd = 1.0, col='red')
lines(year, ytm[3,],type = 'o',lwd = 1.0, col='green')
lines(year, ytm[4,],type = 'o',lwd = 1.0, col='blue')
lines(year, ytm[5,],type = 'o',lwd = 1.0, col='lightblue')
lines(year, ytm[6,],type = 'o',lwd = 1.0, col='brown')
lines(year, ytm[7,],type = 'o',lwd = 1.0, col='orange')
lines(year, ytm[8,],type = 'o',lwd = 1.0, col='grey')
lines(year, ytm[9,],type = 'o',lwd = 1.0, col='darkblue')
lines(year, ytm[10,],type = 'o',lwd = 1.0, col='yellowgreen')

legend('bottomright', legend = c("CAN 0.5 Mar 1 2022","CAN 2.75 Jun 1 2022","CAN 1.75 Mar 1 2023","CAN 1.5 Jun 1 2023","CAN 2.25 Mar 1 2024","CAN 2.5 Jun 1 2024","CAN 1.25 Mar 1 2025","CAN 2.25 Jun 1 2025","CAN 0.25 Mar 1 2026","CAN 1.5 Jun 1 2026"), col = c('red','green','blue','lightblue','brown','orange','grey','darkblue', 'yellowgreen'), lwd = 1.0, cex = 0.8)

dirty_price <- matrix('numeric', nrow = 10, ncol = 10)
for (i in c(1:10)) {
  for (j in c(1:10)) {
    dirty_price[i,j] <- bond.TCF(recorded_date[i], maturity_date[j], coupon_rate[j], freq = 2)$accrued + close_price_table[i,j]
  }
}

cash_flow <- list()
for(i in 1:10){
  cash_flow <- bond.TCF(recorded_date[i], maturity_date[i], coupon_rate[i], freq = 2, redemption_value = 100)$cf
  print(cash_flow)
}

cf_1 = c(100.25)
cf_2 = c(101.375)
cf_3 = c(0.875,0.875,100.875)
cf_4 = c(0.75, 0.75, 100.75)
cf_5 = c(1.125, 1.125, 1.125, 1.125, 101.125)
cf_6 = c(1.25, 1.25, 1.25, 1.25, 101.25)
cf_7 = c(0.625, 0.625, 0.625, 0.625, 0.625, 0.625, 100.625)
cf_8 = c(1.125, 1.125, 1.125, 1.125, 1.125, 1.125, 101.125)
cf_9 = c(0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 100.125)
cf_10 = c (0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 100.75)

