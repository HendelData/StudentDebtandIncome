#SET PATH TO SAVE FILE TO BE USED BY THE APP
path <- "C:/Users/marce/Documents/R/StudentDebtandIncome"

#GET COLLEGE SCORECARD DATA
temp <- tempfile()
download.file("https://ed-public-download.app.cloud.gov/downloads/Most-Recent-Cohorts-Field-of-Study_09262023.zip", temp)
sc_data <- read.table(unz(temp,"Most-Recent-Cohorts-Field-of-Study.csv"), header=T, quote="\"", sep=",")
unlink(temp)

#LIMIT TO BACHELOR'S DEGREE LEVEL WITH ALL NECESSARY FIELDS AVAILABLE FOR DEBT AND INCOME
sc <- subset(sc_data, sc_data$CREDLEV=='3' & sc_data$DEBT_ALL_STGP_EVAL_MEAN!='PrivacySuppressed' &
               sc_data$DEBT_ALL_STGP_EVAL_MDN10YRPAY!='PrivacySuppressed' & sc_data$EARN_MDN_1YR!='PrivacySuppressed' &
               sc_data$EARN_MDN_4YR!='PrivacySuppressed')
sc <- sc[,c(1:11,16,75,124,130)]

#CONVERT VARIABLES AS NEEDED
sc$CIPDESC <- str_sub(sc$CIPDESC, end = -2)
sc$DEBT_ALL_STGP_EVAL_MEAN <- as.numeric(sc$DEBT_ALL_STGP_EVAL_MEAN)
sc$DEBT_ALL_STGP_EVAL_MDN10YRPAY <- as.numeric(sc$DEBT_ALL_STGP_EVAL_MDN10YRPAY)
sc$EARN_MDN_1YR <- as.numeric(sc$EARN_MDN_1YR)
sc$EARN_MDN_4YR <- as.numeric(sc$EARN_MDN_4YR)

#CALCULATE AVERAGE PERCENTAGE INCREASE IN INCOME FROM YEAR 1 TO YEAR 4 AND ASSUME YEAR 4 THROUGH YEAR 10 INCREASE IS HALF THAT RATE
sc$EARN_MDN_10YR_PROJECTED <- round(sc$EARN_MDN_4YR*((1+((sc$EARN_MDN_4YR/sc$EARN_MDN_1YR)^(1/3)-1)/2)^6), 0)
sc$EARN_MDN_10YR_PROJECTED <- ifelse(sc$EARN_MDN_4YR<sc$EARN_MDN_1YR, sc$EARN_MDN_4YR, sc$EARN_MDN_10YR_PROJECTED)

sc_display <- sc[, c(3,7,12:16)]
colnames(sc_display) <- c("Institution","Major","Avg Debt","Avg Payment","Salary 1 Yr","Salary 4 Yr","Projected Salary 10 Yr")

write.csv(sc_display, paste0(path, "/scdata.csv"), quote=TRUE, row.names=FALSE)
