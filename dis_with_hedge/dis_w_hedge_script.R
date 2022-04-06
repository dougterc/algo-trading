source("./dis_w_hedge_functions.R")
#dis.finviz.insider.scraper("https://finviz.com/insidertrading.ashx")
dis.general.loadPackages()
screener <- dis.finviz.screener.scraper("","Overview",0,17)

#upload to db screener
mydb <- dis.database.connect("local","dis_db")

# source("./dis_w_hedge_functions.R")
# dis.database.uploadScreener(mydb,screener)

#screener <- dbGetQuery(mydb,"SELECT * FROM DIS_Screener;")
#have update formula HERE

#simulate webpage inputs
user_id_in <- 1
inputs_id_in <- 1

#get from database
sectors <- c(unique(dbGetQuery(mydb,"SELECT Sector FROM DIS_Screener;")))
desired_sector <- dbGetQuery(mydb,
                             paste0(
                               "SELECT desired_sector_a FROM DIS_Inputs WHERE user_id=",
                               user_id_in," AND inputs_id=",inputs_id_in,";"))[1,1]
industries <- unique(c(dbGetQuery(mydb,
                        paste0(
                          "SELECT industry FROM DIS_Screener WHERE sector='",
                          desired_sector,"';"))$industry))
desired_industry <- dbGetQuery(mydb,
                             paste0(
                               "SELECT desired_industry_a FROM DIS_Inputs WHERE user_id=",
                               user_id_in," AND inputs_id=",inputs_id_in,";"))[1,1]

if(desired_industry == "All") {
  tickers <- unique(c(dbGetQuery(mydb,
                                    paste0(
                                      "SELECT ticker FROM DIS_Screener WHERE sector='",
                                      desired_sector,"';"))$ticker))
} else {
  tickers <- unique(c(dbGetQuery(mydb,
                                 paste0(
                                   "SELECT ticker FROM DIS_Screener WHERE sector='",
                                   desired_sector,"' AND industry='",desired_industry,"';"))$ticker))
}

#Get historical data for DIS
hd <- dis.historical.data(tickers,Sys.Date()-365,Sys.Date(),0)
prices <- hd$Historical
lookback <- 30
temp <- prices %>%
  select(Ticker,RefDate,RetClose)
hPrices <- dis.historical.turnHorizontal(temp) %>%
  filter(RefDate >= Sys.Date()-lookback)
hPrices.clean <- dis.historical.cleanMissing(hPrices)

summary <- temp %>%
  filter(RefDate >= Sys.Date()-lookback) %>%
  group_by(Ticker) %>%
  summarize(ReturnS = sum(RetClose),
            ReturnM = mean(RetClose),
            StdDevR = sd(RetClose)) %>%
  mutate(mean_ratio = ReturnM / StdDevR) %>%
  arrange(desc(mean_ratio))
numstks <- dbGetQuery(mydb,
                      paste0(
                        "SELECT num_of_stocks FROM DIS_Inputs WHERE user_id=",
                        user_id_in," AND inputs_id=",inputs_id_in,";"))[1,1]
                                          
picks <- summary[1:numstks,]


