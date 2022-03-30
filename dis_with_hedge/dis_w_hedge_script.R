source("./dis_w_hedge_functions.R")

#dis.finviz.insider.scraper("https://finviz.com/insidertrading.ashx")
dis.general.loadPackages()
screener <- dis.finviz.screener.scraper("","Overview",0,17)
sectors <- c(unique(screener$Sector))
desired_sector <- "Energy"
ds_index <- grep(desired_sector,sectors)
ticker_by_sector <- screener %>%
  filter(Sector == sectors[ds_index]) %>%
  select(Ticker,Company,Sector,Industry) %>%
  arrange(Industry)
industries <- c(unique(ticker_by_sector$Industry))
desired_industry <- "Oil & Gas Midstream"
di_index <- grep(desired_industry,industries)
ticker_by_industry <- screener %>%
  filter(Industry == industries[di_index]) %>%
  select(Ticker,Company,Sector,Industry) %>%
  arrange(Ticker)
#in DIS
hd <- dis.historical.data(c(ticker_by_industry$Ticker),Sys.Date()-365,Sys.Date(),0)
prices <- hd$Historical
temp <- prices %>%
  select(Ticker,RefDate,RetClose)
hPrices <- dis.historical.turnHorizontal(temp) %>%
  filter(RefDate >= Sys.Date()-30)
hPrices.clean <- dis.historical.cleanMissing(hPrices)

#ALL
hd.all <- dis.historical.data(c(screener$Ticker),Sys.Date()-365,Sys.Date(),0)
prices.all <- hd.all$Historical
temp.all <- prices.all %>%
  select(Ticker,RefDate,RetClose)
hPrices.all <- dis.historical.turnHorizontal(temp.all) %>%
  filter(RefDate >= Sys.Date()-30)
hPrices.clean.all <- dis.historical.cleanMissing(hPrices.all)


#industry lists
temp <- screener %>% 
  select(Sector,Industry) %>%
  arrange(Sector,Industry)
sectorList <- temp[!duplicated(temp),] %>%
  `rownames<-`(c())
sectors <- c(unique(sectorList$Sector))
for(i in seq(sectors)) {
  ind_list <- as.data.frame(matrix('NA',1,1)) %>%
    `colnames<-`(c(paste0("Sector: ",sectors[i])))
  ind_list <- ind_list[-1,]
  ind <- sectorList %>%
    filter(Sector == sectors[i]) %>%
    select(Industry) %>%
    `colnames<-`(c(paste0("Sector: ",sectors[i])))
  ind_list <- rbind(ind_list,ind)
  write_csv(ind_list,paste0("./sector_industry_lists/",sectors[i],".csv"))
}
