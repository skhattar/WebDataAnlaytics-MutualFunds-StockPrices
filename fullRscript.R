library(readxl)
library(Quandl)
library(sqldf)

#substitute in your API key
Quandl.api_key('zxh7xKvwtap6WxT-Tgg4')

#read in excel file
keywords <- read_excel("E:/Downloads/Keyword Count.xlsx", 
                       col_types = c("numeric", "text", "text", 
                                     "date", "text", "text", "text", "text", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

#new column names
colnames <- c("unnamed","industry","company","filing_date","filing_type",
              "weblink","html","ticker",
              "f1","f2","f3","f4","f5","f6","f7","f8","f9","f10",
              "f11","f12","f13","f14","f15","f16","f17","f18","f19","f20")

#create lookup table for new names to old names, swap new names into df
name_lookup <- data.frame(colnames, names(keywords))
names(name_lookup) <- c("colname", "factor")
names(keywords) <- colnames

#convert date to character
keywords$filing_date <- substr(as.character(keywords$filing_date), 1,10)

tick_date <- sqldf("SELECT DISTINCT 
                   filing_date, ticker 
                   FROM 
                   keywords")

#create df with stock info for each company and filing date
#Sys.sleep to stay below API limit of 2000 calls per 10 minutes
stock_data <- Quandl.datatable('WIKI/PRICES', date = '2017/12/03', ticker='AAPL')

for (i in seq(1,1800)) {
    stock_data <- rbind(stock_data,
                        Quandl.datatable('WIKI/PRICES', 
                                         date=tick_date$filing_date[i],
                                         ticker=tick_date$ticker[i]))
}

Sys.sleep((600))

for (i in seq(1801,3600)) {
    stock_data <- rbind(stock_data,
                        Quandl.datatable('WIKI/PRICES', 
                                         date=tick_date$filing_date[i],
                                         ticker=tick_date$ticker[i]))
}

Sys.sleep((600))

for (i in seq(3601,5003)) {
    stock_data <- rbind(stock_data,
                        Quandl.datatable('WIKI/PRICES', 
                                         date=tick_date$filing_date[i],
                                         ticker=tick_date$ticker[i]))
}

stock_data$date <- substr(as.character(stock_data$date), 1,10)

analysis_df <-   sqldf("SELECT
                            a.*, b.* 
                        FROM
                            stock_data AS a
                            LEFT JOIN
                                keywords AS b
                                    ON  a.date   = b.filing_date 
                                    AND a.ticker = b.ticker")


library(readr)

#read in csv
analysis_df <- read_csv("~/analysis_df.csv")

analysis_df <- analysis_df[,-1]
analysis_df <- sqldf("SELECT * FROM analysis_df ORDER BY ticker, date DESC")

#drop companies with data integrity issues
to_keep <- as.data.frame(table(analysis_df$ticker))

to_keep <- subset(to_keep, Freq < 8)

analysis_df <-   sqldf("SELECT 
                       b.* 
                       FROM 
                       to_keep AS a 
                       LEFT JOIN 
                       analysis_df AS b
                       ON a.Var1 = b.ticker")

rm(to_keep)

#add in index and calculate out for each entry
analysis_df$indexer = 1

for (i in seq(2,nrow(analysis_df))) {
    analysis_df$indexer[i] <- ifelse(analysis_df$ticker[i]==analysis_df$ticker[i-1],
                                     analysis_df$indexer[i-1]+1,
                                     analysis_df$indexer[i])
}

#associate each entry with the one following it in time
to_attach <- sqldf("SELECT
                   ticker,
                   date,
                   adj_close,
                   indexer
                   FROM
                   analysis_df
                   ORDER BY
                   ticker, date DESC")

to_attach$date <- substr(as.character(to_attach$date),1,10)

attached <-  sqldf("SELECT
                   a.ticker,
                   a.date,
                   a.adj_close AS adj_close_1,
                   b.adj_close AS adj_close_2
                   FROM 
                   analysis_df AS a
                   INNER JOIN analysis_df AS b
                   ON a.indexer==b.indexer-1
                   AND a.ticker==b.ticker")

#calculate yearly growth rate
attached$growth <- (attached$adj_close_2 / attached$adj_close_1 - 1) * 100

model_df <- analysis_df[,c(1,2,16,17,seq(23,42))]
model_df[is.na(model_df)] <- 0

#convert predictive factors to binary
for (i in seq(5,length(model_df))) {
    model_df[,i] <- (model_df[,i]>0)+0
}

#merge growth rate into model_df
model_df <-  sqldf("SELECT
                   a.*,
                   b.growth
                   FROM
                   model_df AS a
                   INNER JOIN attached AS b
                   ON a.ticker==b.ticker
                   AND a.date==b.date")

rm(attached, analysis_df, to_attach, i)

#create empty data frame to populate with results
results <- data.frame(factors=names(model_df)[5:24],
                      p_value=1,
                      estimate=0)

#only these factors have enough non-zero values for a good prediction
for (i in c(1,2,5,7,9,12,13,15,18,20)){
    model <- glm(model_df$growth ~ model_df[,i+4])
    results$p_value[i] <- summary(model)$coefficients[,4][2]
    results$estimate[i] <- model$coefficients[2]
}

#remove used terms from for loop
rm(model, i)

results <-   sqldf("SELECT
                        a.*,
                        b.factor
                    FROM
                        results AS a
                        LEFT JOIN name_lookup AS b
                            ON a.factors==b.colname")

View(results)