#### Start Of Efficient Frontier ####

# Bring in dependencies
library("httr")
library("jsonlite")
library("keyring")
library("dplyr")
library("ggplot2")
library("plotly")

# Get & Format Price History Data

tickers = c("GOOG", "WMT", "NKE")

for(i in 1:length(tickers)){
  
  reqUrl <- paste0("https://api.stockdata.org/v1/data/eod?symbols=",
                   tickers[i],
                   "&sort=asc&api_token=",
                   keyring::key_get("STOCK_DATA_KEY")
  )
  
  priceHistoryRes <- GET(reqUrl)
  
  tickerPriceData <- data.frame(fromJSON(rawToChar(priceHistoryRes$content))$data)
  
  if(i == 1){
    allData <- tickerPriceData[ , c("date", "close")]
  }else{
    allData <- cbind(allData, tickerPriceData$close)
  }
  
}


colnames(allData) <- c("Date", tickers)



# Get Daily Returns and Summary Data

expectedReturns = NULL
standardDeviations = NULL

for(e in tickers){
  
  newColumnName = paste0(e, "return")
  
  allData <- allData %>%
    mutate(!!newColumnName := (get(e) - lag(get(e))) / lag(get(e)) )
  
  expectedReturns <- cbind(expectedReturns, mean(allData[-1, newColumnName]))
  standardDeviations <- cbind(standardDeviations, sd(allData[-1, newColumnName]))
}

colnames(expectedReturns) <- tickers
colnames(standardDeviations) <- tickers

variances <- standardDeviations**2
modifiedSharpeRatios <- expectedReturns / standardDeviations


# Calculate X an Variance-Covariance Matrices


xDf <- allData[-1,-(1:(length(tickers)+1))] 

colnames(xDf) <- tickers

for(e in tickers){
  xDf <- xDf %>%
    mutate(!!e := get(e) - expectedReturns[1,e])
}

xMatrix <- data.matrix(xDf)
xMatrixTranspose <- t(xMatrix)


varCovar <- (xMatrixTranspose %*% xMatrix) / (nrow(xDf) -1)

# Expected Return and Volatility For Equally weighted portfolio

equalPortfolio <- xDf[1,]

for(e in tickers){
  equalPortfolio <- equalPortfolio %>%
    mutate(!!e := 1/length(tickers))
}

weights <- data.matrix(equalPortfolio)

equalPortfolio$expectedReturn <- sum(weights * expectedReturns)

equalPortfolio$volatility <- sqrt((weights %*% varCovar) %*% t(weights))

equalPortfolio$sharpeRatio <- equalPortfolio$expectedReturn / equalPortfolio$volatility


# Simulate Multiple Portfolio Weights


numOfPortfolios <- 5000

multipleWeight <- xDf[(1:numOfPortfolios),]


for(e in tickers){
  multipleWeight <- multipleWeight %>%
    mutate(!!e := runif(numOfPortfolios))
}

multipleWeight$totalOfRandoms <- rowSums(multipleWeight)

weightColNames <- c()

for(e in tickers){
  newcolumnName <- paste0(e, "weight")
  weightColNames <- c(weightColNames, newcolumnName)
  
  multipleWeight <- multipleWeight %>%
    mutate(!!newcolumnName := get(e) / totalOfRandoms)
}



# Expected Return and Volatility For Different Weighted portfolio


for(i in 1:nrow(multipleWeight)){
  weights <- data.matrix(multipleWeight[i, weightColNames])
  
  multipleWeight[i,("expectedReturn")] <- sum(weights * expectedReturns)
  
  multipleWeight[i,("volatility")] <- sqrt((weights %*% varCovar) %*% t(weights))
}

multipleWeight$sharpeRatio <- multipleWeight$expectedReturn / multipleWeight$volatility

multipleWeight[, c(weightColNames, "expectedReturn", "volatility")] <- round(multipleWeight[, c(weightColNames, "expectedReturn", "volatility")] * 100, 4)

# Generate Interactive Efficeint Frontier Chart


generalPlot <- function(data,knownaes) {
  match_aes <- intersect(names(data), knownaes)
  my_aes_list <- purrr::set_names(purrr::map(match_aes, rlang::sym), match_aes)
  my_aes <- rlang::eval_tidy(quo(aes(!!!my_aes_list)))
  return(my_aes)
}

graph <- ggplot(multipleWeight, aes(x=volatility, y=expectedReturn)) +
  geom_point(aes(color=sharpeRatio))+
  generalPlot(multipleWeight, weightColNames)+
  scale_colour_gradient(low = "red", high = "blue") +
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        axis.line=element_line(color="white"),
        text = element_text(color = "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
  )+
  xlab("Volatility (%)")+
  ylab("Expected Daily Return (%)")+
  ggtitle("Efficient Frontier (Modern Portfolio Theory)")


ggplotly(graph)





#### End Of Efficient Frontier ####