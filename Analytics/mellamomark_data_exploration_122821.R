# import packages
library(tidyverse)
library(tableone)
library(jsonlite)
library(ggplot2)
library(reshape)

# set path
filePath <- "~/repos/github/charlie_dao/Ethermorelore/"

##################
# NFT Owner Data #
##################

# import data
nftOwners <- read.csv(paste0(filePath, "nft_owners_2021-10-28.csv"))

# helper functions
extractNftMetadata <- function(value) {
  if (is.na(value)) {
    return(NA)
  }
  nestedColumns <- c("attributes.trait_type", "attributes.value")

  # extract json data
  jsonDf <- data.frame(fromJSON(value, flatten = TRUE))

  # flatten and transpose nested data values into individual columns
  nonNestedData <- unique(jsonDf[, !names(jsonDf) %in% nestedColumns])
  nestedData <- unique(jsonDf[, names(jsonDf) %in% nestedColumns]) %>%
    spread(nestedColumns[1], nestedColumns[2])

  # concat non-nested data and transposed nested data into a single row
  # returning as list so that it can be placed into a dataframe and later expanded
  flattenedJsonList <- list(c(nonNestedData, nestedData))

  return(flattenedJsonList)
}

cleanNftOwners <- function(nftOwners) {
  nftOwnersCopy <- data.frame(nftOwners)

  finalDf <- nftOwnersCopy %>%
    rowwise() %>%
    mutate(metadataList = extractNftMetadata(metadata)) %>%
    unnest_wider(metadataList, names_repair = "unique") %>%
    na_if(., "") %>%
    rename(., "name" = "name...11", "Attribute Name" = "name...16")
}

# cleaning data
nftOwnersClean <- cleanNftOwners(nftOwners)
nftOwnersClean$`Rarity Score` <- as.numeric(nftOwnersClean$`Rarity Score`)

# summary stats
interestingColumns <- c(
  "amount",
  "contract_type",
  "name",
  "symbol",
  "seller_fee_basis_points",
  "Alignment",
  "Background",
  "Class",
  "Item",
  "Race",
  "Race Varient",
  "Rarity Score",
  "Subclass",
  "Title"
)

CreateTableOne(
  data = nftOwnersClean,
  vars = interestingColumns,
  includeNA = TRUE
)

# viz - histograms
histPlot <- ggplot(nftOwnersClean, aes(x = `Rarity Score`)) +
  geom_histogram(color = "gray") +
  ggtitle("Ethermorelore NFTs: Rarity Score Histogram")

print(histPlot)

# viz - bar plots
barPlotColumns <- c(
  "Alignment",
  "Background",
  "Class",
  "Item",
  "Race",
  "Race Varient",
  "Subclass",
  "Title"
)

for (col in barPlotColumns) {
  barPlotDf <- nftOwnersClean %>%
    select(col) %>%
    drop_na()

  barPlot <- ggplot(barPlotDf, aes(fct_infreq(get(col)))) +
    geom_bar() +
    coord_flip() +
    ggtitle(paste0("Ethermorelore NFTs: ", col, " Count"))

  print(barPlot)
}

#############################
# NFT Sales and Social Data #
#############################

# import data
rawFollowers <- read.csv(paste0(filePath, "raw_followers_2021-10-25.csv"))
salesHistory <- read.csv(paste0(filePath, "sales_history_2021-10-28.csv"))
salesWithFollowers <- read.csv(paste0(filePath, "sales_with_followers_2021-10-28.csv"))
tweets <- read.csv(paste0(filePath, "tweets_2021-10-25.csv"))

# viz - histograms
histPlotDf <- rawFollowers %>%
  mutate(follower_count_10k = replace(follower_count, follower_count > 10000, 10001))

histPlot <- ggplot(histPlotDf, aes(x = follower_count_10k)) +
  geom_histogram(color = "gray") +
  ggtitle("Ethermorelore Twitter Followers: Follower Count (0-10k+)")
print(histPlot)

histPlotDf <- rawFollowers %>%
  filter(follower_count > 10000)

histPlot <- ggplot(histPlotDf, aes(x = follower_count)) +
  geom_histogram(color = "gray") +
  ggtitle("Ethermorelore Twitter Followers: Follower Count (>10k)") +
  scale_x_continuous(breaks = seq(10000, max(histPlotDf$follower_count), 200000))
print(histPlot)

# viz - time series
salesWithFollowers$date <- as.Date(salesWithFollowers$date)
timeSeriesPlotDf <- melt(salesWithFollowers, id.vars = "date")
timeSeriesPlotDf$logValue <- log(timeSeriesPlotDf$value + 1)

timeSeriesPlot <- ggplot(
  timeSeriesPlotDf[timeSeriesPlotDf$date > as.Date('2021-07-31'), ],
  aes(x = date, y = value, color = variable)
  ) +
  geom_line() +
  xlab("") +
  scale_x_date(date_labels = "%m-%Y") +
  ggtitle("Ethermorelore Sales and Social")
print(timeSeriesPlot)

timeSeriesPlot <- ggplot(
  timeSeriesPlotDf[timeSeriesPlotDf$date > as.Date('2021-07-31'), ],
  aes(x = date, y = logValue, color = variable)
) +
  geom_line() +
  xlab("") +
  scale_x_date(date_labels = "%m-%Y") +
  ggtitle("Ethermorelore Sales and Social (Log)")
print(timeSeriesPlot)
