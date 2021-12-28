# import packages
library(tidyverse)
library(tableone)
library(jsonlite)
library(ggplot2)

# import data
filePath <- "~/repos/github/charlie_dao/Ethermorelore/"
nftOwners <- read.csv(paste0(filePath, "nft_owners_2021-10-28.csv"))
# ownedPerOwner <- read.csv(paste0(filePath, "owned_per_owner_2021-10-28.csv"))
rawFollowers <- read.csv(paste0(filePath, "raw_followers_2021-10-25.csv"))
salesHistory <- read.csv(paste0(filePath, "sales_history_2021-10-28.csv"))
salesWithFollowers <- read.csv(paste0(filePath, "sales_with_followers_2021-10-28.csv"))
tweets <- read.csv(paste0(filePath, "tweets_2021-10-25.csv"))

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
  data=nftOwnersClean,
  vars=interestingColumns,
  includeNA=TRUE
  )

# viz - histograms
ggplot(nftOwnersClean, aes(x=`Rarity Score`)) + 
  geom_histogram(binwidth=1, color = "black", fill = "gray") +
  ggtitle('Ethermorelore NFTs: Rarity Score Histogram')

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


# viz - bar plots
for (col in barPlotColumns) {
  barPlotDf <- nftOwnersClean %>%
    select(col) %>%
    drop_na()
  
  barPlot <- ggplot(barPlotDf, aes(fct_infreq(get(col)))) +
    geom_bar() +
    coord_flip() +
    ggtitle(paste0('Ethermorelore NFTs: ', col, ' Count'))
  
  print(barPlot)
}
