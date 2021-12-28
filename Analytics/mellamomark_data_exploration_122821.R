# import packages
library(tidyverse)
library(tableone)
library(jsonlite)

# import data
filePath <- "~/repos/github/charlie_dao/Ethermorelore/"
nftOwners <- read.csv(paste0(filePath, "nft_owners_2021-10-28.csv"))
ownedPerOwner <- read.csv(paste0(filePath, "owned_per_owner_2021-10-28.csv"))
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
    unnest_wider(metadataList, names_repair = "minimal") %>%
    na_if(., "")
}

# cleaning data
nftOwnersClean <- cleanNftOwners(nftOwners)
