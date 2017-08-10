
######function calls #############
getwd()
setwd("/opt/data")

raw_listings <- read.csv("listings_small.csv", 
                         header=TRUE, col.names=c("id","ask","bedrooms","title","address","post_at","created_at",
                                                   "updated_at","source_id","survey_id","latitude","longitude"))

setwd("/usr/src/app")
source("Data_prep.R")

listings_unique <- clean_raw_listing(raw_listings)

#listingDup <- Dupllicate_finder(listings_unique)

listings_unique <- spatial_locator(listings_unique)
no_studio_listing <- studio_analysis(listings_unique)
clustering_data <- clustering_data_prep(no_studio_listing)


#freq_wrd_one_gram <- n_gram_builder(listings_unique, 1)
#freq_wrd_two_gram <- n_gram_builder(listings_unique,2)
#freq_wrd_three_gram <- n_gram_builder(listings_unique,3)
#freq_wrd_four_gram <- n_gram_builder(listings_unique,4)
#freq_wrd_five_gram <- n_gram_builder(listings_unique,5)
#freq_wrd_six_gram <- n_gram_builder(listings_unique,6)

clustering_data$id <- NULL
clustering_data <- as.matrix(clustering_data)
results <- cluster :: clara(clustering_data, 40, "euclidean")

a <- as.data.frame(results$clustering)
names(a) <- "cluster"
clustered_listing <- NULL
clustered_listing <- cbind(no_studio_listing,a)

table_towns <- data.frame(table(unlist(listings_unique$town)))
table_census_tracts <- data.frame(table(unlist(listings_unique$ct10_id)))
table_neighborhoods1 <- data.frame(table(unlist(listings_unique$neighborhood_01)))
table_neighborhoods2 <- data.frame(table(unlist(listings_unique$neighborhood_02)))
table_neighborhoods3 <- data.frame(table(unlist(listings_unique$neighborhood_03)))

##############write the wordGrams to file#############################

setwd("/opt/output/")

write.csv(freq_wrd_one_gram, "freq_wrd_one_gram.csv")
write.csv(freq_wrd_two_gram, "freq_wrd_two_gram.csv")
write.csv(freq_wrd_three_gram, "freq_wrd_three_gram.csv")
write.csv(freq_wrd_four_gram, "freq_wrd_four_gram.csv")
write.csv(freq_wrd_five_gram, "freq_wrd_five_gram.csv")
write.csv(freq_wrd_six_gram, "freq_wrd_six_gram.csv")
write.csv(freq_table, "Frequency table of postings per town.csv")
write.csv(listingDup, "similar_records.csv")

write.csv(table_towns, "table_towns.csv")
write.csv(table_census_tracts, "table_census_tracts.csv")
write.csv(table_neighborhoods1, "table_neighborhoods1.csv")
write.csv(table_neighborhoods2, "table_neighborhoods2.csv")
write.csv(table_neighborhoods3, "table_neighborhoods3.csv")
