# Created by: Pariya Pourmohammadi
# Date: June/09/17
# This code does data preparation including n-gerams, and B of W, studio identification


options(scipen=999)

require(plyr)
require(ggplot2)
require(stringr)
require(stringdist)
require(ngram)
require(sp)
require(rgdal)
require(raster)

clean_raw_listing <- function(listing){

  #Convert date strings to numeric
  listing$post_date <- as.Date(listing$post_at)
  listing$created_date <- as.Date(listing$created_at)
  listing$updated_date <- as.Date(listing$updated_at)

#setwd('/opt/output')
#write.csv(listing, 'date_out.csv')
#stop()
  
  original_title <- listing$title
  listing <- cbind(listing[,1:4],original_title,listing[,5:12])
  
  #remove characters
  listing$title <- gsub("[[:punct:]]", " ", listing$title)
  
  #Remove duplicate titles
  listing <- listing[!(listing$title == 'None'), ]
  listing$uniqueid <- paste(listing$ask,listing$bedrooms,listing$title,listing$latitude,listing$longitude)
  listing <- subset(listing[!duplicated( listing$uniqueid), ])
  
  #drop outliers
  listing <- subset(listing, as.numeric(listing$ask) >= 400 & as.numeric(listing$ask) <= 50000)
  

  listing$title <- gsub("[0-9]*+BR+[0-9]*+BA", "[0-9]*+ BR +[0-9]*+ BA", listing$title, fixed=TRUE)
  listing$title <- gsub(" [0-9]*+BR ", " [0-9]*+  BR ", listing$title, fixed=TRUE)
  listing$title <- gsub(" [0-9]*+BA ", " [0-9]*+  BA ", listing$title, fixed=TRUE)
  listing$title <- gsub('"'," ",listing$title)
  
  listing$title <- toupper(listing$title)
  listing$title <- iconv(listing$title, "UTF-8", "ASCII", sub = " " )
  
  listing$title <- gsub("\n", " ", listing$title)
  listing$title <- gsub("BR ", " BEDROOM ", listing$title)
  listing$title <- gsub("BA ", " BATHROOM ", listing$title)
  listing$title <- gsub("AVAIL ", " AVAILABLE ", listing$title)
  listing$title <- gsub(" SQ ", " SQUARE ", listing$title)
  listing$title <- gsub(" HT ", " HEAT ", listing$title)
  listing$title <- gsub(" HW ", " HOT WATER ", listing$title)
  listing$title <- gsub(" W D ", " WASHER/DRYER ", listing$title)
  listing$title <- gsub(" ST ", " STREET ", listing$title)
  listing$title <- gsub("BD ", " BEDROOM ", listing$title)
  listing$title <- gsub("BED ", " BEDROOM ", listing$title)
  listing$title <- gsub("BDRM", " BEDROOM ", listing$title)
  listing$title <- gsub("BRS ", " BEDROOM ", listing$title)
  listing$title <- gsub("BEDROOMS ", " BEDROOM ", listing$title)
  listing$title <- gsub("RED LINE ", " TRANSIT ", listing$title)
  listing$title <- gsub("BLUE LINE ", " TRANSIT ", listing$title)
  listing$title <- gsub("ORANGE LINE ", " TRANSIT ", listing$title)
  listing$title <- gsub("GREEN LINE ", " TRANSIT ", listing$title)
  listing$title <- gsub("SILVER LINE ", " TRANSIT ", listing$title)
  listing$title <- gsub("REDLINE ", " TRANSIT ", listing$title)
  listing$title <- gsub("BLUELINE ", " TRANSIT ", listing$title)
  listing$title <- gsub("ORANGELINE ", " TRANSIT ", listing$title)
  listing$title <- gsub("GREENLINE ", " TRANSIT ", listing$title)
  listing$title <- gsub("SILVERLINE ", " TRANSIT ", listing$title)
  listing$title <- gsub("BUS ROUTE ", " BUSROUTE ", listing$title)
  listing$title <- gsub("BUSROUTE ", " BUSROUTE ", listing$title)
  listing$title <- gsub(" T ", " TRANSIT ", listing$title)
  listing$title <- gsub("INC ", " INCLUDED ", listing$title)
  listing$title <- gsub("INCLD ", " INCLUDED ", listing$title)
  listing$title <- gsub("INCL ", " INCLUDED ", listing$title)
  listing$title <- gsub("INCLUDING ", " INCLUDED ", listing$title)
  listing$title <- gsub(" A ", " 1 ", listing$title)
  listing$title <- gsub(" MILES ", " MILE ", listing$title)
  listing$title <- gsub(" APT ", " APARTMENT ", listing$title)
  listing$title <- gsub(" WANTED ", " NEEDED ", listing$title)
  listing$title <- gsub(" SPACIOUS ", " LARGE ", listing$title)
  listing$title <- gsub(" NEAT ", " CLEAN ", listing$title)
  listing$title <- gsub(" GORGEOUS ", " BEAUTIFUL ", listing$title)
  listing$title <- gsub(" JP ", " JAMAICA PLAIN ", listing$title)
  listing$title <- gsub(" BLDG ", " BUILDING ", listing$title)
  listing$title <- gsub(" PRKNG ", " PARKING ", listing$title)
  listing$title <- gsub(" BDS ", " BEDROOM ", listing$title)
  listing$title <- gsub(" HWD ", " HARDWOOD ", listing$title)
  
  listing$title <- paste0(" ",listing$title)
  listing$title <- paste0(listing$title, " ")

  numbers <- c(" 1 ", " 2 " ," 3 "," 4 "," 5 "," 6 "," 7 "," 8 "," 9 ", " 10 ")
  numbers_str <- c(" ONE ", " TWO " ," THREE "," FOUR "," FIVE "," SIX "," SEVEN "," EIGHT "," NINE ", " TEN ")

  for (i in (1: length(numbers))){
    listing$title  <- gsub(numbers[i], numbers_str[i], listing$title )
  }

  while(any(str_detect(listing$title, "  "))){
    listing$title <- gsub("  ", " ", listing$title)
  }

  listing <- listing[which(listing$source_id == 2 | (listing$source_id==1 & (str_detect(listing$title, "APARTMENT")
            |str_detect(listing$title, "BED")|str_detect(listing$title, "STUDIO")
            |str_detect(listing$title, "LOFT")|str_detect(listing$title, "CONDO")
            |str_detect(listing$title, "HOUSE")|str_detect(listing$title, "BUILDING")
            |str_detect(listing$title, "UNIT")|str_detect(listing$title, "ROOM")
            |str_detect(listing$title, "BATH")))), ]

  wrd_lst <- c(" ON "," FOR "," A "," AT "," WHERE "," TO "," THE "," OF ",
               " WHEN ", " WITH ", " AND ")
  
  for (i in (1: length(wrd_lst))){
    cl_title  <- gsub(wrd_lst[i], " ", listing$title )
  } 
  
  listing <- cbind(listing[,1:5],cl_title,listing[,6:14])
  
  return(listing)
}

#compute the Jaro–Winkler distance between each title and the 10 next
#if less than 0.15 check the price and # of bedrooms if they are identical 
#label the record as duplicate

Dupllicate_finder <- function(listing){

  dup_indices <- list()
  
  for (i in (1:(length(listing[,1])-10))){
    tmp2<- NULL      
    tmp1 <- NULL

    temp1_dist <- stringdist(listing$title[i],listing$title[(i+1):(i+10)], method = "jw", p=0.1)
    tmp1 <- i+ which(temp1_dist< 0.15)
    tmp1 <- tmp1[which(listing$ask[i] == listing$ask[tmp1] )]
    
    if(length(tmp1 > 0)) {
      tmp2 <- tmp1[which(listing$bedrooms[i] == listing$bedrooms[tmp1])]
    }

    if ((length(tmp2)>0))  {
      dup_indices[length(dup_indices)+1] <- list(c(i, tmp2))
      
    } 
  }
  
  duplicate_listing_indices <- reshape2:: melt(dup_indices)
  names(duplicate_listing_indices) <- c("index", "group")
  duplicate_listing_indices <- duplicate_listing_indices[-which(duplicated(duplicate_listing_indices$index)),]
  
  duplicate_listing <-listing[duplicate_listing_indices$index,]
  group <- duplicate_listing_indices$group
  duplicate_listing <- cbind(duplicate_listing, group)
  
  return(duplicate_listing)
}



##########transfor the text into bag of words (1,2,3,4,5 grams)#############
n_gram_builder <- function(listing,val){
  titles <-listing$cl_title
  
  large_txt <- paste(titles, collapse=" :::: ")

  #uni_grams
  if (val==1) {
    col_names <- c("ngrams", "Freq")
    comb_one <- strsplit(titles,split = " ")
    wrd_gram <- data.frame(table(unlist(comb_one)))
    names(wrd_gram) <- col_names
    wrd_gram <- wrd_gram[str_which(wrd_gram$ngrams, "[^[:digit:]]"), ]
    wrd_gram$prop <- wrd_gram$Freq/sum(wrd_gram$Freq)
  }
  
  #ngram
  else if (val > 1) {
    comb_two <- ngram(large_txt, n = val, sep = " ")
    wrd_gram <- data.frame(get.phrasetable(comb_two))
    wrd_gram <- wrd_gram[-(which(str_detect(wrd_gram$ngrams, ":"))),]
    wrd_gram$prop <- wrd_gram$freq/sum(wrd_gram$freq)
  }
  
  else {print("The value is not acceptable")}
  
  return (wrd_gram)
}


################### to associate the town and census tract ID ########################
###################################################################################
spatial_locator <- function (listing) {
  #check is the files exist in the directory of dsn throw
  #error if not available with releavant message and exit the mothod
  
  list.files('/opt/data/spatial', pattern='\\.shp$')
  
  try(if (file.exists('/opt/data/spatial/towns_MA.shp') == FALSE) 
                      stop("Towns boundary file not available!"))
  
  try(if (file.exists('/opt/data/spatial/comm_type.shp') == FALSE) 
                      stop("Community type boundary file not available!"))
  
  try(if (file.exists('/opt/data/spatial/census_tract.shp') == FALSE) 
                       stop("Census tract boundary file not available!"))
  
  try(if (file.exists('/opt/data/spatial/1partner_city_nhoods.shp') == FALSE) 
                     stop("Level 1 neighborhood file not available"))
  
  try(if (file.exists('/opt/data/spatial/2Bos_neighborhoods.shp') == FALSE) 
                      stop("Level 2 neighborhood file not available!"))
  
  try(if (file.exists('/opt/data/spatial/3MARKET AREAS NEW_region.shp') == FALSE) 
                       stop("Level 3 neighborhood file not available!"))
  
  #read shape files of source boundaries including towns, census tracts, neighborhoods
  towns.shape <- readOGR(dsn=path.expand("/opt/data/spatial"), layer ="towns_MA")
  
  comm_type.shape <- readOGR(dsn=path.expand("/opt/data/spatial"), layer ="comm_type")
  
  tract.shape <- readOGR(dsn=path.expand("/opt/data/spatial"), layer ="census_tract")
  
  neighborhoods.shape <- readOGR(dsn=path.expand("/opt/data/spatial"), layer ="1partner_city_nhoods")
  
  bos_neighborhoods_2.shape <- readOGR(dsn=path.expand("/opt/data/spatial"), layer ="2Bos_neighborhoods")
  
  bos_neighborhoods_3.shape <- readOGR(dsn=path.expand("/opt/data/spatial"), layer ="3MARKET AREAS NEW_region")

  #project the shapefiles to NAD83 
  CRS.new <- CRS( "+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000
                  +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  proj4string(towns.shape) <- CRS.new
  
  
  tract.shape <- spTransform(tract.shape, CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 
                                          +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m
                                          +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  comm_type.shape <-  spTransform(comm_type.shape, CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 
                                                +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m
                                                +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  neighborhoods.shape <- spTransform(neighborhoods.shape, CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 
                                          +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m
                                          +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  bos_neighborhoods_2.shape <- spTransform(bos_neighborhoods_2.shape, CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 
                                          +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m
                                          +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  bos_neighborhoods_3.shape <- spTransform(bos_neighborhoods_3.shape, CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 
                                          +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m
                                          +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  
  #read lat long attributes of the listing records and create event points of WGS1984 geographic projection 
  event.Points <- SpatialPoints(data.frame(latitude = listing$latitude, longitude= listing$longitude),
                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  
  #reproject the points to NAD83
  event.Points <- spTransform(event.Points, CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 
                                          +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m
                                          +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  #overlay any of the boundaries with the listing records
  pnt_towns.shape <- over(event.Points,towns.shape)
  pnt_comm_type.shape <- over(event.Points,comm_type.shape)
  pnt_tract.shape <-over(event.Points,tract.shape) 
  pnt_nei_1.shape <-over(event.Points,neighborhoods.shape) 
  pnt_nei_2.shape <-over(event.Points,bos_neighborhoods_2.shape) 
  pnt_nei_3.shape <-over(event.Points,bos_neighborhoods_3.shape) 
  
  #extract the names from each overlay
  town <- pnt_towns.shape$town
  comm_type <- pnt_comm_type.shape$commtype
  ct10_id <- pnt_tract.shape$ct10_id
  neighborhood_01 <- pnt_nei_1.shape$Neighborho
  neighborhood_02 <- pnt_nei_2.shape$Name
  neighborhood_03 <- pnt_nei_3.shape$SUB_HOOD
  
  #add the boundaries to the listings
  listing <- cbind(listing, town, comm_type, ct10_id, neighborhood_01, neighborhood_02,neighborhood_03)
  
  #the records with no vcommunity type are kept out of the dataset
  listing <- listing[-which(is.na(listing$comm_type)) , ]
  
  return(listing)
}

################ studio identification function/writes studio records to #####################
#################a file and returns a list excluding studio records###########################
##############################################################################################
studio_analysis <- function(listing){
  listing <- listings_unique
  tmp <- listing
  tmp$numRooms <- tmp$bedrooms
  tmp$numRooms[which(tmp$numRooms==0 |tmp$numRooms==1)] <- -1
  
  #Find price threshold for Studio for the whole dataset
  tmp$studio <- 0
  tmp$studio[str_detect(tmp$title, 'STUDIO')] <- 1
  
  studio_recs <- tmp[which(tmp$studio==1),]
  
  studio_statistics <- Threshold_finder(studio_recs)
  
  median_studio_neightborhood <- tapply(studio_recs$ask, studio_recs$neighborhood_01, FUN = median)
  sd_studio_neighborhood <- tapply(studio_recs$ask, studio_recs$neighborhood_01, FUN = sd)
  upper_bound_neighborhood <- median_studio_neightborhood + 2*sd_studio_neighborhood
  lower_bound_neighborhood <- median_studio_neightborhood - 2*sd_studio_neighborhood
  num_studios_neighborhoods <- as.data.frame(table(unlist(studio_recs$neighborhood_01)))

  upper_bound_neighborhood <- reshape2 :: melt(upper_bound_neighborhood)
  lower_bound_neighborhood <- reshape2 :: melt(lower_bound_neighborhood)
  
  range_neighborhood <- data.frame(neighborhood = upper_bound_neighborhood$Var1, count = num_studios_neighborhoods$Freq,lower_bound = lower_bound_neighborhood$value, upper_bound =upper_bound_neighborhood$value)
  
  
  temp <- tmp[which(tmp$bedrooms==0 & tmp$source_id== 2 & tmp$studio==0 & tmp$ask>lower_studio & tmp$ask<upper_studio),] 
  temp1 <- tmp[which(tmp$bedrooms==0 & tmp$source_id== 1 & tmp$studio==0 & tmp$ask>lower_studio & tmp$ask<upper_studio),] 
  
  studio_recs <- rbind(studio_recs,temp, temp1)
  
  #update threshold 
  threshold_stUdio_median <- median(studio_recs[studio_recs$studio==1, ]$ask)
  threshold_stUdio_sd <- sd(studio_recs[studio_recs$studio==1, ]$ask)
  lower_studio <- threshold_stUdio_median - 2*threshold_stUdio_sd
  upper_studio <- threshold_stUdio_median + 2*threshold_stUdio_sd
  
  range_neighborhood$in_range_lower <- 0
  range_neighborhood$in_range_upper <- 0
  range_neighborhood$in_range_lower[which(lower_bound_neighborhood$value<lower_studio)] <- -1
  range_neighborhood$in_range_upper[which(upper_bound_neighborhood$value>upper_studio)] <- +1

  
  text <- paste("The lower and upper bound of the asking price for the studios are $" , round(lower_studio, digits = 0) ," and $", round(upper_studio, digits = 0), 
                " and the median value of studio rent for the whole MA is $",threshold_stUdio_median , sep = "")
  
  studio_recs_cln <- studio_recs[which(studio_recs$ask > lower_studio & studio_recs$ask < upper_studio),]
  studio_prop <- length(studio_recs_cln[,1])/length(studio_recs[,1])
  
  tmp <- tmp[-(which(tmp$id %in% studio_recs$id)),]
  
  median_studio_towns <- tapply(studio_recs_cln$ask, studio_recs_cln$town, FUN = median)
  num_studios_towns <- as.data.frame(table(unlist(studio_recs_cln$town)))
  
  median_studio_towns <- reshape2:: melt(median_studio_towns)
  
  line="THIS IS A SUMMARY STATISTICS OF STUDIOS BASED ON TOWNS"
  studio_stat <- data.frame(town = median_studio_towns$Var1, median_value = median_studio_towns$value, frequency = num_studios_towns$Freq)
  
  setwd("/opt/output")
  
  Head <- line
  sink("studio_stat_town.txt", append = TRUE)
  Head
  text
  studio_stat
  sink()

  write.csv(tmp, "no_studio_records.csv")
  write.csv(studio_recs_cln, "studios.csv")
  
  setwd("/usr/src/app")
  
  return (tmp)
}

#returns a vector of median standard deviation and lower and upper bound based on 2 standard deviations
Threshold_finder <- function (listing){
  
  #Set threshold to 2std dev higher and lower than median
  threshold_stUdio_median <- median(listing[listing$studio==1, ]$ask)
  threshold_stUdio_sd <- sd(listing[listing$studio==1, ]$ask)
  lower_studio <- threshold_stUdio_median - 2*threshold_stUdio_sd
  upper_studio <- threshold_stUdio_median + 2*threshold_stUdio_sd
  
  result <- data.frame(median = threshold_stUdio_median, sdev =threshold_stUdio_sd,
                       lower_bound = lower_studio, upper_bound = upper_studio)
  
  return(result)
}


################ add new dimensions to the data to do clustering #####################
######################################################################################

#clustering_data_prep <- function(listing) {
#  listing<- listings_unique
  #the keywords extracted from the ngrams are used to construct the new data table with new attributes
#  word_list <- c("ONE BEDROOM", "TWO BEDROOM", "THREE BEDROOM", "FOUR BEDROOM", "FIVE BEDROOM",
#                 "SIX BEDROOM", "SEVEN BEDROOM", "EIGHT BEDROOM", "NINE BEDROOM", "TEN BEDROOM", "ROOMMATE", "ONE BEDROOM ONE BATHROOM APARTMENT",
#                 "ONE BATHROOM APARTMENT","TWO BATHROOM APARTMENT","FIVE BATHROOM APARTMENT","THREE BEDROOM APARTMENT","TWO BEDROOM APARTMENT",
#                 "ONE BEDROOM APARTMENT","FOUR BEDROOM APARTMENT","FIVE BEDROOM APARTMENT","SIX BEDROOM APARTMENT",
#                 "ONE BEDROOM ONE BATHROOM APARTMENT", "TWO BEDROOM ONE BATHROOM APARTMENT", "THREE BEDROOM ONE BATHROOM APARTMENT","TWO BEDROOM TWO BATHROOM APARTMENT",
#                 "THREE BEDROOM TWO BATHROOM APARTMENT","FOUR BEDROOM TWO BATHROOM APARTMENT","FOUR BEDROOM ONE BATHROOM APARTMENT",
#                 "FIVE BEDROOM TWO BATHROOM APARTMENT", "ONE BEDROOM ONE BATHROOM CONDO", "TWO BEDROOM ONE BATHROOM HOUSE", 
#                 "NINE BEDROOM THREE BATHROOM APARTMENT", "TEN BEDROOM THREE BATHROOM")
  
#  for (i in (1:length(word_list))){
#    listing[ , word_list[i]] <- 0
#    listing[which(str_detect(listing$cl_title, word_list[i])) , word_list[i]] <- 1
#  }
  
  #different community types are considered as new boolean dimensions
#  community_type <- as.vector(unique(listing$comm_type))
#  for (i in (1:length(community_type))){
#    comm_type_var <- community_type[i]
#    listing[ , comm_type_var] <- 0
#    listing[which(str_detect(listing$comm_type,comm_type_var)) , community_type[i]] <- 1
#  }
  
  #number of bedrooms in the dataset is used to create new dimensions
#  for (i in (1:10)){
#    var <- as.character(i)
#    listing[ , var] <- 0
#    listing[which(listing$bedrooms==i) , var] <- 1
#  }
  
  #the asking price value is broken into 10 quantiles
#  breaks <- classInt :: classIntervals(listing$ask,10,style = "quantile")
  
#  listing$asking_gr<-c( "first", "second", "third", "forth", "fifth",
#                   "sixth","seventh","eighth","ninth","tenth")[
#                     findInterval(listing$ask , as.vector(breaks$brks)) ]
#  
#  asking_breaks <- c( "first", "second", "third", "forth", "fifth",
#     "sixth","seventh","eighth","ninth","tenth")
#  
  #based on the quantile of the price each point falls into the new dimensions are formed
#  for (i in (1:length(asking_breaks))){
#    var <- asking_breaks[i]
#    listing[ , var] <- 0
#    listing[which(str_detect(listing$asking_gr , var)) , var] <- 1
#  }
#  
  #the attributes that are not required will be removed from the dataset
#  listing_names <- names(listing)
#  listing_names <- listing_names[2:24]
#  listing[,listing_names] <- NULL
#  listing$asking_gr<- NULL
#  
#  return(listing)
#}
