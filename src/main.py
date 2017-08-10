#!/usr/bin/env python3

import settings
import pandas as pd
from os import path
from data_prep import *


# Load data from the listings .csv file
listings_path = path.join(settings.paths.DATA, settings.files.LISTINGS)
listings_columns = ['id', 'ask', 'bedrooms', 'title', 'address', 'post_at', 'created_at', 'updated_at', 'source_id', 'survey_id', 'latitude', 'longitude']

raw_listings = pd.read_csv(listings_path, names=listings_columns)


# Start processing the listings data
unique_listings = clean_raw_listings(raw_listings)
#duplicate_listings = duplicate_finder(unique_listings)
unique_listings = spatial_locator(unique_listings)
