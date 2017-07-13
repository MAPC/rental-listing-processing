import re
import string
import settings
import pandas as pd
from os import path
from datetime import datetime
from jellyfish import jaro_winkler


def clean_raw_listings(listings):
  """
    @param DataFrame listings

    @return DataFrame
  """

  # Convert date strings to numeric
  listings['post_date']  = listings['post_at'].apply(lambda x: x.split()[0])
  listings['created_date'] = listings['created_at'].apply(lambda x: x.split()[0])
  listings['updated_date'] = listings['updated_at'].apply(lambda x: x.split()[0])

  # Filter listing titles and duplicates
  listings['original_title'] = listings['title']
  listings['title'] = listings['title'].apply(lambda title: title.translate(str.maketrans('', '', string.punctuation)))

  listings = pd.DataFrame(listings[listings['title'] != 'None'])
  listings['unique_id'] = listings.loc[:, ['ask', 'bedrooms', 'title', 'latitude', 'longitude']].astype(str).sum(axis=1)
  listings = listings.groupby('unique_id').first().reset_index()

  # Drop outliers
  listings = listings[(listings['ask'].astype(int) >= 400) & (listings['ask'].astype(int) <= 50000)]


  regex_title_transformations = {
    "[0-9]*BR[0-9]*BA": "[0-9]* BR [0-9]* BA",
    "[0-9]*BR[0-9]*": "[0-9]* BR [0-9]*",
    "[0-9]*BR": " [0-9]* BR ",
    "[0-9]*BA": " [0-9]* BA ",
  }

  for replacement, expression in regex_title_transformations.items():
    listings['title'] = listings['title'].apply(lambda x: re.sub(expression, replacement, x.upper()))

  # Pad the title
  listings['title'] = listings['title'].apply(lambda x: " " + x + " ")

  fixed_title_transformations = {
    ' ': ['\n', '"'],

    'APARTMENT': [' APT '],
    'AVAILABLE': ['AVAIL '],
    'BATHROOM': ['BA '],
    'BEAUTIFUL': [' GORGEOUS '],
    'BEDROOM': ['BR ', 'BD ', 'BED ', 'BDRM', 'BRS ', 'BEDROOMS ', ' BDS '],
    'BUILDING': ['BLDG'],
    'BUSROUTE': ['BUS ROUTE', 'BUSROUTE'],
    'CLEAN': [' NEAT '],
    'HARDWOOD': [' HWD '],
    'HEAT': [' HT '],
    'HOT WATER': [' HW '],
    'INCLUDED': ['INC ', 'INCLD ', 'INCL ', 'INCLUDING '],
    'JAMAICA PLAIN': [' JP '],
    'LARGE': [' SPACIOUS '],
    'MILE': [' MILES '],
    'NEEDED': [' WANTED '],
    'PARKING': [' PRKNG '],
    'SQUARE': [' SQ '],
    'STREET': [' ST '],
    'TRANSIT': ['RED LINE ', 'BLUE LINE ', 'ORANGE LINE ', 'GREEN LINE ', 'SILVER LINE ', 'REDLINE ', 'BLUELINE ', 'ORANGELINE ', 'GREENLINE ', 'SILVERLINE ', ' T '],
    'WASHER/DRYER': [' W D '],

    'ONE': [' 1 ', ' A '],
    'TWO': [' 2 '],
    'THREE': [' 3 '],
    'FOUR': [' 4 '],
    'FIVE': [' 5 '],
    'SIX': [' 6 '],
    'SEVEN': [' 7 '],
    'EIGHT': [' 8 '],
    'NINE': [' 9 '],
    'TEN': [' 10 '],
  }

  for replacement, words in fixed_title_transformations.items():
    padded_replacement = ' '+replacement+' '

    for word in words:
      listings['title'] = listings['title'].str.replace(word, padded_replacement)


  # Only take the listings that satisfy our language criteria
  listings = listings[
      (listings['source_id'] == 2)
      | ((listings['source_id'] == 1) 
        & (
          (listings['title'].str.contains('BEDROOM'))
          | (listings['title'].str.contains('APARTMENT'))
          | (listings['title'].str.contains('STUDIO'))
          | (listings['title'].str.contains('LOFT'))
          | (listings['title'].str.contains('CONDO'))
          | (listings['title'].str.contains('HOUSE'))
          | (listings['title'].str.contains('BUILDING'))
          | (listings['title'].str.contains('UNIT'))
          | (listings['title'].str.contains('ROOM'))
          | (listings['title'].str.contains('BATH'))
        )
      )
  ]

  # Remove any multi-spaced phrases and cruft words from the titles
  listings['title'] = listings['title'].apply(lambda x: ' '.join(x.split()))
  listings['clean_title'] = listings['title'].apply(lambda x: re.sub(' ON | FOR | A | AT | WHERE | TO | THE | OF | WHEN | WITH | AND ', ' ', x))

  return listings


def duplicate_finder(listings):
  """
    Compute the Jaro–Winkler distance between each title and the 
    following 10. If less than 0.15 check the price and number of 
    bedrooms. If they are identical, label the record as duplicate.

    @param DataFrame listings

    @return 
  """

  duplicate_indices = []



  return listings


def n_gram_builder(listing, value):
  """
    Transform the text into bags of words.

    @param listing
    @param value

    @return
  """


  return listing
