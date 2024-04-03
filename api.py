#!/usr/bin/env python

# make sure to install these packages before running:
# pip install pandas
# pip install sodapy

import pandas as pd
from sodapy import Socrata

# Unauthenticated client only works with public data sets. Note 'None'
# in place of application token, and no username or password:
client = Socrata("www.datos.gov.co", None)

# Example authenticated client (needed for non-public datasets):
# client = Socrata(www.datos.gov.co,
#                  MyAppToken,
#                  userame="user@example.com",
#                  password="AFakePassword")

# First 2000 results, returned as JSON from API / converted to Python list of
# dictionaries by sodapy.
results = client.get("gt2j-8ykr", limit=1000000)

# Convert to pandas DataFrame
results_df = pd.DataFrame.from_records(results)
results_df.to_csv("staying/data_api.csv")
print("Process end")


# data Bogotá

import requests
import json
import pandas as pd

params = params={
    'resource_id': 'b64ba3c4-9e41-41b8-b3fd-2da21d627558', 
    'limit': 1000000,
}
url = 'https://datosabiertos.bogota.gov.co/api/3/action/datastore_search'
r = requests.get(url, params=params).json()

df = pd.DataFrame(r['result']['records'])
df.to_csv('staying/data_api_bog.csv')
print("Process end")