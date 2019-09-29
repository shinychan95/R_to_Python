#!/usr/bin/env python
# coding: utf-8


import os
import warnings
import numpy as np 
import pandas as pd 

dataset = "presidential.csv"

# currnet file directory
wd = os.getcwd()

# check the dataset is in the directory
if not dataset in os.listdir(wd):
    warnings.warn("There is no input data", UserWarning)


# file list in current folder
print(os.listdir("./"))


# Pandas Data Structure - DataFrame
# The dataframe is a two-dimensional data structure. It contains columns.
df = pd.read_csv(dataset)
type(df)


# It is important to check data type of each column
df.info()

df.shape

df.columns

df.describe()

df.head()

# df.iloc[[n],[n]]
# This code brings the data in the N row and N column in the DataFrame.
df.iloc[[0],[3]]

# df.loc[n:n]
# This code allows us to fetch the data in the range we specify.
df.loc[5:7]


# like select in R & SQL
df[["name", "party"]]


df[df["party"] == "Republican"]
# OR
filters = df["party"] == "Republican"
df[filters]



# Because the column ["start"] is "String" data type.
# NO
# df[(df["start"] > 1973) & (df["party"] == "Democratic")]
# YES
df[(df["start"] > "1973") & (df["party"] == "Democratic")]


# if data type is String, there are several ways to control data
# Ex. upper, lower, split, replace, (con)cat ...
df["start"].str.split("-", expand=True, n=1) # n is limit the number of splits

# isin & isnull is also good method to use.
df[df["party"].isin(["Democratic"])].head()


# mutate() in R -> Defining New Column
# And to calculate the interval of date
# We should change the data type string to datetime64
df["start"] = pd.to_datetime(df["start"])
df["end"] = pd.to_datetime(df["end"])
df.info()


# to control date very well, we can figure out how to control data type "datetime64"
df["length"] = (df["end"]-df["start"]).astype('timedelta64[Y]')


# some function just return changed DataFrame.
# So, we need to put the changed DataFrame into previous one
df['elected'] = df['start'].map(lambda x: x.year) + 1


df.loc[(df["elected"] == 1962) | (df["elected"] == 1973), "elected"] = 'NA' 


df = df.rename(columns= {'length' : 'term_length'})


df.sort_values('term_length', ascending=False)


# Define the aggregation calculations
aggregations = {
    "party": { # work on the "party" column
        "N": "count" 
    },
    "start": {     # Now work on the "date" column
        'min_date': "min",
    },
    "end": {
        "max_date": "max" # Find the max, call the result "max_date"
    },
    "term_length": "mean"
}
# Perform groupby aggregation by "month", but only on the rows that are of type "call"
df.groupby('party').agg(aggregations)



Teams = pd.read_csv("Teams.csv")

Teams.head()

Teams.info()

mets = Teams[Teams["teamID"] == "NYN"]

myMets = mets[(mets["yearID"] >= 2004) & (mets["yearID"] <= 2012)]

myMets = myMets[["yearID", "teamID", "W", "L"]]


myMets
# followed codes in the textbook are the same method




# Join method, it is simple in python
flights = pd.read_csv("flights.csv")

flights.info()

flights.head()

airlines = pd.read_csv("airlines.csv")

airlines.head()

flightJoined = pd.merge(left=flights,right=airlines, left_on="carrier", right_on="carrier")

flightJoined[["carrier", "name", "flight", "origin", "dest"]]

flightLeftJoined = pd.merge(left=flights,right=airlines, how='left', left_on="carrier", right_on="carrier")




# Review
Teams = Teams[["yearID", "teamID", "W", "L", "R", "RA"]]

Teams = Teams[(Teams["teamID"] == "NYN") & (Teams["yearID"] >= 2004) & (Teams["yearID"] <= 2012)]

Teams = Teams.rename(columns= {'R' : 'RS'})

Teams["WPct"] = Teams["W"] / (Teams["W"] + Teams["L"])

Teams["WPct_hat"] = 1 / (1 + (Teams["RA"]/Teams["RS"])*(Teams["RA"]/Teams["RS"]))

Teams["W_hat"] = Teams["WPct_hat"] * (Teams["W"] + Teams["L"])

conditions = [
    (Teams["yearID"] == 2004),
    (Teams["yearID"] >= 2011)]

choices = ["Duquette", "alderson"]

Teams["gm"] = np.select(conditions, choices, default="Minaya")

Teams.head()

# Define the aggregation calculations
aggregations = {
    "gm": { # work on the "party" column
        "N": "count" 
    },
    "W": {
        "total_W": "sum"
    },
    "L": {
        "total_L": "sum"
    },
    "WPct": {
        "total_WPct": "sum"
    }
}
# Perform groupby aggregation by "month", but only on the rows that are of type "call"
Teams = Teams.groupby("gm").agg(aggregations)

