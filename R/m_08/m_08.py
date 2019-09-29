#!/usr/bin/env python
# coding: utf-8


# m_08
# Supervised learning examples
# Nicholas Horton (nhorton@amherst.edu)

import pandas as pd
import requests
import io
import os

url = "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
dataset = "census.csv"
# url data column names
names = ["age", "workclass", "fnlwgt", "education", "education.num", "marital.status", 
         "occupation", "relationship", "race", "sex", "capital.gain", "capital.loss", 
         "hours.per.week", "native.country", "income"]

r = requests.post(url)
if r.ok:
    data = r.content.decode('utf8')
    df = pd.read_csv(io.StringIO(data), names=names)
else:
    print("Warning: Cannot import the data from URL")

# currnet file directory
wd = os.getcwd()
df.to_csv(os.path.join(wd, dataset), index=False)

# basic information of the dataframe
print(df.head())
print(df.info())
print(df.describe())

# missing data handling
df_missing = (df=='?').sum()
print(df_missing)



from sklearn import preprocessing

# encode categorical variables using label Encoder
# select all categorical variables
df_categorical = df.select_dtypes(include=['object'])
print(df_categorical.head())


# apply label encoder to df_categorical
le = preprocessing.LabelEncoder()
df_categorical = df_categorical.apply(le.fit_transform)
print(df_categorical.head())




# Drop earlier duplicate columns which had categorical values
df = df.drop(df_categorical.columns,axis=1)
# Concatenate df_categorical dataframe with original dataframe
df = pd.concat([df,df_categorical],axis=1)
print(df.head())


# convert target variable income to categorical
df['income'] = df['income'].astype('category')

# look at column type
print(df.info())


from sklearn.model_selection import train_test_split


# Putting independent variables/features to X
X = df[["capital.gain"]]
# Putting response/dependent variable/feature to y
y = df["income"]
# Splitting the data into train and test
X_train,X_test,y_train,y_test = train_test_split(X,y,test_size=0.20,random_state=99)


# percent for train data
print(y_train.value_counts(normalize=True) * 100)



# Importing decision tree classifier from sklearn library
from sklearn.tree import DecisionTreeClassifier

# Fitting the decision tree with default hyperparameters, apart from
# max_depth which is 5 so that we can plot and read the tree.
dt_default = DecisionTreeClassifier(max_depth=5)
dt_default.fit(X_train,y_train)



# Importing classification report and confusion matrix from sklearn metrics
from sklearn.metrics import classification_report,confusion_matrix,accuracy_score

# making predictions
y_pred_default = dt_default.predict(X_test)

# Printing classifier report after prediction
print(classification_report(y_test,y_pred_default))


# Printing confusion matrix and accuracy
print(confusion_matrix(y_test,y_pred_default))
print(accuracy_score(y_test,y_pred_default))


# Before do below things, you have to install graphviz
# If you're on windows:
# Specifing path for dot file.
os.environ["PATH"] += os.pathsep + 'C:/Program Files (x86)/graphviz-2.38/bin/'
os.environ['PATH'] = os.environ['PATH']+';'+os.environ['CONDA_PREFIX']+r"\Library\bin\graphviz"



# Importing required packages for visualization
from IPython.display import Image  
from sklearn.externals.six import StringIO  
from sklearn.tree import export_graphviz
import pydotplus, graphviz

# Putting features
features = ["capital.gain"]
features

# If you're on windows:
# Specifing path for dot file.
import os
os.environ["PATH"] += os.pathsep + 'C:/Program Files (x86)/graphviz-2.38/bin/'
os.environ['PATH'] = os.environ['PATH']+';'+os.environ['CONDA_PREFIX']+r"\Library\bin\graphviz"

# plotting tree
dot_data = StringIO()  
export_graphviz(dt_default, out_file=dot_data,
                feature_names=features, filled=True,rounded=True)

graph = pydotplus.graph_from_dot_data(dot_data.getvalue())  
Image(graph.create_png())


# like ggplot in R, set the hi_cap_gains
df.loc[df["capital.gain"] >= 5095.5, "hi_cap_gains"] = "True"
df.loc[df["capital.gain"] < 5095.5, "hi_cap_gains"] = "False"

import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np

# to make marker bigger as more and more overapped, count the overlapped data
df_counts = df.groupby(["capital.gain", "income"]).size().reset_index(name='counts')
df_counts["counts"] = np.where(df_counts["counts"] > 1000, 0, df_counts["counts"])

# Draw Stripplot
fig, ax = plt.subplots(figsize=(16,10), dpi= 80)    
sns.stripplot(df_counts["capital.gain"], df_counts["income"], size=df_counts.counts*0.2, ax=ax)
plt.axvline(x=5095.5, color='pink', ls='--', zorder=2)

# Decorations
plt.title('Counts Plot - Size of circle is bigger as more points overlap', fontsize=22)
plt.show()