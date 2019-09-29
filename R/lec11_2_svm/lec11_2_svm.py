#!/usr/bin/env python
# coding: utf-8

import os
import warnings
import pandas as pd # data processing, CSV file I/O
import matplotlib.pyplot as plt
import seaborn as sns

dataset = "iris.csv"
# currnet file directory
wd = os.getcwd()
# check the dataset is in the directory
if not dataset in os.listdir(wd):
    warnings.warn("There is no input data", UserWarning)

# read the dataset
iris=pd.read_csv(os.path.join(wd, dataset))

# show the dataset
iris.head()

# Creating a pairplot to visualize the similarities and especially difference between the species
sns.pairplot(data=iris, hue='Species', palette='Set2')




from sklearn.model_selection import train_test_split
from sklearn.svm import SVC
# Importing the classification report and confusion matrix
from sklearn.metrics import classification_report, confusion_matrix

# input features
x=iris.iloc[:,:-1]
# target variable
y=iris.iloc[:,4]

# training (100) & test set (50)
x_train,x_test, y_train, y_test=train_test_split(x,y,test_size=1/3)

# default kernel is RBF kernel(radial basis function kernel)
m1 = SVC(gamma="auto")
m2 = SVC(gamma="auto", kernel="poly")
m3 = SVC(gamma="auto", kernel="sigmoid")
m4 = SVC(gamma="auto", kernel="linear")

# svm using kernel
print("Model, kernel function: radial basis function")
print(m1.fit(x_train, y_train))
print("number of support vectors for each class: ", m1.n_support_)

print("Model, kernel function: polynomial function")
print(m2.fit(x_train, y_train))
print("number of support vectors for each class: ", m2.n_support_)

print("Model, kernel function: sigmoid function")
print(m3.fit(x_train, y_train))
print("number of support vectors for each class: ", m3.n_support_)

print("Model, kernel function: linear function")
print(m4.fit(x_train, y_train))
print("number of support vectors for each class: ", m4.n_support_)

# measure accuracy
pred11=m1.predict(x_test)
pred12=m2.predict(x_test)
pred13=m3.predict(x_test)
pred14=m4.predict(x_test)


print(confusion_matrix(y_test, pred11))
print(classification_report(y_test, pred11))

print(confusion_matrix(y_test, pred12))
print(classification_report(y_test, pred12))

# If we compare the performance of the different types of kernels 
# we can clearly see that the sigmoid kernel performs the worst.
# This is due to the reason that sigmoid function returns two values, 0 and 1.
# Therefore it is more suitable for binary classification problems. 
# However, in our case we had three output classes.
print(confusion_matrix(y_test, pred13))
print(classification_report(y_test, pred13))

print(confusion_matrix(y_test, pred14))
print(classification_report(y_test, pred14))

pd.crosstab(pred14, y_test, margins=True)

