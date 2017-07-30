import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.preprocessing import LabelEncoder, OneHotEncoder

# Importing the dataset
datatrain = pd.read_csv(r'C:\Users\Admin\Documents\Kaggle\Mercedez\train.csv')
datatrain = datatrain.iloc[:,1:]
y_train = datatrain.iloc[:,0].values
#datatrain = datatrain.iloc[:,1:]
datatest = pd.read_csv(r'C:\Users\Admin\Documents\Kaggle\Mercedez\test.csv')
datatest = datatest.iloc[:,1:]
dataset = pd.concat([datatrain,datatest],axis=0)

#X = dataset.iloc[:, 2:379].values
#==============================================================================
# X = dataset.iloc[:, 2:4].values
# y = dataset.iloc[:, 1:2].values
#==============================================================================

catcols = []
for x in dataset.columns:
    if ((dataset[x].dtype != "int64") & (dataset[x].dtype != "float64")):
        catcols.append(x)
catcols

noncatcols = [x for x in dataset.columns if x not in catcols]


newnames = []
le = LabelEncoder()
for column in catcols:
    le.fit(dataset[column])
    dataset.loc[:,column + "_cat"] = le.transform(dataset[column])
    newnames.append(column+"_cat")
len(newnames)

dataset.drop(catcols,axis=1,inplace=True)

colnames = []
for name in newnames:
    for i in dataset[name].unique():
        colnames.append(name+"_"+str(i))
len(colnames)


enc = OneHotEncoder()
dataset_new = pd.DataFrame(enc.fit_transform(dataset[newnames]).toarray(), columns = colnames,dtype=int)
dataset_old = dataset[noncatcols]

final_data = pd.concat([dataset_new.reset_index(drop=True),dataset_old.reset_index(drop=True)],axis=1)

train_data = final_data[final_data["y"].notnull()]
test_data = final_data[final_data["y"].isnull()]
#==============================================================================
# from sklearn.model_selection import train_test_split
# train_data, test_data = train_test_split(final_data, test_size=.5)
# 
#==============================================================================
X = np.array(train_data.drop("y",axis=1))
y = train_data.loc[:,["y"]].values


#==============================================================================
# # Splitting the dataset into the Training set and Test set
# from sklearn.model_selection import train_test_split
# X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.5, random_state = 0)
# 
#==============================================================================
# Feature Scaling
#==============================================================================
# from sklearn.preprocessing import StandardScaler
# sc = StandardScaler()
# X_train = sc.fit_transform(X_train)
# X_test = sc.transform(X_test)
#==============================================================================

# Part 2 - Now let's make the ANN!


from keras import backend as K

def r2_keras(y_true, y_pred):
    SS_res =  K.sum(K.square( y_true - y_pred )) 
    SS_tot = K.sum(K.square( y_true - K.mean(y_true) ) ) 
    return ( 1 - SS_res/(SS_tot + K.epsilon()) )



# Importing the Keras libraries and packages
import keras
from keras.models import Sequential
from keras.layers import Dense, Dropout

# Initialising the ANN
classifier = Sequential()

# Adding the input layer and the first hidden layer
classifier.add(Dense(units = 290, kernel_initializer = 'uniform', activation = 'relu', input_dim = 579))
classifier.add(Dropout(0.2))
# Adding the second hidden layer
classifier.add(Dense(units = 145, kernel_initializer = 'uniform', activation = 'relu'))
classifier.add(Dropout(0.2))

# Adding the output layer
classifier.add(Dense(units = 1, kernel_initializer = 'uniform',activation="linear"))

# Compiling the ANN
classifier.compile(optimizer = 'adam', loss = 'mean_squared_error', metrics = [r2_keras],lr=0.001, decay=1e-6)

# Fitting the ANN to the Training set
classifier.fit(X, y_train, batch_size = 10, epochs = 200)

# Part 3 - Making predictions and evaluating the model
X_test = np.array(test_data.drop("y",axis=1))
y = train_data.loc[:,["y"]].values
y_prediction = classifier.predict(X_test)
pd.DataFrame(y_prediction).to_csv(r"C:\Users\Admin\Documents\Kaggle\Mercedez\predictions.csv")