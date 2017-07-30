colors = {
 'a': 'seagreen',
 'aa': 'plum',
 'ab': 'darkgreen',
 'ac': 'cyan',
 'ad': 'mediumorchid',
 'ae': 'gold',
 'af': 'm',
 'ag': 'rosybrown',
 'ah': 'orange',
 'ai': 'lightskyblue',
 'aj': 'khaki',
 'ak': 'firebrick',
 'al': 'teal',
 'am': 'mediumspringgreen',
 'an': 'midnightblue',
 'ao': 'pink',
 'ap': 'limegreen',
 'aq': 'limegreen',
 'ar': 'slategray',
 'as': 'saddlebrown',
 'at': 'darkmagenta',
 'au': 'grey',
 'av': 'lavender',
 'aw': 'silver',
 'ax': 'burlywood',
 'ay': 'sandybrown',
 'az': 'darkolivegreen',
 'b': 'black',
 'ba': 'navajowhite',
 'bc': 'lightslategray',
 'c': 'darkorchid',
 'd': 'greenyellow',
 'e': 'lime',
 'f': 'g',
 'g': 'k',
 'h': 'red',
 'i': 'deeppink',
 'j': 'mediumblue',
 'k': 'springgreen',
 'l': 'darkorange',
 'm': 'rebeccapurple',
 'n': 'lightpink',
 'o': 'darkblue',
 'q': 'powderblue',
 'r': 'yellow',
 's': 'green',
 't': 'tomato',
 'u': 'turquoise',
 'v': 'thistle',
 'w': 'chartreuse',
 'x': 'lightcoral',
 'y': 'mediumaquamarine',
 'z': 'darkviolet',
 'p': 'navy'}





import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.preprocessing import LabelEncoder, OneHotEncoder

# Importing the dataset
datatrain = pd.read_csv(r'C:\Users\Admin\Documents\Kaggle\Mercedez\train.csv')
datatrain = datatrain.iloc[:,1:]
#datatrain = datatrain[ ((datatrain['y']> 75) & (datatrain['y']<160))]


y_train = datatrain.iloc[:,0].values
#datatrain = datatrain.iloc[:,1:]
datatest = pd.read_csv(r'C:\Users\Admin\Documents\Kaggle\Mercedez\test.csv')
datatest = datatest.iloc[:,1:]

#==============================================================================
# for x0 in list(np.unique(datatrain["X0"])):
#     datatrain.loc[((datatrain["X0"]==x0) & (datatrain["y"]>160)),["y"]]=\
#     np.median(datatrain.loc[datatrain["X0"]==x0,["y"]])
#==============================================================================

cols = [1,2,3,4,5,6,7,8]
datatrain.drop(datatrain.columns[cols],axis=1,inplace=True)

cols = [0,1,2,3,4,5,6,7]
datatest.drop(datatest.columns[cols],axis=1,inplace=True)


 
dataset = pd.concat([datatrain,datatest],axis=0)



#X = dataset.iloc[:, 2:379].values
#==============================================================================
# X = dataset.iloc[:, 2:4].values
# y = dataset.iloc[:, 1:2].values
#==============================================================================

from sklearn.model_selection import train_test_split
train_data = dataset[(dataset["y"].notnull()) & (dataset["y"]<=200) ]
y_train = train_data.iloc[:,368].values
test_data = dataset[dataset["y"].isnull()]

#==============================================================================
# from sklearn.model_selection import train_test_split
# train_data, test_data = train_test_split(final_data, test_size=.5)
# 
#==============================================================================
    


X = np.array(train_data.drop("y",axis=1))







# Feature Scaling
#==============================================================================
# from sklearn.preprocessing import StandardScaler
# sc_X = StandardScaler()
# X_train = sc_X.fit_transform(X)
# sc_y = StandardScaler()
# y_train = sc_y.fit_transform(y)
#==============================================================================



# Fitting Random Forest Regression to the dataset
from sklearn.ensemble import RandomForestRegressor
#==============================================================================
# from sklearn.model_selection import GridSearchCV
# from keras import backend as K
#==============================================================================
#==============================================================================
# regressor = RandomForestRegressor(random_state = 0,oob_score = True)
# n_estimators = [450, 500,550]
# min_samples_leaf = [20, 30, 40, 50]
# param_grid = dict(n_estimators=n_estimators, min_samples_leaf=min_samples_leaf)
# grid = GridSearchCV(estimator=regressor, param_grid=param_grid)
# grid_result = grid.fit(X, y_train)
# grid_result.best_score_
# grid_result.best_params_
#==============================================================================

regressor = RandomForestRegressor(random_state = 0,oob_score = True,
                                  n_estimators=500,min_samples_leaf=40,
                                  max_depth=6,max_features=0.7)

regressor.fit(X, y_train)
regressor.score(X, y_train)
y_prediction = regressor.predict(X)






def r2_keras(y_true, y_pred):
    SS_res =  np.sum(np.square( y_true - y_pred )) 
    SS_tot = np.sum(np.square( y_true - np.mean(y_true) ) ) 
    return ( 1 - SS_res/(SS_tot) )
r2_keras(y_train,y_prediction)


# Predicting a new result
X_test = np.array(test_data.drop("y",axis=1))
y_prediction = regressor.predict(X_test)

ID = list(range(1,4210))
pd.DataFrame({'ID':ID,'y':y_prediction}).to_csv(r"C:\Users\Admin\Documents\Kaggle\Mercedez\predictions_rf_nocat_final.csv")
