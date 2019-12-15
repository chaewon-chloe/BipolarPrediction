## Bipolar Prediction

Project Description: This project is to predict the suicidality of bipolar disorder I patients with their event-related potential (ERP) data. The ERPs were measured during a response inhibition paradigm (standard Go/NoGo tasks). Three machine learning algorithms - Support vector machine, XGboost, and linear discriminant analysis - were performed. 

### Code lists
* __DataPreparation_Demographics.R:__ 
* __VariableSelection.R:__
* __Functions.R:__ Two manually implemented functions 
  * __SMOTE_data:__
  * __Divide_folds:__
* __DNN.R:__ A Deep Nueral Network (DNN) model using Keras
* __LogisticRegression.R:__ A Logistic regreesion analaysis with SMOTE and cross validation, and some manual function for the logistic regression from scratch.
* __OtherMLmodels.R:__ Functions for KNN, QDA, LDA, SVM and RandomForest
* __Figures.R:__
