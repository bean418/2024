# -*- coding: utf-8 -*-
"""04_LogisticRegression.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1wQ5SESiHK9ZNxBI57C3ymWeK_XLAyV95
"""

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("LogisticRegressionData.csv")
X = df.iloc[:, :-1].values
y = df.iloc[:, -1].values

"""## Seperate data"""

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 0)

"""## Machine learning"""

from sklearn.linear_model import LogisticRegression
cf = LogisticRegression() # cf : classifier
cf.fit(X_train, y_train)

"""## Predict pass of fail when one studied 6 hours."""

cf.predict([[6]]) # 합격

cf.predict_proba([[6]]) # 불합격 확률, 합격 확률

"""## Predict pass of fail when one studied 4 hours."""

cf.predict([[4]]) # 불합격

cf.predict_proba([[4]])

"""## Predict about test sets"""

y_pred = cf.predict(X_test)
y_pred

y_test

X_test

cf.score(X_test, y_test)
# 전체 테스트 세트 중 분류 예측을 성공한 비율, 즉 3/4 = 0.75

"""## Visualization"""

# 데이터가 작기 때문에 부드러운 곡선을 위한 전처리
X_new = np.arange(min(X), max(X), 0.1)
X_new

b = cf.intercept_
m = cf.coef_
y = m * X_new + b
p = 1 / (1 + np.exp(-y)) # sigmoid
p

p.shape

X_new.shape

p = p.reshape(-1) # dimension : auto, -> 4*4 행렬을 2*8 행렬로 바꾸는 등 원소의 개수가 같은 data shape을 자동으로 reshape함.

plt.scatter(X_train, y_train, color = "blue")
plt.plot(X_new, p, color = "green")
plt.plot(X_new, np.full(len(X_new), 0.5), color = "red") # X_new의 개수만큼 0.5로 가득찬 배열 만들기
plt.title("Probability by hours")
plt.xlabel('hours')
plt.ylabel("P")
plt.show()

"""## Visualization (Test set)"""

plt.scatter(X_test, y_test, color = "blue")
plt.plot(X_new, p, color = "green")
plt.plot(X_new, np.full(len(X_new), 0.5), color = "red") # X_new의 개수만큼 0.5로 가득찬 배열 만들기
plt.title("Probability by hours (test)")
plt.xlabel('hours')
plt.ylabel("P")
plt.show()

cf.predict_proba([[4.5]])

"""## Confusion Matrix"""

from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test, y_pred)
cm

# (예측, 실제)
# 불,불(음성 : True negative) 합,불(위양성 : False positive)
# 불,합(위음성 : False negative) 합,합(양성 : True positive)