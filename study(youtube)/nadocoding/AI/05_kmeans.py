# -*- coding: utf-8 -*-
"""05_Kmeans.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/19qlpf_W3R-gNK7tnPrPCPmtxOULqo6Np
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

df = pd.read_csv("KMeansData.csv")
df.head()

X = df.iloc[:, :].values
X[:5]

"""## visualization"""

plt.scatter(X[:, 0], X[:, 1])
plt.title("score by hours")
plt.xlabel("hours")
plt.ylabel("score")
plt.show()

"""## Visualization 2"""

plt.scatter(X[:, 0], X[:, 1], color = "skyblue")
plt.title("score by hours")
plt.xlabel("hours")
plt.ylabel("score")
plt.xlim(0, 100)
plt.ylim(0, 100)
plt.show()

# X와 y의 range가 다르기 때문에 데이터 간 거리의 왜곡이 생기는 것을 방지
# range를 통일한 결과, 데이터 간 X의 차이는 작고 y의 차이가 큰 것을 알 수 있다.

"""## Feature Scaling"""

from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X = sc.fit_transform(X)
X[:5]

"""## Visualization 3"""

plt.figure(figsize = (6,6))
plt.scatter(X[:, 0], X[:, 1])
plt.title("score by hours (after scaling)")
plt.xlabel("hours")
plt.ylabel("score")
plt.show()

"""## Find optimal K (Elbow Method)"""

from sklearn.cluster import KMeans
inertia_list = []
for i in range(1, 11):
    kmeans = KMeans(n_clusters = i, init = "k-means++", random_state = 0)
    kmeans.fit(X)
    inertia_list.append(kmeans.inertia_)

# Unsupervised learning

plt.plot(range(1, 11), inertia_list)
plt.title("Elbow Method")
plt.xlabel("n_cluster")
plt.ylabel("inertia")
plt.show()

"""## 최적의 k값은 4이다."""

K = 4
kmeans = KMeans(n_clusters=K, random_state = 0) # init = "kmeans++" (default)
# kmeans.fit(X)
y_kmeans = kmeans.fit_predict(X) # X 데이터를 이용하여 학습 후 예측되는 값을 반환 -> 적합 + 예측값 동시 진행

y_kmeans[:5]

"""## Visualization using optimal K"""

centers = kmeans.cluster_centers_ # Coordinates of centroid
centers

for cluster in range(K):
    plt.scatter(X[y_kmeans == cluster, 0], X[y_kmeans == cluster, 1], s = 100, edgecolor = "black")
    plt.scatter(centers[cluster, 0], centers[cluster, 1], s = 300,
                edgecolor = "black", color = "yellow", marker = "s")
    plt.text(centers[cluster, 0], centers[cluster, 1], cluster + 1,
             va = "center", ha = "center")

plt.title("Score by hours")
plt.xlabel("hours")
plt.ylabel("score")
plt.show()

"""## Visualization (Original Data)"""

X_org = sc.inverse_transform(X)
X_org[:5]

centers_org = sc.inverse_transform(centers)
centers_org

for cluster in range(K):
    plt.scatter(X_org[y_kmeans == cluster, 0], X_org[y_kmeans == cluster, 1], s = 100, edgecolor = "black")
    plt.scatter(centers_org[cluster, 0], centers_org[cluster, 1], s = 300,
                edgecolor = "black", color = "yellow", marker = "s")
    plt.text(centers_org[cluster, 0], centers_org[cluster, 1], cluster + 1,
             va = "center", ha = "center")

plt.title("Score by hours")
plt.xlabel("hours")
plt.ylabel("score")
plt.show()

"""## 1번 그룹: 시간은 적지만 점수가 높음 -> 공부 효율이 좋다.
## 2번 그룹: 시간은 많지만 점수가 낮음 -> 공부를 제대로 했는가? 집중력이 약한가?
## 3번 그룹: 공부시간이 많고 점수도 높음.
## 4번 그룹: 공부시간이 적고 점수도 낮음.

## Feature scaling을 통해서 정확한 클러스터링을 하는 것이 중요하다.
"""