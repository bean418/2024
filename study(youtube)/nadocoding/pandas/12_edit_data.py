# -*- coding: utf-8 -*-
"""12_Edit_Data.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1o0KyvlHvPXboDC2tnYLalpMwgKduR3Q9
"""

import pandas as pd
import numpy as np

data = {
    '이름' : ['채치수', '정대만', '송태섭', '서태웅', '강백호', '변덕규', '황태산', '윤대협'],
    '학교' : ['북산고', '북산고', '북산고', '북산고', '북산고', '능남고', '능남고', '능남고'],
    '키' : [197, 184, 168, 187, 188, 202, 188, 190],
    '국어' : [90, 40, 80, 40, 15, 80, 55, 100],
    '영어' : [85, 35, 75, 60, 20, 100, 65, 85],
    '수학' : [100, 50, 70, 70, 10, 95, 45, 90],
    '과학' : [95, 55, 80, 75, 35, 85, 40, 95],
    '사회' : [85, 25, 75, 80, 10, 80, 35, 95],
    'SW특기' : ['Python', 'Java', 'Javascript', np.nan, np.nan, 'C', 'PYTHON', 'C#']
}

df = pd.DataFrame(data, index = ["1번", "2번", "3번", "4번", "5번", "6번", "7번", "8번"])
df.index.name = "지원번호"

df_copy = df.copy(deep = True)

"""## Edit Columns"""

df['학교'].replace({'북산고' : '상북고',
                  '능남고' : '무슨고'})

# call by value

df['학교'].replace({'북산고' : '상북고'}, inplace=True)
df

df['SW특기'].str.lower()

df['SW특기'] = df['SW특기'].str.lower()
df

df['SW특기'] = df['SW특기'] = df['SW특기'].str.upper()
df

df['학교'] = df['학교'] + '등학교'
df

"""## Add Column"""

df['총합'] = df['국어'] + df['영어'] + df['수학'] + df['사회'] + df['과학']
df

df['결과'] = 'Fail'

df.loc[df['총합'] >= 400, '결과'] = 'Pass' # loc[row condition, column condition]
df

"""## Drop column"""

df.drop(columns=['총합'])

df.drop(columns=['국어', '영어', '수학'])

"""## Drop row"""

df.drop(index='4번')

filt = df['수학'] < 80
df[filt]

idx = df[filt].index

df.drop(index=idx)

"""## Add row"""

df.loc['9번'] = ['이정환', '해남고등학교', 184, 90, 90, 90, 90, 90,
                'Kotlin', 450, 'Pass']
df

"""## Edit Cell"""

df.loc['4번', 'SW특기'] = 'PYTHON'
df

df.loc['5번', ['학교', 'SW특기']] = ['능남고등학교', 'C']
df

"""## Edit order of columns"""

df.columns

cols = list(df.columns)
cols

df = df[[cols[-1]] + cols[0:-1]]
df

# -> df = df[['결과', '이름', '학교', '키', '국어', '영어', '수학', '과학', '사회', 'SW특기', '총합']]

"""## Edit colnames"""

df = df[['결과', '이름', '학교']]
df

df.columns = ['Result', 'Name', "School"]
df