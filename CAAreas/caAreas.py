import pandas as pd
 
# reading csv file
df1 = pd.read_csv("CommAreas.csv")

df2 = df1[[ 'AREA_NUMBE','COMMUNITY']]


df2.to_csv("FinalCommAreas.csv",index=False)

