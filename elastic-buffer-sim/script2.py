import pandas as pd
import matplotlib.pyplot as plt

# generate data from dumpCsv
df0=pd.read_csv('clocks0.csv')
df1=pd.read_csv('clocks1.csv')

df0.set_index('t', inplace=True)
df1.set_index('t', inplace=True)
df=pd.concat([df0,df1],axis=0)

# df[['eb01','eb10']].plot()
df[['eb01','eb10']].plot()
plt.xlabel('Time (ps)')
plt.ylabel('Elastic buffer occupancy')
plt.title('Step size 1')

# plt.show()
plt.savefig('image1.png')

df[['clk0','clk1']].plot()
plt.xlabel('Time (ps)')
plt.ylabel('Period')
plt.title('Step size 1')

# plt.show()
plt.savefig('image2.png')
