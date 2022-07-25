import pandas as pd
import matplotlib.pyplot as plt

# generate data from dumpCsv
df=pd.read_csv('clocks.csv')
df[['eb01','eb02','eb10','eb12','eb20','eb21']].plot()
plt.xlabel('Time (ps)')
plt.ylabel('Elastic buffer occupancy')
plt.title('Step size 1')

plt.show()

df[['clk0','clk1','clk2']].plot()
plt.xlabel('Time (ps)')
plt.ylabel('Period')
plt.title('Step size 1')

plt.show()
