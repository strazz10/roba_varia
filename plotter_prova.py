import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

## plot per il gen di numeri gaussiani
filename = 'gaussdata.dat'  
data = np.loadtxt(filename, comments='#')

#assuming two columns
x_data = data[:, 0]
y_data = data[:, 1]
x_fit = np.linspace(min(x_data) - 1, max(x_data) + 1, 500)
bin_width = np.abs(x_data[0]-x_data[1])

gaussian = norm.pdf(x_fit, 0, 1) 

plt.figure(figsize=(8, 5))

plt.plot(x_data, y_data/((bin_width)*np.sum(y_data)), linewidth=2,color='skyblue', label='Normalized Data') 
plt.plot(x_fit, gaussian, 'r-', linewidth=2, label='Normalized Gaussian')

#labels and legend
plt.title('Data vs Normalized Gaussian Distribution')
plt.xlabel('Value')
plt.ylabel('Probability Density')
plt.legend()
plt.grid(True)

plt.tight_layout()
plt.show()
