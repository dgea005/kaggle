import numpy as np
import pandas as pd
from scipy.ndimage import imread
from glob import glob
from os.path import basename

files = glob('data/train/*_mask.tif')
average_mask = imread(files[0])
average_mask.fill(0)
average_mask = average_mask.astype(np.float32)

for file in files:
    average_mask += imread(file)
    
average_mask /= 255
max_value = average_mask.max()
koeff = 0.5
average_mask[average_mask < koeff * max_value] = 0
average_mask[average_mask >= koeff * max_value] = 255
average_mask = average_mask.astype(np.uint8)

(pd.DataFrame(average_mask).
    to_csv('data/average_mask.csv', 
    index=False, header=False))
