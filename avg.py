from cpselect.cpselect import cpselect
import sys
import numpy as np
import matplotlib.pyplot as plt
from scipy.spatial import Delaunay
import skimage.io as skio

with open('frontalImgs/%da.pts' % (1), 'r') as file:

    data = file.read().replace('\n', ' ')
a = [float(i) for i in data[26:-3].split()]
x = a[::2]
y = a[1::2]
pts = [[x[i], y[i]] for i in range(len(x))]
pts += [[0,0], [259, 0], [0, 359], [259, 359]] 
tris = Delaunay(np.array(pts))
for i in range(1, 201):
    with open('frontalImgs/%da.pts' % (i), 'r') as file:

        data = file.read().replace('\n', ' ')
    a = [float(i) for i in data[26:-3].split()]
    x = a[::2]
    y = a[1::2]
    pts = [[x[i], y[i]] for i in range(len(x))]
    pts += [[0,0], [259, 0], [0, 359], [259, 359]] 
    imFile = open('frontalImgs/%da.csv' % (i), 'w') 
    for simplex in tris.simplices.copy():
     gaupt1 = pts[simplex[0]]
     gaupt2 = pts[simplex[1]]
     gaupt3 = pts[simplex[2]]
     print ("%f,%f,%f,%f,%f,%f" % (gaupt1[0], gaupt1[1], gaupt2[0], gaupt2[1], gaupt3[0], gaupt3[1],), file=imFile)
    
    print ("Finished %d" % (i))
    
pts = cpselect(sys.argv[1], sys.argv[2])
im1 = skio.imread(sys.argv[1])
im2 = skio.imread(sys.argv[2])
gaupts = np.array([[pt['img1_x'], pt['img1_y']] for pt in pts])
obpoints = np.array([[pt['img2_x'], pt['img2_y']] for pt in pts])
gauTri = Delaunay(gaupts)
plt.figure()
plt.imshow(im1)
plt.triplot(gaupts[:,0], gaupts[:,1], gauTri.simplices.copy())
plt.plot(gaupts[:,0], gaupts[:,1], 'o')
plt.show()
plt.figure()
plt.imshow(im2)
plt.triplot(obpoints[:,0], obpoints[:,1], gauTri.simplices.copy())
plt.plot(obpoints[:,0], obpoints[:,1], 'o')
plt.show()
im1File = open(sys.argv[3], 'w') 
im2File = open(sys.argv[4], 'w') 


for simplex in tris.simplices.copy():
 gaupt1 = gaupts[simplex[0]]
 gaupt2 = gaupts[simplex[1]]
 gaupt3 = gaupts[simplex[2]]
 print ("%f,%f,%f,%f,%f,%f" % (gaupt1[0], gaupt1[1], gaupt2[0], gaupt2[1], gaupt3[0], gaupt3[1],), file=im1File)
 obpt1 = obpoints[simplex[0]]
 obpt2 = obpoints[simplex[1]]
 obpt3 = obpoints[simplex[2]]
 print ("%f,%f,%f,%f,%f,%f" % (obpt1[0], obpt1[1], obpt2[0], obpt2[1], obpt3[0], obpt3[1],), file=im2File)
if __name__ == "__main__":
    # 1. load the image
    # 2. align the two images by calling align_images
    # Now you are ready to write your own code for creating hybrid images!
    print   

