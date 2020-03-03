## Part One 
### Finite Difference Operator
In this part of the project, we explore the gradient of an image. This is represented by the convolutions 
$$D_x = \begin{bmatrix}
1 & -1
\end{bmatrix}$$ and 
$$D_y =\begin{bmatrix}
1 \\
-1 
\end{bmatrix}$$
Here is the cameraman image we will be working with: 

![Cameraman](data2/cameraman.jpg){ height=400 }

This is the image convolved with $D_x$

![Cameraman * $D_x$](data2/cameradx.jpg){ height=400 }
  
This is the image convolved with $D_y$

![Cameraman * $D_y$](data2/camerady.jpg){ height=400 }

We can see that $D_y$ highlights vertical gradients and $D_x$ highlights horizontal gradients. We can construct gradient vectors at every point by taking $[ D_x, D_y ]$. We can plot the magnitude of this vector to combine both the horizontal and vertical edges. 

![$D_x ^2 + D_y ^2$](data2/cameramag.jpg){ height=400 }

To roughly find all of the edge in the image, we can binarize the magnitide of gradient image. Here is the image with threshold $0.4$

![Binarized Magnitude of Gradient](data2/cameramagbin.jpg){ height=400 }

### Derivative of Gaussian Filter

For this part, we apply a gaussian filter before applying the derivative filters.

![Gaussian then gradient x](data2/cameragtdx.jpg){ height=400 }

![Gaussian then gradient y](data2/cameragtdy.jpg){ height=400 }

We can combine the the two filters with convolution and get a new derivatieve of gradient filter.

![Derivative of gaussian filter](data2/camdogx.jpg)

The y derivative of gaussian is just above but transposed. When using this filter instead, we get the same image, as expected due to the associativity of convolution.

![Derivative of gaussian X](data2/cameradogdx.jpg)

![Derivative of gaussian Y](data2/cameradogdy.jpg)

### Image Straightening

In this part of the project, we straightened images based off of the edge orientation histograms. In these examples, we bin orientation vectors on their closest whole degree value, and we pick the rotation with the most in the bins corresponding to the cardinal directions.


| Original | Original Gradient | Straightened | Straightened Gradient
|:--- |:--- |:- |:- |
| ![original](data2/facadenew.jpg) | ![original gradient](data2/facadehist.jpg) | ![straightened](data2/facadenewstraight.jpg) | ![straightened](data2/facadenewhiststraight.jpg)|
| ![original](data2/cat.jpg) | ![original gradient](data2/cathist.jpg) | ![straightened](data2/catstraight.jpg) | ![straightened](data2/cathiststraight.jpg)|
| ![original](data2/bridge.jpg) | ![original gradient](data2/bridgehist.jpg) | ![straightened](data2/bridgestraight.jpg) | ![straightened](data2/bridgehiststraight.jpg)|
| ![original](data2/building.jpg) | ![original gradient](data2/buildinghist.jpg) | ![straightened](data2/buildingstraight.jpg) | ![straightened](data2/buildinghiststraight.jpg)|
| ![original](data2/loaf.jpg) | ![original gradient](data2/loafhist.jpg) | ![straightened](data2/loafstraight.jpg) | ![straightened](data2/loafhiststraight.jpg)|


I actually thought that the cat image would be the failure case since it has soft textures, but the bottom of the cat ended up being a nice enough edge for the algorithm to grab onto it. The building image failed because there are too many edges in various angles due to the geometry of the building. The loaf is an amorphous shape so its hard for the algorithm to find strong edges in any direction. 

## Part 2: Fun with frequencies!

### Image sharpening

For this part of the project, we sharpened images by applying a high pass filter, and adding a scaled version of the resulting image. If $G$ is a gaussian filter, $X$ the image and $\alpha$ the scaling factor, the resulting image is $\alpha(X - G*X) + X = X(\alpha I - \alpha G + I) = X*((\alpha + 1)I - \alpha G)$, where $I$ is the identity filter (the unit impulse). This can be represented with a single filter. 

| Original | Sharpened |
|:--- |:--- |
| ![original](data2/taj.jpg) | ![sharpened](data2/tajsharp.jpg) |
| ![original](data2/cat.jpg) | ![sharpened](data2/catsharp.jpg) |

For evaluation, I blurred the cat image and resharpened it using the filter above.

![blur](data2/catblur.jpg)

![resharp](data2/catresharp.jpg)

While the resharpened image does have the detail emphasized, a lot of the finer details have been blurred away and were not recovered. Therefore, while the image has some sharper edges, it does not contain the details of the original image.

### Hybrid images.

In this part of the project, we create hybrid images by combining the high frequencies of one images with the low frequencies of another image.

| High | Low | Combined |
|:--- |:--- |:- |
| ![high](data2/liz.jpg) | ![low](data2/poc.jpg) | ![hybrid](data2/pocpoc.jpg) |
| ![low](data2/sid.jpg) | ![high](data2/bop.jpg) | ![hybrid](data2/sidbop2.jpg) |
| ![low](data2/sidfourier.jpg) | ![high](data2/bopfourier.jpg) | ![hybrid](data2/sidbfourier.jpg) |
| ![low](data2/cat.jpg) | ![high](data2/loaf.jpg) | ![hybrid](data2/catloaf.jpg) |


Here, we take the fourier transform of the hybrid image of my roomate, Siddhant, when he was younger and now. The bread cat case did not work because they do not share some of the underlying structure of the image, so the illusion does not work well.

### Laplacian and Gaussian Stacks

In this part of the project, we create Laplacian and Gaussian stacks. This involves applying a low pass filter iteratively to an image to get the Gaussian stack. To get the Laplacian stack, we take the pairwise differences of adjacent images in the gaussian stack. This gives us band passed images. Here, we have a gaussian and laplacian stack of the famous dali painting.


| Gaussian | Laplacian |
|:--- |:--- |
| ![gaussian](data2/link0gauss.jpg) | ![Laplacian](data2/link0lapl.jpg) |
| ![gaussian](data2/link3gauss.jpg) | ![Laplacian](data2/link3lapl.jpg) |
| ![gaussian](data2/link6gauss.jpg) | ![Laplacian](data2/link6lapl.jpg) |
| ![gaussian](data2/link9gauss.jpg) | ![Laplacian](data2/link9lapl.jpg) |
| ![gaussian](data2/link14gauss.jpg) | ![Laplacian](data2/link12lapl.jpg) |

Here, we see that in the first images, we see more of the details of the woman and the scene in the painting, but as we go down we can see more of abraham lincoln.

Now, we use the same example from above and see how my roommate transforms from his new self to his young self.

| Gaussian | Laplacian |
|:--- |:--- |
| ![gaussian](data2/sidb0gauss.jpg) | ![Laplacian](data2/sidb0lapl.jpg) |
| ![gaussian](data2/sidb3gauss.jpg) | ![Laplacian](data2/sidb3lapl.jpg) |
| ![gaussian](data2/sidb6gauss.jpg) | ![Laplacian](data2/sidb6lapl.jpg) |
| ![gaussian](data2/sidb9gauss.jpg) | ![Laplacian](data2/sidb9lapl.jpg) |
| ![gaussian](data2/sidb14gauss.jpg) | ![Laplacian](data2/sidb12lapl.jpg) |

### Multiresolution blending

For this part of the project, we blend images together using multiresolution blending. To do this, we use low passes on the filter for different frequency bands of the image. 

![oraple](data2/oraple2.jpg)

I used the same half and half mask on the images of my roomate above.

![Sidbop](data2/bopsid.jpg)

I used the following mask to transfer old Siddhant's hair onto new Siddhants head

![Mask](data2/mask.jpg)

![Sidbop with hair transplant](data2/bopsidhair.jpg)

Now, we show the frequency bands of the two images and the mask

| Old Sid | New Sid | Mask |
|:--- |:--- |:- |
| ![Laplacian](data2/sidbl0lapl.jpg) | ![Laplacian](data2/blackb0lapl.jpg) | ![Mask](data2/mask0gauss.jpg) |
| ![Laplacian](data2/sidbl3lapl.jpg) | ![Laplacian](data2/blackb3lapl.jpg) | ![Mask](data2/mask3gauss.jpg) |
| ![Laplacian](data2/sidbl6lapl.jpg) | ![Laplacian](data2/blackb6lapl.jpg) | ![Mask](data2/mask6gauss.jpg) |
| ![Laplacian](data2/sidbl9lapl.jpg) | ![Laplacian](data2/blackb9lapl.jpg) | ![Mask](data2/mask9gauss.jpg) |
| ![Laplacian](data2/sidbl12lapl.jpg) | ![Laplacian](data2/blackb12lapl.jpg) | ![Mask](data2/mask12gauss.jpg) |
