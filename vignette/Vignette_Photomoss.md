Photomoss workflow
================

Author: Manuel Molina-Bustamante
---
Date: 24/7/2020
---

## What is *photomoss*?

***Photomoss*** is a developement from **mosscoder/crustcover** package
(https://github.com/mosscoder/crustCover). As with *crustcover*, with
***photomoss*** we will meassure the size of moss occupied areas in
field or lab experiments. As *crustcover* achieve this duty, we use the
same principles as (Fischer2012)that take advantage of Near InfraRed
(NIR) and RGB images. With the color channels of this images, we can
achieve several spectral indexes that will allow to measure moss surface
or physiological condition. In contrast with *crustcover* that measures
seven index, *photomoss* can use a great set of 19 spectral indexes. As
*crust cover*, *photomoss* core function can calculate moss area using a
given spectral index and implementigg a custom threshold value, but in
addition, it can apply an authomatic segmentation following a set of 12
different segmentation methods if needed. Other additional
functionalities of *photomoss* in comparison with *crustcover* are the
semiautomatisation of annalysis area over the image, and a segmentation
accuracy test functionality, to test the segmentation accuracy comparing
the calculated surfaces with a the true moss area from a binary mask
image done with ImageJ.


## Installing photomoss

we need to install *devtools* package:

    if(require(devtools)!=T){
      install.packages('devtools')
      require(devtools)
    }

Then we install *photomoss* from my GitHub branch:

    install_github("mossmusgo/photomoss")
    library(photomoss)

Maybe you need to install some other packages, so be aware of warnings
and install them.


## Set working directory structure


This is an important step, because the function searches the images in a
directory structure that has to be always the same.

Our working directory have to include the following folders and files:

-   ***vis*** folder: this folder includes the RGB images.

-   ***nir*** folder: this folder includes the NIR images.

-   ***mask*** folder: this folder includes the binary mask images.
    Mandatory if in the function *ccspectral.df* the argument
    *manual.mask.test = T*.

-   ***rois*** folder: this folder includes the *regions of interest*
    (***rois***)for each pair of nir and vis images (or for each triad
    of nir, vis, mask if it is the case). A *roi* is the region of the
    image where we can find the moss sample to analyze. It is necesary
    to previously create this files using *ImageJ*.

    -   **Attention.1**: if the nir and vis images contain several
        samples the ***rois*** folder must include as many subfolders
        samples. In each subfolder we must put the .roi files for that
        nir-vis image. So, if an image includes four samples, as in
        Figure 1, and we want to analyze them all, we need to have four
        rois. in that subfolder.

    -   **Attention.2**: the .roi files in each subfolder must be
        ordered in the same way as the *names.csv* file (we will talk
        about it later.)


## Set working directory


    wd #your working directory
    setwd (wd)
    tif.path <- getwd()


## Start with the functions



### *chart.from.tif*


We create the chart object (a list of polygons) with the
*chart.from.tif* function. To do this we click over the color cells
chart in the image. Importantnote: folow the order as indicated in the
figure.

    chart<- chart.from.tif(tif.path) 

![image](chart.png){width="5.83333in" height="3.28125in"}

Figure 1


## *roi2polygon.2* and *extractPIX.from.Poly*.


Now we use the *roi2polygon.2* function to create a readable polygon
files (*polys* object) from the ImageJ .roi files. Then we crop the
pixels that fell inside the polygons and obtain a list og ploygon
data.frame (*obs.areas* object)

    roi.paths<- list.files(path = "./rois",pattern=".roi$",full.names = T, recursive = T)

    polys <- lapply(roi.paths, roi2polygon.2, tif.path)


## *ccspectral.df*


This is the core function of photomoss.

The basic result of this function is a dataframe with the **areas in
number of pixels** of background and moss area for each sample. If
argument *descrip = T* the descriptive statistics of the different
areas.

# Arguments in *ccspectral.df*:

-   **tif.path**: the path of the working directory where are the
    ***vis***, ***nir***, ***mask***, ***rois*** folders and
    ***names.csv*** file.

-   **chart**: polygon list obtained with *chart.from.tif* function.

-   **obs.areas**: list of polygons data.frame obtained with
    *extractPIX.from.Poly* function.

-   **pdf**: logical, to present the results in image and histogram of
    moss areas. Default = F.

-   **calculate.thresh**: logical, to Calculate autothreshold. Default =
    T.

-   **descrip**: logical, to calculate statistical descriptors of index
    value in the moss area, backround area,... Default = F.

-   **manual.mask.test**: logical, if you want to test the accuracy of
    image segmentation comparing with handmade drawn moss area. Default
    = F,

-   **index.**: character with what index you want to calculate. By
    default, all options: *index.* = c("NDVI", "SR", "MSAVI", "EVI",
    "CI", "BSCI", "BI","NORR", "NORG", "NORB", "EXR", "EXG","EXB",
    "EXGR", "CIVE", "VEG", "HUE", "SAT", "VAL")\
    The options represent the folowing indexes:

    1.  *NDVI*: **Normalized Differential Vegetation Index.** Is the
        normalize difference beween Near Infrared (NIR) values and
        visible RED values. NDVI is used in teledetection aplications to
        measure physiological active vegetation, because clorophyl
        absorbs Red and reflects NIR. NDVI scales between -1 and 1,
        being 1 the value of an active green leaf.
        $$NDVI = \frac{(NIR - RED)}{(NIR + RED)}$$

    2.  *SR* **Simple Ratio**. Te difference between NIR value and Red
        value, without standarisation. It's an uscaled index
        $$SR = NIR - RED$$

    3.  *MSAVI*: **Second Modified Soil Adjusted Vegetation Index.** Use
        a self-adjusting soil factor to reduce background soil
        influence. MSAVI scales between -1 and 1 (Qi1994).
        $$MSAVI = \frac{2\times NIR + 1 - \sqrt{(2 x NIR + 1)2 - 8 \times (NIR -RED)}}{2}$$

    4.  *EVI*: **Enhanced Vegetation Index.** Is an enhanced NDVI that
        includes a soil adjustment factor and uses the blue band to
        correct the red band atmospheric aerosol distortion.(Liu1995),
        (Huete1999)
        $$EVI = \frac{2.5 x ((NIR - RED) }{(NIR + 6 \times RED - 7.5 \times BLUE + 1))}$$

    5.  *CI*: **Crust Index.** Is based on the standarized difference
        between RED and BLUE bands. It's an index develloped to detect
        Biological Solil Crust with cyaniobacteria. (KARNIELLI1997)
        $$CI = 1 - \frac{RED - BLUE}{RED + BLUE}$$

    6.  *BSCI*: **Biological Soil Crust Index.** Is based on GREEN, RED
        and NIR bands. This index was designed to exacerbate the
        spectral differences between Biological Soil Crusts and bares
        sand, dry plants and green plants. Include an adjustment factor
        with value 2 to magnify the absolute difference between RED and
        GREEN bands (Chen2005)
        $$BSCI = \frac{(1 - 2 \times |RED - GREEN|)}{mean(GREEN, RED, NIR)}$$

    7.  *BI*: Brightness Index. (Escadafal and Bacha 1996)
        $$BI = \sqrt{GREEN^m2 + RED^2 + NIR^2}$$

    8.  *NORR*: Normalized Red.
        $$NORR = \frac{\frac{RED}{RED_max}}{\frac{RED}{RED_max}+\frac{GREEN}{GREEN_max}+
                  \frac{BLUE}{BLUE_max}}$$

    9.  *NORG*: Normalized Green.
        $$\frac{\frac{GREEN}{GREEN_max}}{\frac{RED}{RED_max}+\frac{GREEN}{GREEN_max}+
                  \frac{BLUE}{BLUE_max}}$$

    10. *NORB*: Normalized Blue.
        $$\frac{\frac{BLUE}{BLUE_max}}{\frac{RED}{RED_max}+\frac{GREEN}{GREEN_max}+
                  \frac{BLUE}{BLUE_max}}$$

    11. *EXR*: Excess Red. $$EXR = 1.4 \times NORR - NORG$$

    12. *EXG*: Excess Green. $$EXG = 2 \times NORG - NORR - NORB$$

    13. *EXB*: Excess Blue. $$EXB = 1.4 \times NORB - NORG$$

    14. *EXGR*: Excess Green minus Excess Red. $$EXGR = EXG - EXR$$

    15. *CIVE*: Color index of vegetation extraction.
        $$CIVE = 0.441 \times NORR - 0.81 l \times NORG + 0.385 \times NORB + 18.78745$$

    16. *VEG*: Vegetative. a = 0.667 (Hage et al 2006)
        $$VEG = \frac{NORG}{NORR^a \times NORB^{1-a}}$$

    17. *HUE*:

    18. *SAT*:

    19. *VAL*:

-   \***threshold.method**: character, if *calculate.thresh= T.* The
    autosegmentation method to separate moss from background. The
    argument can have one of the following values: "Huang", "IJDefault",
    "IsoData", "Li", "Mean", "MinErrorI", "Moments", "Otsu",
    "Percentile", "RenyiEntropy", "Shanbhag", "Triangle".

-   **threshold.vector**: numeric, if *calculate.thresh= F* the index
    value to segment the image to separate moss from background. Must
    have the same length than *index.* argument, and must respect
    *index.* argument default order.

-   **descriptors.**: character, if *descrip= T* the statistic
    descriptors of index values in the classified areas. Default:
    *descriptors.* = c("median","mean","sd","min", "max","diff.range")

Example:

    ccspectral.df(tif.path,
                  chart=chart,
                  obs.areas,
                  pdf = F,
                  calculate.thresh = T,
                  descrip = T,
                  manual.mask.test = F,
                  index. = c("NDVI", "SR"),
                  threshold.method = c("Huang"),
                  threshold.vector = NULL,
                  descriptors. = 
                    c("median","mean","sd","min",
                      "max","diff.range" )
                  )

The resulting data.frame is saved in a new folder in your working
directory.
