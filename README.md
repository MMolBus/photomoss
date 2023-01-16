# Photomoss protocol 

Photomoss is a full protocol that involves image retrieval, pre-processing,  calculation of spectral indices and binary segmentation. Whole process is supported by a combination of cheap modified drone cameras and built-for-purpose ImageJ macros and the R package, _PhotomossR_. It offers a research tool to work with Biological Soil Covers, including biological soil crusts. 

1. Image retrieving
2. [Image pre-processing with ImageJ:](https://github.com/MMolBus/photomoss/blob/master/vignettes/vignette_ImageJ_preprocessing/ImageJ_alignment_and_histogram_matching.md) Image pre-processing is based in ImageJ (description below) that enables data preparation to proccess images with R. 
3. [_PhotomossR_ workflow:](https://github.com/MMolBus/photomoss/blob/master/vignettes/vignette_Photomoss_workflow/Vignette_Photomoss.md)PhotomossR is the R part of the  protocol. It is a set of R functions to calculate biological cover in Bilogical Soil Covers by imaging, based on mosscoder/crustCover. This package is the data processing backbone of the protocol. PhotomosR enables authomatic segmentation of images to separate Biological Soil Covers from background. Finally, the user can conduct quality test of obtained results, comparing calculated area with baseline cuts of the processed images.

