## Image pre-processing:

A **ImageJ** _imj_ macro repository providing tools to paired images alignment and histogram matching within a picture series. 
We use ImageJ because it is open-source, very accessible and familiar in biological sciences, fitting with our main goal to develop an free and accessible protocol.

**Note**: You can download ImageJ in <https://imagej.nih.gov/ij/download.html> 

---
### Alignment:

Allow NIR-VIS pictures perfectly match for spectral index calculation. 
* [**Alignment_Process_Folder_v1.ijm**](https://github.com/mossmusgo/photomoss/blob/master/vignettes/vignette_ImageJ_preprocessing/Alignment_Process_Folder_v1.ijm).

---
### Histogram homogenization:

Homogenization reduces exposure differences between images. We select one image from each NIR/VIS series to use it as histogram reference to match the rest of histograms within picture series. 
* [**Histomatch_Process_Folder.ijm**](https://github.com/mossmusgo/photomoss/blob/master/vignettes/vignette_ImageJ_preprocessing/Histomatch_Process_Folder.ijm).

