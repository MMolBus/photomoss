# Image pre-processing:

We use **ImageJ** macro scripts to align each pair of NIR-VIS images and to
homogenize color histograms. You could download ImageJ in
<https://imagej.nih.gov/ij/download.html> We use ImageJ because it is
open-source, very accessible and familiar in biological sciences,
fitting with our main goal to develop an accessible protocol.

## Alignment:

Allow NIR-VIS pictures perfectly match for *Photomoss* index calculation
and homogenization reduces exposure differences between images. You could download imj macro file [**Alignment_Process_Folder_v1.ijm**](https://github.com/mossmusgo/photomoss/blob/master/vignettes/vignette_ImageJ_preprocessing/Alignment_Process_Folder_v1.ijm).


## Histogram homogenization:

We select one image from each NIR/VIS series to use it as histogram
reference to match the rest of histograms from its series. You could download imj macro file [**here**](https://github.com/mossmusgo/photomoss/blob/master/vignettes/vignette_ImageJ_preprocessing/Histomatch_Process_Folder.ijm).

