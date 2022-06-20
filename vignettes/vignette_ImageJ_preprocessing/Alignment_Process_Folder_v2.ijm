/*
 * Macro template to process and align multiple images in a folder
 */
 // -------------------------------------------------------------------
// Written by: Nagore García Medina
// Date: 2018-02
// Contact: nagore.garcia@uam.es
// -------------------------------------------------------------------

// v1.0: -Allow perfectly match between Near Infrared (NIR) and visible RGB images (VIS) for spectral image calculation.

//select near infrared image imput folder
inputNIR = getDirectory("./input_NIR_folder");

//select visible image imput folder
inputVIS = getDirectory("./input_VIS_folder");

//select near infrared image output folder
outputNIR = getDirectory("./aligned_NIR_folder");

//select visible image outut folder
outputVIS = getDirectory("./aligned_VIS_folder");

Dialog.create("File type");
Dialog.addString("File suffix: ", ".tif", 5);
Dialog.show();
suffix = Dialog.getString();

processFolder(inputNIR);

function processFolder(inputNIR) {
	listNIR = getFileList(inputNIR);
	listVIS = getFileList(inputVIS);
	for (i = 0; i < listNIR.length; i++) {
		if(File.isDirectory(listNIR[i]))
			processFolder("" + inputNIR + listNIR[i]);
		if(endsWith(listNIR[i], suffix))
			processFile(inputNIR, outputNIR, listNIR[i]);
	}
}

	

function processFile(inputNIR, outputNIR, file) {
fileVIS = listVIS[i];
filename = replace(file, suffix, "");

open(inputNIR + file);
run("RGB Color");
rename("NIR" + file);

open(inputVIS + fileVIS);
run("RGB Color");
rename("VIS" + fileVIS);

run("Concatenate...", "  title=[Concatenated Stacks] image1=NIR" + file + " image2=[VIS" + fileVIS + "] image3=[-- None --]");
run("Linear Stack Alignment with SIFT", "initial_gaussian_blur=1.60 steps_per_scale_octave=3 minimum_image_size=64 maximum_image_size=1024 feature_descriptor_size=4 feature_descriptor_orientation_bins=8 closest/next_closest_ratio=0.92 maximal_alignment_error=25 inlier_ratio=0.05 expected_transformation=Similarity interpolate");
selectWindow("Aligned 2 of 2");
makeRectangle(320, 96, 3808, 3320);

run("Crop");
run("Stack to Images");
selectWindow("Aligned-0001");
saveAs("Tiff", outputNIR + filename + "_AL");
selectWindow("Aligned-0002");
saveAs("Tiff", outputVIS + filename + "_AL");
run("Close All");
}



