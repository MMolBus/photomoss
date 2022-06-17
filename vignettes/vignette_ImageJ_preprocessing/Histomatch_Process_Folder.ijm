/*
 * Macro template to process multiple images in a folder for histogram matching
 */
//Set directory settings an select reference image
//select image imput folder
input = getDirectory("./input_folder");
//select image output folder
output = getDirectory("./output_folder");
//select reference image from input folder
ref="./input_folder/reference_image.tif";

Dialog.create("File type");
Dialog.addString("File suffix: ", ".tif", 5);
Dialog.show();
suffix = Dialog.getString();

processFolder(input);

function processFolder(input) {
	list = getFileList(input);
	for (i = 0; i < list.length; i++) {
		if(File.isDirectory(list[i]))
			processFolder("" + input + list[i]);
		if(endsWith(list[i], suffix))
			processFile(input, output, list[i]);
	}
}

function processFile(input, output, file) {
	// do the processing here by replacing
	// the following two lines by your own code
	filename = replace(file, suffix, "");  
	//run("Bio-Formats Importer", "open=/home/naga/tiff_kk/CRW_1666.tif autoscale color_mode=Default open_files display_metadata view=Hyperstack stack_order=XYCZT");
	//open("Directory with referece image for histogram matchin")
	open(ref);
	//run("RGB Color");
	run("RGB Stack");
	run("Stack to Images");

	selectWindow("Red");
	rename("1-Red");
	selectWindow("Green");
	rename("3-Green");
	selectWindow("Blue");
	rename("5-Blue");

// this is the part we need to repeat
	//run("Bio-Formats Importer", "open=" + input + file + "autoscale color_mode=Default open_files display_metadata view=Hyperstack stack_order=XYCZT");
	open(input + file);
	//run("RGB Color");
	run("RGB Stack");
	selectWindow(file);
	run("Stack to Images");
	
	selectWindow("Red");
	rename("2-Red");
	selectWindow("Green");
	rename("4-Green");
	selectWindow("Blue");
	rename("6-Blue");

	run("Concatenate...", "  title=[Stack] image1=[1-Red] image2=[2-Red] image3=[3-Green] image4=[4-Green] image5=[5-Blue] image6=[6-Blue]");

	selectWindow("Stack");
	run("Stack Splitter", "number=3");	

	selectWindow("Stack");
	close();

	selectWindow("stk_0001_Stack");
	run("Bleach Correction", "correction=[Histogram Matching]");

	selectWindow("stk_0002_Stack");
	run("Bleach Correction", "correction=[Histogram Matching]");

	selectWindow("stk_0003_Stack");
	run("Bleach Correction", "correction=[Histogram Matching]");

	selectWindow("stk_0001_Stack");
	close();
	selectWindow("stk_0002_Stack");
	close();
	selectWindow("stk_0003_Stack");
	close();

	run("Concatenate...", "  title=[Concatenated Stacks] image1=[DUP_stk_0001_Stack] image2=[DUP_stk_0002_Stack] image3=[DUP_stk_0003_Stack] image4=[-- None --]");
	run("Deinterleave", "how=2 keep");

	selectWindow("Concatenated Stacks #1");
	run("Stack to Images");
	run("Merge Channels...", "c1=Concatenated-0001 c2=Concatenated-0002 c3=Concatenated-0003");

	selectWindow("Concatenated Stacks #2");
	run("Stack to Images");
	run("Merge Channels...", "c1=Concatenated-0001 c2=Concatenated-0002 c3=Concatenated-0003");


	saveAs("Tiff", output + filename + "_HM");

	print("Processing: " + input + file);
	print("Saving to: " + output);

	// close all windows
	while (nImages>0) { 
          selectImage(nImages); 
          close(); 
      } 
  } 

