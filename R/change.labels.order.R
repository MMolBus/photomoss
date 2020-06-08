# TITLE: change.labels.order 
# Function: to name sample labels acording to its apparition in the sample pictures.

change.labels.order <- function(input.path, input.file, pots.per.block) {
  
  if(any(list.files(getwd())%in%"nir") & any(list.files(getwd())%in%"vis")){}else{
    wd <- getwd()
    setwd(input.path)
    on.exit(setwd(wd))
  }
  
  all.names <- read.csv(input.file)
  
  pots <- nrow(all.names)
  
  
  if((pots/pots.per.block)%%1!=0){message("Names do not correspond to the specified number of samples per block")}
  
  blocks <- pots/pots.per.block
  
  pots.per.pic <- 4
  
  if(pots.per.block/pots.per.pic > as.numeric(sub( "\\..*","", as.character(pots.per.block/pots.per.pic)))){
    positions <- c(rbind(1:(pots.per.block/2),((pots.per.block/2)+1):pots.per.block))
   
    no.pic <- c(3,4)
    block.order <- numeric(length(positions)+length(no.pic))
    block.order[no.pic] <- NA
    block.order[!is.na(block.order)] <-positions 
    rm(positions, no.pic)
  }else{
    block.order <- c(rbind(1:(pots.per.block/2),((pots.per.block/2)+1):pots.per.block))}
  
  ordered.pots <- unlist(lapply(seq(0, pots.per.block*(blocks-1), by=pots.per.block), "+", block.order))
  
  names <- as.character(all.names[ordered.pots,])
  
  names[is.na(names)] <- "mossless"
  print(names)
  
  write.csv(names, "names.csv", row.names=F, quote=F)
}
