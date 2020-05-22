# TITLE: change.labels.order 
# Function: to name sample labels acording to its apparition in the sample pictures.

change.labels.order <- function(input.path, input.file) {
  
  if(any(list.files(getwd())%in%"nir") & any(list.files(getwd())%in%"vis")){}else{
    wd <- getwd()
    setwd(input.path)
    on.exit(setwd(wd))
  }
  
  # --------------------------------------------------------------------
  all.names <- read.csv(list.files(md,"names.csv"))
  
  pots.per.block <- 14
  # --------------------------------------------------------------------
  all.names <- read.csv(input.file)
  
  pots <- nrow(all.names)
  
  
  if((pots/14)%%1==0){pots.per.block <- 14}else{
    if((pots/10)%%1==0){pots.per.block <- 10}else{message("Names do not correspond either to blocks of 14 (monosp communities), neither blocks of 10 (mixed communities)")}
  }
  
  blocks <- pots/pots.per.block
  
  pots.per.pic <- 4
  
  if(pots.per.block/pots.per.pic > as.numeric(sub( "\\..*","", as.character(pots.per.block/pots.per.pic)))){
    positions <- c(rbind(1:(pots.per.block/2),((pots.per.block/2)+1):pots.per.block))
    no.pic <- c(2,4)
    block.order <- numeric(length(positions)+length(no.pic))
    block.order[no.pic] <- NA
    block.order[!is.na(order)] <-positions 
    rm(positions, no.pic)
  }else{
    block.order <- c(rbind(1:(pots.per.block/2),((pots.per.block/2)+1):pots.per.block))}
  
  ordered.pots <- unlist(lapply(seq(0, pots.per.block*(blocks-1), by=pots.per.block), "+", block.order))
  
  names <- all.names[ordered.pots,, drop=F]
  
  names$names <- as.character(names$names)
  names[is.na(names$names), "names"] <- "mossless"
  print(names)
  
  write.table(names, "names.csv", row.names=F, quote=F)
}