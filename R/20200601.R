names <- read.csv(list.files(md, "raw.names.csv"))
names <- gsub(".*) ", "", names[,1])

names2 <- as.character(names[141:length(names)])
old<-c(paste0(0, c(1:9)), 10)
new<-c(11:20)

for(i in c(1:10)){
  names2[grep(old[i], names2)]<-sub(old[i], new[i], names2[grep(old[i], names2)])
}

names[141:length(names)] <- names2


all.names <- names

pots <- length(all.names)
pots.per.block <- 14

if((pots/pots.per.block)%%1!=0){message("Names do not correspond to the specified number of samples per block")}

blocks <- pots/pots.per.block

pots.per.pic <- 4

if(pots.per.block/pots.per.pic > as.numeric(sub( "\\..*","", as.character(pots.per.block/pots.per.pic)))){
  positions <- c(rbind(1:(pots.per.block/2),((pots.per.block/2)+1):pots.per.block))
  no.pic <- c(2,4)
  block.order <- numeric(length(positions)+length(no.pic))
  block.order[no.pic] <- NA
  block.order[!is.na(block.order)] <-positions 
  rm(positions, no.pic)
}else{
  block.order <- c(rbind(1:(pots.per.block/2),((pots.per.block/2)+1):pots.per.block))}

ordered.pots <- unlist(lapply(seq(0, pots.per.block*(blocks-1), by=pots.per.block), "+", block.order))

names <- all.names[ordered.pots, drop=F]

names[is.na(names)] <- "mossless"
print(names)
int <- strsplit(names, "-")
int2 <- as.vector(unique(do.call(rbind,lapply(1:length(int), function(i) int[[i]][1]))))
int <- substr(unique(do.call(rbind,lapply(1:length(int), function(i) int[[i]][1]))), 1, 3)   
int <- sub("mos", "mossless", int)
int <- sub("bla", "Whi", int)
int3 <- as.vector(int)

names <- gsub(int2[1], int3[1], names)
names <- gsub(int2[2], int3[2], names) 
names <- gsub(int2[3], int3[3], names)
names <- gsub(int2[4], int3[4], names)
names <- gsub(int2[5], int3[5], names)
names <- gsub(int2[6], int3[6], names)
names <- gsub(int2[7], int3[7], names)
names <- gsub(int2[8], int3[8], names)
names <- gsub("pure-|adult-", "", names)
names <- gsub("NR", "N", names)
int <- strsplit(names, "-")
names <- substr(names, 1,5)
as.vector(do.call(rbind,lapply(1:length(names), function(i) int[[i]][3])))

write.table(names, "names.csv", row.names=F, quote=F)
