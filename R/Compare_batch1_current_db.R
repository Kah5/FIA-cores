library(tidyverse)
library(ggplot2)
library(here)
# code to compare our current (May 2021) fia cores database for AZ, with the previous batch of data

# first read in the "Batch 1" data that Justin submitted to USFS back in 2017
batch1.rwl <- read.delim( "data/ArizonaPeridiocRingWidth-Batch1.txt", sep = ",")
batch1.meta <- read.delim( "data/ArizonaPeriodicTreeRingMetadata-Batch1.txt", sep= "," )                      
 

# read in the .csv downloaded from the Evans lab google document drive as of 5/20/21
FIA.az <- read.csv("data/FIA_meta_all - FIA_meta_all.csv")
summary(FIA.az)



# find all the records that have complete metadata (C) and pass QC (QC)
# summaries 
FIA.az %>% filter(STATUS %in% c("CQC", "IQC"))  %>% group_by(STATUS, SPCD) %>% summarise(n())
FIA.az %>%  group_by(STATUS, SPCD) %>% summarise(n())
FIA.az %>% summarise(n())

# not a real column that we want...but I think thise should replace the Verify column
FIA.az$crossdated <- ifelse(FIA.az$VERIFY %in% "y", "yes", 
                            ifelse(FIA.az$STATUS %in% c("CQC", "IQC"), "yes", "no"))

FIA.az %>% filter(crossdated %in% "yes") %>% group_by(SPCD)%>% summarise(n())

FIA.az %>% filter(crossdated %in% "yes") %>% group_by(COUNTYCD, SPCD)%>% summarise(n())

# check which of these cores were not included in "batch 1"
# join using "TREE","PLOT", "SUBP", "COUNTYCD", "STATECD"
batch1.meta$TREE <- as.character(batch1.meta$TREE)
batch1.meta$PLOT <- as.character(batch1.meta$PLOT)
batch1.meta$SUBP <- as.character(batch1.meta$SUBP)
batch1.meta$STATECD <- as.character(batch1.meta$STATECD)

FIA.az$TREE <- as.character(FIA.az$TREE)
FIA.az$PLOT <- as.character(FIA.az$PLOT)
FIA.az$SUBP <- as.character(FIA.az$SUBP)
FIA.az$STATECD <- as.character(FIA.az$STATECD)

# create a single identifier that includes TREE, SUBP, PLOT, COUNTYCD, STATECD:
FIA.az$id <- paste0(FIA.az$TREE, "-", FIA.az$SUBP, "-", FIA.az$PLOT, "-", FIA.az$COUNTYCD, "-", FIA.az$STATECD, "-", FIA.az$SPCD )
batch1.meta$id <- paste0(batch1.meta$TREE, "-", batch1.meta$SUBP, "-", batch1.meta$PLOT, "-", batch1.meta$COUNTYCD, "-", batch1.meta$STATECD, "-", batch1.meta$SPCD )

# find the number of records that match ids in batch 1
FIA.az <- FIA.az %>% filter(!PLOT %in% NA)
FIA.az %>% summarise(n()) # total with no NA in plots = 2042
FIA.az %>% filter(id %in% batch1.meta$id) %>% summarise(n())
# n() =  1371 of the records in FIA-cores metadata sheet were included in batch 1
FIA.az %>% filter(id %in% batch1.meta$id) %>% group_by(SPCD) %>% summarise(n())
# SPCD `n()`
# <int> <int>
# 1   106   350
# 2   122   873
# 3   202   148


# so that leaves a number of cores that are not in batch 1:
FIA.az %>% filter(!id %in% batch1.meta$id)%>% summarise(n())
# n()
# 1 671
FIA.az %>% filter(!id %in% batch1.meta$id) %>% group_by(SPCD) %>% summarise(n())
# SPCD `n()`
# <int> <int>
# 1   106   582
# 2   122    80
# 3   202     9

FIA.az %>% filter(!id %in% batch1.meta$id) %>% group_by(SPCD, STATUS) %>% summarise(n())

batch1.meta <- batch1.meta %>% filter(!PLOT %in% NA)
batch1.meta %>% filter(id %in% FIA.az$id) %>% summarise(n())
# of the 1364 cores included in batch1.metadata, 1371 have ids matching ids in FIA database...so some are duplicates?

# none of the records are 100% duplicates
FIA.az[duplicated(FIA.az)]
batch1.meta[duplicated(batch1.meta)]

# there are about 10 duplicates
length(FIA.az$id)
length(unique(FIA.az$id))



dup.ids <- FIA.az[duplicated(FIA.az$id),]$id

FIA.az %>% filter(id %in% dup.ids)

ours.in.batch1 <- left_join(batch1.meta, FIA.az, by = c("TREE","PLOT", "SUBP", "COUNTYCD", "STATECD"))
ours.in.batch1 <- left_join(FIA.az, batch1.meta, by = c("TREE","PLOT", "SUBP", "COUNTYCD", "STATECD"))

# if STATUS is CQC then assign VERIFY to 



