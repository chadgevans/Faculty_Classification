###########################################
# Merge HERI 2010 data dumps
###########################################

library(haven)
# Old data used: "FURSTENBERG.SAV."  We were given the variable ACE here, which we weren't in the second transfer
data1 <- read_spss(file.path(Raw,'FURSTENBERG_REVISED.SAV')) # These data contain the "ACE" variable
data2 <- read_spss(file.path(Raw,'FURSTENBERG_REVISED_032515.SAV')) # These data only lack the "ACE" variable

# Add "ACE" to have the full dataset
HERImerged<-left_join(data1,data2)

save(HERImerged, file=file.path(Private_Cache,"HERImerged.RData"))
rm(data1,data2,HERImerged)
