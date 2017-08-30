load(file.path(Private_Cache,"HERImerged.RData"))
df <- subset(HERImerged, HERImerged$YEAR==2010) # HERI also provided us with a sample from 1989.  Subset to faculty in 2010

df[ is.na(df) ] <- NA

# Replace factor values with appropriate labels
labelled<-sapply(df, is.labelled) 
df[labelled] <- sapply(df[labelled], function(x) {
  as_factor(x, labels = "label")
}) # add labels to the values
df <- as.data.frame(unclass(df)) # convert character vectors to factors

# apostrophe is screwing up recoding, drop it.
levels(df$DEGEARN)[c(1,5)]<-c("Bachelors (B.A., B.S., etc.)", "Masters (M.A., M.S., M.F.A., M.B.A., etc.)")
levels(df$DEGWORK)[c(1,5)]<-c("Bachelors (B.A., B.S., etc.)", "Masters (M.A., M.S., M.F.A., M.B.A., etc.)")

rm(HERImerged)


