# rm(list = ls())
library(xml2)
library(stringr)
library(dplyr)

#get food habits data
load("~/git/ECSA/data/allfhsg.Rdata")
allfhsg <- allfhsg %>% filter(!is.na(year)) #filter out NAs

#create year vector for splitting data
year <- paste0(seq(1973,2016,1))

#split data into a list by year
yi <- list()
for (i in 1:length(unique(allfhsg$year))){
  print(unique(allfhsg$year)[i])
  yi[[i]] <- allfhsg %>% filter(year == unique(allfhsg$year)[i])
}

#give years as names
names(yi) <- year

#convert nested list items to data.frames in global environment
list2env(yi , envir = .GlobalEnv)
rm(yi, allfhsg)

#write to file
for (i in 1:length(list(ls())[[1]])){
  
  if (class(get(list(ls())[[1]][i])) == "data.frame"){
    write.csv(get(list(ls())[[1]][i]),
              file = paste0("NEFSC Food habits, annual, NE LME, ",list(ls())[[1]][i]," V1.csv"),
              row.names = F)
  }
  
}

#Write ERDDAP XML for upload---------------------------------------------#

#file names
fnames <- paste0("NEFSC Food habits, annual, NE LME, ",seq(1973,2016,1)," V1.csv")

#data set ids
ids <- paste0("food_habits_",seq(1973,2016,1),"_v1")

#template ERDDAP XML
basexml <- read_xml("~/test_data/fh.xml")

#write ouput
outfile <- "out.xml"
if (file.exists(outfile)) file.remove(outfile)

#insert updated xml into outfile
for (j in 1:length(fnames)){
  
  write(as.character(paste0('<dataset type="EDDTableFromAsciiFiles" datasetID="',ids[j],'" active="true">')),
        file = outfile,
        append = TRUE)
  
  for (i in 1:xml_length(basexml)){
    
    if (i  == 3){
      write(as.character(paste0("<fileNameRegex>",fnames[j],"</fileNameRegex>")),
            file = outfile,
            append = TRUE)
    } else if (i == 9){
      write(as.character(paste0("<preExtractRegex>",str_split(fnames[j],".csv",n=2)[[1]][1],"\\.</preExtractRegex>")),
            file = outfile,
            append = TRUE)
    } else if (i == 16){
      
      for (k in 1:length(xml_children(xml_children(basexml)[i]))){
        
        if (k == 1){
          write(as.character(paste0("<addAttributes>")),
                file = outfile,
                append = TRUE)
          write(as.character(paste0(xml_children(xml_children(basexml)[i])[k])),
                file = outfile,
                append = TRUE)
        } else if (k == 11){
          write(as.character(paste0('<att name="title">',str_split(fnames[j],".csv",n=2)[[1]][1],'</att>')),
                file = outfile,
                append = TRUE)
          write(as.character(paste0("</addAttributes>")),
                file = outfile,
                append = TRUE)
        } else {
          write(as.character(paste0(xml_children(xml_children(basexml)[i])[k])),
                file = outfile,
                append = TRUE)
        }
      }
    } else if (i == 100){
      write(as.character(paste0("</dataset>")),
            file = outfile,
            append = TRUE)
    } else {
      write(as.character(paste0(xml_children(basexml)[i])),
            file = outfile,
            append = TRUE)
    }
    
    
  }
  
}


