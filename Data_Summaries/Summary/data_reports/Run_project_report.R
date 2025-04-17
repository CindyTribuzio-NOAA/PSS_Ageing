#---- Runs the monthly report .qmd

library(quarto)

quarto_render(paste0(getwd(), "/Sample_Data/Summary/data_reports/NPRB2301_sample_reports.qmd"), 
              output_file = paste0("NPRB2301_",Sys.Date(), ".pdf"))

#---- Moves the files to a more meaningful location
rawPath <- getwd()
dataPath <- paste0(getwd(), "/Documents/Project_Updates")

dataFiles <- dir(rawPath, "*.pdf", ignore.case = T, all.files = T)

file.copy(file.path(rawPath, dataFiles), dataPath, overwrite =  T)

file.remove(dataFiles)
