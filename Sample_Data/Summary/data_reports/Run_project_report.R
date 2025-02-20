#---- Runs the monthly report .qmd

library(quarto)

quarto_render(paste0(getwd(), "/Sample_Data/Summary/data_reports/NPRB2301_sample_reports.qmd"), 
              output_file = paste0("NPRB2301_",Sys.Date(), ".pdf"))

