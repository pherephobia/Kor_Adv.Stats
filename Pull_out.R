devtools::source_gist('284671997992aefe295bed34bb53fde6', filename = 'backstitch.R')
infile <- "C:/Users/phere/Dropbox/Scholar/2_Graduates/2018_03_Fall/POLI502_Methodology_of_Political_Analysis/Kor_BasicStats/04-FoundationIF.Rmd"
output <- backstitch(infile, output_type = 'script', chunk_header = "#+")
cat("```r", output, "```", sep = "\n")
                     
                     