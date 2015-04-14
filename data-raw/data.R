paintings <- read.csv("data-raw/paintings.csv",
                           stringsAsFactors = FALSE)
devtools::use_data(paintings, overwrite = TRUE)
