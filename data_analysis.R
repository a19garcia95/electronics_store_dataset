library(readxl)
library(dplyr)
refine <- read_excel("~/Downloads/refine.xlsx")

refine_df <- data.frame(refine)
#toupper(refine_df$company)

refine_df$company[0:6] <- "Phillips"
refine_df$company[7:13] <- "Akzo"
refine_df$company[14:16] <- "Phillips"
refine_df$company[17:21] <- "Van Houten"
refine_df$company[22:25] <- "Unilever"


# 
# for(x in refine_df$company) {
#   if(startsWith(x, "a") = TRUE) {
#     x <- "Akzo"
#     print(refine_df$company[x])
#   }
# }


