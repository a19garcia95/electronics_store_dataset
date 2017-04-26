library(readxl)
library(dplyr)
library(sqldf)
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

refine_df[c("product_code", "product_number")] <- NA

#Need help on doing this with a for loop

refine_df$product_code[1] <- sub("-5", "", refine_df$Product.code...number[1])
refine_df$product_code[2] <- sub("-43", "", refine_df$Product.code...number[2])
refine_df$product_code[3] <- sub("-3", "", refine_df$Product.code...number[3])
refine_df$product_code[4] <- sub("-34", "", refine_df$Product.code...number[4])
refine_df$product_code[5] <- sub("-12", "", refine_df$Product.code...number[5])
refine_df$product_code[6] <- sub("-23", "", refine_df$Product.code...number[6])
refine_df$product_code[7] <- sub("-43", "", refine_df$Product.code...number[7])
refine_df$product_code[8] <- sub("-12", "", refine_df$Product.code...number[8])
refine_df$product_code[9] <- sub("-5", "", refine_df$Product.code...number[9])
refine_df$product_code[10] <- sub("-34", "", refine_df$Product.code...number[10])
refine_df$product_code[11] <- sub("-5", "", refine_df$Product.code...number[11])
refine_df$product_code[12] <- sub("-9", "", refine_df$Product.code...number[12])
refine_df$product_code[13] <- sub("-8", "", refine_df$Product.code...number[13])
refine_df$product_code[14] <- sub("-56", "", refine_df$Product.code...number[14])
refine_df$product_code[15] <- sub("-67", "", refine_df$Product.code...number[15])
refine_df$product_code[16] <- sub("-21", "", refine_df$Product.code...number[16])
refine_df$product_code[17] <- sub("-45", "", refine_df$Product.code...number[17])
refine_df$product_code[18] <- sub("-56", "", refine_df$Product.code...number[18])
refine_df$product_code[19] <- sub("-65", "", refine_df$Product.code...number[19])
refine_df$product_code[20] <- sub("-21", "", refine_df$Product.code...number[20])
refine_df$product_code[21] <- sub("-23", "", refine_df$Product.code...number[21])
refine_df$product_code[22] <- sub("-3", "", refine_df$Product.code...number[22])
refine_df$product_code[23] <- sub("-4", "", refine_df$Product.code...number[23])
refine_df$product_code[24] <- sub("-6", "", refine_df$Product.code...number[24])
refine_df$product_code[25] <- sub("-8", "", refine_df$Product.code...number[25])

#This needs to be done with a for loop as well:

refine_df$product_number[1] <- sub("p-", "", refine_df$Product.code...number[1])
refine_df$product_number[2] <- sub("p-", "", refine_df$Product.code...number[2])
refine_df$product_number[3] <- sub("x-", "", refine_df$Product.code...number[3])
refine_df$product_number[4] <- sub("x-", "", refine_df$Product.code...number[4])
refine_df$product_number[5] <- sub("x-", "", refine_df$Product.code...number[5])
refine_df$product_number[6] <- sub("p-", "", refine_df$Product.code...number[6])
refine_df$product_number[7] <- sub("v-", "", refine_df$Product.code...number[7])
refine_df$product_number[8] <- sub("v-", "", refine_df$Product.code...number[8])
refine_df$product_number[9] <- sub("x-", "", refine_df$Product.code...number[9])
refine_df$product_number[10] <- sub("p-", "", refine_df$Product.code...number[10])
refine_df$product_number[11] <- sub("q-", "", refine_df$Product.code...number[11])
refine_df$product_number[12] <- sub("q-", "", refine_df$Product.code...number[12])
refine_df$product_number[13] <- sub("x-", "", refine_df$Product.code...number[13])
refine_df$product_number[14] <- sub("p-", "", refine_df$Product.code...number[14])
refine_df$product_number[15] <- sub("v-", "", refine_df$Product.code...number[15])
refine_df$product_number[16] <- sub("v-", "", refine_df$Product.code...number[16])
refine_df$product_number[17] <- sub("x-", "", refine_df$Product.code...number[17])
refine_df$product_number[18] <- sub("v-", "", refine_df$Product.code...number[18])
refine_df$product_number[19] <- sub("v-", "", refine_df$Product.code...number[19])
refine_df$product_number[20] <- sub("x-", "", refine_df$Product.code...number[20])
refine_df$product_number[21] <- sub("p-", "", refine_df$Product.code...number[21])
refine_df$product_number[22] <- sub("x-", "", refine_df$Product.code...number[22])
refine_df$product_number[23] <- sub("q-", "", refine_df$Product.code...number[23])
refine_df$product_number[24] <- sub("q-", "", refine_df$Product.code...number[24])
refine_df$product_number[25] <- sub("q-", "", refine_df$Product.code...number[25])

refine_df["product_category"] <- NA

refine_df <- subset(refine_df, select = -c(Laptop, TV, Tablet ) )

refine_df$product_category[which(refine_df$product_code == "p")] <- "Smartphone"
refine_df$product_category[which(refine_df$product_code == "v")] <- "TV"
refine_df$product_category[which(refine_df$product_code == "x")] <- "Laptop"
refine_df$product_category[which(refine_df$product_code == "q")] <- "Tablet"

mutate(refine_df, full_address = paste(address, city, country, sep = ","))

refine_df[c("company_philips", "company_akzo", "company_van_houten", "company_unilever", "product_smartphone", "product_tv", "product_laptop", "product_tablet")] <- NA


refine_df$company_philips[which(refine_df$company == "Phillips")] <- 1
refine_df$company_philips[which(refine_df$company != "Phillips")] <- 0

refine_df$company_akzo[which(refine_df$company == "Akzo")] <- 1
refine_df$company_akzo[which(refine_df$company != "Akzo")] <- 0

refine_df$company_van_houten[which(refine_df$company == "Van Houten")] <- 1
refine_df$company_van_houten[which(refine_df$company != "Van Houten")] <- 0

refine_df$company_unilever[which(refine_df$company == "Unilever")] <- 1
refine_df$company_unilever[which(refine_df$company != "Unilever")] <- 0

refine_df$product_smartphone[which(refine_df$product_category == "Smartphone")] <- 1
refine_df$product_smartphone[which(refine_df$product_category != "Smartphone")] <- 0 

refine_df$product_tv[which(refine_df$product_category == "TV")] <- 1
refine_df$product_tv[which(refine_df$product_category != "TV")] <- 0

refine_df$product_laptop[which(refine_df$product_category == "Laptop")] <- 1
refine_df$product_laptop[which(refine_df$product_category != "Laptop")] <- 0

refine_df$product_tablet[which(refine_df$product_category == "Tablet")] <- 1
refine_df$product_tablet[which(refine_df$product_category != "Tablet")] <- 0




