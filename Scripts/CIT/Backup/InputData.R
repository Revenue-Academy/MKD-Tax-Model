library(readr)
library(readxl)
library(data.table)
library(tidyverse)

#cit_simulation_parameters_raw <- read_excel("CIT-Parameters.xlsx")


library(readxl)
nace_description<- read_excel("Data/CIT/NACE_SUT_table.xlsx", 
                              sheet = "df_nace_cor")




# Data prep ---------------------------------------------------------------

cit_simulation_parameters_raw <- read_excel("CIT-Parameters.xlsx")
cit_simulation_parameters_updated<-cit_simulation_parameters_raw





# dt_cit <-read_excel("Data/CIT/cit_raw_2021.xlsx", 
#                     sheet = "CIT_RAW_R")%>%
#                data.table()
# 

dt_cit <- read_excel("Data/CIT/cit_raw_2021.xlsx", 
                     sheet = "CIT_RAW_R", col_types = c("numeric", 
                                                        "text", "numeric", "text", "text", 
                                                        "text", "text", "text", "text", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric"))
View(dt_cit)








# dt_cit$special_tax_treatment<-as.character(dt_cit$special_tax_treatmen)
# dt_cit$tax_return_with_zeros<-as.character(dt_cit$tax_return_with_zeros)


setDT(dt_cit)
cols_to_encode <- c(
  "special_tax_treatment",
  "tax_return_with_zeros",
  "markings_financial_results",
  "additional_explanation_40",
  "additional_explanation_59",
  "additional_explanation_62"
)

# Ensure all columns exist
miss <- setdiff(cols_to_encode, names(dt_cit))
if (length(miss)) stop("Missing columns in dt_cit: ", paste(miss, collapse = ", "))

# Encode in place (overwrite originals) + keep a mapping table
maps <- vector("list", length(cols_to_encode))
names(maps) <- cols_to_encode

for (cl in cols_to_encode) {
  x <- dt_cit[[cl]]
  
  # Safe character view; keep NA as NA
  x_chr <- as.character(x)
  nz <- !is.na(x_chr)
  x_chr[nz] <- trimws(x_chr[nz])  # optional, removes stray spaces
  
  # Levels in order of first appearance (exclude NA)
  levs <- unique(x_chr[nz])
  
  # Factor with fixed levels; NA stays NA (exclude=NULL)
  f <- factor(x_chr, levels = levs, exclude = NULL)
  
  # 0-based codes; NA -> NA_integer_
  codes <- as.integer(f) - 1L
  codes[is.na(x_chr)] <- NA_integer_
  
  # Overwrite the original column in-place
  set(dt_cit, j = cl, value = codes)
  
  # Store mapping for this column
  maps[[cl]] <- data.table(
    column = cl,
    value  = levs,
    code   = seq_along(levs) - 1L
  )
}

# Tidy mapping table
encoding_map <- rbindlist(maps, use.names = TRUE)

# ---- quick check (optional) ----
# print(encoding_map)
# str(dt_cit[, ..cols_to_encode])




setDT(dt_cit)





growth_factors_small<-read_csv("Data/CIT/growfactors_cit_macedonia.csv")%>%
  data.table()



SimulationYear<-2021

# weights_cit<-read_csv("Data/CIT/cit_weights_macedonia.csv")%>%
#   data.table()



n <- NROW(dt_cit)

same_weight<-1

#same_weight<-56  ### #<--- Fiscal concul

weights_cit <- data.table(
  t0 = rep(same_weight, n),
  t1 = rep(1, n),
  t2 = rep(1, n),
  t3 = rep(1, n),
  t4 = rep(1, n),
  t5 = rep(1, n),
  t6 = rep(1, n)
)
rm(n)



MACRO_FISCAL_INDICATORS <- read_excel("Data/CIT/macro_indicators.xlsx")
