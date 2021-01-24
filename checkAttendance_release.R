library(readxl)
library(tidyverse)
library(fuzzyjoin)

# setwd() to current folder

# ==== NOTE ===== 
# 1. File naming convention: each zoom exported file was named after the class session. 
# For example, Zoom records of participants' online activity for the second class session is named as "class2.xlsx".
# the data frame takes the following structure: 
# structure(list(Name = "Student Name", Email = "Student Email", 
#                `Join Time` = structure(1610561100, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
#                `Leave Time` = structure(1610561340, tzone = "UTC", class = c("POSIXct","POSIXt")),
#                `Duration(Minutes)` = 4, `Alternative Score` = NA), row.names = c(NA, -1L), 
#           class = c("tbl_df", "tbl", "data.frame"))

# 2. gradebook is a direct export from Canvas's gradebook tab.

# 3. Kellogg asked students who audited the class to add "Admit" or "admit" to their profile in Zoom sessions. 
                                                                                 
# ==== load and process function ===
ZoomRecord <- function(session){
  fileName <- paste0('class',session,'.xlsx', sep='')
  df <- read_xlsx(fileName)
  df <- tibble(df)
  df_result <- df %>% group_by(Name) %>% 
    summarize(duration = sum(`Duration(Minutes)`)) %>% 
    filter(!is.na(Name)) %>% 
    filter(!str_detect(Name, '[Aa]dmit')) %>% 
    filter(!str_detect(Name, '\\d+'))
  return(df_result)
}

gradebook <- read_csv('gradebook.csv')
namelist <- str_split(gradebook$Student, ', ', simplify = T)
gradebook$name_reorder <- str_c(namelist[,2], ' ',namelist[,1]) # reorder into "firstName lastName"

class2 <- ZoomRecord(2)
class3 <- ZoomRecord(3)

# ====  merge data sets ==== 
merge_class2 <- fuzzyjoin::stringdist_left_join(gradebook, class2, by=c(name_reorder= 'Name'), method='lcs')
merge_class3 <- fuzzyjoin::stringdist_left_join(gradebook, class3, by=c(name_reorder= 'Name'), method='lcs')

