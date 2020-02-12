
#bringing in the datasheet
#2/11/2020
library(googlesheets4)
ss <- sheets_find()

club_codes <- read_sheet("15fd4sUUpy3eY5fAcgolHsG4sOTpcxmMZL9W06vpkYIY")
club_codes
