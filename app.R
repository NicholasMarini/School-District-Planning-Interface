library(shiny)
library(shinydashboard)
library(shinyjs)
library(data.table)
library(DT)
library(dplyr)
library(shinyauthr)
library(infix)
library(googledrive)
library(googlesheets4)
library(IDPmisc)
library(reshape2)
library(leaflet)
library(tidyverse) 
library(scales)
# library(rJava)
library(XLConnect)
# library(xlsx)
library(htmlwidgets)
library(shinyBS)
library(V8)
library(uuid)
library(yaml)
library(aws.signature)
library(aws.s3)
library(shinyalert)
library(sortable)
library(ragtop)
library(waiter)

config <- read_yaml('config.yaml')



Sys.setenv("AWS_ACCESS_KEY_ID" = config$AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = config$AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = config$DEFAULT_REGION)

source('helpers.R')

Primary_Bucket <- 'ips-tool-scenario-data' #'ipsppt2020' 

get_scenarios_from_s3 <- function() {
  get_object('scenario_metadata.csv', bucket = Primary_Bucket, region = 'us-east-2') %>% 
    rawToChar() %>% 
    read.table(text = ., sep = ',', header =TRUE)
}

jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"

#Misc Function

#Row Names to Column Names

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

save_google_sheet_data_to_s3 <- function() {
  wkBk_id = "https://docs.google.com/spreadsheets/d/1Rgbu8ZtrubhAMfLPiP_DFe-L3bFB4As9wMsyA2dV_GM/edit#gid=0"
  slDf <- sheets_read(ss = wkBk_id, sheet = "School.Level", col_types = ("cccccccccnnnnnnccncccncccccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnc"), na = "NA")
  glDf <- sheets_read(ss = wkBk_id, sheet = "Grade.Level", col_types  = "nccnnnnnnnnnnnn",  na = "NA" )
  s3saveRDS(x = slDf, bucket = Primary_Bucket, object = str_glue('data/slDf.rds'), region = 'us-east-2')
  s3saveRDS(x = glDf, bucket = Primary_Bucket, object = str_glue('data/glDf.rds'), region = 'us-east-2')
}

#SBA Formula Function------------------------------------------------------------------------------------------------

# Set constants

gen_allo_c <- 3980

sc_prek_allo_c <- 910

ell_allo_c <- 1008.81

pov_allo_c <- 796

gen_enr_a <- 0

sc_prek_a <- 0

ell_a <- 0

pov_a <- 0

sba_scenario_total <- 0

sba.calculation <- function(enrollment, sc_prek, ell, direct_cert) {
  
  gen_enr_a <- enrollment * gen_allo_c
  
  sc_prek_a <- sc_prek #* sc_prek_allo_c
  
  ell_a <- ell #* ell_allo_c
  
  pov_a <- ceiling(enrollment * direct_cert) * pov_allo_c
  
  sba_scenario_total <- gen_enr_a + sc_prek_a + ell_a + pov_a
  
  return(sba_scenario_total)
}

#Baseline Formula Function-------------------------------------------------------------------------------------------

ap_constant <- 124135.6

bk_constant <- 61785.56

ms_constant <- 87528.60

ma_constant <- 30780.03

t_constant <- 79184.91

sped_constant <- 79184.91

sw_constant <- 82628.71

supplies_constant <- 33

books_constant <- 8

sub_constant <- 1565.20 

ap <- 0

bk <- 0

ms <- 0

ma <- 0

teach <-0

sped <- 0

sw <- 0

baseline_scenario_total <- 0

baseline.calculation <- function(enrollment, teacher, school_lvl, direct_cert) {
  
  
  ap <- case_when(
    enrollment < 300 | (enrollment < 400 & enrollment >= 300 & direct_cert < .65 ) ~ 0,
    (enrollment < 800 & (enrollment >= 400)) | ((enrollment < 400 & enrollment >= 300 & direct_cert >= .65)) ~ 1,
    enrollment >= 800 & enrollment < 1200 ~ 2,
    enrollment >= 1200 & enrollment < 1600 ~ 3,
    enrollment >= 1600 & enrollment < 2000 ~ 4,
    enrollment >= 2000 ~ 5
  )
  
  
  # Book-Keeper
  
  bk <- case_when(
    enrollment < 800 ~ 1,
    enrollment >= 800 & enrollment < 1200 ~ 2,
    enrollment >= 1200 & enrollment < 1600 ~ 3, 
    enrollment >= 1600 & enrollment < 2000 ~ 4,
    enrollment >= 1600 ~ 5
  )
  
  
  # Media  
  
  ms <- case_when(
    school_lvl == "HS" ~ 1 * ms_constant,
    school_lvl != "HS" ~ 1 * ma_constant
  )
  
  
  # Sped
  
  sped <- case_when(
    enrollment < 800 ~ 3,
    enrollment >= 800 & enrollment <= 1100 ~ 4,
    enrollment > 1100 ~ 5
  )
  
  
  sw <- case_when(
    enrollment < 400 & school_lvl == "HS" ~ 2,
    enrollment < 400 & school_lvl != "HS" ~ 1,
    (enrollment >= 400 & enrollment < 800) & school_lvl == "HS" ~ 3,
    (enrollment >= 400 & enrollment < 800) & school_lvl != "HS" ~ 2,
    (enrollment >= 800 & enrollment < 1200) & school_lvl == "HS" ~ 4,
    (enrollment >= 800 & enrollment < 1200) & school_lvl != "HS" ~ 3,
    (enrollment >= 1200 & enrollment < 1600) & school_lvl == "HS" ~ 5, 
    (enrollment >= 1200 & enrollment < 1600) & school_lvl != "HS" ~ 4, 
    enrollment >= 1600 & school_lvl == "HS" ~ 6,
    enrollment >= 1600 & school_lvl != "HS" ~ 5
  )
  
  
  baseline_scenario_total <- (ap * ap_constant) + (bk * bk_constant) + (ms) + (teacher * t_constant) + (sped * sped_constant) + (sw * sw_constant) + books_constant * enrollment + supplies_constant *enrollment + sub_constant*(ap+1+teacher+sped+sw)
  
  (baseline_scenario_total)
}

#Enrollment Input Tables data setup--------------------------------------------------------------------------------------

Grade <- c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
order <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
index <- data.frame(order,Grade)

#New School Form-------------------------------------------------------------------------------------------------------

# which fields get saved 
fieldsAll <- c("School.Name.SL", "IPS.ID.SL","School.Level", "Access.Type", "Capacity","Longitude","Latitude", "new_school", 
               "Classrooms", "Custodial.Allocation", "Sum.Utilities", "amt_outstanding_last", "debt_holder",
               "Bldg.Weighted.Condition.Score", "Bldg.Academic.Readiness.Score", "Bldg.Technology.Score", "Grounds.Score",
               "Combined.Score", "High.Needs", "High.Needs.and.SQR", "School.Performance", "School.Accountability.Metric", "School.Accountability","Direct.Cert")

# which fields are mandatory
fieldsMandatory <- c("School.Name.SL", "School.Level", "Access.Type", "Capacity", "Longitude","Latitude")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

### save the results to a file - Unneeded Feature
# saveData <- function(data) {
#   fileName <- sprintf("%s_%s.csv",
#                       humanTime(),
#                       digest::digest(data))
#   
#   write.csv(x = data, file = file.path(responsesDir, fileName),
#             row.names = FALSE, quote = TRUE)
# }

# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

#UI where all of the panels,tabs, and outputs exist------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar( disable = TRUE),
  dashboardBody(
    useShinyjs(),
    use_waiter(),
    extendShinyjs(text = "shinyjs.refresh = function() {history.go(0);}", functions = c("refresh")),
    useShinyalert(),
    tags$head(
      tags$style(type="text/css", 
                 ".skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper{ background-color: #ecf0f5}
                  #bucketlist_options {max-height: 420px; overflow-y: scroll;}
                  .rank-list {max-height: 420px; overflow-y: scroll;}
                 "
      )),
    shinyauthr::loginUI('login'),
    uiOutput('app_ui')
    #setBackgroundColor("#2171B5"),
    
  )
)

#Server where all dynamic outputs are generated-------------------------------------------------------------------
server <- function(input, output, session) {
  
  options(gargle_oauth_cache = '.secrets',
          gargle_oauth_email = 'nmarini@kitamba.com')
  
  sheets_auth()
  
  slDf <- s3readRDS(object = str_glue('data/slDf.rds'), bucket = Primary_Bucket, region = 'us-east-2')
  glDf <- s3readRDS(object = str_glue('data/glDf.rds'), bucket = Primary_Bucket, region = 'us-east-2')
  
  # slDf <- slDf[!(slDf$School.Name.SL == "Impact Academy" | slDf$School.Name.SL == "Thrival Academy Indy"),]
  # glDf <- glDf[!(glDf$School.Name == "Impact Academy" | glDf$School.Name == "Thrival Academy Indy"),]
  
  enrInputMstr <- subset(glDf, select = c(Order, School.Name, Year, Count))
  enrInputMstr <- dcast(enrInputMstr, Order + School.Name ~ Year, value.var = 'Count')
  enrInputMstr <- merge(enrInputMstr, index, by.x = "Order", by.y = "order", all.x = TRUE)
  enrInputMstr <- enrInputMstr[,c(1,2,9,6,7,8)]
  enrInputMstr <- enrInputMstr %>%
    mutate(me_change = enrInputMstr$`2021` * 1)
  enrInputMstr$me_delta <- 0
  
  
  #Subgroup Tables data setup--------------------------------------------------------------------------------------------
  
  #Isolates and reformats Mobility and Direct columns from school level dataframe for Subgroup Tables
  subGroupSl <- subset(slDf, select = c(School.Name.SL, Mobility.Rate, Direct.Cert))
  setnames(subGroupSl , "School.Name.SL", "School.Name")
  subGroupSl[subGroupSl == "NA"] <- 0
  as.integer(subGroupSl$Mobility.Rate)
  as.integer(subGroupSl$Direct.Cert)
  
  #Isolates and reformats Ethnicity columns from grade level dataframe for Subgroup Tables
  subGroupMstr <- subset(glDf, select = c(DOE.ID, School.Name, Grade, Year, Count,Asian, Black, Latinx, White, Multiethnic))
  subGroupMstr <- subGroupMstr[subGroupMstr$Year == '2021' & subGroupMstr$Grade != 'alt_ed' & subGroupMstr$Grade != 'sp_ed'& subGroupMstr$Grade != 'ell',]
  subGroupMstr <- subset(subGroupMstr, select = -c(Grade))
  subGroupMstr <- subGroupMstr %>%
    group_by(School.Name, Year) %>%
    summarise_each(funs(sum))
  
  #Calculates and reformats Enthnicity Rates for Subgroup Tables
  subGroupMstr <- subGroupMstr %>%
    mutate(Asian = Asian / Count)  %>%
    mutate(Black = Black / Count) %>%
    mutate(Latinx = Latinx / Count) %>%
    mutate(White = White / Count)  %>%
    mutate(Multiethnic = Multiethnic / Count)
  
  subGroupMstr <- subset(subGroupMstr, select = c(School.Name, Asian, Black, Latinx, Multiethnic, White))
  
  subGroupMstr <- merge(subGroupMstr,subGroupSl, by = "School.Name")
  
  
  #is.na(subGroupMstr)<-sapply(subGroupMstr, is.infinite)
  
  grdLvlSubGrp <- glDf %>% 
    filter(Year == 2021, !Grade %in% c('alt_ed', 'ell')) %>% 
    group_by(School.Name, Grade) %>%
    gather(Race, Students, c(Black, Asian, Latinx, White, Multiethnic)) %>% 
    ungroup() %>% 
    mutate(Share = Students / Count,
           Share = ifelse(is.nan(Share), 0, Share)) %>% 
    select(School.Name, Count, Students, Grade, Race, Share)
  
  grdLvlSubGrp[grdLvlSubGrp$Count == 0, "Students"] <- 0
  
  #Input Schools List----------------------------------------------------------------------------------------------------
  
  inputSchList <- subGroupMstr$School.Name
  # inputSchList <- inputSchList[!inputSchList %in% c("Impact Academy", "KIPP Indy College Prep Middle School", "KIPP Indy Legacy High School", "KIPP Indy Unite Elementary School", "Phalen Leadership Academy@George H Fisher") ]
  inputSchList <- sort(inputSchList)
  #(inputSchList)
  
  #School Level Outputs Setup -----------------------------------------------------------------------------------------
  
  fCosts <- slDf
  water_col_num <- which( colnames(fCosts)=="Water")
  fCosts <- fCosts %>% 
    mutate(Total.Utilities.Cost = select(.,water_col_num:(water_col_num+8)) %>% rowSums(na.rm = TRUE))
  fCosts <- fCosts %>% 
    mutate(Total.Custodial.Cost = Custodial.Allocation * 60169)
  
  sch_lvl_outputs <- data.frame("Schools" = slDf$School.Name.SL)
  
  ordered_gldf <- glDf[order(glDf$Order,glDf$School.Name),]
  
  grades_served <- aggregate(Grade ~ School.Name, data = ordered_gldf[ordered_gldf$Count != 0 & ordered_gldf$School.Name %in% slDf$School.Name.SL & ordered_gldf$Year == 2021,], toString)
  
  #Total Enrollment Subset
  # schLvl_tot_enr_d <-  aggregate(ordered_gldf$Count[ordered_gldf$Year == "2021" & !(ordered_gldf$School.Name %in% c("Crispus Attucks Med Mgnt Jr HS","George Washington Comm Jr HS","Key Learning Community Elem Sch","Key Learning Community High School","Key Learning Community Jr High Sch","Purdue Polytechnical High School","Arlington Community High School", "Broad Ripple Mgnt Jr HS-Prfm Arts", "John Marshall Community Jr HS"))], by = list(School = ordered_gldf$School.Name[ordered_gldf$Year == "2021" & !(ordered_gldf$School.Name %in% c("Crispus Attucks Med Mgnt Jr HS","George Washington Comm Jr HS","Key Learning Community Elem Sch","Key Learning Community High School","Key Learning Community Jr High Sch","Purdue Polytechnical High School","Arlington Community High School", "Broad Ripple Mgnt Jr HS-Prfm Arts", "John Marshall Community Jr HS"))]) , FUN = sum, na.rm = TRUE) 
  # added_rows <- data.frame(School = c("Emmerich Manual High School","George H Fisher Elementary School","KIPP:Indy Schools","McFarland Alternative Center","Step Ahead Academy","Thomas Carr Howe Community School"), x = 0)
  # schLvl_tot_enr_d <- rbind(schLvl_tot_enr_d, added_rows)
  # schLvl_tot_enr_d <- schLvl_tot_enr_d[order(schLvl_tot_enr_d$School),]
  
  sch_lvl_outputs$School_Level <- slDf$School.Level
  sch_lvl_outputs$General_Enrollment_Default <- slDf$enrollment 
  sch_lvl_outputs$General_Enrollment_Scenario <- slDf$enrollment 
  sch_lvl_outputs$General_Enrollment_Delta <- sch_lvl_outputs$General_Enrollment_Scenario - sch_lvl_outputs$General_Enrollment_Default
  
  # Commented out Total Enrollment for future use 
  
  # sch_lvl_outputs$Total_Enrollment_Default[match(schLvl_tot_enr_d$School, sch_lvl_outputs$Schools)] <- schLvl_tot_enr_d$x
  # sch_lvl_outputs$Total_Enrollment_Scenario <- sch_lvl_outputs$Total_Enrollment_Default 
  # sch_lvl_outputs$Total_Enrollment_Delta <- sch_lvl_outputs$Total_Enrollment_Scenario - sch_lvl_outputs$Total_Enrollment_Default
  
  sch_lvl_outputs$School_Performance <- slDf$School.Performance
  sch_lvl_outputs$School_Accountability_Metric <- slDf$School.Accountability.Metric
  sch_lvl_outputs$School_Accountability <- slDf$School.Accountability
  sch_lvl_outputs$High_Needs <- slDf$High.Needs
  sch_lvl_outputs$High_Needs_and_SQR <- slDf$High.Needs.and.SQR
  sch_lvl_outputs$Facility_Condition_Score <- slDf$Bldg.Weighted.Condition.Score
  sch_lvl_outputs$Facility_Academic_Readiness_Score <- slDf$Bldg.Academic.Readiness.Score
  sch_lvl_outputs$Facility_Technology_Score <- slDf$Bldg.Technology.Score
  sch_lvl_outputs$Facility_Grounds_Score <- slDf$Grounds.Score
  sch_lvl_outputs$Combined_Facility_Score <- slDf$Combined.Score
  sch_lvl_outputs$Graduation_Rate <- slDf$Grad.Rate
  sch_lvl_outputs$Grades_Default[sch_lvl_outputs[,1] %in% grades_served$School.Name] <- grades_served[grades_served$School.Name %in% slDf$School.Name.SL, 2]
  sch_lvl_outputs$Grades_Scenario <- sch_lvl_outputs$Grades_Default
  sch_lvl_outputs$Grades_Delta <- 0
  sch_lvl_outputs$Capacity_Default <- slDf$Capacity
  
  #Fix any non-zero Enr/0 capacity rows to avoid Div-by-0 errors
  sch_lvl_outputs$Capacity_Default[sch_lvl_outputs$Capacity_Default == 0 & sch_lvl_outputs$General_Enrollment_Default > 0] <-  sch_lvl_outputs$General_Enrollment_Default[sch_lvl_outputs$Capacity_Default == 0 & sch_lvl_outputs$General_Enrollment_Default > 0]
  
  sch_lvl_outputs$Capacity_Scenario <- sch_lvl_outputs$Capacity_Default
  sch_lvl_outputs$Capacity_Delta <- sch_lvl_outputs$Capacity_Scenario - sch_lvl_outputs$Capacity_Default
  sch_lvl_outputs$Utilization_Default <- sch_lvl_outputs$General_Enrollment_Default/sch_lvl_outputs$Capacity_Default 
  sch_lvl_outputs$Utilization_Scenario <- sch_lvl_outputs$Utilization_Default
  sch_lvl_outputs$Utilization_Delta <- sch_lvl_outputs$Utilization_Scenario - sch_lvl_outputs$Utilization_Default
  sch_lvl_outputs$Baseline_Default <- slDf$total_baseline
  sch_lvl_outputs$Baseline_Scenario <- sch_lvl_outputs$Baseline_Default
  sch_lvl_outputs$Baseline_Delta <- 0
  sch_lvl_outputs$SBA_Default <- slDf$total_sba
  sch_lvl_outputs$SBA_Scenario <- sch_lvl_outputs$SBA_Default
  sch_lvl_outputs$SBA_Delta <- 0
  sch_lvl_outputs$Custodial_Allocation_Default <- slDf$Custodial.Allocation
  sch_lvl_outputs$Custodial_Allocation_Scenario <- sch_lvl_outputs$Custodial_Allocation_Default
  sch_lvl_outputs$Custodial_Allocation_Delta <- 0
  sch_lvl_outputs$Custodial_Cost_Default <- fCosts$Total.Custodial.Cost
  sch_lvl_outputs$Custodial_Cost_Scenario <- sch_lvl_outputs$Custodial_Cost_Default
  sch_lvl_outputs$Custodial_Cost_Delta <- 0
  sch_lvl_outputs$Utilities_Cost_Default <- fCosts$Total.Utilities.Cost
  sch_lvl_outputs$Utilities_Cost_Scenario <- sch_lvl_outputs$Utilities_Cost_Default 
  sch_lvl_outputs$Utilities_Cost_Delta <- 0 
  sch_lvl_outputs$Outstanding_Debt <- slDf$amt_outstanding_last
  sch_lvl_outputs$Owner <- slDf$debt_holder
  
  
  sch_lvl_outputs[is.na(sch_lvl_outputs)] <- 0
  sch_lvl_outputs[sch_lvl_outputs == ""] <- 0
  sch_lvl_outputs$School_Performance[sch_lvl_outputs$School_Performance == 0] <- "Not Available"
  sch_lvl_outputs$Owner[sch_lvl_outputs$Owner == 0 & sch_lvl_outputs$Owner == ""] <- "Not Available"
  sch_lvl_outputs$School_Performance[sch_lvl_outputs$School_Performance == 0] <- "Not Available"
  sch_lvl_outputs$Owner[sch_lvl_outputs$Owner == 0] <- "Not Available"
  
  sch_lvl_subgroups <- grdLvlSubGrp %>% 
    group_by(School.Name, Race) %>% 
    summarise(Default = sum(Students),
              Scenario = 0,
              Delta = Scenario - Default)
  
  sch_lvl_subgroups_defaults <- grdLvlSubGrp %>% 
    group_by(School.Name) %>% 
    mutate(total = sum(Count)) %>% 
    group_by(Schools = School.Name, Race) %>% 
    summarise(Share = sum(Students) / sum(Count)) %>% 
    ungroup() %>% 
    mutate(Share = ifelse(is.nan(Share), 0, Share),
           Race = paste0('Race_', Race, '_Default')) %>% 
    spread(Race, Share)
  
  sch_lvl_subgroups_scenarios <- sch_lvl_subgroups_defaults %>% 
    set_names(str_replace_all(names(.), '_Default$', '_Scenario'))
  
  sch_lvl_subgroups_deltas <- sch_lvl_subgroups_scenarios %>% 
    mutate_at(vars(starts_with('Race')), function(x) 0) %>% 
    set_names(str_replace_all(names(.), '_Scenario$', '_Delta'))
  
  sch_lvl_outputs <- sch_lvl_outputs %>%
    mutate(Schools = as.character(Schools)) %>% 
    left_join(sch_lvl_subgroups_defaults, by = 'Schools') %>% 
    left_join(sch_lvl_subgroups_scenarios, by = 'Schools') %>% 
    left_join(sch_lvl_subgroups_deltas, by = 'Schools') %>% 
    #starts_with('School_Performance'),
    select(Schools,starts_with('General_Enrollment'),  starts_with('School_Accountability'),  starts_with('High_Needs'), Facility_Condition_Score, Facility_Academic_Readiness_Score, Facility_Technology_Score, Facility_Grounds_Score, Combined_Facility_Score,  starts_with('Graduation_Rate'), starts_with('Capacity'), starts_with('Utilization'), starts_with('Grades_'), starts_with('Baseline'), starts_with('SBA'),
           starts_with('Utilities'), starts_with('Custodial'), starts_with('Race_Asian'), starts_with('Race_Black'), starts_with('Race_Latinx'), starts_with('Race_White'), starts_with('Race_Multiethnic'),
           everything()) %>% 
    mutate_at(vars(starts_with('Race')), function(x) coalesce(x, 0))
  
  school_level_summary_table_base_names <- names(sch_lvl_outputs[,!(names(sch_lvl_outputs) %in% c("School_Level"))])
  
  #school_level_summary_table_percentage_columns <- school_level_summary_table_names[str_detect(school_level_summary_table_names, 'Utilization')]
  #school_level_summary_table_numeric_columns <- school_level_summary_table_names[!str_detect(school_level_summary_table_names, 'Utilization|Grades')]
  
  #Map data setup------------------------------------------------------------------------------------------------------
  mDf <- slDf
  mDf <- mDf[!(mDf$enrollment==0),]
  mDf <- mDf[!(mDf$Latitude==0 & mDf$Longitude==0),]
  mDf <- subset(mDf, select = c(School.Name.SL, School.Performance, School.Level, Access.Type, Latitude, Longitude, enrollment, Utilization, Direct.Cert, Combined.Score))
  # mDf[mDf == "NA"] <- 0
  # mDf[mDf == 0] <- NA
  # mDf <- na.omit(mDf)
  
  pchIcons = function(shape = 1, color = 'black', width = 30, height = 30, bg = "transparent", ...) {
    f = tempfile(fileext = '.png')
    png(f, width = width, height = height, bg = bg)
    par(mar = c(0, 0, 0, 0))
    plot.new()
    # points(.5, .5, pch = pch[i], col = col[i], cex = min(width, height) / 8, ...)
    points(.5, .5, pch = shape, bg = color, col = color, cex = min(width, height) / 8, ...)
    dev.off()
    f
  }
  
  colors <- c('ES' = '#8A2BE2', 'MS' = '#0000FF', 'K-8' = '#00FF00', 'HS' = '#FFFF00')
  shapes <- c('Neighborhood' = 21, 'TBD' = 22, 'Innovation' = 25, 'Choice' = 24)
  shape_ids <- c('Circle' = 21, 'TriangleDown' = 25, 'Square' = 22, 'Triangle' = 24)
  
  icon_ids <- expand.grid(Access.Type = c('Neighborhood', 'TBD', 'Innovation', 'Choice'),
                          School.Level = c('ES', 'MS', 'K-8', 'HS'), stringsAsFactors = FALSE) %>% 
    mutate(icon_id = row_number(),
           color = colors[School.Level],
           shape = shapes[Access.Type])
  
  
  
  access_types <- unique(mDf$Access.Type)
  school_levels <- unique(mDf$School.Level)
  
  iconFiles <- map(1:nrow(icon_ids), function(i) {
    this_row <- slice(icon_ids, i)
    shape <- this_row$shape
    color <- this_row$color
    pchIcons(shape = shape, color = color)
  }) %>% unlist()
  
  
  #Enrollment Zone Map Default Data Frame----------------------------------------------------------------------
  
  zMDf <- mDf
  zMDf$Zone <- 0
  
  zMDf_Default <- mDf
  zMDf_Default$Zone <- 0
  
  #Impact Table Setup -----------------------------------------------------------------------------------------
  impact_Table_SubSchLL <- data.frame(subset(slDf, select = c(School.Name.SL, Latitude, Longitude)))
  # impact_Table_SubSchLL <- impact_Table_SubSchLL[!impact_Table_SubSchLL %in% c("Impact Academy", "KIPP Indy College Prep Middle School", "KIPP Indy Legacy High School", "KIPP Indy Unite Elementary School", "Phalen Leadership Academy@George H Fisher") ]
  impact_Table_Source <- data.frame(subset(slDf, select = c(School.Name.SL, School.Level, Capacity, enrollment, Utilization, Access.Type, Condition, Latitude, Longitude)))
  # impact_Table_Source <- impact_Table_Source[!impact_Table_Source %in% c("Impact Academy", "KIPP Indy College Prep Middle School", "KIPP Indy Legacy High School", "KIPP Indy Unite Elementary School", "Phalen Leadership Academy@George H Fisher") ]
  
  impact_Table_Source <- impact_Table_Source[impact_Table_Source$Longitude != 0, ]
  
  #Access Type School Counts Table--------------------------------------------------------------------------------
  
  accTCntD <- slDf
  accTCntD <- subset(accTCntD, select = c(School.Name.SL, Active, Access.Type))
  accTCntD <- na.omit(accTCntD)
  #accTCntD <- accTCntD[accTCntD$Active != 2]
  accTCntD <- accTCntD[accTCntD$Active == 1,]
  
  accTCntD <- subset(accTCntD, select = -c(Active))
  accTCntD <- accTCntD %>% count(Access.Type) %>% spread(Access.Type, n)
  
  accTCntD$All <- as.integer(rowSums(accTCntD[,1:3]))
  
  accTCntD[2,] <- 0
  accTCntD[3,] <- 0
  
  accTCntD$Default.or.Scenario <- c("Default", "Scenario", "Delta")
  accTCntD <- accTCntD[,c(5,4,1,2,3)]
  
  #Facilites Table Data Setup------------------------------------------------------------------------------------------------------
  
  utl_sum <- data.frame(" " = c("Total # of Facilities","Total Custodial Costs","Total Utilities Cost","Total Facilities Cost", "Total Outstanding Debt", "Total # of IPS-owned Facilities",
                                "Total # of MSBC-owned Facilities","Total # of Split-owned Facilities", "Total # of Facilities owned by other parties",
                                "# High Quality Facilities","# Low Quality Facilities"))
  
  
  utl_sum$Default <- 0
  utl_sum$Scenario <- 0
  utl_sum$Delta <- 0
  utl_sum[1,2] <- sum(is.na(slDf$Combined.Score) == FALSE, na.rm = TRUE)
  utl_sum[3,2] <- sum(fCosts[,"Total.Utilities.Cost"],na.rm = TRUE) 
  utl_sum[2,2] <- sum(fCosts$Total.Custodial.Cost,na.rm = TRUE) 
  utl_sum[4,2] <- utl_sum[3,2] + utl_sum[2,2] 
  utl_sum[5,2] <- sum(is.na(slDf$Combined.Score) == FALSE & slDf$amt_outstanding_last, na.rm = TRUE)
  utl_sum[6,2] <- sum(is.na(slDf$Combined.Score) == FALSE & slDf$debt_holder == "IPS", na.rm = TRUE)
  utl_sum[7,2] <- sum(is.na(slDf$Combined.Score) == FALSE & slDf$debt_holder == "MSBC", na.rm = TRUE)
  utl_sum[8,2] <- sum(is.na(slDf$Combined.Score) == FALSE & slDf$debt_holder == "Split", na.rm = TRUE)
  utl_sum[9,2] <- utl_sum[1,2] - sum(utl_sum[6,2], utl_sum[7,2], utl_sum[8,2], na.rm = TRUE)
  utl_sum[10,2] <- sum(fCosts$Combined.Score > 80, na.rm =TRUE) 
  utl_sum[11,2] <- sum(fCosts$Combined.Score < 60, na.rm =TRUE)
  
  utl_sum[2:5,2] <- dollar(utl_sum[2:5,2])
  
  #School Costs and Counts: Default----------------------------------------------------------------------------------------------------------
  
  sch_count <- data.frame("School Count" = c("Total IPS Schools", "Total Receiving SBA", "Total Receiving Baseline"))
  
  sch_count$Default <- 0
  sch_count$Scenario <- 0
  sch_count$Delta <- 0
  sch_count[1,2] <- sum(fCosts$total_sba > 0, na.rm = TRUE) + sum(fCosts$total_baseline > 0, na.rm = TRUE)
  sch_count[2,2] <- sum(slDf$total_sba > slDf$total_baseline)
  sch_count[3,2] <- sum(slDf$total_sba < slDf$total_baseline)
  
  sch_cost <- data.frame("School Costs" = c("Total Overall School IPS", "Total School Baseline", "Cost to Bring All to Baseline","Total School SBA"))
  
  sch_costT <- subset(slDf, select = c(School.Name.SL, enrollment, total_baseline, total_sba))
  sch_costT$enrollment[match(sch_lvl_outputs$Schools, sch_costT$School.Name.SL)] <- sch_lvl_outputs$General_Enrollment_Default
  sch_costT$IPSTotal <- ceiling(pmax(sch_costT$total_baseline, sch_costT$total_sba))
  
  # Baseline Supplement Calc	
  col_list <- c(2,5,24,28,44,45,47,48)
  
  teachers_default_df <- glDf %>% 
    filter(Year == 2021) %>% 
    select(School.Name, Grade, Count) %>% 
    mutate(Teachers = case_when(Grade == 'KG' ~ ceiling(Count / 24),
                                Grade == '1' ~ ceiling(Count / 25),
                                Grade %in% c('2') ~ ceiling(Count / 28),
                                Grade %in% c('3') ~ ceiling(Count / 28),
                                !Grade %in% c('alt_ed', 'ell', 'sp_ed') ~ (Count / 32),
                                TRUE ~ 0),
           Teachers = ceiling(Teachers)) %>% 
    group_by(School.Name) %>% 
    summarise(Teachers = sum(Teachers, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(Teachers > 0) %>% 
    inner_join(select(slDf, School.Name.SL, IPS.ID.SL, School.Level), by = c('School.Name' = 'School.Name.SL')) %>% 
    select(Names = School.Name, 
           Schools = IPS.ID.SL,
           Grades = School.Level,
           Teachers)
  
  
  # teachers_default_df <- 
  #   data.frame("Names" = c("Anna Brochhausen School 88","Arlington Community Middle School","Arlington Woods School 99","Arsenal Technical High School","Brookside School 54","Butler University Lab School 55","Butler University Lab School 60","Carl Wilde School 79","Center for Inquiry School 2","Center for Inquiry School 27","Center for Inquiry School 70","Center for Inquiry School 84","Charles Warren Fairbanks School 105","Christian Park School 82","Clarence Farrington School 61","Cold Spring School","Crispus Attucks Medical Magnet HS","Daniel Webster School 46","Edison School of the Arts 47","Eleanor Skillen School 34","Ernie Pyle School 90","Floro Torrence School 83","Francis W Parker School 356","George S Buck School 94","George W Carver School 87","George W. Julian School 57","George Washington High School","H. L. Harshman Math/Science & World Languages","James A. Garfield School 31","James Russell Lowell School 51","James Whitcomb Riley School 43","Jonathan Jennings School 109","Lew Wallace School 107","Longfellow MS Medical Magnet","Louis B. Russell Jr. School 48","Meredith Nicholson School 96","Merle Sidener Gifted Academy School 359","Northwest Community Middle School","Paul I. Miller School 114","Ralph Waldo Emerson School 58","Raymond F. Brandes School 65","Robert Lee Frost School 106","Rousseau Mcclellan School 91","Shortridge High School","Stephen Foster School 67","Super School @ Frederick Douglass School 19","Theodore Potter School 74","Thomas D. Gregg School 15","Thrival Academy Indy","William McKinley School 39","William Penn School 49"),
  #              "Schools" = c("88","522","99","716","54","55","60","79","302","327","370","384","105","82","61","315","718","346","47","34","90","83","356","94","87","57","721","501","31","51","43","109","107","528","48","96","359","523","114","58","65","106","391","714","367","19","74","15","725","39","49"),
  #              "Grades" = c("ES","MS","ES","HS","ES","K-8","K-8","ES","K-8","K-8","K-8","K-8","ES","ES","ES","K-8","HS","K-8","K-8","ES","ES","ES","K-8","ES","K-8","K-8","HS","MS","K-8","ES","K-8","ES","ES","MS","ES","ES","K-8","MS","ES","ES","ES","ES","K-8","HS","K-8","K-8","ES","ES","HS","ES","K-8"),
  #              "Teachers" = c(16,14,16,69,17,16,26,24,18,18,18,18,14,15,20,17,39,19,24,13,15,12,14,14,17,12,22,18,12,14,19,14,22,16,11,16,14,11,13,13,10,15,22,38,21,21,15,22,3,16,23)
  #   )
  
  baseline_df <- slDf %>% 
    select(Names = School.Name.SL, enrollment, School.Level, Direct.Cert) %>% 
    left_join(teachers_default_df, by = 'Names') %>% 
    mutate(baseline = coalesce(baseline.calculation(enrollment, Teachers, School.Level, Direct.Cert), 0))
  
  all_baseline <- baseline_df$baseline
  # all_baseline <- c(1852013.43,1777700.12,1853858.43,7593707.2,0,0,2019950.45,1938954.34,2754860.44,2592868.22,2102135.56,2101110.56,2102094.56,2102094.56,1563541.41,1770730.32,2265480.78,1809153.74,4402227,2181655.67,2593770.22,1481561.3,0,0,0,1770197.32,1398802.19,1562352.41,0,1563336.41,1807841.74,1398187.19,2574031.64,0,2105702.56,0,0,0,0,1399909.19,1562147.41,2095739.76,0,1563992.41,0,0,0,0,0,2428047,1940922.34,1317027.08,0,0,1726230.63,1566247.41,0,1321209.08,1479388.3,0,0,1481151.3,1236563.97,1643881.52,2428580,4320984.89,0,2347788.89,2346886.89,1643881.52,0,2429851,806842.84,0,1853899.43,2510150.11)
  all_default_sba <- slDf[, col_list]	
  all_default_sba$all_sba <- sba.calculation(slDf$enrollment, slDf$sba_SC, slDf$sba_ELL, slDf$Direct.Cert)	
  all_default_sba$all_baseline <- floor(all_baseline * 100) / 100
  all_default_sba$sba_below_baseline <- 0	
  all_default_sba$sba_below_baseline[all_default_sba$all_sba < all_default_sba$all_baseline] <- 1	
  all_default_sba$sba_to_bl <- 0	
  all_default_sba$sba_to_bl[all_default_sba$sba_below_baseline==1] <- all_default_sba$all_baseline[all_default_sba$sba_below_baseline==1] - all_default_sba$all_sba[all_default_sba$sba_below_baseline==1]
  # Temp fixes 
  all_default_sba$sba_to_bl[all_default_sba$School.Name.SL == "Thrival Academy Indy"] <-0
  
  # School Cost Setup	
  sch_cost$Default <- 0	
  sch_cost$Scenario <- 0	
  sch_cost$Delta <- 0	
  sch_cost[1,2] <- sum(sch_costT$IPSTotal)	
  sch_cost[2,2] <- sum(slDf$total_baseline)	
  sch_cost[3,2] <- abs(sum(all_default_sba$sba_to_bl))
  sch_cost[4,2] <- sum(slDf$total_sba)
  sch_cost[,2] <- dollar(sch_cost[,2])
  
  # Per Student School Cost Setup	
  
  cost_ps <- data.frame("Funding Per Student" = c("Total Funding Per Student", "Total Baseline Funding Per Student", "Total SBA Funding Per Student"))
  
  cost_ps$Default <- 0	
  cost_ps$Scenario <- 0	
  cost_ps$Delta <- 0	
  cost_ps[1,2] <- sum(sch_costT$IPSTotal)/sum(sch_costT$enrollment[sch_costT$IPSTotal > 0], na.rm = TRUE)	
  cost_ps[2,2] <- sum(sch_costT$total_baseline[sch_costT$IPSTotal == sch_costT$total_baseline])/ sum(sch_costT$enrollment[sch_costT$IPSTotal == sch_costT$total_baseline & sch_costT$IPSTotal >0 ])	
  cost_ps[3,2] <- sum(sch_costT$total_sba[sch_costT$IPSTotal == sch_costT$total_sba])/ sum(sch_costT$enrollment[sch_costT$IPSTotal == sch_costT$total_sba& sch_costT$IPSTotal >0 ])	
  cost_ps[,2] <- dollar(cost_ps[,2])
  cost_ps[,3] <- dollar(cost_ps[,3])
  cost_ps[,4] <- dollar(cost_ps[,4])
  
  
  #Average School Characteristics by School Level---------------------------------------------------------------------
  
  avg_school_char <- data.frame("Values" = c("Avg. Classrooms", "Avg. Total Enrollment", "Avg. Capacity", "Avg. Utilization","Avg. Combined Facilities Score", "Avg. Student-Teacher Ratio"))
  
  avg_school_char$All.Default <- 0
  avg_school_char$All.Scenario <- 0
  avg_school_char$All.Delta <- 0
  avg_school_char$ES.Default <- 0
  avg_school_char$ES.Scenario <- 0
  avg_school_char$ES.Delta <- 0
  avg_school_char$MS.Default <- 0
  avg_school_char$MS.Scenario <- 0
  avg_school_char$MS.Delta <- 0
  avg_school_char$K8.Default <- 0
  avg_school_char$K8.Scenario <- 0
  avg_school_char$K8.Delta <- 0
  avg_school_char$HS.Default <- 0
  avg_school_char$HS.Scenario <- 0
  avg_school_char$HS.Delta <- 0
  
  names(avg_school_char) <- c("Metrics","All Default","All Scenario","All Delta","ES Default","ES Scenario","ES Delta","MS Default","MS Scenario","MS Delta",
                              "K-8 Default","K-8 Scenario","K-8 Delta","HS Default","HS Scenario","HS Delta")
  
  active_schools <- slDf$IPS.ID.SL[slDf$enrollment != 0]
  
  avg_school_char[1,2] <- ceiling(mean(slDf$Classrooms[slDf$enrollment != 0], na.rm = TRUE))
  avg_school_char[2,2] <- ceiling(mean(slDf$enrollment[slDf$enrollment != 0], na.rm = TRUE))
  avg_school_char[3,2] <- ceiling(mean(sch_lvl_outputs$Capacity_Default[slDf$enrollment > 0], na.rm = TRUE))
  avg_school_char[4,2] <- round(mean(sch_lvl_outputs$Utilization_Default[slDf$enrollment != 0], na.rm = TRUE),3)
  avg_school_char[5,2] <- mean(sch_lvl_outputs$Combined_Facility_Score[slDf$enrollment != 0 & sch_lvl_outputs$Combined_Facility_Score > 0], na.rm = TRUE) 
  avg_school_char[6,2] <- ceiling(sum(slDf$enrollment[slDf$enrollment != 0], na.rm = TRUE) / 
                                    sum(teachers_default_df$Teachers, na.rm = TRUE))
  
  #ES 
  avg_school_char[1,5] <- ceiling(aggregate(slDf$Classrooms, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                  [nrow(aggregate(slDf$Classrooms, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                    ncol(aggregate(slDf$Classrooms, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))])
  
  avg_school_char[2,5] <- ceiling(aggregate(slDf$enrollment, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                  [nrow(aggregate(slDf$enrollment, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                    ncol(aggregate(slDf$enrollment, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))])
  
  avg_school_char[3,5] <- ceiling(aggregate(sch_lvl_outputs$Capacity_Default, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                  [nrow(aggregate(sch_lvl_outputs$Capacity_Default, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                    ncol(aggregate(sch_lvl_outputs$Capacity_Default, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))])
  
  avg_school_char[4,5] <- round(aggregate(sch_lvl_outputs$Utilization_Default, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                [nrow(aggregate(sch_lvl_outputs$Utilization_Default, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                  ncol(aggregate(sch_lvl_outputs$Utilization_Default, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))]
                                ,3)
  
  avg_school_char[5,5] <- aggregate(sch_lvl_outputs$Combined_Facility_Score, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, sch_lvl_outputs$Combined_Facility_Score > 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)[nrow(aggregate(sch_lvl_outputs$Combined_Facility_Score, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, sch_lvl_outputs$Combined_Facility_Score > 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                                                                                                                                                                                                                  ncol(aggregate(sch_lvl_outputs$Combined_Facility_Score, by = list(slDf$School.Level == "ES", slDf$enrollment != 0, sch_lvl_outputs$Combined_Facility_Score > 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))]
  
  avg_school_char[6,5] <- ceiling(sum(slDf$enrollment[slDf$enrollment != 0 & slDf$School.Level == "ES" & slDf$Active == 1], na.rm = TRUE) / 
                                    sum(teachers_default_df$Teachers[teachers_default_df$Schools %in% active_schools & teachers_default_df$Grades == "ES"], na.rm = TRUE))
  
  #MS
  avg_school_char[1,8] <- ceiling(aggregate(slDf$Classrooms, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                  [nrow(aggregate(slDf$Classrooms, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                    ncol(aggregate(slDf$Classrooms, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))])
  
  avg_school_char[2,8] <- ceiling(aggregate(slDf$enrollment, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                  [nrow(aggregate(slDf$enrollment, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                    ncol(aggregate(slDf$enrollment, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))])
  
  avg_school_char[3,8] <- ceiling(aggregate(sch_lvl_outputs$Capacity_Default, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                  [nrow(aggregate(sch_lvl_outputs$Capacity_Default, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                    ncol(aggregate(sch_lvl_outputs$Capacity_Default, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))])
  
  avg_school_char[4,8] <- round(aggregate(sch_lvl_outputs$Utilization_Default, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                [nrow(aggregate(sch_lvl_outputs$Utilization_Default, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                  ncol(aggregate(sch_lvl_outputs$Utilization_Default, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))]
                                ,3)
  
  avg_school_char[5,8] <- aggregate(sch_lvl_outputs$Combined_Facility_Score, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, sch_lvl_outputs$Combined_Facility_Score > 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)[nrow(aggregate(sch_lvl_outputs$Combined_Facility_Score, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, sch_lvl_outputs$Combined_Facility_Score > 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                                                                                                                                                                                                                  ncol(aggregate(sch_lvl_outputs$Combined_Facility_Score, by = list(slDf$School.Level == "MS", slDf$enrollment != 0, sch_lvl_outputs$Combined_Facility_Score > 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))]
  
  avg_school_char[6,8] <- ceiling(sum(slDf$enrollment[slDf$enrollment != 0 & slDf$School.Level == "MS" & slDf$Active == 1], na.rm = TRUE) / 
                                    sum(teachers_default_df$Teachers[teachers_default_df$Schools %in% active_schools & teachers_default_df$Grades == "MS"], na.rm = TRUE))
  
  #K-8
  avg_school_char[1,11] <- ceiling(aggregate(slDf$Classrooms, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                   [nrow(aggregate(slDf$Classrooms, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                     ncol(aggregate(slDf$Classrooms, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))])
  
  avg_school_char[2,11] <- ceiling(aggregate(slDf$enrollment, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                   [nrow(aggregate(slDf$enrollment, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                     ncol(aggregate(slDf$enrollment, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))])
  
  avg_school_char[3,11] <- ceiling(aggregate(sch_lvl_outputs$Capacity_Default, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                   [nrow(aggregate(sch_lvl_outputs$Capacity_Default, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                     ncol(aggregate(sch_lvl_outputs$Capacity_Default, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))])
  
  avg_school_char[4,11] <- round(aggregate(sch_lvl_outputs$Utilization_Default, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                 [nrow(aggregate(sch_lvl_outputs$Utilization_Default, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                   ncol(aggregate(sch_lvl_outputs$Utilization_Default, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))]
                                 ,3)
  
  avg_school_char[5,11] <- aggregate(sch_lvl_outputs$Combined_Facility_Score, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, sch_lvl_outputs$Combined_Facility_Score > 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)[nrow(aggregate(sch_lvl_outputs$Combined_Facility_Score, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, sch_lvl_outputs$Combined_Facility_Score > 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                                                                                                                                                                                                                    ncol(aggregate(sch_lvl_outputs$Combined_Facility_Score, by = list(slDf$School.Level == "K-8", slDf$enrollment != 0, sch_lvl_outputs$Combined_Facility_Score > 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))]
  
  avg_school_char[6,11] <- ceiling(sum(slDf$enrollment[slDf$enrollment != 0 & slDf$School.Level == "K-8" & slDf$Active == 1], na.rm = TRUE) / 
                                     sum(teachers_default_df$Teachers[teachers_default_df$Schools %in% active_schools & teachers_default_df$Grades == "K-8"], na.rm = TRUE))
  
  #HS
  avg_school_char[1,14] <- ceiling(aggregate(slDf$Classrooms, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                   [nrow(aggregate(slDf$Classrooms, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                     ncol(aggregate(slDf$Classrooms, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))])
  
  avg_school_char[2,14] <- ceiling(aggregate(slDf$enrollment, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                   [nrow(aggregate(slDf$enrollment, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                     ncol(aggregate(slDf$enrollment, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))])
  
  avg_school_char[3,14] <- ceiling(aggregate(sch_lvl_outputs$Capacity_Default, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                   [nrow(aggregate(sch_lvl_outputs$Capacity_Default, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                     ncol(aggregate(sch_lvl_outputs$Capacity_Default, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))])
  
  avg_school_char[4,14] <- round(aggregate(sch_lvl_outputs$Utilization_Default, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)
                                 [nrow(aggregate(sch_lvl_outputs$Utilization_Default, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                   ncol(aggregate(sch_lvl_outputs$Utilization_Default, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))]
                                 ,3)
  
  avg_school_char[5,14] <- aggregate(sch_lvl_outputs$Combined_Facility_Score, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, sch_lvl_outputs$Combined_Facility_Score > 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)[nrow(aggregate(sch_lvl_outputs$Combined_Facility_Score, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, sch_lvl_outputs$Combined_Facility_Score > 0, slDf$Active == 1), FUN = mean, na.rm = TRUE)),
                                                                                                                                                                                                                                   ncol(aggregate(sch_lvl_outputs$Combined_Facility_Score, by = list(slDf$School.Level == "HS", slDf$enrollment != 0, sch_lvl_outputs$Combined_Facility_Score > 0, slDf$Active == 1), FUN = mean, na.rm = TRUE))]
  
  avg_school_char[6,14] <- ceiling(sum(slDf$enrollment[slDf$enrollment != 0 & slDf$School.Level == "HS" & slDf$Active == 1], na.rm = TRUE) / 
                                     sum(teachers_default_df$Teachers[teachers_default_df$Schools %in% active_schools & teachers_default_df$Grades == "HS"], na.rm = TRUE))
  
  #School & Enrollment by Grade------------------------------------------------------------------------
  
  school_enr_gr <- data.frame("Default or Scenario" = c("Default", "Scenario", "Delta"))
  
  school_enr_gr$PreK <- 0
  school_enr_gr$K <- 0 
  school_enr_gr$First <- 0 
  school_enr_gr$Second <- 0 
  school_enr_gr$Third <- 0 
  school_enr_gr$Fourth <- 0 
  school_enr_gr$Fifth <- 0 
  school_enr_gr$Sixth <- 0 
  school_enr_gr$Seventh <- 0 
  school_enr_gr$Ninth <- 0
  school_enr_gr$Tenth <- 0 
  school_enr_gr$Eleventh <- 0 
  school_enr_gr$Twelfth <- 0 
  school_enr_gr$Total <- 0 
  
  names(school_enr_gr) <- c("Default or Scenario","PK","K","1","2","3","4","5","6","7","8","9","10","11","12")
  
  school_enr_gr[1,3] <-  aggregate(glDf$Count, by = list(glDf$Grade == "KG" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,4] <-  aggregate(glDf$Count, by = list(glDf$Grade == "1" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,5] <-  aggregate(glDf$Count, by = list(glDf$Grade == "2" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,6] <-  aggregate(glDf$Count, by = list(glDf$Grade == "3" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,7] <-  aggregate(glDf$Count, by = list(glDf$Grade == "4" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,8] <-  aggregate(glDf$Count, by = list(glDf$Grade == "5" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,9] <-  aggregate(glDf$Count, by = list(glDf$Grade == "6" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,10] <-  aggregate(glDf$Count, by = list(glDf$Grade == "7" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,11] <-  aggregate(glDf$Count, by = list(glDf$Grade == "8" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,12] <-  aggregate(glDf$Count, by = list(glDf$Grade == "9" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,13] <-  aggregate(glDf$Count, by = list(glDf$Grade == "10" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,14] <-  aggregate(glDf$Count, by = list(glDf$Grade == "11" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,15] <-  aggregate(glDf$Count, by = list(glDf$Grade == "12" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr[1,2] <-  aggregate(glDf$Count, by = list(glDf$Grade == "PKG" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  
  #School & Enrollment by School level------------------------------------------------------------------------------------------------------ 
  
  school_enr_gr_2 <- data.frame("Default or Scenario" = c("Default", "Scenario", "Delta"))
  
  school_enr_gr_2$PreK <- 0
  school_enr_gr_2$K <- 0 
  school_enr_gr_2$First <- 0 
  school_enr_gr_2$Second <- 0 
  school_enr_gr_2$Third <- 0 
  school_enr_gr_2$Fourth <- 0 
  school_enr_gr_2$Fifth <- 0 
  school_enr_gr_2$Sixth <- 0 
  school_enr_gr_2$Seventh <- 0 
  school_enr_gr_2$Ninth <- 0
  school_enr_gr_2$Tenth <- 0 
  school_enr_gr_2$Eleventh <- 0 
  school_enr_gr_2$Twelfth <- 0 
  school_enr_gr_2$Total <- 0 
  
  names(school_enr_gr_2) <- c("Default or Scenario","PK","K","1","2","3","4","5","6","7","8","9","10","11","12")
  
  school_enr_gr_2[1,3] <-  sum(glDf$Grade == 'KG' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,4] <-  sum(glDf$Grade == '1' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,5] <-  sum(glDf$Grade == '2' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,6] <-  sum(glDf$Grade == '3' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,7] <-  sum(glDf$Grade == '4' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,8] <-  sum(glDf$Grade == '5' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,9] <-  sum(glDf$Grade == '6' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,10] <-  sum(glDf$Grade == '7' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,11] <-  sum(glDf$Grade == '8' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,12] <-  sum(glDf$Grade == '9' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,13] <-  sum(glDf$Grade == '10' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,14] <-  sum(glDf$Grade == '11' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,15] <-  sum(glDf$Grade == '12' & glDf$Count != 0 & glDf$Year == 2021)
  school_enr_gr_2[1,2] <-  sum(glDf$Grade == 'PKG' & glDf$Count != 0 & glDf$Year == 2021)
  
  
  school_enr_gr_2
  
  #School & Enrollment by Subgroups---------------------------------------------------------------------------------------------------------
  #Asian
  
  school_enr_gr_Asian <- data.frame("Default or Scenario" = c("Default", "Scenario", "Delta"))
  
  school_enr_gr_Asian$PreK <- 0
  school_enr_gr_Asian$K <- 0 
  school_enr_gr_Asian$First <- 0 
  school_enr_gr_Asian$Second <- 0 
  school_enr_gr_Asian$Third <- 0 
  school_enr_gr_Asian$Fourth <- 0 
  school_enr_gr_Asian$Fifth <- 0 
  school_enr_gr_Asian$Sixth <- 0 
  school_enr_gr_Asian$Seventh <- 0 
  school_enr_gr_Asian$Ninth <- 0
  school_enr_gr_Asian$Tenth <- 0 
  school_enr_gr_Asian$Eleventh <- 0 
  school_enr_gr_Asian$Twelfth <- 0 
  school_enr_gr_Asian$Total <- 0 
  
  names(school_enr_gr_Asian) <- c("Default or Scenario","PK","K","1","2","3","4","5","6","7","8","9","10","11","12")
  
  school_enr_gr_Asian[1,3] <-  aggregate(glDf$Asian, by = list(glDf$Grade == 'KG' & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,4] <-  aggregate(glDf$Asian, by = list(glDf$Grade == "1" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,5] <-  aggregate(glDf$Asian, by = list(glDf$Grade == "2" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,6] <-  aggregate(glDf$Asian, by = list(glDf$Grade == "3" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,7] <-  aggregate(glDf$Asian, by = list(glDf$Grade == "4" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,8] <-  aggregate(glDf$Asian, by = list(glDf$Grade == "5" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,9] <-  aggregate(glDf$Asian, by = list(glDf$Grade == "6" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,10] <-  aggregate(glDf$Asian, by = list(glDf$Grade == "7" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,11] <-  aggregate(glDf$Asian, by = list(glDf$Grade == "8" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,12] <-  aggregate(glDf$Asian, by = list(glDf$Grade == "9" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,13] <-  aggregate(glDf$Asian, by = list(glDf$Grade == "10" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,14] <-  aggregate(glDf$Asian, by = list(glDf$Grade == "11" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,15] <-  aggregate(glDf$Asian, by = list(glDf$Grade == "12" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Asian[1,2] <-  aggregate(glDf$Asian, by = list(glDf$Grade == 'PKG' & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  
  
  school_enr_gr_Asian_2 <- data.frame("Default or Scenario" = c("Default", "Scenario", "Delta"))
  
  school_enr_gr_Asian_2$PreK <- 0
  school_enr_gr_Asian_2$K <- 0 
  school_enr_gr_Asian_2$First <- 0 
  school_enr_gr_Asian_2$Second <- 0 
  school_enr_gr_Asian_2$Third <- 0 
  school_enr_gr_Asian_2$Fourth <- 0 
  school_enr_gr_Asian_2$Fifth <- 0 
  school_enr_gr_Asian_2$Sixth <- 0 
  school_enr_gr_Asian_2$Seventh <- 0 
  school_enr_gr_Asian_2$Ninth <- 0
  school_enr_gr_Asian_2$Tenth <- 0 
  school_enr_gr_Asian_2$Eleventh <- 0 
  school_enr_gr_Asian_2$Twelfth <- 0 
  school_enr_gr_Asian_2$Total <- 0 
  
  school_enr_gr_Asian_2[1,3] <-  sum(glDf$Grade == 'KG' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,4] <-  sum(glDf$Grade == '1' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,5] <-  sum(glDf$Grade == '2' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,6] <-  sum(glDf$Grade == '3' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,7] <-  sum(glDf$Grade == '4' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,8] <-  sum(glDf$Grade == '5' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,9] <-  sum(glDf$Grade == '6' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,10] <-  sum(glDf$Grade == '7' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,11] <-  sum(glDf$Grade == '8' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,12] <-  sum(glDf$Grade == '9' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,13] <-  sum(glDf$Grade == '10' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,14] <-  sum(glDf$Grade == '11' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,15] <-  sum(glDf$Grade == '12' & glDf$Asian != 0 & glDf$Year == 2021)
  school_enr_gr_Asian_2[1,2] <-  sum(glDf$Grade == 'PKG' & glDf$Asian != 0 & glDf$Year == 2021)
  
  #Black
  school_enr_gr_Black <- data.frame("Default or Scenario" = c("Default", "Scenario", "Delta"))
  
  school_enr_gr_Black$PreK <- 0
  school_enr_gr_Black$K <- 0 
  school_enr_gr_Black$First <- 0 
  school_enr_gr_Black$Second <- 0 
  school_enr_gr_Black$Third <- 0 
  school_enr_gr_Black$Fourth <- 0 
  school_enr_gr_Black$Fifth <- 0 
  school_enr_gr_Black$Sixth <- 0 
  school_enr_gr_Black$Seventh <- 0 
  school_enr_gr_Black$Ninth <- 0
  school_enr_gr_Black$Tenth <- 0 
  school_enr_gr_Black$Eleventh <- 0 
  school_enr_gr_Black$Twelfth <- 0 
  school_enr_gr_Black$Total <- 0 
  
  names(school_enr_gr_Black) <- c("Default or Scenario","PK","K","1","2","3","4","5","6","7","8","9","10","11","12")
  
  school_enr_gr_Black[1,3] <-  aggregate(glDf$Black, by = list(glDf$Grade == 'KG' & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,4] <-  aggregate(glDf$Black, by = list(glDf$Grade == "1" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,5] <-  aggregate(glDf$Black, by = list(glDf$Grade == "2" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,6] <-  aggregate(glDf$Black, by = list(glDf$Grade == "3" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,7] <-  aggregate(glDf$Black, by = list(glDf$Grade == "4" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,8] <-  aggregate(glDf$Black, by = list(glDf$Grade == "5" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,9] <-  aggregate(glDf$Black, by = list(glDf$Grade == "6" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,10] <-  aggregate(glDf$Black, by = list(glDf$Grade == "7" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,11] <-  aggregate(glDf$Black, by = list(glDf$Grade == "8" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,12] <-  aggregate(glDf$Black, by = list(glDf$Grade == "9" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,13] <-  aggregate(glDf$Black, by = list(glDf$Grade == "10" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,14] <-  aggregate(glDf$Black, by = list(glDf$Grade == "11" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,15] <-  aggregate(glDf$Black, by = list(glDf$Grade == "12" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Black[1,2] <-  aggregate(glDf$Black, by = list(glDf$Grade == 'PKG' & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  
  school_enr_gr_Black_2 <- data.frame("Default or Scenario" = c("Default", "Scenario", "Delta"))
  
  school_enr_gr_Black_2$PreK <- 0
  school_enr_gr_Black_2$K <- 0 
  school_enr_gr_Black_2$First <- 0 
  school_enr_gr_Black_2$Second <- 0 
  school_enr_gr_Black_2$Third <- 0 
  school_enr_gr_Black_2$Fourth <- 0 
  school_enr_gr_Black_2$Fifth <- 0 
  school_enr_gr_Black_2$Sixth <- 0 
  school_enr_gr_Black_2$Seventh <- 0 
  school_enr_gr_Black_2$Ninth <- 0
  school_enr_gr_Black_2$Tenth <- 0 
  school_enr_gr_Black_2$Eleventh <- 0 
  school_enr_gr_Black_2$Twelfth <- 0 
  school_enr_gr_Black_2$Total <- 0 
  
  school_enr_gr_Black_2[1,3] <-  sum(glDf$Grade == 'KG' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,4] <-  sum(glDf$Grade == '1' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,5] <-  sum(glDf$Grade == '2' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,6] <-  sum(glDf$Grade == '3' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,7] <-  sum(glDf$Grade == '4' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,8] <-  sum(glDf$Grade == '5' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,9] <-  sum(glDf$Grade == '6' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,10] <-  sum(glDf$Grade == '7' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,11] <-  sum(glDf$Grade == '8' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,12] <-  sum(glDf$Grade == '9' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,13] <-  sum(glDf$Grade == '10' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,14] <-  sum(glDf$Grade == '11' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,15] <-  sum(glDf$Grade == '12' & glDf$Black != 0 & glDf$Year == 2021)
  school_enr_gr_Black_2[1,2] <-  sum(glDf$Grade == 'PKG' & glDf$Black != 0 & glDf$Year == 2021)
  
  #Latinx
  
  school_enr_gr_Latinx <- data.frame("Default or Scenario" = c("Default", "Scenario", "Delta"))
  
  school_enr_gr_Latinx$PreK <- 0
  school_enr_gr_Latinx$K <- 0 
  school_enr_gr_Latinx$First <- 0 
  school_enr_gr_Latinx$Second <- 0 
  school_enr_gr_Latinx$Third <- 0 
  school_enr_gr_Latinx$Fourth <- 0 
  school_enr_gr_Latinx$Fifth <- 0 
  school_enr_gr_Latinx$Sixth <- 0 
  school_enr_gr_Latinx$Seventh <- 0 
  school_enr_gr_Latinx$Ninth <- 0
  school_enr_gr_Latinx$Tenth <- 0 
  school_enr_gr_Latinx$Eleventh <- 0 
  school_enr_gr_Latinx$Twelfth <- 0 
  school_enr_gr_Latinx$Total <- 0 
  
  names(school_enr_gr_Latinx) <- c("Default or Scenario","PK","K","1","2","3","4","5","6","7","8","9","10","11","12")
  
  school_enr_gr_Latinx[1,3] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == 'KG' & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,4] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == "1" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,5] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == "2" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,6] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == "3" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,7] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == "4" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,8] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == "5" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,9] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == "6" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,10] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == "7" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,11] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == "8" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,12] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == "9" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,13] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == "10" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,14] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == "11" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,15] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == "12" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Latinx[1,2] <-  aggregate(glDf$Latinx, by = list(glDf$Grade == 'PKG' & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  
  school_enr_gr_Latinx_2 <- data.frame("Default or Scenario" = c("Default", "Scenario", "Delta"))
  
  school_enr_gr_Latinx_2$PreK <- 0
  school_enr_gr_Latinx_2$K <- 0 
  school_enr_gr_Latinx_2$First <- 0 
  school_enr_gr_Latinx_2$Second <- 0 
  school_enr_gr_Latinx_2$Third <- 0 
  school_enr_gr_Latinx_2$Fourth <- 0 
  school_enr_gr_Latinx_2$Fifth <- 0 
  school_enr_gr_Latinx_2$Sixth <- 0 
  school_enr_gr_Latinx_2$Seventh <- 0 
  school_enr_gr_Latinx_2$Ninth <- 0
  school_enr_gr_Latinx_2$Tenth <- 0 
  school_enr_gr_Latinx_2$Eleventh <- 0 
  school_enr_gr_Latinx_2$Twelfth <- 0 
  school_enr_gr_Latinx_2$Total <- 0 
  
  school_enr_gr_Latinx_2[1,3] <-  sum(glDf$Grade == 'KG' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,4] <-  sum(glDf$Grade == '1' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,5] <-  sum(glDf$Grade == '2' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,6] <-  sum(glDf$Grade == '3' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,7] <-  sum(glDf$Grade == '4' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,8] <-  sum(glDf$Grade == '5' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,9] <-  sum(glDf$Grade == '6' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,10] <-  sum(glDf$Grade == '7' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,11] <-  sum(glDf$Grade == '8' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,12] <-  sum(glDf$Grade == '9' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,13] <-  sum(glDf$Grade == '10' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,14] <-  sum(glDf$Grade == '11' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,15] <-  sum(glDf$Grade == '12' & glDf$Latinx != 0 & glDf$Year == 2021)
  school_enr_gr_Latinx_2[1,2] <-  sum(glDf$Grade == 'PKG' & glDf$Latinx != 0 & glDf$Year == 2021)
  
  
  #White
  
  school_enr_gr_White <- data.frame("Default or Scenario" = c("Default", "Scenario", "Delta"))
  
  school_enr_gr_White$PreK <- 0
  school_enr_gr_White$K <- 0 
  school_enr_gr_White$First <- 0 
  school_enr_gr_White$Second <- 0 
  school_enr_gr_White$Third <- 0 
  school_enr_gr_White$Fourth <- 0 
  school_enr_gr_White$Fifth <- 0 
  school_enr_gr_White$Sixth <- 0 
  school_enr_gr_White$Seventh <- 0 
  school_enr_gr_White$Ninth <- 0
  school_enr_gr_White$Tenth <- 0 
  school_enr_gr_White$Eleventh <- 0 
  school_enr_gr_White$Twelfth <- 0 
  school_enr_gr_White$Total <- 0 
  
  names(school_enr_gr_White) <- c("Default or Scenario","PK","K","1","2","3","4","5","6","7","8","9","10","11","12")
  
  school_enr_gr_White[1,3] <-  aggregate(glDf$White, by = list(glDf$Grade == 'KG' & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,4] <-  aggregate(glDf$White, by = list(glDf$Grade == "1" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,5] <-  aggregate(glDf$White, by = list(glDf$Grade == "2" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,6] <-  aggregate(glDf$White, by = list(glDf$Grade == "3" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,7] <-  aggregate(glDf$White, by = list(glDf$Grade == "4" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,8] <-  aggregate(glDf$White, by = list(glDf$Grade == "5" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,9] <-  aggregate(glDf$White, by = list(glDf$Grade == "6" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,10] <-  aggregate(glDf$White, by = list(glDf$Grade == "7" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,11] <-  aggregate(glDf$White, by = list(glDf$Grade == "8" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,12] <-  aggregate(glDf$White, by = list(glDf$Grade == "9" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,13] <-  aggregate(glDf$White, by = list(glDf$Grade == "10" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,14] <-  aggregate(glDf$White, by = list(glDf$Grade == "11" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,15] <-  aggregate(glDf$White, by = list(glDf$Grade == "12" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_White[1,2] <-  aggregate(glDf$White, by = list(glDf$Grade == 'PKG' & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  
  school_enr_gr_White_2 <- data.frame("Default or Scenario" = c("Default", "Scenario", "Delta"))
  
  school_enr_gr_White_2$PreK <- 0
  school_enr_gr_White_2$K <- 0 
  school_enr_gr_White_2$First <- 0 
  school_enr_gr_White_2$Second <- 0 
  school_enr_gr_White_2$Third <- 0 
  school_enr_gr_White_2$Fourth <- 0 
  school_enr_gr_White_2$Fifth <- 0 
  school_enr_gr_White_2$Sixth <- 0 
  school_enr_gr_White_2$Seventh <- 0 
  school_enr_gr_White_2$Ninth <- 0
  school_enr_gr_White_2$Tenth <- 0 
  school_enr_gr_White_2$Eleventh <- 0 
  school_enr_gr_White_2$Twelfth <- 0 
  school_enr_gr_White_2$Total <- 0 
  
  school_enr_gr_White_2[1,3] <-  sum(glDf$Grade == 'KG' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,4] <-  sum(glDf$Grade == '1' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,5] <-  sum(glDf$Grade == '2' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,6] <-  sum(glDf$Grade == '3' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,7] <-  sum(glDf$Grade == '4' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,8] <-  sum(glDf$Grade == '5' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,9] <-  sum(glDf$Grade == '6' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,10] <-  sum(glDf$Grade == '7' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,11] <-  sum(glDf$Grade == '8' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,12] <-  sum(glDf$Grade == '9' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,13] <-  sum(glDf$Grade == '10' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,14] <-  sum(glDf$Grade == '11' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,15] <-  sum(glDf$Grade == '12' & glDf$White != 0 & glDf$Year == 2021)
  school_enr_gr_White_2[1,2] <-  sum(glDf$Grade == 'PKG' & glDf$White != 0 & glDf$Year == 2021)
  
  #Multiethnic
  
  school_enr_gr_Multiethnic <- data.frame("Default or Scenario" = c("Default", "Scenario", "Delta"))
  
  school_enr_gr_Multiethnic$PreK <- 0
  school_enr_gr_Multiethnic$K <- 0 
  school_enr_gr_Multiethnic$First <- 0 
  school_enr_gr_Multiethnic$Second <- 0 
  school_enr_gr_Multiethnic$Third <- 0 
  school_enr_gr_Multiethnic$Fourth <- 0 
  school_enr_gr_Multiethnic$Fifth <- 0 
  school_enr_gr_Multiethnic$Sixth <- 0 
  school_enr_gr_Multiethnic$Seventh <- 0 
  school_enr_gr_Multiethnic$Ninth <- 0
  school_enr_gr_Multiethnic$Tenth <- 0 
  school_enr_gr_Multiethnic$Eleventh <- 0 
  school_enr_gr_Multiethnic$Twelfth <- 0 
  school_enr_gr_Multiethnic$Total <- 0 
  
  names(school_enr_gr_Multiethnic) <- c("Default or Scenario","PK","K","1","2","3","4","5","6","7","8","9","10","11","12")
  
  school_enr_gr_Multiethnic[1,3] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == 'KG' & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,4] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == "1" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,5] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == "2" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,6] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == "3" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,7] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == "4" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,8] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == "5" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,9] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == "6" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,10] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == "7" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,11] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == "8" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,12] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == "9" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,13] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == "10" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,14] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == "11" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,15] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == "12" & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  school_enr_gr_Multiethnic[1,2] <-  aggregate(glDf$Multiethnic, by = list(glDf$Grade == 'PKG' & glDf$Year == 2021)  , FUN = sum, na.rm = TRUE)[2,2]
  
  school_enr_gr_Multiethnic_2 <- data.frame("Default or Scenario" = c("Default", "Scenario", "Delta"))
  
  school_enr_gr_Multiethnic_2$PreK <- 0
  school_enr_gr_Multiethnic_2$K <- 0 
  school_enr_gr_Multiethnic_2$First <- 0 
  school_enr_gr_Multiethnic_2$Second <- 0 
  school_enr_gr_Multiethnic_2$Third <- 0 
  school_enr_gr_Multiethnic_2$Fourth <- 0 
  school_enr_gr_Multiethnic_2$Fifth <- 0 
  school_enr_gr_Multiethnic_2$Sixth <- 0 
  school_enr_gr_Multiethnic_2$Seventh <- 0 
  school_enr_gr_Multiethnic_2$Ninth <- 0
  school_enr_gr_Multiethnic_2$Tenth <- 0 
  school_enr_gr_Multiethnic_2$Eleventh <- 0 
  school_enr_gr_Multiethnic_2$Twelfth <- 0 
  school_enr_gr_Multiethnic_2$Total <- 0 
  
  school_enr_gr_Multiethnic_2[1,3] <-  sum(glDf$Grade == 'KG' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,4] <-  sum(glDf$Grade == '1' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,5] <-  sum(glDf$Grade == '2' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,6] <-  sum(glDf$Grade == '3' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,7] <-  sum(glDf$Grade == '4' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,8] <-  sum(glDf$Grade == '5' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,9] <-  sum(glDf$Grade == '6' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,10] <-  sum(glDf$Grade == '7' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,11] <-  sum(glDf$Grade == '8' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,12] <-  sum(glDf$Grade == '9' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,13] <-  sum(glDf$Grade == '10' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,14] <-  sum(glDf$Grade == '11' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,15] <-  sum(glDf$Grade == '12' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  school_enr_gr_Multiethnic_2[1,2] <-  sum(glDf$Grade == 'PKG' & glDf$Multiethnic != 0 & glDf$Year == 2021)
  
  #Subgroups by Facilities Condition-----------------------------------------------------------------------------------
  
  subgroups <- data.frame("Demographic" = c("SWD","ELL","FRL","Asian", "Black", "Latinx", "White", "Multiethnic"))
  
  subgroups$default1 <- 0
  subgroups$default2 <- 0
  subgroups$default3 <- 0
  subgroups$default4 <- 0
  subgroups$default5 <- 0
  subgroups$default6 <- 0
  subgroups$default7 <- 0
  subgroups$default8 <- 0
  subgroups$default9 <- 0
  
  
  names(subgroups) <- c("Demographic","% in HQ Facilities - Default","% in HQ Facilities - Scenario",
                        "% in HQ Facilities - Delta","% in LQ Facilities - Default","% in LQ Facilities - Scenario",
                        "% in LQ Facilities - Delta","% in Overcrowded Facilities - Default","% in Overcrowded Facilities - Scenario",
                        "% in Overcrowded Facilities - Delta")
  
  hq_schools <- slDf[slDf$Combined.Score > 80,5]
  lq_schools <- slDf[slDf$Combined.Score < 60,5]
  
  overcap_schools <- sch_lvl_outputs[sch_lvl_outputs$Utilization_Default > 1,1]
  
  subgroups[5,2] <- sum(glDf[glDf$School.Name %in% hq_schools[[1]] & glDf$Year == 2021,"Black"]) / sum(glDf[glDf$Year == 2021,"Black"])
  subgroups[1,2] <- sum(glDf$Count[glDf$Grade == "sp_ed" & glDf$School.Name %in% hq_schools[[1]] & glDf$Year == 2021]) / sum(glDf[glDf$Grade == "sp_ed" & glDf$Year == 2021,"Count"])
  subgroups[2,2] <- sum(glDf$Count[glDf$Grade == "ell" & glDf$School.Name %in% hq_schools[[1]] & glDf$Year == 2021]) / sum(glDf[glDf$Grade == "ell" & glDf$Year == 2021,"Count"])
  subgroups[3,2] <- sum(slDf[slDf$Combined.Score > 80,"Direct.Cert"] * slDf[slDf$Combined.Score > 80,"enrollment"],na.rm = TRUE) / sum(slDf$Direct.Cert * slDf$enrollment, na.rm = TRUE)
  subgroups[4,2] <- sum(glDf[glDf$School.Name %in% hq_schools[[1]] & glDf$Year == 2021,"Asian"]) / sum(glDf[glDf$Year == 2021, "Asian"])
  subgroups[6,2] <- sum(glDf[glDf$School.Name %in% hq_schools[[1]] & glDf$Year == 2021,"Latinx"]) / sum(glDf[glDf$Year == 2021, "Latinx"])
  subgroups[7,2] <- sum(glDf[glDf$School.Name %in% hq_schools[[1]] & glDf$Year == 2021,"White"]) /sum(glDf[glDf$Year == 2021, "White"])
  subgroups[8,2] <- sum(glDf[glDf$School.Name %in% hq_schools[[1]] & glDf$Year == 2021,"Multiethnic"]) /sum(glDf[glDf$Year == 2021, "Multiethnic"])
  
  subgroups[5,5] <- sum(glDf[glDf$School.Name %in% lq_schools[[1]] & glDf$Year == 2021,"Black"]) / sum(glDf[glDf$Year == 2021,"Black"])
  subgroups[1,5] <- sum(glDf$Count[glDf$Grade == "sp_ed" & glDf$School.Name %in% lq_schools[[1]] & glDf$Year == 2021]) / sum(glDf[glDf$Grade == "sp_ed" & glDf$Year == 2021,"Count"])
  subgroups[2,5] <- sum(glDf$Count[glDf$Grade == "ell" & glDf$School.Name %in% lq_schools[[1]] & glDf$Year == 2021]) / sum(glDf[glDf$Grade == "ell" & glDf$Year == 2021,"Count"])
  subgroups[3,5] <- sum(slDf[slDf$Combined.Score < 60,"Direct.Cert"] * slDf[slDf$Combined.Score < 60,"enrollment"],na.rm = TRUE) / sum(slDf$Direct.Cert * slDf$enrollment, na.rm = TRUE)
  subgroups[4,5] <- sum(glDf[glDf$School.Name %in% lq_schools[[1]] & glDf$Year == 2021,"Asian"]) / sum(glDf[glDf$Year == 2021, "Asian"])
  subgroups[6,5] <- sum(glDf[glDf$School.Name %in% lq_schools[[1]] & glDf$Year == 2021,"Latinx"]) / sum(glDf[glDf$Year == 2021, "Latinx"])
  subgroups[7,5] <- sum(glDf[glDf$School.Name %in% lq_schools[[1]] & glDf$Year == 2021,"White"]) /sum(glDf[glDf$Year == 2021, "White"])
  subgroups[8,5] <- sum(glDf[glDf$School.Name %in% lq_schools[[1]] & glDf$Year == 2021,"Multiethnic"]) /sum(glDf[glDf$Year == 2021, "Multiethnic"])
  
  subgroups[5,8] <- sum(glDf[glDf$School.Name %in% overcap_schools & glDf$Year == 2021,"Black"]) /sum(glDf[glDf$Year == 2021,"Black"])
  subgroups[1,8] <- sum(glDf$Count[glDf$Grade == "sp_ed" & glDf$School.Name %in% overcap_schools & glDf$Year == 2021])/ sum(glDf[glDf$Grade == "sp_ed" & glDf$Year == 2021,"Count"])
  subgroups[2,8] <- sum(glDf$Count[glDf$Grade == "ell" & glDf$School.Name %in% overcap_schools & glDf$Year == 2021]) /  sum(glDf[glDf$Grade == "ell" & glDf$Year == 2021,"Count"])
  subgroups[3,8] <- sum(slDf[slDf$Combined.Score > 80,"Direct.Cert"] * slDf[slDf$Combined.Score > 80,"enrollment"],na.rm = TRUE) / sum(slDf$Direct.Cert * slDf$enrollment, na.rm = TRUE)
  subgroups[4,8] <- sum(glDf[glDf$School.Name %in% overcap_schools & glDf$Year == 2021,"Asian"]) / sum(glDf[glDf$Year == 2021, "Asian"])
  subgroups[6,8] <- sum(glDf[glDf$School.Name %in% overcap_schools & glDf$Year == 2021,"Latinx"]) / sum(glDf[glDf$Year == 2021, "Latinx"])
  subgroups[7,8] <- sum(glDf[glDf$School.Name %in% overcap_schools & glDf$Year == 2021,"White"]) / sum(glDf[glDf$Year == 2021, "White"])
  subgroups[8,8] <- sum(glDf[glDf$School.Name %in% overcap_schools & glDf$Year == 2021,"Multiethnic"]) /sum(glDf[glDf$Year == 2021, "Multiethnic"])
  
  #Enrollment Zone Tab Tables------------------------------------------------------------------------------------------
  
  zone_sch_char <- data.frame("Metrics" = c("Grades Served","Total Capacity","Average Utilization", "Average Combined Facility Score", "# of High Quality Facilities",
                                            "# of Low Quality Facilities","# of High Needs Schools", "Average School Accountability Metric"))
  
  zone_sch_char$Zone_1 <- 0
  zone_sch_char$Zone_2 <- 0
  zone_sch_char$Zone_3 <- 0
  zone_sch_char$Zone_4 <- 0
  zone_sch_char$Zone_5 <- 0
  zone_sch_char$Zone_6 <- 0
  zone_sch_char$Zone_7 <- 0
  zone_sch_char$Zone_8 <- 0
  
  names(zone_sch_char) <- c("Metrics","Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5", "Zone 6", "Zone 7", "Zone 8" )
  
  zone_gr_lvl_enr <- data.frame("Grade" = c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed"))
  
  zone_gr_lvl_enr$Zone_1 <- 0
  zone_gr_lvl_enr$Zone_2 <- 0
  zone_gr_lvl_enr$Zone_3 <- 0
  zone_gr_lvl_enr$Zone_4 <- 0
  zone_gr_lvl_enr$Zone_5 <- 0
  zone_gr_lvl_enr$Zone_6 <- 0
  zone_gr_lvl_enr$Zone_7 <- 0
  zone_gr_lvl_enr$Zone_8 <- 0
  zone_gr_lvl_enr$Total <- 0
  
  names(zone_gr_lvl_enr) <- c("Grade","Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5", "Zone 6", "Zone 7", "Zone 8", "Total" )
  
  zone_subgroups <- data.frame("Demographic" = c("SWD","ELL","FRL","Asian", "Black", "Latinx", "White", "Multiethnic"), stringsAsFactors = FALSE)
  
  zone_subgroups$Zone_1 <- 0
  zone_subgroups$Zone_2 <- 0
  zone_subgroups$Zone_3 <- 0
  zone_subgroups$Zone_4 <- 0
  zone_subgroups$Zone_5 <- 0
  zone_subgroups$Zone_6 <- 0
  zone_subgroups$Zone_7 <- 0
  zone_subgroups$Zone_8 <- 0
  
  names(zone_subgroups) <- c("Demographic","Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5", "Zone 6", "Zone 7", "Zone 8")
  
  zone_fin <- data.frame("Zone" = c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5", "Zone 6", "Zone 7", "Zone 8"), stringsAsFactors = FALSE)
  zone_fin$Total_Baseline <- "$0"
  zone_fin$Total_SBA <- "$0"
  zone_fin$Total_Funding <- "$0"
  names(zone_fin) <- c("Zone","Total Baseline", "Total SBA", "Total Funding")
  
  
  trigger_scenario <- function() {
    
    #Start Counter for Net Scenario Changes Table
    counter$counterValue <- counter$counterValue + 1
    
    #Remove NA Schools
    schLvl$df <- filter(schLvl$df, !is.na(School.Name.SL))
    
    #Scenario if a school was Closed
    if (cntCls$close == 1) {
      
      #Define Closed Schools for future use
      x$closed_schools <- x$df1 %>% 
        group_by(School.Name) %>% 
        summarise(delta = sum(me_delta),
                  og = sum(`2021`)) %>% 
        ungroup() %>% 
        filter(delta == -1 * og, og != 0) %>% 
        pull(School.Name)
      
      #Set Active/Capacity to 0, Custodial NA to 0
      schLvl$df[schLvl$df$School.Name.SL %in%  x$closed_schools,][["Active"]] <- "0"
      schLvl$df[schLvl$df$School.Name.SL %in%  x$closed_schools,][["Capacity"]] <- 0
      schLvl$df$Custodial.Allocation[is.nan(schLvl$df$Custodial.Allocation)] <- 0
      
      #Remove Closed School from Input List
      x$inputSchList <- x$inputSchList[!(x$inputSchList %in% x$closed_schools)]
      
      #Create schLvl$dfs to distinguish Open and Closed Schools to adjust now and remerge later
      schLvl$dfO <- rbind(schLvl$df[schLvl$df$Active %in% c(NA, 1, 2),])
      schLvl$dfC <- schLvl$df[schLvl$df$Active == 0,]
      
      #Active School Facilities Adjustments-----------------------------------------------------------------------------------
      
      schLvl$dfO <- schLvl$dfO %>%
        rowwise() %>% 
        mutate(Sum.Utilities = sum(Water, Sewer, Electric, Fuel.Heat.Gas, Fire.Service.Charges, Fog.Service.Charges, Snow.Removal, Grass.Tree.Services, Trash.Removal, na.rm = TRUE)) 
      schLvl$dfO$UPercent <- 1
      schLvl$dfO$Cost.Per.Custodian <- 60169
      schLvl$dfO <- schLvl$dfO %>%
        mutate(Total.Custodial.Cost = Custodial.Allocation * Cost.Per.Custodian)
      
      #Inactive School Facilities Adjustments---------------------------------------------------------------------------------
      
      schLvl$dfC <- schLvl$dfC %>% 
        filter(!is.na(School.Name.SL)) %>%
        rowwise() %>% 
        mutate(Sum.Utilities = sum(Water, Sewer, Electric, Fuel.Heat.Gas, Fire.Service.Charges, Fog.Service.Charges, Snow.Removal, Grass.Tree.Services, Trash.Removal, na.rm = TRUE)) 
      schLvl$dfC$UPercent <- .3
      schLvl$dfC$Custodial.Allocation <- 1
      schLvl$dfC$Cost.Per.Custodian <- 60169
      schLvl$dfC <- schLvl$dfC %>%
        mutate(Total.Custodial.Cost = Custodial.Allocation * Cost.Per.Custodian)
      
      #Reconnect Active and Inactive School Facilities------------------------------------------------------------
      schLvl$df <- rbind(schLvl$dfO, schLvl$dfC)
      schLvl$df$Total.Utilities.Cost <- schLvl$df$Sum.Utilities * schLvl$df$UPercent
      cntCls$close<-0
      
      #If no schools were closed -> simpler process, just calculate a few columns
    } else if (cntCls$close == 0) {
      schLvl$df <- schLvl$df %>% 
        rowwise() %>% 
        mutate(Sum.Utilities = sum(Water, Sewer, Electric, Fuel.Heat.Gas, Fire.Service.Charges, Fog.Service.Charges, Snow.Removal, Grass.Tree.Services, Trash.Removal, na.rm = TRUE))
      schLvl$df$UPercent <- 1
      schLvl$df$Cost.Per.Custodian <- 60169
      schLvl$df <- schLvl$df %>%
        mutate(Total.Custodial.Cost = Custodial.Allocation * Cost.Per.Custodian)
      schLvl$df <- schLvl$df %>% 
        mutate(Total.Utilities.Cost = Sum.Utilities * UPercent) %>% 
        ungroup() 
    }
    
    
    #Identify any Inactive Schools that were reactived (i.e. given enrollment) and add Enrollment
    
    inactive_schools <- slDf$School.Name.SL[slDf$enrollment == 0]
    
    k <- x$df1[x$df1$School.Name %in% inactive_schools,]
    k1 <- x$df2[x$df2$School.Name %in% inactive_schools,]
    k2 <- x$df3[x$df3$School.Name %in% inactive_schools,]
    k3 <- x$df4[x$df4$School.Name %in% inactive_schools,]
    k4 <- x$df5[x$df5$School.Name %in% inactive_schools,]
    k5 <- x$df6[x$df6$School.Name %in% inactive_schools,]
    k6 <- x$df7[x$df7$School.Name %in% inactive_schools,]
    k7 <- x$df8[x$df8$School.Name %in% inactive_schools,]
    k8 <- x$df9[x$df9$School.Name %in% inactive_schools,]
    k9 <- x$df10[x$df10$School.Name %in% inactive_schools,]
    k10 <- x$df11[x$df11$School.Name %in% inactive_schools,]
    
    x$inact_schools <- rbind(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10)
    x$inact_schools <- x$inact_schools[x$inact_schools$me_delta > 0,]
    
    #Regenerating School Level----------------------------------------------------------
    
    y <- x$df1
    y1 <- x$df2
    y2 <- x$df3
    y3 <- x$df4
    y4 <- x$df5
    y5 <- x$df6
    y6 <- x$df7
    y7 <- x$df8
    y8 <- x$df9
    y9 <- x$df10
    y10 <- x$df11
    
    ybind_ss <- y
    ybind_is <- rbind(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)
    
    ybind_ss <- ybind_ss[ybind_ss$School.Name == input$subSchool,]
    ybind_is <- ybind_is[ybind_is$School.Name != input$subSchool ,]
    ybind_is <- ybind_is[!duplicated(ybind_is[,c("School.Name", "Grade", "me_change")]),]
    ybind_is <- ybind_is[order(-abs(ybind_is$me_delta)),]
    ybind_is <- ybind_is[!duplicated(ybind_is[,c("School.Name", "Grade")]),]
    
    ybind <- rbind(ybind_ss, ybind_is)
    
    ybind$Grade <- sapply(ybind$Grade,as.character)
    ybind$sch_lvl <- NA
    
    pullcols <- c("School.Name", "me_change")
    
    #Create Lists of Schools by Grade
    schools_with_K <- ybind[ybind$Grade == 'KG',pullcols]
    schools_with_K <- schools_with_K[schools_with_K$me_change > 0, 1]
    
    schools_with_1 <- ybind[ybind$Grade == "1",pullcols]
    schools_with_1 <- schools_with_1[schools_with_1$me_change > 0, 1]
    
    schools_with_2 <- ybind[ybind$Grade == "2",pullcols]
    schools_with_2 <- schools_with_2[schools_with_2$me_change > 0, 1]
    
    schools_with_3 <- ybind[ybind$Grade == "3",pullcols]
    schools_with_3 <- schools_with_3[schools_with_3$me_change > 0, 1]
    
    schools_with_4 <- ybind[ybind$Grade == "4",pullcols]
    schools_with_4 <- schools_with_4[schools_with_4$me_change > 0, 1]
    
    schools_with_5 <- ybind[ybind$Grade == "5",pullcols]
    schools_with_5 <- schools_with_5[schools_with_5$me_change > 0, 1]
    
    schools_with_6 <- ybind[ybind$Grade == "6",pullcols]
    schools_with_6 <- schools_with_6[schools_with_6$me_change > 0, 1]
    
    schools_with_7 <- ybind[ybind$Grade == "7",pullcols]
    schools_with_7 <- schools_with_7[schools_with_7$me_change > 0, 1]
    
    schools_with_8 <- ybind[ybind$Grade == "8",pullcols]
    schools_with_8 <- schools_with_8[schools_with_8$me_change > 0, 1]
    
    schools_with_9 <- ybind[ybind$Grade == "9",pullcols]
    schools_with_9 <- schools_with_9[schools_with_9$me_change > 0, 1]
    
    schools_with_10 <- ybind[ybind$Grade == "10",pullcols]
    schools_with_10 <- schools_with_10[schools_with_10$me_change > 0, 1]
    
    schools_with_11 <- ybind[ybind$Grade == "11",pullcols]
    schools_with_11 <- schools_with_11[schools_with_11$me_change > 0, 1]
    
    schools_with_12 <- ybind[ybind$Grade == "12",pullcols]
    schools_with_12 <- schools_with_12[schools_with_12$me_change > 0, 1]
    
    #List of Schools by Grade to List of Schools by School Type
    sch_list <- ybind[!duplicated(ybind[,c("School.Name")]),"School.Name"]
    
    k8_sch_list <- sch_list[(sch_list %in% schools_with_K | sch_list %in% schools_with_1 | sch_list %in% schools_with_2 | 
                               sch_list %in% schools_with_3 | sch_list %in% schools_with_4 | sch_list %in% schools_with_5 | 
                               sch_list %in% schools_with_6) & (sch_list %in% schools_with_7 | sch_list %in% schools_with_8)]
    
    elementary_sch_list <- sch_list[!(sch_list %in% k8_sch_list) & (sch_list %in% schools_with_K | sch_list %in%  schools_with_1 | 
                                                                      sch_list %in%  schools_with_2 | sch_list %in%  schools_with_3 | sch_list %in%  schools_with_4 | 
                                                                      sch_list %in%  schools_with_5 | sch_list %in%  schools_with_6)]
    
    middle_sch_list <- sch_list[!(sch_list %in% k8_sch_list) & (sch_list %in% schools_with_7 | sch_list %in%  schools_with_8)]
    
    high_sch_list <- sch_list[sch_list %in% schools_with_9  & sch_list %in% schools_with_10 & sch_list %in%  schools_with_11 & sch_list %in%  schools_with_12] 
    
    #Replace in schLvl$df with updated School Level
    schLvl$df[schLvl$df$School.Name.SL %in% k8_sch_list, "School.Level"] <- "K-8"
    schLvl$df[schLvl$df$School.Name.SL %in% elementary_sch_list, "School.Level"] <- "ES"
    schLvl$df[schLvl$df$School.Name.SL %in% middle_sch_list, "School.Level"] <- "MS"
    schLvl$df[schLvl$df$School.Name.SL %in% high_sch_list, "School.Level"] <- "HS"
    
    #Manual Fixes
    schLvl$df[schLvl$df$School.Name.SL == "KIPP Indy College Prep Middle School","School.Level"] <- "MS"
    schLvl$df[schLvl$df$School.Name.SL == "Avondale Meadows Middle School","School.Level"] <- "MS"
    
    # Facilities Summary Scenario------------------------------------------------------------------------------------
    # Move df to Reactive df
    schLvl$utl_sum <- utl_sum
    
    #Reformat to numerics so that Delta Column calculations can be carried out
    schLvl$utl_sum[3,2] <- sum(fCosts[,"Total.Utilities.Cost"],na.rm = TRUE) 
    schLvl$utl_sum[2,2] <- sum(fCosts$Total.Custodial.Cost,na.rm = TRUE) 
    schLvl$utl_sum[4,2] <- as.numeric(schLvl$utl_sum[3,2]) + as.numeric(schLvl$utl_sum[2,2])
    schLvl$utl_sum[5,2] <- sum(slDf$amt_outstanding_last, na.rm = TRUE)
    
    #Re-add Stray Closed schools + Re-Activated schools (TBH I forgot exactly why this is needed - I think it is for multiple closed schools over several submits)
    schlvl_df <- schLvl$df
    schlvl_dfc <- schLvl$dfC
    schlvl_dfc$Total.Utilities.Cost <- schlvl_dfc$Sum.Utilities * schlvl_dfc$UPercent
    schlvl_dfc<-schlvl_dfc[names(schlvl_df)]
    schlvl_df[match(schlvl_dfc$School.Name.SL, schlvl_df$School.Name.SL), ] <- schlvl_dfc
    sum_inact_schools <- x$inact_schools %>%
      distinct(School.Name, Grade, .keep_all = TRUE) %>%
      group_by(School.Name) %>%
      summarise(Count=sum(me_delta))
    schlvl_df[unique(match(x$inact_schools$School.Name, schlvl_df$School.Name.SL)), "enrollment"] <- sum_inact_schools[,2]
    na_conds <- is.na(schlvl_df[unique(match(x$inact_schools$School.Name, schlvl_df$School.Name.SL)), "Condition"]) == TRUE
    schlvl_df[unique(match(x$inact_schools$School.Name, schlvl_df$School.Name.SL)), "Condition"][na_conds] <- "TBD"
    schlvl_df[unique(match(x$inact_schools$School.Name, schlvl_df$School.Name.SL)), "Total.Custodial.Cost"] <- 60169
    schlvl_df[unique(match(x$inact_schools$School.Name, schlvl_df$School.Name.SL)), "Custodial.Allocation"] <- 1
    schlvl_df[unique(match(x$inact_schools$School.Name, schlvl_df$School.Name.SL)), "Capacity"] <- sum_inact_schools[,2]
    
    #Scenario Level Facilities Summary Table Calculations
    schLvl$utl_sum[1,3] <- sum(is.na(schlvl_df$Combined.Score) == FALSE & !(schlvl_df$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE) 
    schLvl$utl_sum[3,3] <- as.numeric(sum(schlvl_df[,"Total.Utilities.Cost"],na.rm = TRUE))
    schLvl$utl_sum[2,3] <- as.numeric(sum(schlvl_df$Total.Custodial.Cost,na.rm = TRUE)) 
    schLvl$utl_sum[4,3] <- as.numeric(schLvl$utl_sum[3,3]) + as.numeric(schLvl$utl_sum[2,3])
    schLvl$utl_sum[5,3] <- sum(schlvl_df$amt_outstanding_last, na.rm = TRUE) 
    schLvl$utl_sum[6,3] <- sum(is.na(schlvl_df$Combined.Score) == FALSE & schlvl_df$debt_holder == "IPS" & !(schlvl_df$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
    schLvl$utl_sum[7,3] <- sum(is.na(schlvl_df$Combined.Score) == FALSE & schlvl_df$debt_holder == "MSBC" & !(schlvl_df$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
    schLvl$utl_sum[8,3] <- sum(is.na(schlvl_df$Combined.Score) == FALSE & schlvl_df$debt_holder == "Split" & !(schlvl_df$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
    schLvl$utl_sum[9,3] <- schLvl$utl_sum[1,3] - sum(schLvl$utl_sum[6,3], schLvl$utl_sum[7,3], schLvl$utl_sum[8,3], na.rm = TRUE)
    schLvl$utl_sum[10,3] <- sum(schlvl_df$Combined.Score > 80 & !(schlvl_df$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
    schLvl$utl_sum[11,3] <- sum(schlvl_df$Combined.Score < 60 & !(schlvl_df$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
    
    #Delta Column Calculation
    schLvl$utl_sum[,4] <- as.numeric(schLvl$utl_sum[,3]) - as.numeric(schLvl$utl_sum[,2])
    schLvl$utl_sum[2:5,2] <- dollar(as.numeric(schLvl$utl_sum[2:5,2]))
    schLvl$utl_sum[2:5,3] <- dollar(as.numeric(schLvl$utl_sum[2:5,3]))
    schLvl$utl_sum[2:5,4] <- dollar_format(negative_parens = TRUE)(as.numeric(schLvl$utl_sum[2:5,4]))
    
    #Save to Reactive Variable for Download
    schLvl_utl_sum$sce <- schLvl$utl_sum
    #Output
    output$utl_sum <- renderDT(schLvl$utl_sum,
                               colnames = c("Facilities Metrics", "Default", "Scenario", "Delta"), 
                               filter = "none",
                               selection = 'none',
                               options = list( paging = FALSE, searching = FALSE))
    
    
    
    #School Count By Access Type Update-----------------------------------------------------------------------------------
    schLvl$accTCnt1 <- accTCntD
    schLvl$accTCnt1 <- subset(schLvl$accTCnt1[1,], select = -c(Default.or.Scenario))
    schLvl$accTCnt2 <- schLvl$df
    schLvl$accTCnt2 <- subset(schLvl$accTCnt2, select = c(School.Name.SL, Active, Access.Type))  
    schLvl$accTCnt2 <- na.omit(schLvl$accTCnt2)
    schLvl$accTCnt2[schLvl$accTCnt2$School.Name.SL %in% sum_inact_schools[,1], "Active"] <- "1"
    schLvl$accTCnt2 <- schLvl$accTCnt2[schLvl$accTCnt2$Active == 1,]
    schLvl$accTCnt2 <- subset(schLvl$accTCnt2, select = -c(Active))
    schLvl$accTCnt2 <- schLvl$accTCnt2 %>% count(Access.Type) %>% spread(Access.Type, n)
    schLvl$accTCnt2$TBD <- ifelse(ncol(schLvl$accTCnt2) == 2, 0, schLvl$accTCnt2$TBD)
    schLvl$accTCnt2$All <- as.integer(rowSums(schLvl$accTCnt2[,1:ncol(schLvl$accTCnt2)]))
    schLvl$accTCnt2 <- schLvl$accTCnt2[,c("All","Choice","Neighborhood","TBD")]
    
    schLvl$accTCntS <- rbind(schLvl$accTCnt1, schLvl$accTCnt2)
    
    schLvl$accTCntS[3,1:4] <- schLvl$accTCntS[2,1:4] - schLvl$accTCntS[1,1:4]
    
    schLvl$accTCntS$Default.or.Scenario <- c("Default", "Scenario", "Delta")
    
    schLvl$accTCntS <- schLvl$accTCntS[,c(5,1,2,3,4)]
    
    
    #schLvl$utl_sum[9,3] <- NROW(schLvl$df$Condition) - sum(schLvl$df$Condition == "Excellent", na.rm =TRUE) - sum(schLvl$df$Condition == "Good", na.rm =TRUE) - sum(schLvl$df$Condition == "Fair", na.rm =TRUE) - sum(schLvl$df$Condition == "Poor", na.rm =TRUE)
    
    #Net Scenario Table Update-----------------------------------------------------------------------------------------------
    
    y <- x$df1
    y1 <- x$df2
    y2 <- x$df3
    y3 <- x$df4
    y4 <- x$df5
    y5 <- x$df6
    y6 <- x$df7
    y7 <- x$df8
    y8 <- x$df9
    y9 <- x$df10
    y10 <- x$df11
    
    
    x$df_changes <- rbind(y, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10) %>% 
      filter(me_delta != 0)
    
    # Add Placeholder 0
    changes_helper <- x$df_changes
    changes_helper_dupes <- aggregate(changes_helper, by = list(changes_helper$School.Name, changes_helper$Grade), length)
    changes_helper_dupes <- changes_helper_dupes[,1:3]
    
    for (row in 1:nrow(changes_helper_dupes)) {
      
      count <- changes_helper_dupes[row,"Order"]
      names <- changes_helper_dupes[row,1]
      grade <- changes_helper_dupes[row,2]
      
      if (count == 10) {
        p <- changes_helper[changes_helper$School.Name == names & changes_helper$Grade == grade,]
        p <- p[1,]
        p$me_change <- p$me_change - p$me_delta
        p$me_delta <- 0
        
        changes_helper[changes_helper$School.Name == names & changes_helper$Grade == grade,] <- p
        changes_helper <- rbind(p, changes_helper)
      }
      
      
    }
    #Continue Net Scenario Table Calc
    
    if (nrow(changes_helper) == 0) {
      changes_helper[1,] <- x$df1[1,]
    }
    
    x$df_allCh <- changes_helper
    x$df_allCh$counter <- counter$counterValue
    x$df_all <- rbind(x$df_all, x$df_allCh) %>% unique()
    
    new_changes <- x$df_all %>% 
      select(School.Name, Grade, me_delta, counter) %>% 
      group_by(School.Name, Grade, me_delta) %>% 
      filter(counter == min(counter)) %>% 
      ungroup() %>% 
      mutate(Grade = as.character(Grade))
    
    new_changes <- new_changes[!duplicated(new_changes[, c("School.Name", "Grade")], fromLast = T),]
    
    x$df_allCh <- x$df_allCh[0,]
    
    # Potential Fix for if we need blank Net Scenario Change Table
    # if(nrow(new_changes) == 0) {
    #        x$net_scenario_change_table <- NULL
    # } else {
    # 
    
    x$net_scenario_change_table <- expand.grid(
      School.Name = new_changes$School.Name,
      Grade = Grade,
      stringsAsFactors = FALSE
    ) %>% 
      unique() %>% 
      left_join(new_changes, by = c('School.Name', 'Grade')) %>% 
      mutate(me_delta = coalesce(me_delta, 0)) %>% 
      group_by(School.Name) %>% 
      mutate(counter = coalesce(counter, max(counter, na.rm = TRUE))) %>% 
      ungroup() %>% 
      spread(Grade, me_delta, fill = 0) %>% 
      gather(grade, students, -c(School.Name, counter)) %>% 
      group_by(School.Name, grade, counter) %>% 
      summarise(students = sum(students)) %>%
      ungroup() %>% 
      spread(grade, students) %>%
      select(`School Name` = School.Name, all_of(Grade), `Update #` = counter)
    
    net_scen_table_sorted <- x$net_scenario_change_table
    net_scen_table_sorted <- net_scen_table_sorted %>% group_by(`School Name`) %>% filter(n() > 1)
    net_scen_table_sorted$PK[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$PK[tail(which(net_scen_table_sorted$PK !=0),1)]) == 0, 0, net_scen_table_sorted$PK[tail(which(net_scen_table_sorted$PK !=0),1)])
    net_scen_table_sorted$K[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$K[tail(which(net_scen_table_sorted$K !=0),1)]) == 0, 0, net_scen_table_sorted$K[tail(which(net_scen_table_sorted$K !=0),1)])
    net_scen_table_sorted$`1`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`1`[tail(which(net_scen_table_sorted$`1` !=0),1)]) == 0, 0, net_scen_table_sorted$`1`[tail(which(net_scen_table_sorted$`1` !=0),1)])
    net_scen_table_sorted$`2`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`2`[tail(which(net_scen_table_sorted$`2` !=0),1)]) == 0, 0, net_scen_table_sorted$`2`[tail(which(net_scen_table_sorted$`2` !=0),1)])
    net_scen_table_sorted$`3`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`3`[tail(which(net_scen_table_sorted$`3` !=0),1)]) == 0, 0, net_scen_table_sorted$`3`[tail(which(net_scen_table_sorted$`3` !=0),1)])
    net_scen_table_sorted$`4`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`4`[tail(which(net_scen_table_sorted$`4` !=0),1)]) == 0, 0, net_scen_table_sorted$`4`[tail(which(net_scen_table_sorted$`4` !=0),1)])
    net_scen_table_sorted$`5`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`5`[tail(which(net_scen_table_sorted$`5` !=0),1)]) == 0, 0, net_scen_table_sorted$`5`[tail(which(net_scen_table_sorted$`5` !=0),1)])
    net_scen_table_sorted$`6`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`6`[tail(which(net_scen_table_sorted$`6` !=0),1)]) == 0, 0, net_scen_table_sorted$`6`[tail(which(net_scen_table_sorted$`6` !=0),1)])
    net_scen_table_sorted$`7`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`7`[tail(which(net_scen_table_sorted$`7` !=0),1)]) == 0, 0, net_scen_table_sorted$`7`[tail(which(net_scen_table_sorted$`7` !=0),1)])
    net_scen_table_sorted$`8`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`8`[tail(which(net_scen_table_sorted$`8` !=0),1)]) == 0, 0, net_scen_table_sorted$`8`[tail(which(net_scen_table_sorted$`8` !=0),1)])
    net_scen_table_sorted$`9`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`9`[tail(which(net_scen_table_sorted$`9` !=0),1)]) == 0, 0, net_scen_table_sorted$`9`[tail(which(net_scen_table_sorted$`9` !=0),1)])
    net_scen_table_sorted$`10`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`10`[tail(which(net_scen_table_sorted$`10` !=0),1)]) == 0, 0, net_scen_table_sorted$`10`[tail(which(net_scen_table_sorted$`10` !=0),1)])
    net_scen_table_sorted$`11`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`11`[tail(which(net_scen_table_sorted$`11` !=0),1)]) == 0, 0, net_scen_table_sorted$`11`[tail(which(net_scen_table_sorted$`11` !=0),1)])
    net_scen_table_sorted$`12`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`12`[tail(which(net_scen_table_sorted$`12` !=0),1)]) == 0, 0, net_scen_table_sorted$`12`[tail(which(net_scen_table_sorted$`12` !=0),1)])
    net_scen_table_sorted$Alt_Ed[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$Alt_Ed[tail(which(net_scen_table_sorted$Alt_Ed !=0),1)]) == 0, 0, net_scen_table_sorted$Alt_Ed[tail(which(net_scen_table_sorted$Alt_Ed !=0),1)])
    net_scen_table_sorted$Sp_Ed[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$Sp_Ed[tail(which(net_scen_table_sorted$Sp_Ed !=0),1)]) == 0, 0, net_scen_table_sorted$Sp_Ed[tail(which(net_scen_table_sorted$Sp_Ed !=0),1)])
    net_scen_table_sorted$ELL[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$ELL[tail(which(net_scen_table_sorted$ELL !=0),1)]) == 0, 0, net_scen_table_sorted$ELL[tail(which(net_scen_table_sorted$ELL !=0),1)])
    names(net_scen_table_sorted[,2:18]) <- Grade
    net_scen_table_sorted_2 <-  net_scen_table_sorted[!duplicated(net_scen_table_sorted$`School Name`),]
    net_scen_table_sorted_2 <- net_scen_table_sorted_2[is.na(net_scen_table_sorted_2$PK) == FALSE,]
    x_net_scenario_change_table <- x$net_scenario_change_table
    x_net_scenario_change_table[match(net_scen_table_sorted_2$`School Name`,  x_net_scenario_change_table$`School Name`),] <- net_scen_table_sorted_2
    colnames(x_net_scenario_change_table)[1] <- "School"
    
    totals <- x_net_scenario_change_table %>% 
      distinct(School, .keep_all = TRUE) %>%
      select(all_of(Grade)) %>% 
      colSums(na.rm = TRUE) %>% 
      as.data.frame() %>% 
      t() %>% 
      as_tibble() %>% 
      mutate(`School Name` = 'Total',
             `Update #` = 'All') %>% 
      select(`School Name`, all_of(Grade), `Update #`)
    
    output$xy <- renderDT({
      
      if (unique(x$df_changes$me_delta) == 0) {
        x_net_scenario_change_table <- totals[FALSE,]
      } else {
        working_nsct <- x$net_scenario_change_table[,2:18]
        row_counts <- rowSums(x$net_scenario_change_table == 0)
        
        x_net_scenario_change_table <- x$net_scenario_change_table[row_counts != 17,] %>% 
          rbind(totals)
      }
      
      datatable(x_net_scenario_change_table, filter = 'none', selection = 'none', rownames=FALSE,
                options = list(pageLength = 17, paging = FALSE, searching = FALSE, sort = FALSE, scrollX = TRUE)) #ensures all rows are visible, removes paging and search box from datatable output
    })
    
    # Loop through each individual school df and add changes to 2021 scenario -------------------------------------------------------
    
    #### Old Loop
    
    # x$df <- x$df %>%
    #   left_join(x$df_changes, by = c('School.Name', 'Grade')) %>%
    #   mutate(me_change.x = ifelse(!is.na(me_delta.y), me_change.y, `2021.x`)) %>%
    #   select(-ends_with('.y')) %>%
    #   set_names(str_replace_all(names(.), '\\.x$', '')) %>% 
    #   mutate(me_delta = me_change - `2021`) %>% 
    #   unique() %>% 
    #   group_by(School.Name, Grade) %>% 
    #   mutate(row = row_number()) %>% 
    #   filter(row == max(row)) %>% 
    #   ungroup() %>% 
    #   select(-row)
    
    #Add helper x$df_changes that adds placeholder rows for 10-row school/grade combos 
    #This is needed otherwise the grade is completely omitted from the output
    #10-row combos occur when there is a change to an enrollment and then a subsequent trade that reverts that change back to the default enr
    
    map(1:11, function(n) {
      x[[str_glue('df{n}')]] <- x[[str_glue('df{n}')]] %>% 
        left_join(changes_helper, by = c('School.Name', 'Grade')) %>% 
        mutate(me_change.x = ifelse(!is.na(me_delta.y), me_change.y, `2021.x`)) %>% 
        select(-ends_with('.y')) %>% 
        set_names(str_replace_all(names(.), '\\.x$', '')) %>% 
        mutate(me_delta = me_change - `2021`) %>%
        #Remove sets of 10
        group_by_all() %>%
        filter(n() != 10) %>%
        unique() %>% 
        #Arrange and ungroup
        arrange(Order, School.Name, Grade, desc(me_change)) %>%
        ungroup()
    })
    
    #Updated School Level Enrollment---------------------------------------------------------------------------------------
    subgroup_deltas <- x$df1 %>% 
      select(School.Name, Grade, Delta = me_delta) %>% 
      mutate(School.Name = as.character(School.Name),
             Grade = as.character(Grade)) %>% 
      left_join(select(schLvl$grdLvlSubGrp, School.Name, Grade, Race, Share), by = c('School.Name', 'Grade')) %>%
      arrange(Delta) %>% 
      mutate(Delta = Delta * Share) %>% 
      group_by(School.Name, Race) %>% 
      summarise(Delta = sum(Delta)) %>% 
      ungroup() %>% 
      filter(!is.na(Race)) %>% 
      mutate(Race = paste0('Race_', Race, '_Delta')) %>% 
      spread(Race, Delta)
    
    
    #Updated School Level Enrollment---------------------------------------------------------------------------------------
    subgroup_deltas <- x$df1 %>% 
      select(School.Name, Grade, Delta = me_delta) %>% 
      mutate(School.Name = as.character(School.Name),
             Grade = as.character(Grade)) %>% 
      left_join(select(schLvl$grdLvlSubGrp, School.Name, Grade, Race, Share), by = c('School.Name', 'Grade')) %>%
      arrange(Delta) %>% 
      mutate(Delta = Delta * Share) %>% 
      group_by(School.Name, Race) %>% 
      summarise(Delta = sum(Delta)) %>% 
      ungroup() %>% 
      filter(!is.na(Race)) %>% 
      mutate(Race = paste0('Race_', Race, '_Delta')) %>% 
      spread(Race, Delta)
    
    #School Level Outputs Enrollment Update-----------------------------------------------------------------------------------
    schLvl$enr <- subset(x$df1, select = c(School.Name, Grade, me_change))
    
    #Total Enrollment Subset
    schLvl_tot_enr <- schLvl$enr
    schLvl_tot_enr <- subset(schLvl_tot_enr, select = c(School.Name, me_change))
    schLvl_tot_enr <- aggregate(me_change~School.Name, schLvl_tot_enr, sum)
    # schLvl_tot_enr <- schLvl_tot_enr[!(schLvl_tot_enr$School.Name %in% c("Crispus Attucks Med Mgnt Jr HS","George Washington Comm Jr HS","Key Learning Community Elem Sch","Key Learning Community High School","Key Learning Community Jr High Sch","Purdue Polytechnical High School","Arlington Community High School", "Broad Ripple Mgnt Jr HS-Prfm Arts", "John Marshall Community Jr HS")),]
    # added_rows <- data.frame(School.Name = c("KIPP:Indy Schools","McFarland Alternative Center","Step Ahead Academy","Thomas Carr Howe Community School", "Phalen Leadership Academy@George H Fisher"), me_change = 0)
    # schLvl_tot_enr <- rbind(schLvl_tot_enr, added_rows)
    schLvl_tot_enr <- schLvl_tot_enr[order(schLvl_tot_enr$School.Name),]
    
    #General Enrollment
    schLvl$enr <- schLvl$enr[schLvl$enr$Grade != "Alt_Ed" & schLvl$enr$Grade != "ELL" & schLvl$enr$Grade != "Sp_Ed",]
    schLvl$enr <- subset(schLvl$enr, select = c(School.Name, me_change))
    schLvl$enr <- aggregate(me_change~School.Name, schLvl$enr, sum)
    schLvl$df <- merge(x = schLvl$df, y = schLvl$enr, by.x = "School.Name.SL", by.y = "School.Name", all.x = TRUE)
    schLvl$df <- schLvl$df %>% 
      mutate(Active = ifelse(enrollment == 0 & me_change > 0 & is.na(me_change) == FALSE, 1, Active))
    schLvl$df <- subset(schLvl$df, select = -c(enrollment, Utilization))
    names(schLvl$df)[names(schLvl$df) == "me_change"] <- "enrollment"
    
    #Replace with updated Capacity
    schLvl$df$Capacity[match(schlvl_df$School.Name.SL, schLvl$df$School.Name.SL)] <- schlvl_df$Capacity
    
    names(schLvl$df)[names(schLvl$df) == "me_change"] <- "enrollment"
    schLvl$df[is.na(schLvl$df$enrollment),][["enrollment"]] <- 0
    
    #Make Placeholder 
    # temp_schlvldf_enr <- schLvl$df 
    # temp_schlvldf_enr[match(schLvl_tot_enr$School.Name, temp_schlvldf_enr$School.Name.SL),"enrollment"] <- schLvl_tot_enr$me_change
    
    #Fix Non-zero Enrollment, Zero Capacity rows  
    schLvl$df$Capacity[schLvl$df$Capacity == 0 & schLvl$df$enrollment > 0] <- schLvl$df$enrollment[schLvl$df$Capacity == 0 & schLvl$df$enrollment > 0]
    
    schLvl$df <- schLvl$df %>% 
      mutate(Utilization =  schLvl$df$enrollment / schLvl$df$Capacity)
    schLvl$df$Utilization[is.nan(schLvl$df$Utilization)] <- 0
    
    #School Level Outputs funding Update-----------------------------------------------------------------------------------
    
    schLvl$schOP <- subset(schLvl$sch_lvl_outputs, select = -c(General_Enrollment_Scenario, General_Enrollment_Delta, Capacity_Scenario, Capacity_Delta, 
                                                               Utilization_Scenario, Utilization_Delta, Baseline_Scenario, Baseline_Delta, SBA_Scenario, SBA_Delta))
    
    #Replace School Level in schLvl$schOP with updated School Level
    schLvl$schOP[schLvl$schOP$Schools %in% k8_sch_list, "School_Level"] <- "K-8"
    schLvl$schOP[schLvl$schOP$Schools %in% elementary_sch_list, "School_Level"] <- "ES"
    schLvl$schOP[schLvl$schOP$Schools %in% middle_sch_list, "School_Level"] <- "MS"
    schLvl$schOP[schLvl$schOP$Schools %in% high_sch_list, "School_Level"] <- "HS"
    
    schLvl$funding <- subset(schLvl$df, select = c(School.Name.SL,Active, District.School , enrollment,School.Level, Direct.Cert, sba_SC, sba_ELL ))
    schLvl$funding <- schLvl$funding[order(schLvl$funding$School.Name.SL),]
    schLvl$funding <- schLvl$funding[schLvl$funding$Active == 1 & is.na(schLvl$funding$Active) == FALSE,]
    
    #Teacher Count
    schLvl$tc <- subset(x$df1, select = c(School.Name, Grade, me_change))
    schLvl$tck <- schLvl$tc[schLvl$tc$Grade == 'K',]
    schLvl$tc1 <- schLvl$tc[schLvl$tc$Grade == "1",]
    schLvl$tc2 <- schLvl$tc[schLvl$tc$Grade == "2" | schLvl$tc$Grade == "3",]
    schLvl$tc4 <- schLvl$tc[schLvl$tc$Grade != "K" & schLvl$tc$Grade != "1" & schLvl$tc$Grade != "2" & schLvl$tc$Grade != "3" & schLvl$tc$Grade != "Alt_Ed" & schLvl$tc$Grade != "ELL" & schLvl$tc$Grade != "Sp_Ed",]
    schLvl$tck$teach_count <- ceiling(schLvl$tck$me_change/24)
    schLvl$tc1$teach_count <- ceiling(schLvl$tc1$me_change/25)
    schLvl$tc2$teach_count <- ceiling(schLvl$tc2$me_change/28)
    schLvl$tc4$teach_count <- ceiling(schLvl$tc4$me_change/32)
    schLvl$tc <- rbind(schLvl$tck,schLvl$tc1,schLvl$tc2,schLvl$tc4)
    schLvl$tc <- aggregate(schLvl$tc$teach_count, by = list(schLvl$tc$School.Name), FUN = sum, na.rm = TRUE)
    schLvl$funding <- merge(x = schLvl$funding, y = schLvl$tc, by.x = "School.Name.SL", by.y = "Group.1", all.x = TRUE)
    
    schLvl$funding$baseline_sce <- ceiling(baseline.calculation(schLvl$funding$enrollment, schLvl$funding$x, schLvl$funding$School.Level, schLvl$funding$Direct.Cert))
    #schLvl$funding <- merge(x = schLvl$funding, y = schLvl$fundingMerge, by.x = "School.Name.SL", by.y = "School.Name.SL", all.x = TRUE)
    schLvl$funding$sba_sce <- round(sba.calculation(schLvl$funding$enrollment,  schLvl$funding$sba_SC, schLvl$funding$sba_ELL, schLvl$funding$Direct.Cert),0)
    
    ## Save all Scenario funding totals for future uses	
    all_funding <- schLvl$funding
    
    schLvl$funding$sba_sce[schLvl$funding$sba_sce < schLvl$funding$baseline_sce] <- 0
    schLvl$funding$baseline_sce[schLvl$funding$baseline_sce < schLvl$funding$sba_sce] <- 0
    schLvl$funding[schLvl$funding$School.Name.SL %in% sum_inact_schools[,1], "District.School"] <- 1
    schLvl$funding$baseline_sce[schLvl$funding$District.School == 0] <- 0
    schLvl$funding[is.na(schLvl$funding) == TRUE] <- 0
    schLvl$funding <- subset(schLvl$funding, select = c(School.Name.SL, baseline_sce, sba_sce))
    
    
    #Merge and Formatting
    schLvl$schOPMerge <- subset(schLvl$df, select = c(School.Name.SL, enrollment, Capacity, Utilization))
    schLvl$schOPMerge <- merge(x = schLvl$schOPMerge, y = schLvl$funding, by.x = "School.Name.SL", by.y = "School.Name.SL", all.x = TRUE)
    schLvl$schOPMerge[is.na(schLvl$schOPMerge$baseline_sce),][["baseline_sce"]] <- 0
    schLvl$schOPMerge[is.na(schLvl$schOPMerge$sba_sce),][["sba_sce"]] <- 0
    names(schLvl$schOPMerge)[names(schLvl$schOPMerge) == "enrollment"] <- "General_Enrollment_Scenario"
    names(schLvl$schOPMerge)[names(schLvl$schOPMerge) == "Capacity"] <- "Capacity_Scenario"
    names(schLvl$schOPMerge)[names(schLvl$schOPMerge) == "Utilization"] <- "Utilization_Scenario"
    names(schLvl$schOPMerge)[names(schLvl$schOPMerge) == "baseline_sce"] <- "Baseline_Scenario"
    names(schLvl$schOPMerge)[names(schLvl$schOPMerge) == "sba_sce"] <- "SBA_Scenario"
    
    
    #Grades Columns
    schLvl$schOP <- schLvl$schOP[order(schLvl$schOP$Schools),]
    grades_served_sce <- aggregate(Grade ~ School.Name, data = x$df1[x$df1$me_change != 0 & x$df1$School.Name %in% schLvl$schOP$Schools,], toString)
    schLvl$schOP$Grades_Scenario <- 0
    schLvl$schOP$Grades_Delta <- 0
    schLvl$schOP$Grades_Scenario[schLvl$schOP$Schools %in% grades_served_sce$School.Name] <- grades_served_sce[grades_served_sce$School.Name %in% schLvl$schOP$Schools, 2]
    
    schLvl$schOP$Grades_Scenario <-  sapply(strsplit(as.character(schLvl$schOP$Grades_Scenario), split=","), function(x) {paste(unique(trimws(x)), collapse = ', ') } )
    schLvl$schOP$Grades_Scenario <- gsub("Sp_Ed", "sp_ed", schLvl$schOP$Grades_Scenario)
    schLvl$schOP$Grades_Scenario <- gsub("ELL", "ell", schLvl$schOP$Grades_Scenario)
    schLvl$schOP$Grades_Scenario <- gsub("Alt_Ed", "alt_ed", schLvl$schOP$Grades_Scenario)
    
    
    for (row in 1:nrow(schLvl$schOP)) {
      
      a <- lapply(strsplit(as.character(schLvl$schOP[row,"Grades_Default"]),split=','),trimws)
      b <- lapply(strsplit(as.character(schLvl$schOP[row,"Grades_Scenario"]),split=','),trimws)
      
      len_a <-length(a[[1]])
      len_b <- length(b[[1]])
      
      ifelse(a == "0", len_a <- 0, len_a <- len_a)
      ifelse(b == "0", len_b <- 0, len_b <- len_b)
      
      schLvl$schOP[row,"Grades_Delta"] <- case_when(
        len_b < len_a ~ paste0("-",paste(unique(setdiff(a[[1]],b[[1]])), collapse = ",")),
        len_b > len_a ~ paste0(unique(setdiff(b[[1]],a[[1]])), collapse = ",")
      )
      
    }
    
    #Final Merge and Reformatting
    schLvl$schOP <- merge(x = schLvl$schOP, y = schLvl$schOPMerge, by.x = "Schools", by.y = "School.Name.SL", all.x = TRUE)
    # schLvl$schOP[match(schLvl_tot_enr$School.Name, schLvl$schOP$Schools),"General_Enrollment_Scenario"] <- schLvl_tot_enr$me_change
    schLvl$schOP$General_Enrollment_Delta <- schLvl$schOP$General_Enrollment_Scenario - schLvl$schOP$General_Enrollment_Default
    schLvl$schOP$General_Enrollment_Delta <- schLvl$schOP$General_Enrollment_Scenario - schLvl$schOP$General_Enrollment_Default
    schLvl$schOP$Capacity_Delta <- as.numeric(schLvl$schOP$Capacity_Scenario) - as.numeric(schLvl$schOP$Capacity_Default)
    schLvl$schOP$Utilization_Delta <- schLvl$schOP$Utilization_Scenario - schLvl$schOP$Utilization_Default
    schLvl$schOP$Baseline_Delta <- schLvl$schOP$Baseline_Scenario - schLvl$schOP$Baseline_Default
    schLvl$schOP$SBA_Delta <- schLvl$schOP$SBA_Scenario - schLvl$schOP$SBA_Default
    # schLvl$schOP$Custodial_Cost_Default <- 0
    # schLvl$schOP$Utilities_Cost_Default <- 0
    # schLvl$schOP[1:nrow(fCosts),"Custodial_Cost_Default"] <- fCosts$Total.Custodial.Cost
    # schLvl$schOP[1:nrow(fCosts),"Utilities_Cost_Default"] <- fCosts$Total.Utilities.Cost
    schLvl$schOP$Custodial_Allocation_Scenario[match(schlvl_df$School.Name.SL, schLvl$schOP$Schools)] <-  schlvl_df$Custodial.Allocation
    schLvl$schOP$Custodial_Cost_Scenario[match(schlvl_df$School.Name.SL,schLvl$schOP$Schools)] <- schlvl_df$Total.Custodial.Cost
    schLvl$schOP[is.na(schLvl$schOP)] <- 0
    schLvl$schOP$Custodial_Allocation_Delta <- schLvl$schOP$Custodial_Allocation_Scenario - schLvl$schOP$Custodial_Allocation_Default
    schLvl$schOP$Custodial_Cost_Delta <- schLvl$schOP$Custodial_Cost_Scenario - schLvl$schOP$Custodial_Cost_Default
    schLvl$schOP[match(schlvl_df$School.Name.SL, schLvl$schOP$Schools),"Utilities_Cost_Scenario"] <- schlvl_df[,"Total.Utilities.Cost"]
    schLvl$schOP$Utilities_Cost_Delta <- schLvl$schOP$Utilities_Cost_Scenario -  schLvl$schOP$Utilities_Cost_Default
    drop_cols <- c("Race_American.Indian_Default","Race_American.Indian_Scenario","Race_American.Indian_Delta","Race_Native Hawaiian or Pacific Islander_Default","Race_Native Hawaiian or Pacific Islander_Scenario","Race_Native Hawaiian or Pacific Islander_Delta","Race_Unreported_Default","Race_Unreported_Scenario","Race_Unreported_Delta")
    schLvl$schOP <- schLvl$schOP[,!(names(schLvl$schOP) %in% drop_cols)]
    schLvl$schOP[is.na(schLvl$schOP) == TRUE] <- 0
    schLvl$schOP[schLvl$schOP == ""] <- 0
    schLvl$schOP[schLvl$schOP$Baseline_Delta == -1, "Baseline_Scenario"] <- schLvl$schOP[schLvl$schOP$Baseline_Delta == -1, "Baseline_Default"]
    schLvl$schOP$Baseline_Delta <- schLvl$schOP$Baseline_Scenario - schLvl$schOP$Baseline_Default
    
    # School Level Scenario Updates------------
    # Identify empty schools
    
    zero_enroll <- schLvl$df[schLvl$df$enrollment == 0,1]
    
    impact_school <- schLvl$grdLvlSubGrp %>% 	
      filter(School.Name == input$subSchool) %>% 	
      select(Grade, Race, Share)	
    
    #Subgroups Calc
    subgroup_deltas <- x$net_scenario_change_table %>% 	
      filter(!`School Name` %in% input$subSchool) %>% 	
      gather(grade, delta, -c(`School Name`, `Update #`)) %>% 	
      filter(delta != 0) %>% 	
      left_join(impact_school, by = c('grade' = 'Grade')) %>% 	
      mutate(delta = round(delta * Share)) %>% 	
      select(`School Name`, grade, delta, Race)	
    
    
    new_subgroup_shares <- schLvl$grdLvlSubGrp %>% 	
      filter(School.Name != input$subSchool) %>% 	
      left_join(subgroup_deltas, by = c('School.Name' = 'School Name', 'Grade' = 'grade', 'Race')) %>% 	
      mutate(delta = coalesce(delta, 0)) %>% 	
      group_by(School.Name, Grade) %>% 	
      mutate(Students = Students + delta,	
             Count = Count + sum(delta),	
             Share = Students / Count) %>% 	
      ungroup() %>% 	
      mutate(Share = ifelse(is.nan(Share), 0, Share))	
    
    # Subgroups without subsetting for Subgroups by Facilities
    subgroup_deltas_all <- x$net_scenario_change_table %>% 	
      gather(grade, delta, -c(`School Name`, `Update #`)) %>% 	
      filter(delta != 0) %>% 	
      left_join(impact_school, by = c('grade' = 'Grade')) %>% 	
      mutate(delta = round(delta * Share)) %>% 	
      select(`School Name`, grade, delta, Race)
    
    schLvl$new_subgroup_shares_all <- schLvl$grdLvlSubGrp %>% 	
      left_join(subgroup_deltas_all, by = c('School.Name' = 'School Name', 'Grade' = 'grade', 'Race')) %>% 	
      mutate(delta = coalesce(delta, 0)) %>% 	
      group_by(School.Name, Grade) %>% 	
      mutate(Students = Students + delta,	
             Count = Count + sum(delta),	
             Share = Students / Count) %>% 	
      ungroup() %>% 	
      mutate(Share = ifelse(is.nan(Share), 0, Share))
    
    sch_lvl_subgroups_defaults <- schLvl$grdLvlSubGrp %>% 	
      group_by(School.Name) %>% 	
      mutate(total = sum(Count)) %>% 	
      group_by(Schools = School.Name, Race) %>% 	
      summarise(Share = sum(Students) / sum(Count)) %>% 	
      ungroup() %>% 	
      mutate(Share = ifelse(is.nan(Share), 0, Share),	
             Race = paste0('Race_', Race, '_Default')) %>% 	
      spread(Race, Share)	
    
    sch_lvl_subgroups_scenarios <- new_subgroup_shares %>%	
      select(School.Name, Count, Students, Grade, Race, Share) %>% 	
      rbind(filter(schLvl$grdLvlSubGrp, School.Name == input$subSchool)) %>% 	
      mutate(Count = replace(Count, School.Name %in% zero_enroll, 0)) %>%	
      group_by(School.Name, Race) %>% 	
      summarise(Share = sum(Students) / sum(Count),	
                Share = ifelse(is.nan(Share), 0, Share),	
                Share = ifelse(is.infinite(Share),0,Share)) %>% 	
      ungroup() %>% 	
      select(Schools = School.Name, Race, Share) %>% 	
      mutate(Race = paste0('Race_', Race, '_Scenario'),
             Share = ifelse(Share < 0, 0, Share)) %>%  	
      spread(Race, Share)
    
    sch_lvl_subgroups_deltas <- gather(sch_lvl_subgroups_scenarios, Race, Share, -Schools) %>% 	
      mutate(Race = str_replace_all(Race, '_Scenario$', '')) %>% 	
      left_join(gather(sch_lvl_subgroups_defaults, Race, Share, -Schools) %>% mutate(Race = str_replace_all(Race, '_Default$', '')), by = c('Schools', 'Race')) %>% 	
      mutate(Share = Share.x - Share.y) %>% 	
      select(-c(ends_with('.x'), ends_with('.y'))) %>% 	
      mutate(Race = paste0(Race, '_Delta')) %>% 	
      spread(Race, Share)
    
    #Prep Cleaned Download File
    school_level_summary_table <- schLvl$schOP %>% 
      select(-starts_with('Race_')) %>% 
      left_join(sch_lvl_subgroups_defaults, by = 'Schools') %>% 
      left_join(sch_lvl_subgroups_scenarios, by = 'Schools') %>% 
      left_join(sch_lvl_subgroups_deltas, by = 'Schools') %>% 
      select(all_of(school_level_summary_table_base_names)) %>% 
      mutate_at(vars(starts_with('Race')), function(x) coalesce(x, 0)) %>% 
      set_names(str_replace_all(names(.), '^Race_', '')) %>% 
      set_names(str_replace_all(names(.), '\\.|_', ' '))
    
    school_level_summary_table_names <- names(school_level_summary_table)
    school_level_summary_table_percentage_columns <- school_level_summary_table_names[str_detect(school_level_summary_table_names, 'Utilization|^(Asian|Black|Latinx|White|Multiethnic)')]
    school_level_summary_table_numeric_columns <- school_level_summary_table_names[ !str_detect(school_level_summary_table_names, 'Owner') &  !str_detect(school_level_summary_table_names, 'School Performance') & !str_detect(school_level_summary_table_names, 'Schools') &!str_detect(school_level_summary_table_names, 'Grades') & !school_level_summary_table_names %in% school_level_summary_table_percentage_columns]
    school_level_summary_table_dollar_columns <- school_level_summary_table_names[str_detect(school_level_summary_table_names, 'Outstanding Debt') | (!str_detect(school_level_summary_table_names, 'School Performance') & !str_detect(school_level_summary_table_names, 'Schools') &str_detect(school_level_summary_table_names, '^(Baseline|SBA)') & !school_level_summary_table_names %in% c(school_level_summary_table_percentage_columns))]
    
    school_level_summary_table %>% 
      datatable(extensions = "FixedColumns", options = list(autoWidth = TRUE,
                                                            columnDefs = list(list(targets=c(0, 7, 8,9), visible=TRUE, width='200')),
                                                            paging = FALSE, 
                                                            scrollX = TRUE, 
                                                            scrollY = "600px", 
                                                            fixedColumns = list(leftColumns = 1)), 
                rownames = FALSE) %>% 
      formatPercentage(school_level_summary_table_percentage_columns) %>% 
      formatCurrency(school_level_summary_table_numeric_columns, currency = '', digits = 0) %>% 
      formatCurrency(school_level_summary_table_dollar_columns, digits = 0)
    
    schLvl$dlschLvlFile <- school_level_summary_table
    
    
    #School Level Outputs Render - Scenario ------------------------------------------------------------------------------
    output$sch_lvl_outputs <- renderDT({
      
      # schLvl$sch_lvl_outputs[!( schLvl$sch_lvl_outputs$Schools %in% c("McFarland Alternative Center","Phalen Leadership Academy @ Francis Scott Key 103","Impact Academy","KIPP:Indy Schools", "KIPP Indy College Prep Middle School", "KIPP Indy Legacy High School", "KIPP Indy Unite Elementary School", "Phalen Leadership Academy@George H Fisher")), !(names(schLvl$sch_lvl_outputs) %in% c("School_Level"))] %>% 
      
      school_level_summary_table_df <- schLvl$sch_lvl_outputs %>%
        select(-starts_with('Race_')) %>% 
        left_join(sch_lvl_subgroups_defaults, by = 'Schools') %>% 
        left_join(sch_lvl_subgroups_scenarios, by = 'Schools') %>% 
        left_join(sch_lvl_subgroups_deltas, by = 'Schools') %>% 
        select(all_of(school_level_summary_table_base_names)) %>% 
        mutate_at(vars(starts_with('Race')), function(x) coalesce(x, 0)) %>% 
        set_names(str_replace_all(names(.), '^Race_', '')) %>% 
        set_names(str_replace_all(names(.), '\\.|_', ' '))
      
      
      
      school_level_summary_table_names <- names(school_level_summary_table)
      school_level_summary_table_percentage_columns <- school_level_summary_table_names[str_detect(school_level_summary_table_names, 'Utilization|^(Asian|Black|Latinx|White|Multiethnic)|Graduation')]
      school_level_summary_table_numeric_columns <- school_level_summary_table_names[!str_detect(school_level_summary_table_names, 'School Accountability') &!str_detect(school_level_summary_table_names, 'High Needs') & !str_detect(school_level_summary_table_names, 'Owner') & !str_detect(school_level_summary_table_names, 'School Performance') & !str_detect(school_level_summary_table_names, 'Schools') &!str_detect(school_level_summary_table_names, 'Grades') & !school_level_summary_table_names %in% school_level_summary_table_percentage_columns]
      school_level_summary_table_dollar_columns <- school_level_summary_table_names[str_detect(school_level_summary_table_names, 'Utilities Cost Default') | str_detect(school_level_summary_table_names, 'Utilities Cost Scenario') |str_detect(school_level_summary_table_names, 'Utilities Cost Delta') |str_detect(school_level_summary_table_names, 'Custodial Cost Default') | str_detect(school_level_summary_table_names, 'Custodial Cost Scenario') |  str_detect(school_level_summary_table_names, 'Custodial Cost Delta') | str_detect(school_level_summary_table_names, 'Outstanding Debt') | (!str_detect(school_level_summary_table_names, 'School Performance') & !str_detect(school_level_summary_table_names, 'Schools') &str_detect(school_level_summary_table_names, '^(Baseline|SBA)') & !school_level_summary_table_names %in% c(school_level_summary_table_percentage_columns))]
      
      
      
      school_level_summary_table %>% 
        datatable(extensions = "FixedColumns", options = list(autoWidth = TRUE,
                                                              columnDefs = list(list(targets= c(0,5,20,21,22,54), visible=TRUE, width='250')),
                                                              paging = FALSE, 
                                                              scrollX = TRUE, 
                                                              scrollY = "600px", 
                                                              fixedColumns = list(leftColumns = 1)), 
                  rownames = FALSE) %>% 
        formatPercentage(school_level_summary_table_percentage_columns) %>% 
        formatCurrency(school_level_summary_table_numeric_columns, currency = '', digits = 0) %>% 
        formatCurrency(school_level_summary_table_dollar_columns, digits = 0)
    })
    
    
    # Set-Up Grade Level Scenarios
    
    is.nan.data.frame <- function(x)
      do.call(cbind, lapply(x, is.nan))
    
    glDf_sg_pct <- glDf[glDf$Year == 2021,]
    glDf_sg_pct[glDf_sg_pct == "sp_ed"] <- "Sp_Ed"
    glDf_sg_pct[glDf_sg_pct == "ell"] <- "ELL"
    # glDf_sg_pct[glDf_sg_pct == "KG"] <- "K"
    glDf_sg_pct[glDf_sg_pct == "alt_ed"] <- "Alt_Ed"
    glDf_sg_pct$Asian_pct <- glDf_sg_pct$Asian / glDf_sg_pct$Count
    glDf_sg_pct$Black_pct <- glDf_sg_pct$Black / glDf_sg_pct$Count
    glDf_sg_pct$Latinx_pct <- glDf_sg_pct$Latinx / glDf_sg_pct$Count
    glDf_sg_pct$White_pct <- glDf_sg_pct$White / glDf_sg_pct$Count
    glDf_sg_pct$Multiethnic_pct <- glDf_sg_pct$Multiethnic / glDf_sg_pct$Count
    glDf_sg_pct[is.nan(glDf_sg_pct)] <- 0
    me_change_sce <- x$df1[,2:7]
    glDf_sce <- merge(glDf_sg_pct, me_change_sce, by.x = c("School.Name", "Grade"), by.y = c("School.Name", "Grade"), all =TRUE)
    glDf_sce$Asian_sce <- round(glDf_sce$Asian_pct * glDf_sce[,24])	
    glDf_sce$Black_sce <- round(glDf_sce$Black_pct * glDf_sce[,24])	
    glDf_sce$Latinx_sce <- round(glDf_sce$Latinx_pct * glDf_sce[,24])	
    glDf_sce$White_sce <- round(glDf_sce$White_pct * glDf_sce[,24])	
    glDf_sce$Multiethnic_sce <- round(glDf_sce$Multiethnic_pct * glDf_sce[,24])
    glDf_sce[is.na(glDf_sce)] <- 0
    
    # Master School and Enrollment by Grade Update---------------------------
    
    school_enr_gr[2,3] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == 'K') , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,4] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == "1") , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,5] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == "2")  , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,6] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == "3")  , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,7] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == "4")  , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,8] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == "5")  , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,9] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == "6")  , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,10] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == "7")  , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,11] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == "8")  , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,12] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == "9")  , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,13] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == "10")  , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,14] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == "11")  , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,15] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == "12")  , FUN = sum, na.rm = TRUE)[2,2]
    school_enr_gr[2,2] <-  aggregate(x$df1[,7], by = list(x$df1$Grade == 'PK')  , FUN = sum, na.rm = TRUE)[2,2]
    
    school_enr_gr[3,2:15] <-   school_enr_gr[2,2:15] - school_enr_gr[1,2:15]
    
    # By Subgroups------------------	
    #Asian	
    
    school_enr_gr_Asian[2,3] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == 'KG' & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,4] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "1" & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,5] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "2" & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,6] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "3" & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,7] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "4" & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,8] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "5" & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,9] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "6" & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,10] <- aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "7" & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,11] <- aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "8" & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,12] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "9" & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,13] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "10" & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,14] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "11" & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,15] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "12" & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Asian[2,2] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == 'PKG' & schLvl$new_subgroup_shares_all$Race == "Asian")  , FUN = sum, na.rm = TRUE)[2,2]	
    
    school_enr_gr_Asian[3,2:15] <-   school_enr_gr_Asian[2,2:15] - school_enr_gr_Asian[1,2:15]	
    
    #Black	
    
    school_enr_gr_Black[2,3] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == 'KG' & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,4] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "1" & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,5] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "2" & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,6] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "3" & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,7] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "4" & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,8] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "5" & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,9] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "6" & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,10] <- aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "7" & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,11] <- aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "8" & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,12] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "9" & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,13] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "10" & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,14] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "11" & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,15] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "12" & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Black[2,2] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == 'PKG' & schLvl$new_subgroup_shares_all$Race == "Black")  , FUN = sum, na.rm = TRUE)[2,2]	
    
    school_enr_gr_Black[3,2:15] <-   school_enr_gr_Black[2,2:15] - school_enr_gr_Black[1,2:15]	
    
    #Latinx	
    
    school_enr_gr_Latinx[2,3] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == 'KG' & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,4] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "1" & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,5] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "2" & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,6] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "3" & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,7] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "4" & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,8] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "5" & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,9] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "6" & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,10] <- aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "7" & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,11] <- aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "8" & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,12] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "9" & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,13] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "10" & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,14] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "11" & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,15] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "12" & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Latinx[2,2] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == 'PKG' & schLvl$new_subgroup_shares_all$Race == "Latinx")  , FUN = sum, na.rm = TRUE)[2,2]	
    
    school_enr_gr_Latinx[3,2:15] <-   school_enr_gr_Latinx[2,2:15] - school_enr_gr_Latinx[1,2:15]	
    
    #White	
    
    school_enr_gr_White[2,3] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == 'KG' & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,4] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "1" & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,5] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "2" & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,6] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "3" & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,7] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "4" & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,8] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "5" & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,9] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "6" & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,10] <- aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "7" & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,11] <- aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "8" & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,12] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "9" & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,13] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "10" & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,14] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "11" & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,15] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "12" & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_White[2,2] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == 'PKG' & schLvl$new_subgroup_shares_all$Race == "White")  , FUN = sum, na.rm = TRUE)[2,2]	
    
    school_enr_gr_White[3,2:15] <-   school_enr_gr_White[2,2:15] - school_enr_gr_White[1,2:15]	
    
    #Multiethnic	
    
    school_enr_gr_Multiethnic[2,3] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == 'KG' & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,4] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "1" & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,5] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "2" & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,6] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "3" & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,7] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "4" & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,8] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "5" & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,9] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "6" & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,10] <- aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "7" & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,11] <- aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "8" & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,12] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "9" & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,13] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "10" & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,14] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "11" & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,15] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == "12" & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    school_enr_gr_Multiethnic[2,2] <-  aggregate(schLvl$new_subgroup_shares_all$Students, by = list(schLvl$new_subgroup_shares_all$Grade == 'PKG' & schLvl$new_subgroup_shares_all$Race == "Multiethnic")  , FUN = sum, na.rm = TRUE)[2,2]	
    
    school_enr_gr_Multiethnic[3,2:15] <-   school_enr_gr_Multiethnic[2,2:15] - school_enr_gr_Multiethnic[1,2:15]	
    
    # Update Master School and Enrollment by School-----------	
    
    school_enr_gr_2[2,3] <-  sum(x$df1$Grade == 'KG' & x$df1[,7] != 0)	
    school_enr_gr_2[2,4] <-  sum(x$df1$Grade == '1' & x$df1[,7] != 0)	
    school_enr_gr_2[2,5] <-  sum(x$df1$Grade == '2' & x$df1[,7] != 0)	
    school_enr_gr_2[2,6] <-  sum(x$df1$Grade == '3' & x$df1[,7] != 0)	
    school_enr_gr_2[2,7] <-  sum(x$df1$Grade == '4' & x$df1[,7]!= 0)	
    school_enr_gr_2[2,8] <-  sum(x$df1$Grade == '5' & x$df1[,7] != 0)	
    school_enr_gr_2[2,9] <-  sum(x$df1$Grade == '6' & x$df1[,7] != 0)	
    school_enr_gr_2[2,10] <-  sum(x$df1$Grade == '7' & x$df1[,7] != 0)	
    school_enr_gr_2[2,11] <-  sum(x$df1$Grade == '8' & x$df1[,7] != 0)	
    school_enr_gr_2[2,12] <-  sum(x$df1$Grade == '9' & x$df1[,7] != 0)	
    school_enr_gr_2[2,13] <-  sum(x$df1$Grade == '10' & x$df1[,7] != 0)	
    school_enr_gr_2[2,14] <-  sum(x$df1$Grade == '11' & x$df1[,7] != 0)	
    school_enr_gr_2[2,15] <-  sum(x$df1$Grade == '12' & x$df1[,7] != 0)	
    school_enr_gr_2[2,2] <-  sum(x$df1$Grade == 'PKG' & x$df1[,7] != 0)	
    
    school_enr_gr_2[3,2:15] <-   school_enr_gr_2[2,2:15] - school_enr_gr_2[1,2:15]	
    
    # Update Master School and Enrollment by School by Subgroup	
    
    #Asian	
    
    school_enr_gr_Asian_2[2,3] <-  sum(schLvl$new_subgroup_shares_all$Grade == 'KG' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    school_enr_gr_Asian_2[2,4] <-  sum(schLvl$new_subgroup_shares_all$Grade == '1' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")
    school_enr_gr_Asian_2[2,5] <-  sum(schLvl$new_subgroup_shares_all$Grade == '2' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    school_enr_gr_Asian_2[2,6] <-  sum(schLvl$new_subgroup_shares_all$Grade == '3' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    school_enr_gr_Asian_2[2,7] <-  sum(schLvl$new_subgroup_shares_all$Grade == '4' & schLvl$new_subgroup_shares_all$Students!= 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    school_enr_gr_Asian_2[2,8] <-  sum(schLvl$new_subgroup_shares_all$Grade == '5' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    school_enr_gr_Asian_2[2,9] <-  sum(schLvl$new_subgroup_shares_all$Grade == '6' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    school_enr_gr_Asian_2[2,10] <-  sum(schLvl$new_subgroup_shares_all$Grade == '7' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    school_enr_gr_Asian_2[2,11] <-  sum(schLvl$new_subgroup_shares_all$Grade == '8' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    school_enr_gr_Asian_2[2,12] <-  sum(schLvl$new_subgroup_shares_all$Grade == '9' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    school_enr_gr_Asian_2[2,13] <-  sum(schLvl$new_subgroup_shares_all$Grade == '10' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    school_enr_gr_Asian_2[2,14] <-  sum(schLvl$new_subgroup_shares_all$Grade == '11' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    school_enr_gr_Asian_2[2,15] <-  sum(schLvl$new_subgroup_shares_all$Grade == '12' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    school_enr_gr_Asian_2[2,2] <-  sum(schLvl$new_subgroup_shares_all$Grade == 'PKG' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Asian")	
    
    school_enr_gr_Asian_2[3,2:15] <-   school_enr_gr_Asian_2[2,2:15] - school_enr_gr_Asian_2[1,2:15]	
    
    #Black	
    
    school_enr_gr_Black_2[2,3] <-  sum(schLvl$new_subgroup_shares_all$Grade == 'KG' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,4] <-  sum(schLvl$new_subgroup_shares_all$Grade == '1' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,5] <-  sum(schLvl$new_subgroup_shares_all$Grade == '2' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,6] <-  sum(schLvl$new_subgroup_shares_all$Grade == '3' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,7] <-  sum(schLvl$new_subgroup_shares_all$Grade == '4' & schLvl$new_subgroup_shares_all$Students!= 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,8] <-  sum(schLvl$new_subgroup_shares_all$Grade == '5' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,9] <-  sum(schLvl$new_subgroup_shares_all$Grade == '6' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,10] <-  sum(schLvl$new_subgroup_shares_all$Grade == '7' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,11] <-  sum(schLvl$new_subgroup_shares_all$Grade == '8' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,12] <-  sum(schLvl$new_subgroup_shares_all$Grade == '9' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,13] <-  sum(schLvl$new_subgroup_shares_all$Grade == '10' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,14] <-  sum(schLvl$new_subgroup_shares_all$Grade == '11' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,15] <-  sum(schLvl$new_subgroup_shares_all$Grade == '12' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    school_enr_gr_Black_2[2,2] <-  sum(schLvl$new_subgroup_shares_all$Grade == 'PKG' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Black")	
    
    school_enr_gr_Black_2[3,2:15] <- school_enr_gr_Black_2[2,2:15] - school_enr_gr_Black_2[1,2:15]	
    
    #Latinx	
    
    school_enr_gr_Latinx_2[2,3] <-  sum(schLvl$new_subgroup_shares_all$Grade == 'KG' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,4] <-  sum(schLvl$new_subgroup_shares_all$Grade == '1' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,5] <-  sum(schLvl$new_subgroup_shares_all$Grade == '2' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,6] <-  sum(schLvl$new_subgroup_shares_all$Grade == '3' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,7] <-  sum(schLvl$new_subgroup_shares_all$Grade == '4' & schLvl$new_subgroup_shares_all$Students!= 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,8] <-  sum(schLvl$new_subgroup_shares_all$Grade == '5' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,9] <-  sum(schLvl$new_subgroup_shares_all$Grade == '6' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,10] <-  sum(schLvl$new_subgroup_shares_all$Grade == '7' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,11] <-  sum(schLvl$new_subgroup_shares_all$Grade == '8' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,12] <-  sum(schLvl$new_subgroup_shares_all$Grade == '9' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,13] <-  sum(schLvl$new_subgroup_shares_all$Grade == '10' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,14] <-  sum(schLvl$new_subgroup_shares_all$Grade == '11' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,15] <-  sum(schLvl$new_subgroup_shares_all$Grade == '12' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    school_enr_gr_Latinx_2[2,2] <-  sum(schLvl$new_subgroup_shares_all$Grade == 'PKG' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Latinx")	
    
    school_enr_gr_Latinx_2[3,2:15] <-   school_enr_gr_Latinx_2[2,2:15] - school_enr_gr_Latinx_2[1,2:15]	
    
    #White	
    
    school_enr_gr_White_2[2,3] <-  sum(schLvl$new_subgroup_shares_all$Grade == 'KG' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,4] <-  sum(schLvl$new_subgroup_shares_all$Grade == '1' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,5] <-  sum(schLvl$new_subgroup_shares_all$Grade == '2' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,6] <-  sum(schLvl$new_subgroup_shares_all$Grade == '3' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,7] <-  sum(schLvl$new_subgroup_shares_all$Grade == '4' & schLvl$new_subgroup_shares_all$Students!= 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,8] <-  sum(schLvl$new_subgroup_shares_all$Grade == '5' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,9] <-  sum(schLvl$new_subgroup_shares_all$Grade == '6' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,10] <-  sum(schLvl$new_subgroup_shares_all$Grade == '7' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,11] <-  sum(schLvl$new_subgroup_shares_all$Grade == '8' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,12] <-  sum(schLvl$new_subgroup_shares_all$Grade == '9' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,13] <-  sum(schLvl$new_subgroup_shares_all$Grade == '10' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,14] <-  sum(schLvl$new_subgroup_shares_all$Grade == '11' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,15] <-  sum(schLvl$new_subgroup_shares_all$Grade == '12' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    school_enr_gr_White_2[2,2] <-  sum(schLvl$new_subgroup_shares_all$Grade == 'PKG' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "White")	
    
    school_enr_gr_White_2[3,2:15] <-   school_enr_gr_White_2[2,2:15] - school_enr_gr_White_2[1,2:15]	
    
    #Multiethnic	
    
    school_enr_gr_Multiethnic_2[2,3] <-  sum(schLvl$new_subgroup_shares_all$Grade == 'KG' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,4] <-  sum(schLvl$new_subgroup_shares_all$Grade == '1' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,5] <-  sum(schLvl$new_subgroup_shares_all$Grade == '2' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,6] <-  sum(schLvl$new_subgroup_shares_all$Grade == '3' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,7] <-  sum(schLvl$new_subgroup_shares_all$Grade == '4' & schLvl$new_subgroup_shares_all$Students!= 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,8] <-  sum(schLvl$new_subgroup_shares_all$Grade == '5' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,9] <-  sum(schLvl$new_subgroup_shares_all$Grade == '6' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,10] <-  sum(schLvl$new_subgroup_shares_all$Grade == '7' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,11] <-  sum(schLvl$new_subgroup_shares_all$Grade == '8' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,12] <-  sum(schLvl$new_subgroup_shares_all$Grade == '9' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,13] <-  sum(schLvl$new_subgroup_shares_all$Grade == '10' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,14] <-  sum(schLvl$new_subgroup_shares_all$Grade == '11' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,15] <-  sum(schLvl$new_subgroup_shares_all$Grade == '12' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    school_enr_gr_Multiethnic_2[2,2] <-  sum(schLvl$new_subgroup_shares_all$Grade == 'PKG' & schLvl$new_subgroup_shares_all$Students != 0 & schLvl$new_subgroup_shares_all$Race == "Multiethnic")	
    
    school_enr_gr_Multiethnic_2[3,2:15] <-   school_enr_gr_Multiethnic_2[2,2:15] - school_enr_gr_Multiethnic_2[1,2:15]	
    
    # For Download Dataframe
    
    school_enr_gr_sce$sce <- if (input$subgroup_by_grade == "Asian"){	
      if (input$sch_enr_gr == "General Enrollment") {	
        school_enr_gr_Asian	
      }	else if (input$sch_enr_gr == "School"){	
        school_enr_gr_Asian_2	
      }	
    } else if (input$subgroup_by_grade == "Black"){	
      if (input$sch_enr_gr == "General Enrollment") {	
        school_enr_gr_Black	
      } else if (input$sch_enr_gr == "School"){	
        school_enr_gr_Black_2	
      }	
    }	else if (input$subgroup_by_grade == "Latinx"){	
      if (input$sch_enr_gr == "General Enrollment") {	
        school_enr_gr_Latinx	
      } else if (input$sch_enr_gr == "School"){	
        school_enr_gr_Latinx_2	
      }	
    }	else if (input$subgroup_by_grade == "White"){	
      if (input$sch_enr_gr == "General Enrollment") {	
        school_enr_gr_White	
      }	else if (input$sch_enr_gr == "School"){	
        school_enr_gr_White_2	
      }	
    } else if (input$subgroup_by_grade == "Multiethnic"){	
      if (input$sch_enr_gr == "General Enrollment") {	
        school_enr_gr_Multiethnic	
      } else if (input$sch_enr_gr == "School"){	
        school_enr_gr_Multiethnic_2	
      }	
    }	else if (input$subgroup_by_grade == "All"){	
      if (input$sch_enr_gr == "General Enrollment") {	
        school_enr_gr	
      } else if (input$sch_enr_gr == "School"){	
        school_enr_gr_2	
      }	
    }
    
    # Output	
    
    
    output$school_enr_gr <- renderDT({	
      
      if (input$subgroup_by_grade == "Asian"){	
        if (input$sch_enr_gr == "General Enrollment") {	
          school_enr_gr_Asian	
        }	
        else if (input$sch_enr_gr == "School"){	
          school_enr_gr_Asian_2	
        }	
      }	
      else if (input$subgroup_by_grade == "Black"){	
        if (input$sch_enr_gr == "General Enrollment") {	
          school_enr_gr_Black	
        }	
        else if (input$sch_enr_gr == "School"){	
          school_enr_gr_Black_2	
        }	
      }	
      else if (input$subgroup_by_grade == "Latinx"){	
        if (input$sch_enr_gr == "General Enrollment") {	
          school_enr_gr_Latinx	
        }	
        else if (input$sch_enr_gr == "School"){	
          school_enr_gr_Latinx_2	
        }	
      }	
      else if (input$subgroup_by_grade == "White"){	
        if (input$sch_enr_gr == "General Enrollment") {	
          school_enr_gr_White	
        }	
        else if (input$sch_enr_gr == "School"){	
          school_enr_gr_White_2	
        }	
      }	
      else if (input$subgroup_by_grade == "Multiethnic"){	
        if (input$sch_enr_gr == "General Enrollment") {	
          school_enr_gr_Multiethnic	
        }	
        else if (input$sch_enr_gr == "School"){	
          school_enr_gr_Multiethnic_2	
        }	
      }	
      else if (input$subgroup_by_grade == "All"){	
        if (input$sch_enr_gr == "General Enrollment") {	
          school_enr_gr	
        }	
        else if (input$sch_enr_gr == "School"){	
          school_enr_gr_2	
        }	
      }	
    },	
    filter = "none",	
    selection = 'none',	
    options = list(paginate = FALSE, searching = FALSE, info = FALSE), rownames = FALSE)
    
    
    #Average School Characteristics by School Level Update-----------------------------------------------------------------
    sce_active_schools <- schLvl$df$School.Name.SL[schLvl$df$enrollment != 0]
    sce_es_schools <- schLvl$df$School.Name.SL[schLvl$df$School.Level == "ES"]
    sce_ms_schools <- schLvl$df$School.Name.SL[schLvl$df$School.Level == "MS"]
    sce_k8_schools <- schLvl$df$School.Name.SL[schLvl$df$School.Level == "K-8"]
    sce_hs_schools <- schLvl$df$School.Name.SL[schLvl$df$School.Level == "HS"]
    
    schLvl$df$Utilization[!is.finite( schLvl$df$Utilization)] <- NA
    
    
    
    avg_school_char[1,3] <- ceiling(mean(schLvl$df$Classrooms[schLvl$df$enrollment != 0], na.rm = TRUE))
    avg_school_char[2,3] <- ceiling(aggregate(schLvl$schOP$General_Enrollment_Scenario, by = list(schLvl$schOP$General_Enrollment_Scenario != 0), FUN = mean, na.rm = TRUE)[2,2])
    avg_school_char[3,3] <- ceiling(mean(schLvl$df$Capacity[schLvl$df$enrollment != 0], na.rm = TRUE))
    avg_school_char[4,3] <- round(mean(schLvl$schOP$Utilization_Scenario[schLvl$schOP$General_Enrollment_Scenario != 0], na.rm = T),3)
    avg_school_char[5,3] <- mean(schLvl$df$Combined.Score[schLvl$df$enrollment != 0 & schLvl$df$Combined.Score > 0], na.rm = TRUE)
    avg_school_char[6,3] <- ceiling(sum(schLvl$df$enrollment[schLvl$df$enrollment != 0], na.rm = TRUE)/sum(schLvl$tc$x[schLvl$tc$Group.1 %in% sce_active_schools & schLvl$tc$x != 0], na.rm = TRUE))
    
    #ES 
    avg_school_char[1,6] <- ceiling(aggregate(schLvl$df$Classrooms, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                    [nrow(aggregate(schLvl$df$Classrooms, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                      ncol(aggregate(schLvl$df$Classrooms, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))])
    
    avg_school_char[2,6] <- ceiling(aggregate(schLvl$df$enrollment, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                    [nrow(aggregate(schLvl$df$enrollment, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                      ncol(aggregate(schLvl$df$enrollment, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))])
    
    avg_school_char[3,6] <- ceiling(aggregate(schLvl$df$Capacity, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                    [nrow(aggregate(schLvl$df$Capacity, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                      ncol(aggregate(schLvl$df$Capacity, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))])
    
    avg_school_char[4,6] <- round(aggregate(schLvl$df$Utilization, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                  [nrow(aggregate(schLvl$df$Utilization, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                    ncol(aggregate(schLvl$df$Utilization, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))]
                                  ,3)
    
    avg_school_char[5,6] <- aggregate(schLvl$df$Combined.Score, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Combined.Score > 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)[nrow(aggregate(schLvl$df$Combined.Score, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Combined.Score > 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                                                                                                                                                                                                     ncol(aggregate(schLvl$df$Combined.Score, by = list(schLvl$df$School.Level == "ES", schLvl$df$enrollment != 0, schLvl$df$Combined.Score > 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))]
    
    avg_school_char[6,6] <- ceiling(sum(schLvl$df$enrollment[schLvl$df$enrollment != 0 & schLvl$df$School.Level == "ES" & schLvl$df$Active == 1], na.rm = TRUE) / 
                                      sum(schLvl$tc$x[schLvl$tc$Group.1 %in% sce_es_schools & schLvl$tc$x != 0], na.rm = TRUE))
    
    
    #MS
    avg_school_char[1,9] <- ceiling(aggregate(schLvl$df$Classrooms, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                    [nrow(aggregate(schLvl$df$Classrooms, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                      ncol(aggregate(schLvl$df$Classrooms, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))])
    
    avg_school_char[2,9] <- ceiling(aggregate(schLvl$df$enrollment, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                    [nrow(aggregate(schLvl$df$enrollment, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                      ncol(aggregate(schLvl$df$enrollment, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))])
    
    avg_school_char[3,9] <- ceiling(aggregate(schLvl$df$Capacity, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                    [nrow(aggregate(schLvl$df$Capacity, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                      ncol(aggregate(schLvl$df$Capacity, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))])
    
    avg_school_char[4,9] <- round(aggregate(schLvl$df$Utilization, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                  [nrow(aggregate(schLvl$df$Utilization, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                    ncol(aggregate(schLvl$df$Utilization, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))]
                                  ,3)
    
    avg_school_char[5,9] <- aggregate(schLvl$df$Combined.Score, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Combined.Score > 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)[nrow(aggregate(schLvl$df$Combined.Score, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Combined.Score > 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                                                                                                                                                                                                     ncol(aggregate(schLvl$df$Combined.Score, by = list(schLvl$df$School.Level == "MS", schLvl$df$enrollment != 0, schLvl$df$Combined.Score > 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))]
    
    avg_school_char[6,9] <- ceiling(sum(schLvl$df$enrollment[schLvl$df$enrollment != 0 & schLvl$df$School.Level == "MS" & schLvl$df$Active == 1], na.rm = TRUE) / 
                                      sum(schLvl$tc$x[schLvl$tc$Group.1 %in% sce_ms_schools & schLvl$tc$x != 0], na.rm = TRUE))
    
    #K-8
    avg_school_char[1,12] <- ceiling(aggregate(schLvl$df$Classrooms, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                     [nrow(aggregate(schLvl$df$Classrooms, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                       ncol(aggregate(schLvl$df$Classrooms, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))])
    
    avg_school_char[2,12] <- ceiling(aggregate(schLvl$df$enrollment, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                     [nrow(aggregate(schLvl$df$enrollment, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                       ncol(aggregate(schLvl$df$enrollment, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))])
    
    avg_school_char[3,12] <- ceiling(aggregate(schLvl$df$Capacity, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                     [nrow(aggregate(schLvl$df$Capacity, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                       ncol(aggregate(schLvl$df$Capacity, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))])
    
    avg_school_char[4,12] <- round(aggregate(schLvl$df$Utilization, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                   [nrow(aggregate(schLvl$df$Utilization, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                     ncol(aggregate(schLvl$df$Utilization, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))]
                                   ,3)
    
    avg_school_char[5,12] <- aggregate(schLvl$df$Combined.Score, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Combined.Score > 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)[nrow(aggregate(schLvl$df$Combined.Score, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Combined.Score > 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                                                                                                                                                                                                       ncol(aggregate(schLvl$df$Combined.Score, by = list(schLvl$df$School.Level == "K-8", schLvl$df$enrollment != 0, schLvl$df$Combined.Score > 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))]
    
    avg_school_char[6,12] <- ceiling(sum(schLvl$df$enrollment[schLvl$df$enrollment != 0 & schLvl$df$School.Level == "K-8" & schLvl$df$Active == 1], na.rm = TRUE) / 
                                       sum(schLvl$tc$x[schLvl$tc$Group.1 %in% sce_k8_schools & schLvl$tc$x != 0], na.rm = TRUE))
    
    #HS
    avg_school_char[1,15] <- ceiling(aggregate(schLvl$df$Classrooms, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                     [nrow(aggregate(schLvl$df$Classrooms, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                       ncol(aggregate(schLvl$df$Classrooms, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))])
    
    avg_school_char[2,15] <- ceiling(aggregate(schLvl$df$enrollment, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                     [nrow(aggregate(schLvl$df$enrollment, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                       ncol(aggregate(schLvl$df$enrollment, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))])
    
    avg_school_char[3,15] <- ceiling(aggregate(schLvl$df$Capacity, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                     [nrow(aggregate(schLvl$df$Capacity, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                       ncol(aggregate(schLvl$df$Capacity, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))])
    
    avg_school_char[4,15] <- round(aggregate(schLvl$df$Utilization, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)
                                   [nrow(aggregate(schLvl$df$Utilization, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                     ncol(aggregate(schLvl$df$Utilization, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))]
                                   ,3)
    
    avg_school_char[5,15] <- aggregate(schLvl$df$Combined.Score, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Combined.Score > 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)[nrow(aggregate(schLvl$df$Combined.Score, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Combined.Score > 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE)),
                                                                                                                                                                                                                      ncol(aggregate(schLvl$df$Combined.Score, by = list(schLvl$df$School.Level == "HS", schLvl$df$enrollment != 0, schLvl$df$Combined.Score > 0, schLvl$df$Active == 1), FUN = mean, na.rm = TRUE))]
    
    avg_school_char[6,15] <- ceiling(sum(schLvl$df$enrollment[schLvl$df$enrollment != 0 & schLvl$df$School.Level == "HS" & schLvl$df$Active == 1], na.rm = TRUE) / 
                                       sum(schLvl$tc$x[schLvl$tc$Group.1 %in% sce_hs_schools & schLvl$tc$x != 0], na.rm = TRUE))
    
    #Delta Calc
    avg_school_char[,4] <- avg_school_char[,3] - avg_school_char[,2]
    avg_school_char[,7] <- avg_school_char[,6] - avg_school_char[,5]
    avg_school_char[,10] <- avg_school_char[,9] - avg_school_char[,8]
    avg_school_char[,13] <- avg_school_char[,12] - avg_school_char[,11]
    avg_school_char[,16] <- avg_school_char[,15] - avg_school_char[,14]
    
    #Save for Download
    avg_school_char_sce$sce <- avg_school_char
    
    #Render
    output$avg_school_char <- renderDT({
      og_names <- names(avg_school_char)
      
      avg_school_char %>% 
        mutate(order = row_number()) %>% 
        gather(metric, value, -c(Metrics, order)) %>% 
        mutate(value = case_when(Metrics == 'Avg. Utilization' ~ scales::percent(value),
                                 TRUE ~ as.character(prettyNum(round(value), big.mark = ',')))) %>% 
        spread(metric, value) %>% 
        arrange(order) %>% 
        select(all_of(og_names)) %>% 
        datatable(options = list(sort = FALSE, info = FALSE, searching = FALSE, paginate = FALSE), rownames = FALSE)
    })
    
    #School Allocation Summary: Scenario-----------------------------------------------------------------------------------------------
    
    
    #Schools Cost Update -----------------------------------------------------------------------------------------------------
    sch_costT_sce <- subset(schLvl$schOP[!(schLvl$schOP$Schools == "Impact Academy"),], select = c(Schools, General_Enrollment_Scenario, Baseline_Scenario, SBA_Scenario) )
    sch_costT_sce$IPSTotal <- pmax(sch_costT_sce$Baseline_Scenario, sch_costT_sce$SBA_Scenario)
    distSchChk <- subset(schLvl$df, select = c(School.Name.SL, District.School) )
    
    sch_costT_sce <- merge(x = sch_costT_sce, y = distSchChk, by.x = "Schools", by.y = "School.Name.SL", all.x = TRUE)
    #sch_costT_sce$IPSTotal[sch_costT_sce$District.School == 0] <- 0
    sch_costT_sce$Check[sch_costT_sce$IPSTotal!= 0 & sch_costT_sce$Baseline_Scenario == sch_costT_sce$IPSTotal & sch_costT_sce$General_Enrollment_Scenario > 0] <- 0
    sch_costT_sce$Check[sch_costT_sce$IPSTotal != 0 & sch_costT_sce$SBA_Scenario == sch_costT_sce$IPSTotal & sch_costT_sce$General_Enrollment_Scenario > 0] <- 1 
    #Re-calc without formatting to enable Delta calculations	
    sch_cost[1,2] <- sum(sch_costT$IPSTotal)	
    sch_cost[2,2] <- sum(slDf$total_baseline)	
    sch_cost[3,2] <- abs(sum(ceiling(all_default_sba$sba_to_bl)))	
    sch_cost[4,2] <- sum(slDf$total_sba)	
    
    #Re-calc sba/baseline without subsetting	
    recalc_sce_sba_bl <- all_funding[all_funding$enrollment != 0,]	
    recalc_sce_sba_bl$sba_below_baseline <- 0	
    recalc_sce_sba_bl$sba_below_baseline[recalc_sce_sba_bl$baseline_sce > recalc_sce_sba_bl$sba_sce] <- 1	
    recalc_sce_sba_bl$sba_to_bl <- 0 	
    recalc_sce_sba_bl$sba_to_bl[recalc_sce_sba_bl$sba_below_baseline == 1] <- recalc_sce_sba_bl$baseline_sce[recalc_sce_sba_bl$sba_below_baseline == 1] - recalc_sce_sba_bl$sba_sce[recalc_sce_sba_bl$sba_below_baseline == 1]
    
    
    #School Cost Scenario Table creation	
    sch_cost[1,3] <- sum(ceiling(sch_costT_sce$IPSTotal), na.rm = TRUE)	
    sch_cost[2,3] <- sum(ceiling(schLvl$schOP$Baseline_Scenario), na.rm = TRUE)	
    sch_cost[3,3] <- abs(sum(recalc_sce_sba_bl$sba_to_bl))
    sch_cost[4,3] <- sum(schLvl$schOP$SBA_Scenario, na.rm = TRUE)
    
    sch_cost[,4] <- as.numeric(sch_cost[,3]) - as.numeric(sch_cost[,2])
    
    
    sch_cost[,2] <- dollar(as.numeric(sch_cost[,2]))
    sch_cost[,3] <- dollar(as.numeric(sch_cost[,3]))
    sch_cost[,4] <- dollar_format(negative_parens = TRUE)(as.numeric(sch_cost[,4]))
    
    sch_cost_sce$sce <- sch_cost
    
    output$v1 <- renderDT(sch_cost,
                          colnames = c("School Costs", "Default", "Scenario", "Delta"),
                          filter = "none",
                          selection = 'none',
                          options = list( paging = FALSE, searching = FALSE))
    
    #Per Student Funding Scenario Table creation----------------------------------------------------------------------------	
    
    cost_ps[,4] <- 0
    
    cost_ps[1,2] <- sum(sch_costT$IPSTotal)/sum(sch_costT$enrollment[sch_costT$IPSTotal > 0], na.rm = TRUE)	
    cost_ps[2,2] <- sum(sch_costT$total_baseline[sch_costT$IPSTotal == sch_costT$total_baseline])/ sum(sch_costT$enrollment[sch_costT$IPSTotal == sch_costT$total_baseline & sch_costT$IPSTotal >0 ])	
    cost_ps[3,2] <- sum(sch_costT$total_sba[sch_costT$IPSTotal == sch_costT$total_sba])/ sum(sch_costT$enrollment[sch_costT$IPSTotal == sch_costT$total_sba& sch_costT$IPSTotal >0 ])	
    
    cost_ps[1,3] <- sum(sch_costT_sce$IPSTotal)/sum(sch_costT_sce$General_Enrollment_Scenario[sch_costT_sce$IPSTotal >0], na.rm = TRUE)	
    cost_ps[2,3] <- sum(sch_costT_sce$Baseline_Scenario[sch_costT_sce$Baseline_Scenario > 0])/ sum(sch_costT_sce$General_Enrollment_Scenario[sch_costT_sce$Baseline_Scenario == sch_costT_sce$IPSTotal & sch_costT_sce$IPSTotal > 0])	
    cost_ps[3,3] <- sum(sch_costT_sce$SBA_Scenario[sch_costT_sce$SBA_Scenario > 0])/ sum(sch_costT_sce$General_Enrollment_Scenario[sch_costT_sce$SBA_Scenario == sch_costT_sce$IPSTotal & sch_costT_sce$IPSTotal > 0])
    
    cost_ps[,4] <- as.numeric(cost_ps[,3]) - as.numeric(cost_ps[,2])
    
    cost_ps[,2] <- dollar(as.numeric(cost_ps[,2]))
    cost_ps[,3] <- dollar(as.numeric(cost_ps[,3]))
    cost_ps[,4] <- dollar_format(negative_parens = TRUE)(as.numeric(cost_ps[,4]))
    
    cost_ps_sce$sce <- cost_ps
    
    output$cost_ps <- renderDT(cost_ps,
                               colnames = c("Per Student Costs", "Default", "Scenario", "Delta"),
                               filter = "none",
                               selection = 'none',
                               options = list( paging = FALSE, searching = FALSE))
    
    #School Count Update ---------------------------------------------------------------------------------------
    sch_count[1,3] <- sum(sch_costT_sce$IPSTotal > 0) + count(unique(sum_inact_schools[,1]))
    sch_count[2,3] <- sum(sch_costT_sce$Check == 1, na.rm = TRUE)
    sch_count[3,3] <- sum(sch_costT_sce$Check == 0, na.rm = TRUE)
    
    sch_count[,4] <- sch_count[,3] - sch_count[,2]
    
    sch_count_sce$sce <-  sch_count 
    
    output$z1 <- renderDT(sch_count,
                          colnames = c("School Counts", "Default", "Scenario", "Delta"), 
                          filter = "none",
                          selection = 'none',
                          options = list( paging = FALSE, searching = FALSE))
    
    #Subgroups by Facility Condition--------------------------------------------------------------------------------------
    hq_schools <- schLvl$df[schLvl$df$Combined.Score > 80, 1]
    lq_schools <- schLvl$df[schLvl$df$Combined.Score < 60, 1]
    overcap_schools <- schLvl$df[schLvl$df$Utilization > 1, 1]
    
    subgroups[1,3] <- sum(x$df1[x$df1$Grade == "Sp_Ed" & x$df1$School.Name %in% hq_schools,7]) / sum(x$df1[x$df1$Grade == "Sp_Ed",7])
    subgroups[2,3] <- sum(x$df1[x$df1$Grade == "ELL" & x$df1$School.Name %in% hq_schools,7]) / sum(x$df1[x$df1$Grade == "ELL",7])
    subgroups[3,3] <- sum(schLvl$df[schLvl$df$Combined.Score > 80,"Direct.Cert"] * schLvl$df[schLvl$df$Combined.Score > 80,"enrollment"],na.rm = TRUE) / sum(schLvl$df$Direct.Cert * schLvl$df$enrollment, na.rm = TRUE)
    subgroups[4,3] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% hq_schools & schLvl$new_subgroup_shares_all$Race == "Asian"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "Asian"], na.rm =  TRUE)
    subgroups[5,3] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% hq_schools & schLvl$new_subgroup_shares_all$Race == "Black"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "Black"], na.rm =  TRUE)
    subgroups[6,3] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% hq_schools & schLvl$new_subgroup_shares_all$Race == "Latinx"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "Latinx"], na.rm =  TRUE)
    subgroups[7,3] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% hq_schools & schLvl$new_subgroup_shares_all$Race == "White"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "White"], na.rm =  TRUE)
    subgroups[8,3] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% hq_schools & schLvl$new_subgroup_shares_all$Race == "Multiethnic"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "Multiethnic"], na.rm =  TRUE)
    
    
    subgroups[1,6] <- sum(x$df1[x$df1$Grade == "Sp_Ed" & x$df1$School.Name %in% lq_schools,7]) / sum(x$df1[x$df1$Grade == "Sp_Ed",7])
    subgroups[2,6] <- sum(x$df1[x$df1$Grade == "ELL" & x$df1$School.Name %in% lq_schools,7]) / sum(x$df1[x$df1$Grade == "ELL",7])
    subgroups[3,6] <- sum(schLvl$df[schLvl$df$Combined.Score < 60,"Direct.Cert"] * schLvl$df[schLvl$df$Combined.Score < 60,"enrollment"],na.rm = TRUE) / sum(schLvl$df$Direct.Cert * schLvl$df$enrollment, na.rm = TRUE)
    subgroups[4,6] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% lq_schools & schLvl$new_subgroup_shares_all$Race == "Asian"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "Asian"], na.rm =  TRUE)
    subgroups[5,6] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% lq_schools & schLvl$new_subgroup_shares_all$Race == "Black"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "Black"], na.rm =  TRUE)
    subgroups[6,6] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% lq_schools & schLvl$new_subgroup_shares_all$Race == "Latinx"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "Latinx"], na.rm =  TRUE)
    subgroups[7,6] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% lq_schools & schLvl$new_subgroup_shares_all$Race == "White"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "White"], na.rm =  TRUE)
    subgroups[8,6] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% lq_schools & schLvl$new_subgroup_shares_all$Race == "Multiethnic"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "Multiethnic"], na.rm =  TRUE)
    
    
    subgroups[1,9] <- sum(x$df1[x$df1$Grade == "Sp_Ed" & x$df1$School.Name %in% overcap_schools,7]) / sum(x$df1[x$df1$Grade == "Sp_Ed",7])
    subgroups[2,9] <- sum(x$df1[x$df1$Grade == "ELL" & x$df1$School.Name %in% overcap_schools,7]) / sum(x$df1[x$df1$Grade == "ELL",7])
    subgroups[3,9] <- sum(schLvl$df[schLvl$df$Combined.Score > 80,"Direct.Cert"] * schLvl$df[schLvl$df$Combined.Score > 80,"enrollment"],na.rm = TRUE) / sum(schLvl$df$Direct.Cert * schLvl$df$enrollment, na.rm = TRUE)
    subgroups[4,9] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% overcap_schools & schLvl$new_subgroup_shares_all$Race == "Asian"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "Asian"], na.rm =  TRUE)
    subgroups[5,9] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% overcap_schools & schLvl$new_subgroup_shares_all$Race == "Black"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "Black"], na.rm =  TRUE)
    subgroups[6,9] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% overcap_schools & schLvl$new_subgroup_shares_all$Race == "Latinx"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "Latinx"], na.rm =  TRUE)
    subgroups[7,9] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% overcap_schools & schLvl$new_subgroup_shares_all$Race == "White"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "White"], na.rm =  TRUE)
    subgroups[8,9] <- sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$School.Name %in% overcap_schools & schLvl$new_subgroup_shares_all$Race == "Multiethnic"], na.rm =  TRUE) / sum(schLvl$new_subgroup_shares_all$Students[schLvl$new_subgroup_shares_all$Race == "Multiethnic"], na.rm =  TRUE)
    
    subgroups[,4] <- subgroups[,3] - subgroups[,2]
    subgroups[,7] <- subgroups[,6] - subgroups[,5] 
    subgroups[,10] <- subgroups[,9] - subgroups[,8] 
    
    subgroups_sce$sce <- subgroups
    
    #Output Subgroups by Facility Condition
    
    output$subgroups <- renderDT({
      datatable(subgroups, options = list(paginate = FALSE, searching = FALSE, info = FALSE), rownames = FALSE) %>% 
        formatPercentage(names(subgroups)[!names(subgroups) == 'Demographic'])
    })
    
    #IPS Attrition Table reactive dataframe--------------------------------------------------------------------------------
    
    attrition <- reactive({	
      
      attrColNms <- c("Total Displaced", "Total No Longer Enrolled", "Attrition (/Growth) Rate")	
      
      attrDenom <- x$df_all[x$df_all$me_delta < 0,]	
      attrDenom <- attrDenom[!duplicated(attrDenom[, c("School.Name", "Grade")], fromLast=T),]	
      attrDenom <- abs(sum(attrDenom$me_delta) *-1)		
      
      attrNum <- x$df_all[x$df_all$me_delta > 0,]	
      attrNum <- attrNum[!duplicated(attrNum[, c("School.Name", "Grade", "me_delta")], fromLast=T),]
      attrNum <- attrNum[attrNum$counter == max(attrNum$counter),]
      attrNum <- attrNum[!duplicated(attrNum[, c("School.Name", "Grade")]),]
      attrNum <- abs(sum(attrNum$me_delta) + attrDenom * -1)
      
      attrRate <- abs(attrNum/attrDenom)
      attrRate <- sprintf("%.0f %%", 100*attrRate) 	
      attrVals <- c(attrDenom, attrNum, attrRate)	
      
      attrition <- cbind(attrColNms, attrVals)	
      
      
    })
    
    #Updated Map Scenario Data Frame-----------------------------------------------------------------------------------------
    schLvl$mDf <- schLvl$df
    schLvl$mDf <- subset(schLvl$mDf, select = c(School.Name.SL, School.Performance, School.Level, Access.Type, Latitude, Longitude, enrollment, Utilization, Direct.Cert, Combined.Score))
    schLvl$mDf <- schLvl$mDf[!(schLvl$mDf$Latitude==0 & schLvl$mDf$Longitude==0),]
    schLvl$mDf$enrollment[schLvl$mDf$enrollment == "NA"] <- 0
    schLvl$mDf <- schLvl$mDf[!(schLvl$mDf$enrollment == 0),]
    schLvl$mDf <- schLvl$mDf[is.na(schLvl$mDf$enrollment) == FALSE,]
    # 
    # schLvl$icon_ids <- schLvl$mDf %>%
    #   select(Access.Type, School.Level) %>%
    #   unique() %>%
    #   mutate(schLvl$icon_id = row_number())
    # 
    # schLvl$access_types <- unique(schLvl$mDf$Access.Type)
    # schLvl$school_levels <- unique(schLvl$mDf$School.Level)
    # 
    # schLvl$iconFiles <- map(1:nrow(icon_ids), function(i) {
    #   this_row <- slice(schLvl$icon_ids, i)
    #   shape <- which(access_types == this_row$Access.Type) + 20
    #   color <- colors[[this_row$School.Level]]
    #   pchIcons(shape = shape, color = color)
    # }) %>% unlist()
    
    #IPS Attrition Table Output Table--------------------------------------------------------------------------------
    output$xz <- renderDT(attrition(),
                          colnames = c("Scenario Schools", "Students +/-"),
                          filter = "none",
                          selection = 'none', 
                          options = list( paging = FALSE, searching = FALSE))
    
    updateSelectInput(session, "subSchool", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch1", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch2", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch3", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch4", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch5", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch6", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch7", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch8", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch9", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch10", selected = "Anna Brochhausen School 88")
    
    Items <- c("Scenario Title", "Description")
    Text <- c(input$textT, input$textD)
    x$titleDescription <-as.data.frame(rbind(Items, Text))
    x$titleDescription <- t(x$titleDescription)
    
    #Clear Trigger Tables on Submit------------
    dummy_tracker <- x$net_scenario_change_table
    dummy_tracker <- dummy_tracker[1,1:18]
    dummy_tracker[1,1] <- "# of Students Remaining"
    dummy_tracker[1,2:18] <- 0
    colnames(dummy_tracker)[1] <- "Grade"
    
    output$tracker <- renderDT({
      dummy_tracker %>% 
        datatable(filter = 'none', selection = 'none', rownames=FALSE,
                  options = list(pageLength = 17, paging = FALSE, searching = FALSE, sort = FALSE, scrollX = TRUE)) #ensures all rows are visible, removes paging and search box from datatable output
    })
    
    # Update selectInputs with new school
    updateSelectInput(session, 'subSchool', choices = x$inputSchList)
    map(1:11, function(num) {
      updateSelectInput(session, str_glue("iSch{num}"), choices = x$inputSchList)
    })
  }
  
  observeEvent(input$subSchool,{
    if(counter$counterValue == 0){
      LlDf <- impact_Table_SubSchLL
      
      LlDf <- LlDf[LlDf$School.Name.SL == isolate(input$subSchool), ]
      
      Source <- impact_Table_Source 
      
      Source <-Source[!Source$School.Name.SL %in% c("KIPP:Indy Schools"), ]
      
      Source$Seats.Remaining <- Source$Capacity - Source$enrollment
      
      Source$Distance <- round(measurements::conv_unit(geosphere::distm(Source[ , c("Longitude", "Latitude")],
                                                                        c(LlDf$Longitude[1], LlDf$Latitude[1]), fun = geosphere::distHaversine), "m", "mile"), digits = 2)
      
      Source <- Source[order(Source$Distance),]
      
      Master <- subset(Source, select=c("School.Name.SL","Distance", "School.Level", "Capacity", "Utilization", "Seats.Remaining", "Access.Type", "Condition"))
      
      Master <- Master[Master$School.Name.SL != isolate(input$subSchool),]
      
      output$impact <- renderDT({
        Master  %>% 
          datatable(
            colnames = c("Schools","Distance from Subject School (mi)", "School Level", "Capacity", "Utilization","Seats Remaining", "Access Type", "Facility Condition"),
            rownames=FALSE, 
            options = list(paging = FALSE, searching = FALSE, sort = FALSE, scrollY = "120px")) %>% 
          formatPercentage("Utilization")
      })
    } else if (counter$counterValue > 0){
      
      impact_Table_SubSchLL <- data.frame(subset(schLvl$df, select = c(School.Name.SL, Latitude, Longitude)))
      # impact_Table_SubSchLL <- impact_Table_SubSchLL[!impact_Table_SubSchLL %in% c("Impact Academy", "KIPP Indy College Prep Middle School", "KIPP Indy Legacy High School", "KIPP Indy Unite Elementary School", "Phalen Leadership Academy@George H Fisher") ]
      impact_Table_Source <- data.frame(subset(schLvl$df, select = c(School.Name.SL, School.Level, enrollment, Capacity, Utilization, Access.Type, Condition, Latitude, Longitude)))
      # impact_Table_Source <- impact_Table_Source[!impact_Table_Source %in% c("Impact Academy", "KIPP Indy College Prep Middle School", "KIPP Indy Legacy High School", "KIPP Indy Unite Elementary School", "Phalen Leadership Academy@George H Fisher") ]
      
      impact_Table_Source <- impact_Table_Source[impact_Table_Source$Longitude != 0, ]
      
      LlDf <- impact_Table_SubSchLL
      
      LlDf <- LlDf[LlDf$School.Name.SL == isolate(input$subSchool), ]
      
      Source <- impact_Table_Source
      
      Source <-Source[!Source$School.Name.SL %in% c("KIPP:Indy Schools"), ]
      
      Source$Seats.Remaining <- Source$Capacity - Source$enrollment
      
      Source$Distance <- round(measurements::conv_unit(geosphere::distm(Source[ , c("Longitude", "Latitude")],
                                                                        c(LlDf$Longitude[1], LlDf$Latitude[1]), fun = geosphere::distHaversine), "m", "mile"), digits = 2)
      
      Source <- Source[order(Source$Distance),]
      
      Impact_Master <- subset(Source, select=c("School.Name.SL","Distance", "School.Level", "Capacity", "Utilization","Seats.Remaining", "Access.Type", "Condition"))
      
      Impact_Master <- Impact_Master[Impact_Master$School.Name.SL != isolate(input$subSchool),]
      
      #Impact_Master[!Impact_Master$School.Name.SL %in% c("Emmerich Manual High School","George H Fisher Elementary School","KIPP:Indy Schools","McFarland Alternative Center","Step Ahead Academy","Thomas Carr Howe Community School") ]
      
      output$impact <- renderDT({
        Impact_Master  %>% 
          datatable(
            colnames = c("Schools","Distance from Subject School (mi)", "School Level", "Capacity", "Utilization", "Seats Remaining", "Access Type", "Facility Condition"),
            rownames=FALSE, 
            options = list(paging = FALSE, searching = FALSE, sort = FALSE, scrollY = "120px")) %>% 
          formatPercentage("Utilization")
      })
      
      
    }
  })
  
  trigger_tracker_1 <- function(){
    
    t <- x$df1
    t1 <- x$df2
    t2 <- x$df3
    t3 <- x$df4
    t4 <- x$df5
    t5 <- x$df6
    t6 <- x$df7
    t7 <- x$df8
    t8 <- x$df9
    t9 <- x$df10
    t10 <- x$df11
    
    #Create List of selected schools
    sel_sch <- c(input$subSchool)
    
    x$tracker_df_changes <- rbind(t, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) 
    
    x$tracker_df_changes[,8] <- x$tracker_df_changes[,7] - x$tracker_df_changes[,6]
    
    x$tracker_df_allCh <- x$tracker_df_changes
    
    #Check if changes made
    ifelse(nrow(x$tracker_df_changes) == 0, x$tracker_df_allCh[1,] <- 0, x$tracker_df_allCh <- x$tracker_df_allCh)
    
    #Append to existing tracker
    x$tracker_df_allCh$counter <- counter$counterValue
    x$tracker_df_all <- rbind(x$tracker_df_allCh)
    
    #Create List of selected schools
    sel_sch <- c(input$subSchool)
    
    x$tracker_df_all <- rbind(x$tracker_df_allCh)
    
    old_changes <- x$tracker_df_all %>% 
      select(School.Name, Grade, me_delta, counter) %>% 
      group_by_all() %>%
      filter(n() == 10) %>%
      ungroup() %>% 
      mutate(Grade = as.character(Grade))
    
    new_changes <- x$tracker_df_all %>% 
      select(School.Name, Grade, me_delta, counter) %>% 
      group_by_all() %>%
      filter(n() != 10) %>%
      ungroup() %>% 
      mutate(Grade = as.character(Grade))
    
    #Remove Old Values from New Changes
    new_changes <- new_changes[!duplicated(new_changes[, c("School.Name", "Grade")]),]	
    new_changes <- new_changes[new_changes$School.Name %in% sel_sch,]
    
    #Zero-out unaffected rows
    new_changes[!(new_changes$School.Name %in% old_changes$School.Name & new_changes$Grade %in% old_changes$Grade), "me_delta"] <- 0
    
    #Update New Changes based off Enr from Prior Submits
    s2_delta <- ifelse(nrow(old_changes) > 0, new_changes[new_changes$School.Name %in% old_changes$School.Name & new_changes$Grade %in% old_changes$Grade, "me_delta"], 0) 
    s1_delta <- ifelse(nrow(old_changes) > 0, old_changes[!duplicated(old_changes[, c("School.Name", "Grade")]),"me_delta"], 0) 
    new_changes[new_changes$School.Name %in% old_changes$School.Name & new_changes$Grade %in% old_changes$Grade, "me_delta"] <- s2_delta[[1]] - s1_delta[[1]]  
    
    #Create Tracker
    net_scenario_change_table <- expand.grid(
      School.Name = new_changes$School.Name,
      Grade = Grade,
      stringsAsFactors = FALSE
    ) %>% 
      unique() %>% 
      left_join(new_changes, by = c('School.Name', 'Grade')) %>% 
      mutate(me_delta = coalesce(me_delta, 0)) %>% 
      group_by(School.Name) %>% 
      ungroup() %>% 
      spread(Grade, me_delta, fill = 0) %>% 
      gather(grade, students, -c(School.Name)) %>% 
      group_by(School.Name, grade) %>% 
      summarise(students = sum(students)) %>%
      ungroup() %>% 
      spread(grade, students) %>%
      select(`School Name` = School.Name, all_of(Grade), `Update #` = 1)
    
    net_scen_table_sorted <- net_scenario_change_table
    net_scen_table_sorted <- net_scen_table_sorted %>% group_by(`School Name`) %>% filter(n() > 1)
    net_scen_table_sorted$PK[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$PK[tail(which(net_scen_table_sorted$PK !=0),1)]) == 0, 0, net_scen_table_sorted$PK[tail(which(net_scen_table_sorted$PK !=0),1)])
    net_scen_table_sorted$K[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$K[tail(which(net_scen_table_sorted$K !=0),1)]) == 0, 0, net_scen_table_sorted$K[tail(which(net_scen_table_sorted$K !=0),1)])
    net_scen_table_sorted$`1`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`1`[tail(which(net_scen_table_sorted$`1` !=0),1)]) == 0, 0, net_scen_table_sorted$`1`[tail(which(net_scen_table_sorted$`1` !=0),1)])
    net_scen_table_sorted$`2`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`2`[tail(which(net_scen_table_sorted$`2` !=0),1)]) == 0, 0, net_scen_table_sorted$`2`[tail(which(net_scen_table_sorted$`2` !=0),1)])
    net_scen_table_sorted$`3`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`3`[tail(which(net_scen_table_sorted$`3` !=0),1)]) == 0, 0, net_scen_table_sorted$`3`[tail(which(net_scen_table_sorted$`3` !=0),1)])
    net_scen_table_sorted$`4`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`4`[tail(which(net_scen_table_sorted$`4` !=0),1)]) == 0, 0, net_scen_table_sorted$`4`[tail(which(net_scen_table_sorted$`4` !=0),1)])
    net_scen_table_sorted$`5`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`5`[tail(which(net_scen_table_sorted$`5` !=0),1)]) == 0, 0, net_scen_table_sorted$`5`[tail(which(net_scen_table_sorted$`5` !=0),1)])
    net_scen_table_sorted$`6`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`6`[tail(which(net_scen_table_sorted$`6` !=0),1)]) == 0, 0, net_scen_table_sorted$`6`[tail(which(net_scen_table_sorted$`6` !=0),1)])
    net_scen_table_sorted$`7`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`7`[tail(which(net_scen_table_sorted$`7` !=0),1)]) == 0, 0, net_scen_table_sorted$`7`[tail(which(net_scen_table_sorted$`7` !=0),1)])
    net_scen_table_sorted$`8`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`8`[tail(which(net_scen_table_sorted$`8` !=0),1)]) == 0, 0, net_scen_table_sorted$`8`[tail(which(net_scen_table_sorted$`8` !=0),1)])
    net_scen_table_sorted$`9`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`9`[tail(which(net_scen_table_sorted$`9` !=0),1)]) == 0, 0, net_scen_table_sorted$`9`[tail(which(net_scen_table_sorted$`9` !=0),1)])
    net_scen_table_sorted$`10`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`10`[tail(which(net_scen_table_sorted$`10` !=0),1)]) == 0, 0, net_scen_table_sorted$`10`[tail(which(net_scen_table_sorted$`10` !=0),1)])
    net_scen_table_sorted$`11`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`11`[tail(which(net_scen_table_sorted$`11` !=0),1)]) == 0, 0, net_scen_table_sorted$`11`[tail(which(net_scen_table_sorted$`11` !=0),1)])
    net_scen_table_sorted$`12`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`12`[tail(which(net_scen_table_sorted$`12` !=0),1)]) == 0, 0, net_scen_table_sorted$`12`[tail(which(net_scen_table_sorted$`12` !=0),1)])
    net_scen_table_sorted$Alt_Ed[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$Alt_Ed[tail(which(net_scen_table_sorted$Alt_Ed !=0),1)]) == 0, 0, net_scen_table_sorted$Alt_Ed[tail(which(net_scen_table_sorted$Alt_Ed !=0),1)])
    net_scen_table_sorted$Sp_Ed[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$Sp_Ed[tail(which(net_scen_table_sorted$Sp_Ed !=0),1)]) == 0, 0, net_scen_table_sorted$Sp_Ed[tail(which(net_scen_table_sorted$Sp_Ed !=0),1)])
    net_scen_table_sorted$ELL[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$ELL[tail(which(net_scen_table_sorted$ELL !=0),1)]) == 0, 0, net_scen_table_sorted$ELL[tail(which(net_scen_table_sorted$ELL !=0),1)])
    names(net_scen_table_sorted[,2:18]) <- Grade
    net_scen_table_sorted_2 <-  net_scen_table_sorted[!duplicated(net_scen_table_sorted$`School Name`),]
    net_scen_table_sorted_2 <- net_scen_table_sorted_2[is.na(net_scen_table_sorted_2$PK) == FALSE,]
    x_net_scenario_change_table <- net_scenario_change_table
    x_net_scenario_change_table[match(net_scen_table_sorted_2$`School Name`,  x_net_scenario_change_table$`School Name`),] <- net_scen_table_sorted_2
    colnames(x_net_scenario_change_table)[1] <- "School"
    
    tracker_total <- x_net_scenario_change_table[x_net_scenario_change_table$`Update #` == max(x_net_scenario_change_table$`Update #`),] %>% 
      distinct(School, .keep_all = TRUE) %>%
      select(all_of(Grade)) %>% 
      colSums(na.rm = TRUE) %>% 
      as.data.frame() %>% 
      t() %>% 
      as_tibble() %>% 
      mutate(`School Name` = 'Total',
             `Update #` = 'All') %>% 
      select(`School Name`, all_of(Grade))
    
    setnames(tracker_total, 'School Name', 'Grade')
    tracker_total[1,1] <- "# of Students Remaining"
    tracker_total <- tracker_total[,1:18] 
    
    ifelse(nrow(x$tracker_df_changes) == 0, tracker_total[,1:18] <- 0, tracker_total <- tracker_total)
    
    output$tracker <- renderDT({
      tracker_total %>% 
        datatable(filter = 'none', selection = 'none', rownames=FALSE,
                  options = list(pageLength = 17, paging = FALSE, searching = FALSE, sort = FALSE, scrollX = TRUE)) #ensures all rows are visible, removes paging and search box from datatable output
    })
  }
  
  trigger_tracker_2 <- function() {
    
    t <- x$df1
    t1 <- x$df2
    t2 <- x$df3
    t3 <- x$df4
    t4 <- x$df5
    t5 <- x$df6
    t6 <- x$df7
    t7 <- x$df8
    t8 <- x$df9
    t9 <- x$df10
    t10 <- x$df11
    
    #Create List of selected schools
    sel_sch <- c(input$subSchool, input$iSch1, input$iSch2,input$iSch3,input$iSch4,input$iSch5,input$iSch6,input$iSch7,input$iSch8,input$iSch19,input$iSch10)
    
    x$tracker_df_changes <- rbind(t, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) 
    
    x$tracker_df_changes[,8] <- x$tracker_df_changes[,7] - x$tracker_df_changes[,6]
    
    x$tracker_df_allCh <- x$tracker_df_changes
    
    #Remove unresolved leftover students from tracker
    holdover_entries <- rename(count(x$tracker_df_allCh, School.Name, Grade, me_delta), Freq = n)
    holdover_entries <- holdover_entries[!duplicated(holdover_entries[, c("School.Name", "Grade", "me_delta")]),]	
    holdover_entries <- holdover_entries[holdover_entries$Freq == 11,1:2]
    holdover_entries <- paste(holdover_entries$School.Name, holdover_entries$Grade, sep = "_")
    
    #Check if changes made
    ifelse(nrow(x$tracker_df_changes) == 0, x$tracker_df_allCh[1,] <- 0, x$tracker_df_allCh <- x$tracker_df_allCh)
    ifelse(nrow(x$tracker_df_allCh) == 0, x$tracker_df_allCh[1,] <- 0, x$tracker_df_allCh <-x$tracker_df_allCh)
    
    #Append to existing tracker
    x$tracker_df_allCh$counter <- counter$counterValue
    x$tracker_df_all <- rbind(x$tracker_df_allCh)
    
    #Create Old + New Changes Data Frame
    old_changes <- x$tracker_df_all %>% 
      select(School.Name, Grade, me_delta) %>% 
      group_by_all() %>%
      filter(n() == 10) %>%
      ungroup() %>% 
      mutate(Grade = as.character(Grade)) %>%
      arrange(desc(School.Name))
    
    new_changes <- x$tracker_df_all %>% 
      select(School.Name, Grade, me_delta, counter) %>% 
      group_by_all() %>%
      filter(n() == 1) %>%
      ungroup() %>% 
      mutate(Grade = as.character(Grade)) %>%
      arrange(desc(School.Name))
    
    #Remove Old Values from New Changes
    new_changes <- new_changes[!duplicated(new_changes[, c("School.Name", "Grade")]),]	
    new_changes <- new_changes[new_changes$School.Name %in% sel_sch,]
    
    # #Zero-out unaffected rows
    # new_changes[!(new_changes$School.Name %in% old_changes$School.Name & new_changes$Grade %in% old_changes$Grade), "me_delta"] <- 0
    
    #Update New Changes based off Enr from Prior Submits
    s2_delta <- ifelse(nrow(old_changes) > 0, new_changes[new_changes$School.Name %in% old_changes$School.Name & new_changes$Grade %in% old_changes$Grade, "me_delta"], 0) 
    s1_delta <- ifelse(nrow(old_changes) > 0, old_changes[!duplicated(old_changes[, c("School.Name", "Grade")]),"me_delta"], 0) 
    new_changes[new_changes$School.Name %in% old_changes$School.Name & new_changes$Grade %in% old_changes$Grade, "me_delta"] <- s2_delta[[1]] - s1_delta[[1]]  
    
    #Reset Tracker
    x$tracker_df_allCh <- x$tracker_df_allCh[0,]
    
    #Create Tracker
    net_scenario_change_table <- expand.grid(
      School.Name = new_changes$School.Name,
      Grade = Grade,
      stringsAsFactors = FALSE
    ) %>% 
      unique() %>% 
      left_join(new_changes, by = c('School.Name', 'Grade')) %>% 
      mutate(me_delta = coalesce(me_delta, 0)) %>% 
      group_by(School.Name) %>% 
      ungroup() %>% 
      spread(Grade, me_delta, fill = 0) %>% 
      gather(grade, students, -c(School.Name)) %>% 
      group_by(School.Name, grade) %>% 
      summarise(students = sum(students)) %>%
      ungroup() %>% 
      spread(grade, students) %>%
      select(`School Name` = School.Name, all_of(Grade))
    
    # net_scen_table_sorted <- net_scenario_change_table
    # net_scen_table_sorted <- net_scen_table_sorted %>% group_by(`School Name`) %>% filter(n() > 1)
    # net_scen_table_sorted$PK[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$PK[tail(which(net_scen_table_sorted$PK !=0),1)]) == 0, 0, net_scen_table_sorted$PK[tail(which(net_scen_table_sorted$PK !=0),1)])
    # net_scen_table_sorted$K[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$K[tail(which(net_scen_table_sorted$K !=0),1)]) == 0, 0, net_scen_table_sorted$K[tail(which(net_scen_table_sorted$K !=0),1)])
    # net_scen_table_sorted$`1`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`1`[tail(which(net_scen_table_sorted$`1` !=0),1)]) == 0, 0, net_scen_table_sorted$`1`[tail(which(net_scen_table_sorted$`1` !=0),1)])
    # net_scen_table_sorted$`2`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`2`[tail(which(net_scen_table_sorted$`2` !=0),1)]) == 0, 0, net_scen_table_sorted$`2`[tail(which(net_scen_table_sorted$`2` !=0),1)])
    # net_scen_table_sorted$`3`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`3`[tail(which(net_scen_table_sorted$`3` !=0),1)]) == 0, 0, net_scen_table_sorted$`3`[tail(which(net_scen_table_sorted$`3` !=0),1)])
    # net_scen_table_sorted$`4`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`4`[tail(which(net_scen_table_sorted$`4` !=0),1)]) == 0, 0, net_scen_table_sorted$`4`[tail(which(net_scen_table_sorted$`4` !=0),1)])
    # net_scen_table_sorted$`5`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`5`[tail(which(net_scen_table_sorted$`5` !=0),1)]) == 0, 0, net_scen_table_sorted$`5`[tail(which(net_scen_table_sorted$`5` !=0),1)])
    # net_scen_table_sorted$`6`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`6`[tail(which(net_scen_table_sorted$`6` !=0),1)]) == 0, 0, net_scen_table_sorted$`6`[tail(which(net_scen_table_sorted$`6` !=0),1)])
    # net_scen_table_sorted$`7`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`7`[tail(which(net_scen_table_sorted$`7` !=0),1)]) == 0, 0, net_scen_table_sorted$`7`[tail(which(net_scen_table_sorted$`7` !=0),1)])
    # net_scen_table_sorted$`8`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`8`[tail(which(net_scen_table_sorted$`8` !=0),1)]) == 0, 0, net_scen_table_sorted$`8`[tail(which(net_scen_table_sorted$`8` !=0),1)])
    # net_scen_table_sorted$`9`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`9`[tail(which(net_scen_table_sorted$`9` !=0),1)]) == 0, 0, net_scen_table_sorted$`9`[tail(which(net_scen_table_sorted$`9` !=0),1)])
    # net_scen_table_sorted$`10`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`10`[tail(which(net_scen_table_sorted$`10` !=0),1)]) == 0, 0, net_scen_table_sorted$`10`[tail(which(net_scen_table_sorted$`10` !=0),1)])
    # net_scen_table_sorted$`11`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`11`[tail(which(net_scen_table_sorted$`11` !=0),1)]) == 0, 0, net_scen_table_sorted$`11`[tail(which(net_scen_table_sorted$`11` !=0),1)])
    # net_scen_table_sorted$`12`[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$`12`[tail(which(net_scen_table_sorted$`12` !=0),1)]) == 0, 0, net_scen_table_sorted$`12`[tail(which(net_scen_table_sorted$`12` !=0),1)])
    # net_scen_table_sorted$Alt_Ed[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$Alt_Ed[tail(which(net_scen_table_sorted$Alt_Ed !=0),1)]) == 0, 0, net_scen_table_sorted$Alt_Ed[tail(which(net_scen_table_sorted$Alt_Ed !=0),1)])
    # net_scen_table_sorted$Sp_Ed[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$Sp_Ed[tail(which(net_scen_table_sorted$Sp_Ed !=0),1)]) == 0, 0, net_scen_table_sorted$Sp_Ed[tail(which(net_scen_table_sorted$Sp_Ed !=0),1)])
    # net_scen_table_sorted$ELL[net_scen_table_sorted$`Update #` == 1] <- ifelse(length(net_scen_table_sorted$ELL[tail(which(net_scen_table_sorted$ELL !=0),1)]) == 0, 0, net_scen_table_sorted$ELL[tail(which(net_scen_table_sorted$ELL !=0),1)])
    # names(net_scen_table_sorted[,2:18]) <- Grade
    # net_scen_table_sorted_2 <-  net_scen_table_sorted[!duplicated(net_scen_table_sorted$`School Name`),]
    # net_scen_table_sorted_2 <- net_scen_table_sorted_2[is.na(net_scen_table_sorted_2$PK) == FALSE,]
    x_net_scenario_change_table <- net_scenario_change_table
    # x_net_scenario_change_table[match(net_scen_table_sorted_2$`School Name`,  x_net_scenario_change_table$`School Name`),] <- net_scen_table_sorted_2
    colnames(x_net_scenario_change_table)[1] <- "School"
    
    tracker_total <- x_net_scenario_change_table %>% 
      distinct(School, .keep_all = TRUE) %>%
      select(all_of(Grade)) %>% 
      colSums(na.rm = TRUE) %>% 
      as.data.frame() %>% 
      t() %>% 
      as_tibble() %>% 
      mutate(`School Name` = 'Total',
             `Update #` = 'All') %>% 
      select(`School Name`, all_of(Grade), `Update #`)
    
    setnames(tracker_total, 'School Name', 'Grade')
    tracker_total[1,1] <- "# of Students Remaining"
    tracker_total <- tracker_total[,1:18] 
    
    ifelse(nrow(x$tracker_df_changes) == 0, tracker_total[,1:18] <- 0, tracker_total <- tracker_total)
    
    output$tracker <- renderDT({
      tracker_total %>% 
        datatable(filter = 'none', selection = 'none', rownames=FALSE,
                  options = list(pageLength = 17, paging = FALSE, searching = FALSE, sort = FALSE, scrollX = TRUE)) #ensures all rows are visible, removes paging and search box from datatable output
    })
  }
  
  trigger_reset <- function() {
    schLvl$df <- slDf
    
    x$df1 <- enrInputMstr
    x$df2 <- enrInputMstr
    x$df3 <- enrInputMstr
    x$df4 <- enrInputMstr
    x$df5 <- enrInputMstr
    x$df6 <- enrInputMstr
    x$df7 <- enrInputMstr
    x$df8 <- enrInputMstr
    x$df9 <- enrInputMstr
    x$df10 <- enrInputMstr
    x$df11 <- enrInputMstr
    x$df_changes = NULL
    x$df_changes <- tibble()
    x$df_all = NULL 
    x$df_all <- tibble()
    x$df_allCh = NULL
    x$df_allCh <- tibble()
    
    
    rm("attrition")
    x$net_scenario_change_table <- data.frame(NA)
    
    updateSelectInput(session, "subSchool", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch1", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch2", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch3", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch4", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch5", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch6", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch7", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch8", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch9", selected = "Anna Brochhausen School 88")
    updateSelectInput(session, "iSch10c", selected = "Anna Brochhausen School 88")
    
    output$xy <- renderDT({
      
      totals <- x$net_scenario_change_table %>% 
        select(all_of(Grade)) %>% 
        colSums(na.rm = TRUE) %>% 
        as.data.frame() %>% 
        t() %>% 
        as_tibble() %>% 
        mutate(`School Name` = 'Total',
               `Update #` = 'All') %>% 
        select(`School Name`, all_of(Grade), `Update #`) %>% 
        mutate_at(vars(all_of(Grade)), function(x) x - x)
      
      x$net_scenario_change_table %>% 
        mutate_at(vars(all_of(Grade)), function(x) x - x) %>% 
        rbind(totals) %>%
        datatable(filter = 'none', selection = 'none', rownames=FALSE,
                  options = list(pageLength = 17, paging = FALSE, searching = FALSE, sort = FALSE, scrollX = TRUE)) #ensures all rows are visible, removes paging and search box from datatable output
    })
  }
  
  users <- tibble(
    username = 'user',
    password = 'ips123'
  )
  
  credentials <- callModule(shinyauthr::login, 'login', 
                            data = users,
                            user_col = username,
                            pwd_col = password,
                            log_out = reactive(logout_init()))
  
  logout_init <- callModule(shinyauthr::logout, 'logout', reactive(credentials()$user_auth))
  
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = 'body')
    } else {
      shinyjs::addClass(selector = 'body')
    }
  })
  
  output$app_ui <- renderUI({
    req(credentials()$user_auth)
    
    #Start of UI---- 
    
    tabsetPanel(
      
      id="display_tab",
      
      tabPanel("School Inputs", 
               fluidRow(
                 box(
                   title = "Instructions, Scenario Title, & Description", status = "success", solidHeader = TRUE, collapsible = TRUE, 
                   tags$b("Instructions:"),
                   p("Update your scenario by selecting a subject school to move students from. Select one or more impact schools to 
                         move students to. Update enrollment at the subject and impact schools to reflect the portfolio changes you wish to
                         make and click submit to apply them to your scenario."),
                   p("You can apply as many changes as you wish to your scenario though changes must be made one subject school at a time."),
                   p("Add title and description of your scenario below to record and include them in the export of data and maps you export"), 
                   textInput("scenario_title", "Enter a title for your scenario:"),
                   textAreaInput("scenario_description", "Enter a description for your scenario:"),
                   div(style = 'display:inline-block',
                       div(style = 'display:inline-block', actionButton('load', 'Load scenario', icon = icon('upload'))),
                       div(style = 'display:inline-block', withBusyIndicatorUI(actionButton('save', 'Save Scenario', icon = icon('save'))))
                   ),
                   bsModal('scenario_load_modal', 'Load scenario', 'load', size = 'large',
                           uiOutput('scenario_load_ui')
                   )
                 ),
                 
                 box(
                   title = "Subject School", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                   selectInput("subSchool", "Select a Subject School", unique(isolate(x$inputSchList)), selected = "Anna Brochhausen School 88"),
                   column(width = 4,
                          h5("Subgroups 2020-21"),
                          tableOutput("sub_SGTbl"),
                          br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                          p("Clicking the Close Subject School button zeros out enrollment and reduces the overall costs associated 
                                with the subject school facility", align = "bottom" )),
                   column(width = 8,
                          DTOutput('x0')),
                   # downloadButton('download','Download'),
                   # actionButton("submitTest","Submit"),
                   actionButton("closeSchool","Close School")),
                 
               ),
               
               fluidRow(
                 box(
                   title = "Net Scenario Change", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                   DTOutput('xy')),
                 
                 box(
                   title = "Scenario Attrition", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                   DTOutput('xz')),
               ),
               
               fluidRow( 
                 
                 box(
                   title = "Reset", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                   p("Click to reset changes"),
                   actionButton("reset","Reset"),
                   p("(Click here to returns all enrollment values to the default state)")),
                 
                 box(
                   title = "Submit", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                   p("Click here to add all changes made to your scenario"),
                   actionButton("submit","Submit"),
                   p("(Double check that all entries are accurate)")),
               ),
               
               
               fluidRow( 
                 
                 box(
                   title = "Subject School Students Remaining", status = "danger", solidHeader = TRUE, collapsible = TRUE, width =,
                   DTOutput('tracker')),
                 box(
                   title = "Potential Impact Schools", status = "danger", solidHeader = TRUE, collapsible = TRUE, 
                   DTOutput('impact')),
                 
               ),
               
               fluidRow(
                 map(2:11, function(num) {
                   box(
                     title = str_glue("Impact School #{num}"), status = "info", solidHeader = TRUE, collapsible = TRUE,
                     selectInput(str_glue("iSch{num}"), "Select an Impact School", unique(isolate(sort(x$inputSchList))), selected = "Anna Brochhausen School 88"),
                     column(width = 4,
                            h5("Subgroups 2020-21"),
                            tableOutput(str_glue("sub_SGTbl{num}"))),
                     column(width = 8,
                            DTOutput(str_glue('x{num}'))))
                 })
               ),
               
      ),
      
      ## New School Tab--------
      # fieldsAll <- c("School.Name.SL", "IPS.ID.SL","School.Level", "Access.Type", "Capacity","Longitude","Latitude", 
      #                "Classrooms", "Custodial.Allocation", "Sum.Utilities", "amt_outstanding_last", "debt_holder", "Condition",
      #                "Bldg.Weighted.Condition.Score", "Bldg.Academic.Readiness.Score", "Bldg.Technology.Score", "Grounds.Score",
      #                "Combined.Score", "High.Needs", "High.Needs.and.SQR", "School.Performance", "School.Accountability")
      # 
      tabPanel(
        "Open New School",
        box(
          title = "Open a New School", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          br(),
          column(width = 12, align ="center",
                 p("To create a new school in your scenario, fill out as many of the school fields as possible."),
                 p("Required fields are marked with an *."),
                 br(),
                 tags$b("Data Specifications:"),
                 p("Direct Cert has to be a number between 0 and 100"),
                 p("The Facility Condition Scores have to be numbers between 0 and 100"),
                 p("The School Accountability Metric has to be a number between 0 and 10"),
                 br(),
                 tags$b("High and Low Quality Facilities:"),
                 p("Facilities are considered High Quality when their Combined Facilities Score is above 80%"),
                 p("Facilities are considered Low Quality when their Combined Facilities Score is below 60%"),
                 br(),
                 tags$b("Finding Latitude and Longitude:"),
                 p("Latitude and Longitude can be found using ", a(href ='https://tinyurl.com/y56rhfrf', "this tool.", target="_blank")),
                 br(),
                 p("When you have finished adding data, click the button below to add your new school to your scenario portfolio."),
                 br(),
                 withBusyIndicatorUI(actionButton('submit_sch', 'Submit', class = "btn-primary")),
                 shinyjs::hidden(
                   div(
                     id = "thankyou_msg",
                     actionLink("submit_another", "Submit a New Response")
                   )
                 ),
                 br(),br(),br(),
                 p("After adding your new school to your scenario portfolio, you can return to the School Inputs tab to move students from an existing IPS school to your new school."),
          )
        ),
        box(
          title = "New School Data", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          div(
            id = "form",
            
            textInput("School.Name.SL", "School Name*", ""),
            selectInput("School.Level", "School Level*",
                        c("", "ES", "MS", "K-8","HS","Other")),
            numericInput("Capacity", "Capacity*",""),
            selectInput("Access.Type", "Access Type*",
                        c("",  "Choice", "Neighborhood")),
            numericInput("Latitude", "Latitude*", ""),
            numericInput("Longitude", "Longitude*", ""),
            numericInput("Classrooms", "Classrooms", ""),
            numericInput("Direct.Cert", "Direct Cert %", ""),
            checkboxInput("new_school", "Will this school operate a facility new to the IPS portfolio? (If so, a Combined Facility Score is required.)", FALSE),
            numericInput("Custodial.Allocation", "# of Custodians", ""),
            numericInput("Sum.Utilities", "Sum of Utilities Cost", ""),
            numericInput("amt_outstanding_last", "Outstanding Debt", ""),
            selectInput("debt_holder", "Debt Owner",
                        c("","IPS", "MSBC", "Split", "Other"),
            ),
            numericInput("Bldg.Weighted.Condition.Score", "Facility Weighted Condition Score", ""),
            numericInput("Bldg.Academic.Readiness.Score", "Facility Academic Readiness Score", ""),
            numericInput("Bldg.Technology.Score", "Facility Technology Score", ""),
            numericInput("Grounds.Score", "Facility Grounds Score", ""),
            numericInput("Combined.Score", "Combined Facility Score", ""),
            selectInput("High.Needs", "High Needs?",
                        c("","Yes","No","Other")),
            selectInput("High.Needs.and.SQR", "High Needs and SQR?",
                        c("","Yes","No","Other")),
            selectInput("School.Performance", "School Performance",
                        c("Other","A","B","C","D","F","Other")),
            numericInput("School.Accountability.Metric", "School Accountability Metric", 0, max = 10),
            selectInput("School.Accountability", "School Accountability",
                        c("","Highly Effective","Effective","Improvement Necessary","Ineffective","Other")),
          )
        )
      ),
      
      ## Enrollment Zones UI--------
      tabPanel("Enrollment Zones",
               box(
                 title = "School Zone Selection", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                 width = 12,
                 fluidRow(
                   column(
                     width = 6, 
                     tags$b("Enrollment Zone Configuration"),
                     br(),
                     p("The Enrollment Zones tab allows you to evaluate groups of schools from your scenario by assigning zones to each school. 
                       Below you can drag and drop from the full list of schools into any of the eight buckets below to assign their zones. 
                       Once you have finished assigning your zones, you can submit these assignments by clicking the ‘Submit Zones’ button to the right. 
                       This will calculate the zone specific outputs in the tables and map below."),
                     p("Please configure your scenario before assigning enrollment zones.")
                   ),
                   column(width = 3, align ="center"),
                   column(width = 3, align ="right",
                          div(style="display:inline-block;width:90%;text-align: center;", actionButton("zone_submit","Submit Zones", width = "250px"),
                              p("After you have completed your zone assignments, click here to view outputs."),
                              br(),
                              p("To save these zone assignments to your scenario, return to the School Inputs tab and click ‘Save Scenario.’")),
                   ),
                 ),
                 uiOutput('bucketlist_ui')
               ),
               box(
                 title = "School Characteristics by Zone", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                 DTOutput("zone_sch_char")
               ),
               box(
                 title = "Grade Level Enrollment by Zone", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                 DTOutput("zone_gr_lvl_enr")
               ),
               box(
                 title = "Subgroup Percentages by Zone", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                 DTOutput("zone_subgroups")
               ),
               box(
                 title = "Funding Summary by Zone", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                 DTOutput("zone_fin")
               ),
               box(
                 title = "Zone Schools Map", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
                 leafletOutput("Zmap")),
      ),
      
      tabPanel("School Level Outputs",
               box(
                 title = "School Level Summary", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                 DTOutput("sch_lvl_outputs"),
                 downloadButton('download_Data','Download Scenario Data')
               )
      ),
      
      tabPanel("District Level Outputs",
               box(
                 title = "School Counts by Access Types", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                 DTOutput("sATC"),
               ),
               box(
                 title = "Average School Characteristics by School Level", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                 DTOutput("avg_school_char"),
               ),
               box(
                 title = "Schools & Enrollment by Grade", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                 column(width = 4,
                        selectInput("sch_enr_gr", "Select to view School or Enrollment Counts", c("School","General Enrollment"), selected = "General Enrollment")),
                 column(width = 4,
                        selectInput("subgroup_by_grade", "Select a Subgroup", c("All","Asian", "Black", "Latinx","Multiethnic","White"))),
                 DTOutput("school_enr_gr"),
               ),
               box(
                 title = "Subgroups by Facilities Condition", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                 DTOutput("subgroups")
               ),
               box(
                 title = "Facilities Summary", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                 DTOutput("utl_sum")
               ),
               box(
                 title = "School Allocation Summary", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                 DTOutput("z1"),
                 DTOutput("v1"),
                 DTOutput("cost_ps"),
                 downloadButton('download_Data1','Download Scenario Data')
               ),
               
      ),
      
      tabPanel("Map",
               fluidRow(
                 column(3,
                        selectInput("scenarioType", "Default or Scenario", list("Default","Scenario"))),
                 column(3,
                        selectInput("accessType", "Access Type", list("All","Choice","Neighborhood","TBD"), selected = "All")),
                 
               ),
               
               checkboxGroupInput("schoolLevel", "School Level",list("ES","MS","K-8","HS"),selected = c(unique(mDf$School.Level)),inline = TRUE),
               
               tags$head(
                 tags$script(src = jsfile),
                 ## All CSS
                 tags$style(type="text/css",
                            "#map{
                            height: calc(90vh - 100px) !important;
                            }")),
               
               leafletOutput("map"))
      #leafletOutput("map")
      
    )
  })
  
  #Access Type School Counts: Default---------------------------------------------------------------------------
  
  output$sATC <- renderDT({
    if (counter$counterValue == 0) {
      accTCntD
    }
    else if (counter$counterValue > 0){
      schLvl$accTCntS
    }
  },
  colnames = c("Default or Scenario", "All", "Choice", "Neighborhood", "TBD"),
  filter = "none",
  selection = 'none',
  options = list(paginate = FALSE, searching = FALSE, info = FALSE), rownames = FALSE)
  
  #Enter Basic Periphery Data Tables------------------------------------------------------------------------------
  
  #district_level_facilities_summary_table_currency_rows <- school_level_summary_table_names[str_detect(school_level_summary_table_names, 'Utilization')]
  
  output$utl_sum <- renderDT(utl_sum,
                             colnames = c("Facilities Metrics", "Default", "Scenario", "Delta"), 
                             filter = "none",
                             selection = 'none',
                             options = list( paging = FALSE, searching = FALSE), rownames = FALSE)
  
  output$z1 <- renderDT(sch_count,
                        colnames = c("School Counts", "Default", "Scenario", "Delta"), 
                        filter = "none",
                        selection = 'none',
                        options = list( paging = FALSE, searching = FALSE), rownames = FALSE)
  
  output$v1 <- renderDT(sch_cost, 
                        colnames = c("School Costs", "Default", "Scenario", "Delta"), 
                        filter = "none",
                        selection = 'none',
                        options = list( paging = FALSE, searching = FALSE), rownames = FALSE)
  
  output$cost_ps <- renderDT(cost_ps,
                             colnames = c("Per Student Costs", "Default", "Scenario", "Delta"),
                             filter = "none",
                             selection = 'none',
                             options = list( paging = FALSE, searching = FALSE), rownames = FALSE)
  
  output$avg_school_char <- renderDT({
    og_names <- names(avg_school_char)
    
    avg_school_char %>% 
      mutate(order = row_number()) %>% 
      gather(metric, value, -c(Metrics, order)) %>% 
      mutate(value = case_when(Metrics == 'Avg. Utilization' ~ scales::percent(value),
                               TRUE ~ as.character(prettyNum(round(value), big.mark = ',')))) %>% 
      spread(metric, value) %>% 
      arrange(order) %>% 
      select(all_of(og_names)) %>% 
      datatable(options = list(sort = FALSE, info = FALSE, searching = FALSE, paginate = FALSE), rownames = FALSE)
  })
  
  output$school_enr_gr <- renderDT({
    if (input$subgroup_by_grade == "Asian"){
      if (input$sch_enr_gr == "General Enrollment") {
        school_enr_gr_Asian
      }
      else if (input$sch_enr_gr == "School"){
        school_enr_gr_Asian_2
      }
    }
    else if (input$subgroup_by_grade == "Black"){
      if (input$sch_enr_gr == "General Enrollment") {
        school_enr_gr_Black
      }
      else if (input$sch_enr_gr == "School"){
        school_enr_gr_Black_2
      }
    }
    else if (input$subgroup_by_grade == "Latinx"){
      if (input$sch_enr_gr == "General Enrollment") {
        school_enr_gr_Latinx
      }
      else if (input$sch_enr_gr == "School"){
        school_enr_gr_Latinx_2
      }
    }
    else if (input$subgroup_by_grade == "White"){
      if (input$sch_enr_gr == "General Enrollment") {
        school_enr_gr_White
      }
      else if (input$sch_enr_gr == "School"){
        school_enr_gr_White_2
      }
    } 
    else if (input$subgroup_by_grade == "Multiethnic"){
      if (input$sch_enr_gr == "General Enrollment") {
        school_enr_gr_Multiethnic
      }
      else if (input$sch_enr_gr == "School"){
        school_enr_gr_Multiethnic_2
      }
    } 
    else if (input$subgroup_by_grade == "All"){
      if (input$sch_enr_gr == "General Enrollment") {
        school_enr_gr
      }
      else if (input$sch_enr_gr == "School"){
        school_enr_gr_2
      }
    }
  },
  filter = "none",
  selection = 'none',
  options = list(paginate = FALSE, searching = FALSE, info = FALSE), rownames = FALSE)
  
  output$subgroups <- renderDT({
    datatable(subgroups, options = list(paginate = FALSE, searching = FALSE, info = FALSE), rownames = FALSE) %>% 
      formatPercentage(names(subgroups)[!names(subgroups) == 'Demographic'])
  })
  
  #New School Form Valdiation---------------------------------------------
  
  #New School Submit Button---------------------------------------------
  
  ### New School Submit Rules
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
             },
             logical(1))
    
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit_sch", condition = mandatoryFilled)
  })
  
  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })    
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_sch, {
    
    withBusyIndicatorServer('submit_sch', {
      
      # User-experience stuff
      shinyjs::disable("submit_sch")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Save the data (show an error message in case of error)
      tryCatch({
        ### Commented Out Save Data - Unneeded 
        # saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit_sch")
        shinyjs::hide("submit_msg")
      })
      
      #Data Validation
      
      #Validate Inputs and Show Error Messages
      validate(
        need(ifelse(is.na(between(input$Direct.Cert,0,100)) == TRUE, TRUE, between(input$Direct.Cert,0,100)), 'Please ensure Direct Cert % is between 0 and 100. Please press "Submit a New Response" to reset the form.' ),
        need(ifelse(is.na(between(input$Bldg.Weighted.Condition.Score,0,100)) == TRUE, TRUE, between(input$Bldg.Weighted.Condition.Score,0,100)), 'Please select a number between 0 and 100. Please press "Submit a New Response" to reset the form.'),
        need(ifelse(is.na(between(input$Bldg.Academic.Readiness.Score,0,100)) == TRUE, TRUE, between(input$Bldg.Weighted.Condition.Score,0,100)), 'Please select a number between 0 and 100. Please press "Submit a New Response" to reset the form.'),
        need(ifelse(is.na(between(input$Bldg.Technology.Score,0,100)) == TRUE, TRUE, between(input$Bldg.Technology.Score,0,100)), 'Please select a number between 0 and 100. Please press "Submit a New Response" to reset the form.'),
        need(ifelse(is.na(between(input$Grounds.Score,0,100)) == TRUE, TRUE, between(input$Grounds.Score,0,100)), 'Please select a number between 0 and 100. Please press "Submit a New Response" to reset the form.'),
        need(ifelse(is.na(between(input$Combined.Score,0,100)) == TRUE, TRUE, between(input$Combined.Score,0,100)), 'Please select a number between 0 and 100. Please press "Submit a New Response" to reset the form.'),
        need(ifelse(is.na(between(input$School.Accountability.Metric, 0, 10)) == TRUE, TRUE, between(input$School.Accountability.Metric, 0, 10)), 'Please select a number between 0 and 10. Please press "Submit a New Response" to reset the form.')
        
      )
      
      new_sch <- as.data.frame(formData())
      
      #Assign New Name to Variable
      new_sch_name <- as.character(new_sch[1,1])
      
      #Append to InputSchList
      x$inputSchList <- append(x$inputSchList, new_sch_name)
      x$inputSchList <- sort( x$inputSchList)
      
      #Subsets new_sch dataframe into x$mDf columns, updating default values where necessary
      new_sch_mDf <- new_sch
      new_sch_mDf$enrollment <- 0
      new_sch_mDf$Utilization <- 0
      if (is.blank(new_sch_mDf$School.Performance)){new_sch_mDf$School.Performance <- 'Not Available'}
      ifelse(is.na(new_sch_mDf$Direct.Cert),new_sch_mDf$Direct.Cert <- 0, new_sch_mDf$Direct.Cert <- as.numeric(new_sch_mDf$Direct.Cert)/100)
      new_sch_mDf$Access.Type <- as.character(new_sch_mDf$Access.Type)
      new_sch_mDf <- subset(new_sch_mDf, select=c(School.Name.SL, School.Performance, School.Level, Access.Type, Latitude, Longitude, enrollment, Utilization, Direct.Cert, Combined.Score))
      
      #Adds new_sch row to reactive mDf & reformats numeric columns
      x$mDf <- rbind(x$mDf, new_sch_mDf)
      
      x$mDf[6:10] <- lapply(x$mDf[6:10], as.numeric)
      x$mDf$School.Name.SL <- unlist(x$mDf$School.Name.SL, use.names = FALSE)
      x$mDf$School.Level <- unlist(x$mDf$School.Level, use.names = FALSE)
      x$mDf$Latitude <- unlist(x$mDf$Latitude, use.names = FALSE)
      
      #Run Scenario to Produce $df files
      trigger_scenario()
      
      #Make blank x$df 
      xdf_PH <- x$df1[FALSE,]
      
      #Add Blank rows for each grade
      xdf_PH[1,] <- list(1,new_sch_name, "PK", 0,0,0,0,0)
      xdf_PH[2,] <- list(2,new_sch_name, "K", 0,0,0,0,0)
      xdf_PH[3,] <- list(3,new_sch_name, "1", 0,0,0,0,0)
      xdf_PH[4,] <- list(4,new_sch_name, "2", 0,0,0,0,0)
      xdf_PH[5,] <- list(5,new_sch_name, "3", 0,0,0,0,0)
      xdf_PH[6,] <- list(6,new_sch_name, "4", 0,0,0,0,0)
      xdf_PH[7,] <- list(7,new_sch_name, "5", 0,0,0,0,0)
      xdf_PH[8,] <- list(8,new_sch_name, "6", 0,0,0,0,0)
      xdf_PH[9,] <- list(9,new_sch_name, "7", 0,0,0,0,0)
      xdf_PH[10,] <- list(10,new_sch_name, "8", 0,0,0,0,0)
      xdf_PH[11,] <- list(11,new_sch_name, "9", 0,0,0,0,0)
      xdf_PH[12,] <- list(12,new_sch_name, "10", 0,0,0,0,0)
      xdf_PH[13,] <- list(13,new_sch_name, "11", 0,0,0,0,0)
      xdf_PH[14,] <- list(14,new_sch_name, "12", 0,0,0,0,0)
      xdf_PH[15,] <- list(15,new_sch_name, "Alt_Ed", 0,0,0,0,0)
      xdf_PH[16,] <- list(16,new_sch_name, "ELL", 0,0,0,0,0)
      xdf_PH[17,] <- list(17,new_sch_name, "Sp_Ed", 0,0,0,0,0)
      
      #Append to x$df
      
      x$df1 <- bind_rows(x$df1, xdf_PH)
      
      map(1:11, function(n){
        x[[str_glue('df{n}')]] <- bind_rows(x[[str_glue('df{n}')]], xdf_PH)
        x[[str_glue('df{n}')]][!duplicated( x[[str_glue('df{n}')]][,1:ncol( x[[str_glue('df{n}')]])]),]
      })
      
      x$df1[!duplicated(x$df1[,1:ncol(x$df1)]),]
      
      #Repeat with schlvl$df
      schlvldf_PH <- schLvl$df[FALSE,]
      
      #Fill out row with Formdata
      schlvldf_PH[1,"School.Name.SL"] <- as.character(new_sch[1,"School.Name.SL"])
      schlvldf_PH[1,"IPS.ID.SL"] <- "TBD"
      #schlvldf_PH[1,"Condition"] <- "TBD"
      schlvldf_PH[1,"School.Level"] <- as.character(new_sch[1,"School.Level"])
      schlvldf_PH[1,"Access.Type"] <- as.character(new_sch[1,"Access.Type"])
      schlvldf_PH[1,"Capacity"] <- as.numeric(as.character(new_sch[1,"Capacity"]))
      schlvldf_PH[1,"Longitude"] <- as.numeric(as.character(new_sch[1,"Longitude"]))
      schlvldf_PH[1,"Latitude"] <- as.numeric(as.character(new_sch[1,"Latitude"]))
      schlvldf_PH[1,"Classrooms"] <- as.numeric(as.character(new_sch[1,"Classrooms"]))
      schlvldf_PH[1,"Custodial.Allocation"] <- as.numeric(as.character(new_sch[1,"Custodial.Allocation"]))
      schlvldf_PH[1,"Sum.Utilities"] <- as.numeric(as.character(new_sch[1,"Sum.Utilities"]))
      schlvldf_PH[1,"amt_outstanding_last"] <- as.numeric(as.character(new_sch[1,"amt_outstanding_last"]))
      schlvldf_PH[1,"debt_holder"] <- as.character(new_sch[1,"debt_holder"])
      schlvldf_PH[1,"Bldg.Weighted.Condition.Score"] <- as.numeric(as.character(new_sch[1,"Bldg.Weighted.Condition.Score"]))
      schlvldf_PH[1,"Bldg.Academic.Readiness.Score"] <- as.numeric(as.character(new_sch[1,"Bldg.Academic.Readiness.Score"]))
      schlvldf_PH[1,"Bldg.Technology.Score"] <- as.numeric(as.character(new_sch[1,"Bldg.Technology.Score"]))
      schlvldf_PH[1,"Grounds.Score"] <- as.numeric(as.character(new_sch[1,"Grounds.Score"]))
      schlvldf_PH[1,"Combined.Score"] <- as.numeric(as.character(new_sch[1,"Combined.Score"]))
      schlvldf_PH[1,"High.Needs"] <- as.character(new_sch[1,"High.Needs"])
      schlvldf_PH[1,"High.Needs.and.SQR"] <- as.character(new_sch[1,"High.Needs.and.SQR"])
      schlvldf_PH[1,"School.Performance"] <- as.character(new_sch[1,"School.Performance"])
      schlvldf_PH[1,"School.Accountability.Metric"] <- as.numeric(as.character(new_sch[1,"School.Accountability.Metric"]))
      schlvldf_PH[1,"School.Accountability"] <- as.character(new_sch[1,"School.Accountability"])
      schlvldf_PH[1,"Direct.Cert"] <- as.numeric(as.character(new_sch[1,"Direct.Cert"]))/100
      schlvldf_PH[1,"amt_outstanding_last"] <- as.numeric(as.character(new_sch[1,"amt_outstanding_last"]))
      schlvldf_PH[1,"debt_holder"] <- as.character(new_sch[1,"debt_holder"])
      
      #Calculate/Fill-In columns based on form data
      schlvldf_PH[1,"Cost.Per.Custodian"] <- 60169
      schlvldf_PH[1,"Total.Custodial.Cost"] <-  ifelse(is.na(schlvldf_PH[1,"Custodial.Allocation"]) == TRUE,0,schlvldf_PH[1,"Cost.Per.Custodian"] * schlvldf_PH[1,"Custodial.Allocation"])
      
      schlvldf_PH[1,"UPercent"] <- 1
      schlvldf_PH[1, "Total.Utilities.Cost"] <- ifelse(is.na(schlvldf_PH[1,"Sum.Utilities"]) == TRUE,0,schlvldf_PH[1,"UPercent"] * schlvldf_PH[1,"Sum.Utilities"])
      #Allow Total Utilies Cost to be Recalculated by dumping total into Water column
      schlvldf_PH[1, "Water"] <- schlvldf_PH[1, "Total.Utilities.Cost"]
      
      schlvldf_PH[1,"District.School"] <- "1"
      schlvldf_PH[1,"Active"] <- "1"
      
      #Determine if new school should be counted
      
      #Store conditional
      check <- new_sch[1,"new_school"
      ]
      #Check omitted schools list is null
      check_null <- is.null(schLvl$omit_new_sch)
      
      #Append depending on Checks
      schLvl$omit_new_sch <- ifelse(check, ifelse(check_null, "", schLvl$omit_new_sch),  append(schLvl$omit_new_sch,new_sch_name) )
      
      #Append to schLvl$df
      schLvl$df <- bind_rows(schLvl$df, schlvldf_PH)
      
      #Prepare re-run of scenario
      upd_row_num <- nrow(schLvl$sch_lvl_outputs)+1
      
      schLvl$sch_lvl_outputs[upd_row_num,] <- NA
      schLvl$sch_lvl_outputs[upd_row_num,1] <- new_sch_name
      schLvl$sch_lvl_outputs[upd_row_num,2:7] <- 0
      schLvl$sch_lvl_outputs[upd_row_num,8:17] <- c(as.character(new_sch[1,"School.Performance"]), as.numeric(as.character(new_sch[1,"School.Accountability.Metric"])), as.character(new_sch[1,"School.Accountability"]), as.character(new_sch[1,"High.Needs"]),
                                                    as.character(new_sch[1,"High.Needs.and.SQR"]), as.numeric(as.character(new_sch[1,"Bldg.Weighted.Condition.Score"])),
                                                    as.numeric(as.character(new_sch[1,"Bldg.Academic.Readiness.Score"])), as.numeric(as.character(new_sch[1,"Bldg.Technology.Score"])),
                                                    as.numeric(as.character(new_sch[1,"Grounds.Score"])),as.numeric(as.character(new_sch[1,"Combined.Score"])))
      schLvl$sch_lvl_outputs[upd_row_num,18] <- 0
      schLvl$sch_lvl_outputs[upd_row_num,19] <- as.numeric(as.character(new_sch[1,"Capacity"]))
      schLvl$sch_lvl_outputs[upd_row_num,20] <- schLvl$sch_lvl_outputs[upd_row_num,19] - schLvl$sch_lvl_outputs[upd_row_num,18]
      schLvl$sch_lvl_outputs[upd_row_num,21] <- 0
      schLvl$sch_lvl_outputs[upd_row_num,22] <- as.numeric(schLvl$sch_lvl_outputs[upd_row_num,6])/schLvl$sch_lvl_outputs[upd_row_num,19]
      schLvl$sch_lvl_outputs[upd_row_num,23] <- as.numeric(schLvl$sch_lvl_outputs[upd_row_num,22]) - as.numeric(schLvl$sch_lvl_outputs[upd_row_num,21])
      schLvl$sch_lvl_outputs[upd_row_num,24:33] <- 0
      schLvl$sch_lvl_outputs[upd_row_num,34] <-  as.numeric(as.character(new_sch[1,"Sum.Utilities"]))
      schLvl$sch_lvl_outputs[upd_row_num,35] <- 0
      schLvl$sch_lvl_outputs[upd_row_num,36] <-  as.numeric(as.character(new_sch[1,"Custodial.Allocation"]))
      schLvl$sch_lvl_outputs[upd_row_num,37] <- schLvl$sch_lvl_outputs[upd_row_num,36] - schLvl$sch_lvl_outputs[upd_row_num,35]
      schLvl$sch_lvl_outputs[upd_row_num,38] <- 0
      schLvl$sch_lvl_outputs[upd_row_num,39] <-  as.numeric(as.character(new_sch[1,"Custodial.Allocation"])) *60169
      schLvl$sch_lvl_outputs[upd_row_num,40] <- schLvl$sch_lvl_outputs[upd_row_num,39] - schLvl$sch_lvl_outputs[upd_row_num,38]
      schLvl$sch_lvl_outputs[upd_row_num,41:57] <- 0
      schLvl$sch_lvl_outputs[upd_row_num,58] <- as.character(new_sch[1,"School.Level"])
      schLvl$sch_lvl_outputs[upd_row_num,59] <- as.numeric(as.character(new_sch[1,"amt_outstanding_last"]))
      schLvl$sch_lvl_outputs[upd_row_num,60] <- as.character(new_sch[1,"debt_holder"])
      
      #Update Grade Level Subgroups
      grdLvlSubGrp_PH <- schLvl$grdLvlSubGrp[schLvl$grdLvlSubGrp$School.Name == "Anna Brochhausen School 88",]
      grdLvlSubGrp_PH[,"School.Name"] <- new_sch_name 
      grdLvlSubGrp_PH[,2:3] <- 0
      grdLvlSubGrp_PH[,6] <- 0
      
      schLvl$grdLvlSubGrp <- bind_rows(schLvl$grdLvlSubGrp, grdLvlSubGrp_PH)  
      
      
      trigger_scenario()
    })
    
  })
  
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form") 
    shinyjs::hide("thankyou_msg")
  })
  
  #Render Default School Level Outputs Dataframe------------------------------------------------------------------------------
  
  output$sch_lvl_outputs <- renderDT({
    
    # sch_lvl_outputs[!(sch_lvl_outputs$Schools %in% c("McFarland Alternative Center","Phalen Leadership Academy @ Francis Scott Key 103","Impact Academy","KIPP:Indy Schools", "KIPP Indy College Prep Middle School", "KIPP Indy Legacy High School", "KIPP Indy Unite Elementary School", "Phalen Leadership Academy@George H Fisher")),!(names(sch_lvl_outputs) %in% c("School_Level"))] %>% 
    
    
    school_level_summary_table_df <- sch_lvl_outputs  %>% 
      set_names(str_replace_all(names(.), '^Race_', '')) %>% 
      set_names(str_replace_all(names(.), '\\.|_', ' '))
    
    school_level_summary_table_names <- names(school_level_summary_table_df)
    school_level_summary_table_percentage_columns <- school_level_summary_table_names[str_detect(school_level_summary_table_names, 'Utilization|^(Asian|Black|Latinx|White|Multiethnic)|Graduation')]
    school_level_summary_table_numeric_columns <- school_level_summary_table_names[!str_detect(school_level_summary_table_names, 'School Level') &!str_detect(school_level_summary_table_names, 'School Accountability') &!str_detect(school_level_summary_table_names, 'High Needs') & !str_detect(school_level_summary_table_names, 'Owner') & !str_detect(school_level_summary_table_names, 'School Performance') & !str_detect(school_level_summary_table_names, 'Schools') &!str_detect(school_level_summary_table_names, 'Grades') & !school_level_summary_table_names %in% school_level_summary_table_percentage_columns]
    school_level_summary_table_dollar_columns <- school_level_summary_table_names[str_detect(school_level_summary_table_names, 'Utilities Cost Default') | str_detect(school_level_summary_table_names, 'Utilities Cost Scenario') |str_detect(school_level_summary_table_names, 'Utilities Cost Delta') |str_detect(school_level_summary_table_names, 'Custodial Cost Default') | str_detect(school_level_summary_table_names, 'Custodial Cost Scenario') |  str_detect(school_level_summary_table_names, 'Custodial Cost Delta') | str_detect(school_level_summary_table_names, 'Outstanding Debt') | (!str_detect(school_level_summary_table_names, 'School Performance') & !str_detect(school_level_summary_table_names, 'Schools') &str_detect(school_level_summary_table_names, '^(Baseline|SBA)') & !school_level_summary_table_names %in% c(school_level_summary_table_percentage_columns))]
    
    school_level_summary_table_df %>% 
      datatable(extensions = "FixedColumns", options = list(autoWidth = TRUE,
                                                            columnDefs = list(list(targets=c(0,5,20,21,22,54), visible=TRUE, width='250')),
                                                            paging = FALSE, 
                                                            scrollX = TRUE, 
                                                            scrollY = "600px", 
                                                            fixedColumns = list(leftColumns = 1)), 
                rownames = FALSE) %>%  
      formatPercentage(school_level_summary_table_percentage_columns) %>% 
      formatCurrency(school_level_summary_table_numeric_columns, currency = '', digits = 0) %>% 
      formatCurrency(school_level_summary_table_dollar_columns, digits = 0)
  })
  
  #Default/Blank Enrollment Zone Tables Render----------------------------------------------------------------------
  output$zone_sch_char <- renderDT({
    og_names <- names(zone_sch_char)
    
    zone_sch_char %>% 
      mutate(order = row_number()) %>% 
      gather(metric, value, -c(Metrics, order)) %>% 
      mutate(value = case_when(Metrics == 'Utilization' ~ scales::percent(value),
                               TRUE ~ as.character(prettyNum(round(value), big.mark = ',')))) %>% 
      spread(metric, value) %>% 
      arrange(order) %>% 
      select(all_of(og_names)) %>% 
      datatable(options = list(sort = FALSE, info = FALSE, searching = FALSE, paginate = FALSE), rownames = FALSE)
  })
  
  output$zone_gr_lvl_enr <- renderDT(zone_gr_lvl_enr, 
                                     filter = "none",
                                     selection = 'none',
                                     options = list( paging = FALSE, searching = FALSE), rownames = FALSE)
  
  #Transpose columns and rows and convert to numerics
  zone_subgroups <- as.data.frame(t(zone_subgroups), row.names = FALSE)
  zone_subgroups <- header.true(zone_subgroups)
  zone_subgroups$Zone <- c("Zone 1", "Zone 2", "Zone 3", "Zone 4"," Zone 5", "Zone 6", "Zone 7", "Zone 8")
  zone_subgroups <- zone_subgroups %>%
    select(Zone, everything())
  zone_subgroups[,2] <- as.numeric(as.character(zone_subgroups[,2]))
  zone_subgroups[,3] <- as.numeric(as.character(zone_subgroups[,3]))
  zone_subgroups[,4] <- as.numeric(as.character(zone_subgroups[,4]))
  zone_subgroups[,5] <- as.numeric(as.character(zone_subgroups[,5]))
  zone_subgroups[,6] <- as.numeric(as.character(zone_subgroups[,6]))
  zone_subgroups[,7] <- as.numeric(as.character(zone_subgroups[,7]))
  zone_subgroups[,8] <- as.numeric(as.character(zone_subgroups[,8]))
  zone_subgroups[,9] <- as.numeric(as.character(zone_subgroups[,9]))
  names(zone_subgroups[,1]) <- "Zone"
  
  # #Add Benchmark
  # zone_subgroups$Total <- 0
  # zone_subgroups[1,"Total"] <- sum(glDf[glDf$Year == "2021" & (glDf$Grade == "sp_ed" | glDf$Grade == "alt_ed"), "Count"], na.rm = TRUE)/ sum(glDf[glDf$Year == "2021", "Count"], na.rm = TRUE)
  # zone_subgroups[2,"Total"] <- sum(glDf[glDf$Year == "2021" & glDf$Grade == "ell", "Count"], na.rm = TRUE)/ sum(glDf[glDf$Year == "2021", "Count"], na.rm = TRUE)
  # zone_subgroups[3,"Total"] <- mean(slDf$Direct.Cert)
  # zone_subgroups[4,"Total"] <- sum(glDf[glDf$Year == "2021", "Asian"], na.rm = TRUE)/ sum(glDf[glDf$Year == "2021", "Count"], na.rm = TRUE)
  # zone_subgroups[5,"Total"] <- sum(glDf[glDf$Year == "2021", "Black"], na.rm = TRUE)/ sum(glDf[glDf$Year == "2021", "Count"], na.rm = TRUE)
  # zone_subgroups[6,"Total"] <- sum(glDf[glDf$Year == "2021", "Latinx"], na.rm = TRUE)/ sum(glDf[glDf$Year == "2021", "Count"], na.rm = TRUE)
  # zone_subgroups[7,"Total"] <- sum(glDf[glDf$Year == "2021", "White"], na.rm = TRUE)/ sum(glDf[glDf$Year == "2021", "Count"], na.rm = TRUE)
  # zone_subgroups[8,"Total"] <- sum(glDf[glDf$Year == "2021", "Multiethnic"], na.rm = TRUE)/ sum(glDf[glDf$Year == "2021", "Count"], na.rm = TRUE)
  
  ## Zone Output ----
  output$zone_subgroups <- renderDT({datatable(zone_subgroups, 
                                               options = list(paginate = FALSE, searching = FALSE, info = FALSE), rownames = FALSE) %>% 
      formatPercentage(names(zone_subgroups)[!names(zone_subgroups) == 'Zone'])
  })
  
  # Zone Financing Output----
  
  output$zone_fin <- renderDT(zone_fin, 
                              filter = "none",
                              selection = 'none',
                              options = list( paging = FALSE, searching = FALSE), rownames = FALSE)
  
  #trigger_zone----------------------------------------------------------------------------------------------------
  
  trigger_zone <- function() {    
    
    # Set-Up Grade Level Scenarios---------------------------
    
    if (counter$counterValue == 0) {
      
      is.nan.data.frame <- function(x)
        do.call(cbind, lapply(x, is.nan))
      
      zone_glDf_sg_pct <- glDf[glDf$Year == 2021,]
      zone_glDf_sg_pct[zone_glDf_sg_pct == "sp_ed"] <- "Sp_Ed"
      zone_glDf_sg_pct[zone_glDf_sg_pct == "ell"] <- "ELL"
      zone_glDf_sg_pct[zone_glDf_sg_pct == "KG"] <- "K"
      zone_glDf_sg_pct[zone_glDf_sg_pct == "alt_ed"] <- "Alt_Ed"
      zone_glDf_sg_pct$Asian_pct <- zone_glDf_sg_pct$Asian / zone_glDf_sg_pct$Count
      zone_glDf_sg_pct$Black_pct <- zone_glDf_sg_pct$Black / zone_glDf_sg_pct$Count
      zone_glDf_sg_pct$Latinx_pct <- zone_glDf_sg_pct$Latinx / zone_glDf_sg_pct$Count
      zone_glDf_sg_pct$White_pct <- zone_glDf_sg_pct$White / zone_glDf_sg_pct$Count
      zone_glDf_sg_pct$Multiethnic_pct <- zone_glDf_sg_pct$Multiethnic / zone_glDf_sg_pct$Count
      zone_glDf_sg_pct[is.nan(zone_glDf_sg_pct)] <- 0
      me_change_sce <- x$df1[,2:7]
      zone_glDf_sce <- merge(zone_glDf_sg_pct, me_change_sce, by.x = c("School.Name", "Grade"), by.y = c("School.Name", "Grade"), all =TRUE)
      zone_glDf_sce$Asian_sce <- round(zone_glDf_sce$Asian_pct * zone_glDf_sce[,24])	
      zone_glDf_sce$Black_sce <- round(zone_glDf_sce$Black_pct * zone_glDf_sce[,24])	
      zone_glDf_sce$Latinx_sce <- round(zone_glDf_sce$Latinx_pct * zone_glDf_sce[,24])	
      zone_glDf_sce$White_sce <- round(zone_glDf_sce$White_pct * zone_glDf_sce[,24])	
      zone_glDf_sce$Multiethnic_sce <- round(zone_glDf_sce$Multiethnic_pct * zone_glDf_sce[,24])
      zone_glDf_sce[is.na(zone_glDf_sce)] <- 0
      
      #Zone Specific schLvl$df, schlvloutputs, GlDf
      zone_1_schlvl <- slDf[slDf$School.Name.SL %in% input$zone_1_sch,]
      zone_1_schlvloutputs <- sch_lvl_outputs[sch_lvl_outputs$Schools %in% input$zone_1_sch,]
      zone_1_x_df <- glDf[glDf$Year == "2021" & glDf$School.Name %in% input$zone_1_sch,]
      # zone_1_gldf_sce <- zone_glDf_sce[zone_glDf_sce$School.Name %in% input$zone_1_sch,]
      # zone_1_gldf_sce <- zone_1_gldf_sce[order(zone_1_gldf_sce$Order),]
      
      #Grades Served
      zone_1_grades <-  unique(trimws(str_split(paste(zone_1_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_1_grades[zone_1_grades == "alt_ed"] <- "Alt_Ed"
      zone_1_grades[zone_1_grades == "ell"] <- "ELL"
      zone_1_grades[zone_1_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_1_grades <- zone_1_grades[zone_1_grades != 0]
      zone_1_grades <- paste(zone_1_grades[order(match(zone_1_grades,grade_order))], collapse = ", ")
      
      ## Zones 2-8-----------------------------------------------
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_2_schlvl <- slDf[slDf$School.Name.SL %in% input$zone_2_sch,]
      zone_2_schlvloutputs <- sch_lvl_outputs[sch_lvl_outputs$Schools %in% input$zone_2_sch,]
      zone_2_x_df <- glDf[glDf$Year == "2021" & glDf$School.Name %in% input$zone_2_sch,]
      
      #Grades Served
      zone_2_grades <-  unique(trimws(str_split(paste(zone_2_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_2_grades[zone_2_grades == "alt_ed"] <- "Alt_Ed"
      zone_2_grades[zone_2_grades == "ell"] <- "ELL"
      zone_2_grades[zone_2_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_2_grades <- zone_2_grades[zone_2_grades != 0]
      zone_2_grades <- paste(zone_2_grades[order(match(zone_2_grades[zone_2_grades != "0"],grade_order))], collapse = ", ")
      
      ##
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_3_schlvl <- slDf[slDf$School.Name.SL %in% input$zone_3_sch,]
      zone_3_schlvloutputs <- sch_lvl_outputs[sch_lvl_outputs$Schools %in% input$zone_3_sch,]
      zone_3_x_df <- glDf[glDf$Year == "2021" & glDf$School.Name %in% input$zone_3_sch,]
      
      #Grades Served
      zone_3_grades <-  unique(trimws(str_split(paste(zone_3_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_3_grades[zone_3_grades == "alt_ed"] <- "Alt_Ed"
      zone_3_grades[zone_3_grades == "ell"] <- "ELL"
      zone_3_grades[zone_3_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_3_grades <- zone_3_grades[zone_3_grades != 0]
      zone_3_grades <- paste(zone_3_grades[order(match(zone_3_grades[zone_3_grades != "0"],grade_order))], collapse = ", ")
      
      ##
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_4_schlvl <- slDf[slDf$School.Name.SL %in% input$zone_4_sch,]
      zone_4_schlvloutputs <- sch_lvl_outputs[sch_lvl_outputs$Schools %in% input$zone_4_sch,]
      zone_4_x_df <- glDf[glDf$Year == "2021" & glDf$School.Name %in% input$zone_4_sch,]
      
      #Grades Served
      zone_4_grades <-  unique(trimws(str_split(paste(zone_4_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_4_grades[zone_4_grades == "alt_ed"] <- "Alt_Ed"
      zone_4_grades[zone_4_grades == "ell"] <- "ELL"
      zone_4_grades[zone_4_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_4_grades <- zone_4_grades[zone_4_grades != 0]
      zone_4_grades <- paste(zone_4_grades[order(match(zone_4_grades[zone_4_grades != "0"],grade_order))], collapse = ", ")
      
      ##
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_5_schlvl <- slDf[slDf$School.Name.SL %in% input$zone_5_sch,]
      zone_5_schlvloutputs <- sch_lvl_outputs[sch_lvl_outputs$Schools %in% input$zone_5_sch,]
      zone_5_x_df <- glDf[glDf$Year == "2021" & glDf$School.Name %in% input$zone_5_sch,]
      
      #Grades Served
      zone_5_grades <-  unique(trimws(str_split(paste(zone_5_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_5_grades[zone_5_grades == "alt_ed"] <- "Alt_Ed"
      zone_5_grades[zone_5_grades == "ell"] <- "ELL"
      zone_5_grades[zone_5_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_5_grades <- zone_5_grades[zone_5_grades != 0]
      zone_5_grades <- paste(zone_5_grades[order(match(zone_5_grades[zone_5_grades != "0"],grade_order))], collapse = ", ")
      
      ##
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_6_schlvl <- slDf[slDf$School.Name.SL %in% input$zone_6_sch,]
      zone_6_schlvloutputs <- sch_lvl_outputs[sch_lvl_outputs$Schools %in% input$zone_6_sch,]
      zone_6_x_df <- glDf[glDf$Year == "2021" & glDf$School.Name %in% input$zone_6_sch,]
      
      #Grades Served
      zone_6_grades <-  unique(trimws(str_split(paste(zone_6_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_6_grades[zone_6_grades == "alt_ed"] <- "Alt_Ed"
      zone_6_grades[zone_6_grades == "ell"] <- "ELL"
      zone_6_grades[zone_6_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_6_grades <- zone_6_grades[zone_6_grades != 0]
      zone_6_grades <- paste(zone_6_grades[order(match(zone_6_grades[zone_6_grades != "0"],grade_order))], collapse = ", ")
      
      ##
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_7_schlvl <- slDf[slDf$School.Name.SL %in% input$zone_7_sch,]
      zone_7_schlvloutputs <- sch_lvl_outputs[sch_lvl_outputs$Schools %in% input$zone_7_sch,]
      zone_7_x_df <- glDf[glDf$Year == "2021" & glDf$School.Name %in% input$zone_7_sch,]
      
      #Grades Served
      zone_7_grades <-  unique(trimws(str_split(paste(zone_7_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_7_grades[zone_7_grades == "alt_ed"] <- "Alt_Ed"
      zone_7_grades[zone_7_grades == "ell"] <- "ELL"
      zone_7_grades[zone_7_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_7_grades <- zone_7_grades[zone_7_grades != 0]
      zone_7_grades <- paste(zone_7_grades[order(match(zone_7_grades[zone_7_grades != "0"],grade_order))], collapse = ", ")
      
      ##
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_8_schlvl <- slDf[slDf$School.Name.SL %in% input$zone_8_sch,]
      zone_8_schlvloutputs <- sch_lvl_outputs[sch_lvl_outputs$Schools %in% input$zone_8_sch,]
      zone_8_x_df <- glDf[glDf$Year == "2021" & glDf$School.Name %in% input$zone_8_sch,]
      
      #Grades Served
      zone_8_grades <-  unique(trimws(str_split(paste(zone_8_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_8_grades[zone_8_grades == "alt_ed"] <- "Alt_Ed"
      zone_8_grades[zone_8_grades == "ell"] <- "ELL"
      zone_8_grades[zone_8_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_8_grades <- zone_8_grades[zone_8_grades != 0]
      zone_8_grades <- paste(zone_8_grades[order(match(zone_8_grades[zone_8_grades != "0"],grade_order))], collapse = ", ")
      
      
      ## Zone School Characterisitics table update-------------------------------------------------------------------------------------------------
      
      #Grades
      zone_sch_char[1,2] <- zone_1_grades
      zone_sch_char[1,3] <- zone_2_grades
      zone_sch_char[1,4] <- zone_3_grades
      zone_sch_char[1,5] <- zone_4_grades
      zone_sch_char[1,6] <- zone_5_grades
      zone_sch_char[1,7] <- zone_6_grades
      zone_sch_char[1,8] <- zone_7_grades
      zone_sch_char[1,9] <- zone_8_grades
      
      #Capacity
      zone_sch_char[2,2] <- sum(zone_1_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,3] <- sum(zone_2_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,4] <- sum(zone_3_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,5] <- sum(zone_4_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,6] <- sum(zone_5_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,7] <- sum(zone_6_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,8] <- sum(zone_7_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,9] <- sum(zone_8_schlvl$Capacity, na.rm = TRUE)
      
      #Utilization
      zone_sch_char[3,2] <- sum(zone_1_schlvl$enrollment, na.rm =TRUE) / sum(zone_1_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,3] <- sum(zone_2_schlvl$enrollment, na.rm =TRUE) / sum(zone_2_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,4] <- sum(zone_3_schlvl$enrollment, na.rm =TRUE) / sum(zone_3_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,5] <- sum(zone_4_schlvl$enrollment, na.rm =TRUE) / sum(zone_4_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,6] <- sum(zone_5_schlvl$enrollment, na.rm =TRUE) / sum(zone_5_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,7] <- sum(zone_6_schlvl$enrollment, na.rm =TRUE) / sum(zone_6_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,8] <- sum(zone_7_schlvl$enrollment, na.rm =TRUE) / sum(zone_7_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,9] <- sum(zone_8_schlvl$enrollment, na.rm =TRUE) / sum(zone_8_schlvl$Capacity, na.rm = TRUE)
      
      #Combined Score
      zone_sch_char[4,2] <- ceiling(mean(as.numeric(zone_1_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,3] <- ceiling(mean(as.numeric(zone_2_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,4] <- ceiling(mean(as.numeric(zone_3_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,5] <- ceiling(mean(as.numeric(zone_4_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,6] <- ceiling(mean(as.numeric(zone_5_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,7] <- ceiling(mean(as.numeric(zone_6_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,8] <- ceiling(mean(as.numeric(zone_7_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,9] <- ceiling(mean(as.numeric(zone_8_schlvl$Combined.Score), na.rm = TRUE))
      
      #HQ Schools Count by Zone
      zone_sch_char[5,2] <- sum(zone_1_schlvl$Combined.Score >= 80 & !(zone_1_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,3] <- sum(zone_2_schlvl$Combined.Score >= 80 & !(zone_2_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,4] <- sum(zone_3_schlvl$Combined.Score >= 80 & !(zone_3_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,5] <- sum(zone_4_schlvl$Combined.Score >= 80 & !(zone_4_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,6] <- sum(zone_5_schlvl$Combined.Score >= 80 & !(zone_5_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,7] <- sum(zone_6_schlvl$Combined.Score >= 80 & !(zone_6_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,8] <- sum(zone_7_schlvl$Combined.Score >= 80 & !(zone_7_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,9] <- sum(zone_8_schlvl$Combined.Score >= 80 & !(zone_8_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      
      #LQ Schools Count by Zone
      zone_sch_char[6,2] <- sum(zone_1_schlvl$Combined.Score <= 60 & !(zone_1_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,3] <- sum(zone_2_schlvl$Combined.Score <= 60 & !(zone_2_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,4] <- sum(zone_3_schlvl$Combined.Score <= 60 & !(zone_3_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,5] <- sum(zone_4_schlvl$Combined.Score <= 60 & !(zone_4_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,6] <- sum(zone_5_schlvl$Combined.Score <= 60 & !(zone_5_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,7] <- sum(zone_6_schlvl$Combined.Score <= 60 & !(zone_6_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,8] <- sum(zone_7_schlvl$Combined.Score <= 60 & !(zone_7_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,9] <- sum(zone_8_schlvl$Combined.Score <= 60 & !(zone_8_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      
      #Num of High Needs
      zone_sch_char[7,2] <- sum(zone_1_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,3] <- sum(zone_2_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,4] <- sum(zone_3_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,5] <- sum(zone_4_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,6] <- sum(zone_5_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,7] <- sum(zone_6_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,8] <- sum(zone_7_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,9] <- sum(zone_8_schlvl$High.Needs == "Yes", na.rm = TRUE)
      
      #Average School Acct Metric
      zone_sch_char[8,2] <- round(mean(as.numeric(zone_1_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,3] <- round(mean(as.numeric(zone_2_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,4] <- round(mean(as.numeric(zone_3_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,5] <- round(mean(as.numeric(zone_4_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,6] <- round(mean(as.numeric(zone_5_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,7] <- round(mean(as.numeric(zone_6_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,8] <- round(mean(as.numeric(zone_7_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,9] <- round(mean(as.numeric(zone_8_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      
      
      zone_sch_char[zone_sch_char == ""] <- 0
      zone_sch_char[zone_sch_char == "NaN"] <- 0
      
      #Download
      
      zone_sch_char_sce$sce <- zone_sch_char
      
      format_util_sce <-  zone_sch_char_sce$sce %>% 
        slice(n()) %>%
        mutate(order = row_number()) %>% 
        gather(metric, value, -c(Metrics, order)) %>%
        mutate(Metrics = as.character(Metrics)) %>%
        mutate(value = as.numeric(value)) %>%
        mutate(value = case_when(Metrics == 'Average Utilization' ~ scales::percent(value), TRUE ~ as.character(prettyNum(round(value), big.mark = ',')))) %>% 
        spread(metric, value)
      
      zone_sch_char_sce$sce[zone_sch_char_sce$sce$Metrics == "Average Utilization",] <- format_util_sce[,-2] 
      
      #Output
      
      output$zone_sch_char <- output$zone_sch_char <- renderDT({
        format_util <-  zone_sch_char %>% 
          slice(3) %>%
          mutate(order = row_number()) %>% 
          gather(metric, value, -c(Metrics, order)) %>%
          mutate(Metrics = as.character(Metrics)) %>%
          mutate(value = as.numeric(value)) %>%
          mutate(value = case_when(Metrics == 'Average Utilization' ~ scales::percent(value), TRUE ~ as.character(prettyNum(round(value, digits = 2), big.mark = ',')))) %>% 
          spread(metric, value)
        
        zone_sch_char[zone_sch_char$Metrics == "Average Utilization",] <- format_util[,-2] 
        
        zone_sch_char %>% datatable(options = list(sort = FALSE, info = FALSE, searching = FALSE, paginate = FALSE), rownames = FALSE)
      })
      
      ## Enrollment by Zone by School table update-------------------------------------------------------------------------------------------------
      
      zone_gr_lvl_enr[,2] <- ifelse(nrow(zone_1_x_df) > 0, aggregate(Count ~ Grade+Order, data = zone_1_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,3] <- ifelse(nrow(zone_2_x_df) > 0, aggregate(Count ~ Grade+Order, data = zone_2_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,4] <- ifelse(nrow(zone_3_x_df) > 0, aggregate(Count ~ Grade+Order, data = zone_3_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,5] <- ifelse(nrow(zone_4_x_df) > 0, aggregate(Count ~ Grade+Order, data = zone_4_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,6] <- ifelse(nrow(zone_5_x_df) > 0, aggregate(Count ~ Grade+Order, data = zone_5_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,7] <- ifelse(nrow(zone_6_x_df) > 0, aggregate(Count ~ Grade+Order, data = zone_6_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,8] <- ifelse(nrow(zone_7_x_df) > 0, aggregate(Count ~ Grade+Order, data = zone_7_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,9] <- ifelse(nrow(zone_8_x_df) > 0, aggregate(Count ~ Grade+Order, data = zone_8_x_df, sum)[3], 0)
      
      zone_gr_lvl_enr[,10] <- rowSums(zone_gr_lvl_enr[2:9])
      
      zone_gr_lvl_enr_sce$sce <- zone_gr_lvl_enr
      
      output$zone_gr_lvl_enr <- renderDT(zone_gr_lvl_enr, 
                                         filter = "none",
                                         selection = 'none',
                                         options = list( paging = FALSE, searching = FALSE), rownames = FALSE)
      
      ## Subgroup Pcts by Zone ---------------------------------------------------------------------------
      ## Grouped by Calculation Type
      
      #Recreate Default Table
      
      zone_subgroups <- data.frame("Demographic" = c("SWD","ELL","FRL","Asian", "Black", "Latinx", "White", "Multiethnic"), stringsAsFactors = FALSE)
      
      zone_subgroups$Zone_1 <- 0
      zone_subgroups$Zone_2 <- 0
      zone_subgroups$Zone_3 <- 0
      zone_subgroups$Zone_4 <- 0
      zone_subgroups$Zone_5 <- 0
      zone_subgroups$Zone_6 <- 0
      zone_subgroups$Zone_7 <- 0
      zone_subgroups$Zone_8 <- 0
      
      names(zone_subgroups) <- c("Demographic","Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5", "Zone 6", "Zone 7", "Zone 8")
      
      ### Sp_Ed/ELL ----
      
      zone_subgroups[1,2] <- ifelse(nrow(zone_1_x_df) > 0, sum(zone_1_x_df[zone_1_x_df$Grade == "sp_ed","Count"]) / sum(zone_1_x_df[,"Count"]),0)
      zone_subgroups[1,3] <- ifelse(nrow(zone_2_x_df) > 0, sum(zone_2_x_df[zone_2_x_df$Grade == "sp_ed","Count"]) / sum(zone_2_x_df[,"Count"]),0)
      zone_subgroups[1,4] <- ifelse(nrow(zone_3_x_df) > 0, sum(zone_3_x_df[zone_3_x_df$Grade == "sp_ed","Count"]) / sum(zone_3_x_df[,"Count"]),0)
      zone_subgroups[1,5] <- ifelse(nrow(zone_4_x_df) > 0, sum(zone_4_x_df[zone_4_x_df$Grade == "sp_ed","Count"]) / sum(zone_4_x_df[,"Count"]),0)
      zone_subgroups[1,6] <- ifelse(nrow(zone_5_x_df) > 0, sum(zone_5_x_df[zone_5_x_df$Grade == "sp_ed","Count"]) / sum(zone_5_x_df[,"Count"]),0)
      zone_subgroups[1,7] <- ifelse(nrow(zone_6_x_df) > 0, sum(zone_6_x_df[zone_6_x_df$Grade == "sp_ed","Count"]) / sum(zone_6_x_df[,"Count"]),0)
      zone_subgroups[1,8] <- ifelse(nrow(zone_7_x_df) > 0, sum(zone_7_x_df[zone_7_x_df$Grade == "sp_ed","Count"]) / sum(zone_7_x_df[,"Count"]),0)
      zone_subgroups[1,9] <- ifelse(nrow(zone_8_x_df) > 0, sum(zone_8_x_df[zone_8_x_df$Grade == "sp_ed","Count"]) / sum(zone_8_x_df[,"Count"]),0)
      
      zone_subgroups[2,2] <- ifelse(nrow(zone_1_x_df) > 0, sum(zone_1_x_df[zone_1_x_df$Grade == "ell","Count"]) / sum(zone_1_x_df[,"Count"]),0)
      zone_subgroups[2,3] <- ifelse(nrow(zone_2_x_df) > 0, sum(zone_2_x_df[zone_2_x_df$Grade == "ell","Count"]) / sum(zone_2_x_df[,"Count"]),0)
      zone_subgroups[2,4] <- ifelse(nrow(zone_3_x_df) > 0, sum(zone_3_x_df[zone_3_x_df$Grade == "ell","Count"]) / sum(zone_3_x_df[,"Count"]),0)
      zone_subgroups[2,5] <- ifelse(nrow(zone_4_x_df) > 0, sum(zone_4_x_df[zone_4_x_df$Grade == "ell","Count"]) / sum(zone_4_x_df[,"Count"]),0)
      zone_subgroups[2,6] <- ifelse(nrow(zone_5_x_df) > 0, sum(zone_5_x_df[zone_5_x_df$Grade == "ell","Count"]) / sum(zone_5_x_df[,"Count"]),0)
      zone_subgroups[2,7] <- ifelse(nrow(zone_6_x_df) > 0, sum(zone_6_x_df[zone_6_x_df$Grade == "ell","Count"]) / sum(zone_6_x_df[,"Count"]),0)
      zone_subgroups[2,8] <- ifelse(nrow(zone_7_x_df) > 0, sum(zone_7_x_df[zone_7_x_df$Grade == "ell","Count"]) / sum(zone_7_x_df[,"Count"]),0)
      zone_subgroups[2,9] <- ifelse(nrow(zone_8_x_df) > 0, sum(zone_8_x_df[zone_8_x_df$Grade == "ell","Count"]) / sum(zone_8_x_df[,"Count"]),0)
      
      ### FRL ----
      zone_subgroups[3,2] <- ifelse(nrow(zone_1_schlvl) > 0, sum(zone_1_schlvl[,"Direct.Cert"] * zone_1_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_1_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,3] <- ifelse(nrow(zone_2_schlvl) > 0, sum(zone_2_schlvl[,"Direct.Cert"] * zone_2_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_2_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,4] <- ifelse(nrow(zone_3_schlvl) > 0, sum(zone_3_schlvl[,"Direct.Cert"] * zone_3_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_3_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,5] <- ifelse(nrow(zone_4_schlvl) > 0, sum(zone_4_schlvl[,"Direct.Cert"] * zone_4_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_4_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,6] <- ifelse(nrow(zone_5_schlvl) > 0, sum(zone_5_schlvl[,"Direct.Cert"] * zone_5_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_5_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,7] <- ifelse(nrow(zone_6_schlvl) > 0, sum(zone_6_schlvl[,"Direct.Cert"] * zone_6_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_6_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,8] <- ifelse(nrow(zone_7_schlvl) > 0, sum(zone_7_schlvl[,"Direct.Cert"] * zone_7_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_7_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,9] <- ifelse(nrow(zone_8_schlvl) > 0, sum(zone_8_schlvl[,"Direct.Cert"] * zone_8_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_8_schlvl$enrollment, na.rm = TRUE), 0)
      
      ### Ethnicity Subgroups ----
      
      zone_subgroups[4,2] <- ifelse(nrow(zone_1_x_df) > 0, sum(zone_1_x_df[,"Asian"],na.rm = T)/sum(zone_1_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[4,3] <- ifelse(nrow(zone_2_x_df) > 0, sum(zone_2_x_df[,"Asian"],na.rm = T)/sum(zone_2_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[4,4] <- ifelse(nrow(zone_3_x_df) > 0, sum(zone_3_x_df[,"Asian"],na.rm = T)/sum(zone_3_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[4,5] <- ifelse(nrow(zone_4_x_df) > 0, sum(zone_4_x_df[,"Asian"],na.rm = T)/sum(zone_4_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[4,6] <- ifelse(nrow(zone_5_x_df) > 0, sum(zone_5_x_df[,"Asian"],na.rm = T)/sum(zone_5_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[4,7] <- ifelse(nrow(zone_6_x_df) > 0, sum(zone_6_x_df[,"Asian"],na.rm = T)/sum(zone_6_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[4,8] <- ifelse(nrow(zone_7_x_df) > 0, sum(zone_7_x_df[,"Asian"],na.rm = T)/sum(zone_7_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[4,9] <- ifelse(nrow(zone_8_x_df) > 0, sum(zone_8_x_df[,"Asian"],na.rm = T)/sum(zone_8_x_df[,"Count"],na.rm = T), 0)
      
      zone_subgroups[5,2] <- ifelse(nrow(zone_1_x_df) > 0, sum(zone_1_x_df[,"Black"],na.rm = T)/sum(zone_1_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[5,3] <- ifelse(nrow(zone_2_x_df) > 0, sum(zone_2_x_df[,"Black"],na.rm = T)/sum(zone_2_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[5,4] <- ifelse(nrow(zone_3_x_df) > 0, sum(zone_3_x_df[,"Black"],na.rm = T)/sum(zone_3_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[5,5] <- ifelse(nrow(zone_4_x_df) > 0, sum(zone_4_x_df[,"Black"],na.rm = T)/sum(zone_4_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[5,6] <- ifelse(nrow(zone_5_x_df) > 0, sum(zone_5_x_df[,"Black"],na.rm = T)/sum(zone_5_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[5,7] <- ifelse(nrow(zone_6_x_df) > 0, sum(zone_6_x_df[,"Black"],na.rm = T)/sum(zone_6_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[5,8] <- ifelse(nrow(zone_7_x_df) > 0, sum(zone_7_x_df[,"Black"],na.rm = T)/sum(zone_7_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[5,9] <- ifelse(nrow(zone_8_x_df) > 0, sum(zone_8_x_df[,"Black"],na.rm = T)/sum(zone_8_x_df[,"Count"],na.rm = T), 0)
      
      zone_subgroups[6,2] <- ifelse(nrow(zone_1_x_df) > 0, sum(zone_1_x_df[,"Latinx"],na.rm = T)/sum(zone_1_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[6,3] <- ifelse(nrow(zone_2_x_df) > 0, sum(zone_2_x_df[,"Latinx"],na.rm = T)/sum(zone_2_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[6,4] <- ifelse(nrow(zone_3_x_df) > 0, sum(zone_3_x_df[,"Latinx"],na.rm = T)/sum(zone_3_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[6,5] <- ifelse(nrow(zone_4_x_df) > 0, sum(zone_4_x_df[,"Latinx"],na.rm = T)/sum(zone_4_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[6,6] <- ifelse(nrow(zone_5_x_df) > 0, sum(zone_5_x_df[,"Latinx"],na.rm = T)/sum(zone_5_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[6,7] <- ifelse(nrow(zone_6_x_df) > 0, sum(zone_6_x_df[,"Latinx"],na.rm = T)/sum(zone_6_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[6,8] <- ifelse(nrow(zone_7_x_df) > 0, sum(zone_7_x_df[,"Latinx"],na.rm = T)/sum(zone_7_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[6,9] <- ifelse(nrow(zone_8_x_df) > 0, sum(zone_8_x_df[,"Latinx"],na.rm = T)/sum(zone_8_x_df[,"Count"],na.rm = T), 0)
      
      zone_subgroups[7,2] <- ifelse(nrow(zone_1_x_df) > 0, sum(zone_1_x_df[,"White"],na.rm = T)/sum(zone_1_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[7,3] <- ifelse(nrow(zone_2_x_df) > 0, sum(zone_2_x_df[,"White"],na.rm = T)/sum(zone_2_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[7,4] <- ifelse(nrow(zone_3_x_df) > 0, sum(zone_3_x_df[,"White"],na.rm = T)/sum(zone_3_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[7,5] <- ifelse(nrow(zone_4_x_df) > 0, sum(zone_4_x_df[,"White"],na.rm = T)/sum(zone_4_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[7,6] <- ifelse(nrow(zone_5_x_df) > 0, sum(zone_5_x_df[,"White"],na.rm = T)/sum(zone_5_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[7,7] <- ifelse(nrow(zone_6_x_df) > 0, sum(zone_6_x_df[,"White"],na.rm = T)/sum(zone_6_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[7,8] <- ifelse(nrow(zone_7_x_df) > 0, sum(zone_7_x_df[,"White"],na.rm = T)/sum(zone_7_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[7,9] <- ifelse(nrow(zone_8_x_df) > 0, sum(zone_8_x_df[,"White"],na.rm = T)/sum(zone_8_x_df[,"Count"],na.rm = T), 0)
      
      zone_subgroups[8,2] <- ifelse(nrow(zone_1_x_df) > 0, sum(zone_1_x_df[,"Multiethnic"],na.rm = T)/sum(zone_1_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[8,3] <- ifelse(nrow(zone_2_x_df) > 0, sum(zone_2_x_df[,"Multiethnic"],na.rm = T)/sum(zone_2_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[8,4] <- ifelse(nrow(zone_3_x_df) > 0, sum(zone_3_x_df[,"Multiethnic"],na.rm = T)/sum(zone_3_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[8,5] <- ifelse(nrow(zone_4_x_df) > 0, sum(zone_4_x_df[,"Multiethnic"],na.rm = T)/sum(zone_4_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[8,6] <- ifelse(nrow(zone_5_x_df) > 0, sum(zone_5_x_df[,"Multiethnic"],na.rm = T)/sum(zone_5_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[8,7] <- ifelse(nrow(zone_6_x_df) > 0, sum(zone_6_x_df[,"Multiethnic"],na.rm = T)/sum(zone_6_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[8,8] <- ifelse(nrow(zone_7_x_df) > 0, sum(zone_7_x_df[,"Multiethnic"],na.rm = T)/sum(zone_7_x_df[,"Count"],na.rm = T), 0)
      zone_subgroups[8,9] <- ifelse(nrow(zone_8_x_df) > 0, sum(zone_8_x_df[,"Multiethnic"],na.rm = T)/sum(zone_8_x_df[,"Count"],na.rm = T), 0)
      
      zone_subgroups[zone_subgroups == "NaN"] <- 0
      
      #Transpose columns and rows and convert to numerics
      zone_subgroups <- as.data.frame(t(zone_subgroups), row.names = FALSE)
      zone_subgroups <- header.true(zone_subgroups)
      zone_subgroups$Zone <- c("Zone 1", "Zone 2", "Zone 3", "Zone 4"," Zone 5", "Zone 6", "Zone 7", "Zone 8")
      zone_subgroups <- zone_subgroups %>%
        select(Zone, everything())
      zone_subgroups[,2] <- as.numeric(as.character(zone_subgroups[,2]))
      zone_subgroups[,3] <- as.numeric(as.character(zone_subgroups[,3]))
      zone_subgroups[,4] <- as.numeric(as.character(zone_subgroups[,4]))
      zone_subgroups[,5] <- as.numeric(as.character(zone_subgroups[,5]))
      zone_subgroups[,6] <- as.numeric(as.character(zone_subgroups[,6]))
      zone_subgroups[,7] <- as.numeric(as.character(zone_subgroups[,7]))
      zone_subgroups[,8] <- as.numeric(as.character(zone_subgroups[,8]))
      zone_subgroups[,9] <- as.numeric(as.character(zone_subgroups[,9]))
      names(zone_subgroups[,1]) <- "Zone"
      
      zone_subgroups_sce$sce <- zone_subgroups 
      
      ## Output ----
      output$zone_subgroups <-   renderDT({datatable(zone_subgroups, 
                                                     options = list(paginate = FALSE, searching = FALSE, info = FALSE), rownames = FALSE) %>% 
          formatPercentage(names(zone_subgroups)[!names(zone_subgroups) == 'Zone'])
      })
      
      ### Funding Subgroups ----
      zone_fin$'Total Baseline' <- 0
      zone_fin$'Total SBA' <- 0
      zone_fin$'Total Funding' <- 0
      
      zone_fin[1,"Total SBA"] <- sum(zone_1_schlvloutputs$SBA_Default)
      zone_fin[1,"Total Baseline"] <- sum(zone_1_schlvloutputs$Baseline_Default)
      zone_fin[1,"Total Funding"] <- zone_fin[1,"Total SBA"] + zone_fin[1,"Total Baseline"]
      
      zone_fin[2,"Total SBA"] <- sum(zone_2_schlvloutputs$SBA_Default)
      zone_fin[2,"Total Baseline"] <- sum(zone_2_schlvloutputs$Baseline_Default)
      zone_fin[2,"Total Funding"] <- zone_fin[2,"Total SBA"] + zone_fin[2,"Total Baseline"]
      
      zone_fin[3,"Total SBA"] <- sum(zone_3_schlvloutputs$SBA_Default)
      zone_fin[3,"Total Baseline"] <- sum(zone_3_schlvloutputs$Baseline_Default)
      zone_fin[3,"Total Funding"] <- zone_fin[3,"Total SBA"] + zone_fin[3,"Total Baseline"]
      
      zone_fin[4,"Total SBA"] <- sum(zone_4_schlvloutputs$SBA_Default)
      zone_fin[4,"Total Baseline"] <- sum(zone_4_schlvloutputs$Baseline_Default)
      zone_fin[4,"Total Funding"] <- zone_fin[4,"Total SBA"] + zone_fin[4,"Total Baseline"]
      
      zone_fin[5,"Total SBA"] <- sum(zone_5_schlvloutputs$SBA_Default)
      zone_fin[5,"Total Baseline"] <- sum(zone_5_schlvloutputs$Baseline_Default)
      zone_fin[5,"Total Funding"] <- zone_fin[5,"Total SBA"] + zone_fin[5,"Total Baseline"]
      
      zone_fin[6,"Total SBA"] <- sum(zone_6_schlvloutputs$SBA_Default)
      zone_fin[6,"Total Baseline"] <- sum(zone_6_schlvloutputs$Baseline_Default)
      zone_fin[6,"Total Funding"] <- zone_fin[6,"Total SBA"] + zone_fin[6,"Total Baseline"]
      
      zone_fin[7,"Total SBA"] <- sum(zone_7_schlvloutputs$SBA_Default)
      zone_fin[7,"Total Baseline"] <- sum(zone_7_schlvloutputs$Baseline_Default)
      zone_fin[7,"Total Funding"] <- zone_fin[7,"Total SBA"] + zone_fin[7,"Total Baseline"]
      
      zone_fin[8,"Total SBA"] <- sum(zone_8_schlvloutputs$SBA_Default)
      zone_fin[8,"Total Baseline"] <- sum(zone_8_schlvloutputs$Baseline_Default)
      zone_fin[8,"Total Funding"] <- zone_fin[8,"Total SBA"] + zone_fin[8,"Total Baseline"]
      
      zone_fin[,2] <- dollar(zone_fin[,2])
      zone_fin[,3] <- dollar(zone_fin[,3])
      zone_fin[,4] <- dollar(zone_fin[,4])
      
      zone_fin_sce$sce <- zone_fin
      
      output$zone_fin <- renderDT(zone_fin, 
                                  filter = "none",
                                  selection = 'none',
                                  options = list( paging = FALSE, searching = FALSE), rownames = FALSE)
      
    }
    
    #Else If Scenario -----------------------------------------------------
    
    else if (counter$counterValue > 0){
      
      is.nan.data.frame <- function(x)
        do.call(cbind, lapply(x, is.nan))
      
      glDf_PH <- glDf[glDf$Year == 2021 & glDf$School.Name == "Anna Brochhausen School 88",]
      glDf_PH[,4:nrow(glDf_PH)] <- 0
      
      #Make placeholder rows in glDf for new schools for match down below
      new_sch_list <- unique(x$df1[!(x$df1$School.Name %in% glDf$School.Name),"School.Name"])
      
      if (nrow(new_sch_list) > 0) { for (name in new_sch_list) {
        #Change Name and append to glDf
        glDf_PH[,"School.Name"] <- name
        bind_rows(glDf, glDf_PH)
        
        #Recreate Placeholder Table for new school if needed
        glDf_PH <- glDf[glDf$Year == 2021 & glDf$School.Name == "Anna Brochhausen School 88",]
        glDf_PH[,4:nrow(glDf_PH)] <- 0
        
      }}
      
      #Create Scenario Level glDf with basic recalc of enrollment by Subgroup
      zone_glDf_sg_pct <- glDf[glDf$Year == 2021,]
      zone_glDf_sg_pct[zone_glDf_sg_pct == "sp_ed"] <- "Sp_Ed"
      zone_glDf_sg_pct[zone_glDf_sg_pct == "ell"] <- "ELL"
      zone_glDf_sg_pct[zone_glDf_sg_pct == "KG"] <- "K"
      zone_glDf_sg_pct[zone_glDf_sg_pct == "alt_ed"] <- "Alt_Ed"
      zone_glDf_sg_pct$Asian_pct <- zone_glDf_sg_pct$Asian / zone_glDf_sg_pct$Count
      zone_glDf_sg_pct$Black_pct <- zone_glDf_sg_pct$Black / zone_glDf_sg_pct$Count
      zone_glDf_sg_pct$Latinx_pct <- zone_glDf_sg_pct$Latinx / zone_glDf_sg_pct$Count
      zone_glDf_sg_pct$White_pct <- zone_glDf_sg_pct$White / zone_glDf_sg_pct$Count
      zone_glDf_sg_pct$Multiethnic_pct <- zone_glDf_sg_pct$Multiethnic / zone_glDf_sg_pct$Count
      zone_glDf_sg_pct[is.nan(zone_glDf_sg_pct)] <- 0
      me_change_sce <- x$df1[,2:7]
      zone_glDf_sce <- merge(zone_glDf_sg_pct, me_change_sce, by.x = c("School.Name", "Grade"), by.y = c("School.Name", "Grade"), all =TRUE)
      zone_glDf_sce$Asian_sce <- round(zone_glDf_sce$Asian_pct * zone_glDf_sce[,24])	
      zone_glDf_sce$Black_sce <- round(zone_glDf_sce$Black_pct * zone_glDf_sce[,24])	
      zone_glDf_sce$Latinx_sce <- round(zone_glDf_sce$Latinx_pct * zone_glDf_sce[,24])	
      zone_glDf_sce$White_sce <- round(zone_glDf_sce$White_pct * zone_glDf_sce[,24])	
      zone_glDf_sce$Multiethnic_sce <- round(zone_glDf_sce$Multiethnic_pct * zone_glDf_sce[,24])
      zone_glDf_sce[is.na(zone_glDf_sce)] <- 0
      
      # Zone School Level Scenario Updates------------
      
      #Zone Specific schLvl$df, schlvloutputs, GlDf
      
      zone_1_schlvl <- schLvl$df[schLvl$df$School.Name.SL %in% input$zone_1_sch,]
      zone_1_schlvloutputs <- schLvl$schOP[schLvl$schOP$Schools %in% input$zone_1_sch,]
      zone_1_x_df <- x$df1[x$df1$School.Name %in% input$zone_1_sch,]
      zone_1_gldf_sce <- zone_glDf_sce[zone_glDf_sce$School.Name %in% input$zone_1_sch,]
      zone_1_gldf_sce <- zone_1_gldf_sce[order(zone_1_gldf_sce$Order),]
      zone_1_new_subgroups <- schLvl$new_subgroup_shares_all[schLvl$new_subgroup_shares_all$School.Name %in% input$zone_1_sch,]
      
      #Grades Served
      zone_1_grades <-  unique(trimws(str_split(paste(zone_1_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_1_grades[zone_1_grades == "alt_ed"] <- "Alt_Ed"
      zone_1_grades[zone_1_grades == "ell"] <- "ELL"
      zone_1_grades[zone_1_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_1_grades <- zone_1_grades[zone_1_grades != 0]
      zone_1_grades <- paste(zone_1_grades[order(match(zone_1_grades[zone_1_grades != "0"],grade_order))], collapse = ", ")
      
      
      ## Zones 2-8-----------------------------------------------
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_2_schlvl <- schLvl$df[schLvl$df$School.Name.SL %in% input$zone_2_sch,]
      zone_2_schlvloutputs <- schLvl$schOP[schLvl$schOP$Schools %in% input$zone_2_sch,]
      zone_2_x_df <- x$df1[x$df1$School.Name %in% input$zone_2_sch,]
      zone_2_gldf_sce <- zone_glDf_sce[zone_glDf_sce$School.Name %in% input$zone_2_sch,]
      zone_2_gldf_sce <- zone_2_gldf_sce[order(zone_2_gldf_sce$Order),]
      zone_2_new_subgroups <- schLvl$new_subgroup_shares_all[schLvl$new_subgroup_shares_all$School.Name %in% input$zone_2_sch,]
      
      #Grades Served
      zone_2_grades <-  unique(trimws(str_split(paste(zone_2_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_2_grades[zone_2_grades == "alt_ed"] <- "Alt_Ed"
      zone_2_grades[zone_2_grades == "ell"] <- "ELL"
      zone_2_grades[zone_2_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_2_grades <- zone_2_grades[zone_2_grades != 0]
      zone_2_grades <- paste(zone_2_grades[order(match(zone_2_grades[zone_2_grades != "0"],grade_order))], collapse = ", ")
      
      
      ##
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_3_schlvl <- schLvl$df[schLvl$df$School.Name.SL %in% input$zone_3_sch,]
      zone_3_schlvloutputs <- schLvl$schOP[schLvl$schOP$Schools %in% input$zone_3_sch,]
      zone_3_x_df <- x$df1[x$df1$School.Name %in% input$zone_3_sch,]
      zone_3_gldf_sce <- zone_glDf_sce[zone_glDf_sce$School.Name %in% input$zone_3_sch,]
      zone_3_gldf_sce <- zone_3_gldf_sce[order(zone_3_gldf_sce$Order),]
      zone_3_new_subgroups <- schLvl$new_subgroup_shares_all[schLvl$new_subgroup_shares_all$School.Name %in% input$zone_3_sch,]
      
      #Grades Served
      zone_3_grades <-  unique(trimws(str_split(paste(zone_3_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_3_grades[zone_3_grades == "alt_ed"] <- "Alt_Ed"
      zone_3_grades[zone_3_grades == "ell"] <- "ELL"
      zone_3_grades[zone_3_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_3_grades <- zone_3_grades[zone_3_grades != 0]
      zone_3_grades <- paste(zone_3_grades[order(match(zone_3_grades[zone_3_grades != "0"],grade_order))], collapse = ", ")
      
      ##
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_4_schlvl <- schLvl$df[schLvl$df$School.Name.SL %in% input$zone_4_sch,]
      zone_4_schlvloutputs <- schLvl$schOP[schLvl$schOP$Schools %in% input$zone_4_sch,]
      zone_4_x_df <- x$df1[x$df1$School.Name %in% input$zone_4_sch,]
      zone_4_gldf_sce <- zone_glDf_sce[zone_glDf_sce$School.Name %in% input$zone_4_sch,]
      zone_4_gldf_sce <- zone_4_gldf_sce[order(zone_4_gldf_sce$Order),]
      zone_4_new_subgroups <- schLvl$new_subgroup_shares_all[schLvl$new_subgroup_shares_all$School.Name %in% input$zone_4_sch,]
      
      #Grades Served
      zone_4_grades <-  unique(trimws(str_split(paste(zone_4_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_4_grades[zone_4_grades == "alt_ed"] <- "Alt_Ed"
      zone_4_grades[zone_4_grades == "ell"] <- "ELL"
      zone_4_grades[zone_4_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_4_grades <- zone_4_grades[zone_4_grades != 0]
      zone_4_grades <- paste(zone_4_grades[order(match(zone_4_grades[zone_4_grades != "0"],grade_order))], collapse = ", ")
      
      ##
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_5_schlvl <- schLvl$df[schLvl$df$School.Name.SL %in% input$zone_5_sch,]
      zone_5_schlvloutputs <- schLvl$schOP[schLvl$schOP$Schools %in% input$zone_5_sch,]
      zone_5_x_df <- x$df1[x$df1$School.Name %in% input$zone_5_sch,]
      zone_5_gldf_sce <- zone_glDf_sce[zone_glDf_sce$School.Name %in% input$zone_5_sch,]
      zone_5_gldf_sce <- zone_5_gldf_sce[order(zone_5_gldf_sce$Order),]
      zone_5_new_subgroups <- schLvl$new_subgroup_shares_all[schLvl$new_subgroup_shares_all$School.Name %in% input$zone_5_sch,]
      
      #Grades Served
      zone_5_grades <-  unique(trimws(str_split(paste(zone_5_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_5_grades[zone_5_grades == "alt_ed"] <- "Alt_Ed"
      zone_5_grades[zone_5_grades == "ell"] <- "ELL"
      zone_5_grades[zone_5_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_5_grades <- zone_5_grades[zone_5_grades != 0]
      zone_5_grades <- paste(zone_5_grades[order(match(zone_5_grades[zone_5_grades != "0"],grade_order))], collapse = ", ")
      
      ##
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_6_schlvl <- schLvl$df[schLvl$df$School.Name.SL %in% input$zone_6_sch,]
      zone_6_schlvloutputs <- schLvl$schOP[schLvl$schOP$Schools %in% input$zone_6_sch,]
      zone_6_x_df <- x$df1[x$df1$School.Name %in% input$zone_6_sch,]
      zone_6_gldf_sce <- zone_glDf_sce[zone_glDf_sce$School.Name %in% input$zone_6_sch,]
      zone_6_gldf_sce <- zone_6_gldf_sce[order(zone_6_gldf_sce$Order),]
      zone_6_new_subgroups <- schLvl$new_subgroup_shares_all[schLvl$new_subgroup_shares_all$School.Name %in% input$zone_6_sch,]
      
      #Grades Served
      zone_6_grades <-  unique(trimws(str_split(paste(zone_6_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_6_grades[zone_6_grades == "alt_ed"] <- "Alt_Ed"
      zone_6_grades[zone_6_grades == "ell"] <- "ELL"
      zone_6_grades[zone_6_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_6_grades <- zone_6_grades[zone_6_grades != 0]
      zone_6_grades <- paste(zone_6_grades[order(match(zone_6_grades[zone_6_grades != "0"],grade_order))], collapse = ", ")
      
      ##
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_7_schlvl <- schLvl$df[schLvl$df$School.Name.SL %in% input$zone_7_sch,]
      zone_7_schlvloutputs <- schLvl$schOP[schLvl$schOP$Schools %in% input$zone_7_sch,]
      zone_7_x_df <- x$df1[x$df1$School.Name %in% input$zone_7_sch,]
      zone_7_gldf_sce <- zone_glDf_sce[zone_glDf_sce$School.Name %in% input$zone_7_sch,]
      zone_7_gldf_sce <- zone_7_gldf_sce[order(zone_7_gldf_sce$Order),]
      zone_7_new_subgroups <- schLvl$new_subgroup_shares_all[schLvl$new_subgroup_shares_all$School.Name %in% input$zone_7_sch,]
      
      #Grades Served
      zone_7_grades <-  unique(trimws(str_split(paste(zone_7_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_7_grades[zone_7_grades == "alt_ed"] <- "Alt_Ed"
      zone_7_grades[zone_7_grades == "ell"] <- "ELL"
      zone_7_grades[zone_7_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_7_grades <- zone_7_grades[zone_7_grades != 0]
      zone_7_grades <- paste(zone_7_grades[order(match(zone_7_grades[zone_7_grades != "0"],grade_order))], collapse = ", ")
      
      ##
      
      #Zone Specific schLvl$df amd schlvloutputs
      zone_8_schlvl <- schLvl$df[schLvl$df$School.Name.SL %in% input$zone_8_sch,]
      zone_8_schlvloutputs <- schLvl$schOP[schLvl$schOP$Schools %in% input$zone_8_sch,]
      zone_8_x_df <- x$df1[x$df1$School.Name %in% input$zone_8_sch,]
      zone_8_gldf_sce <- zone_glDf_sce[zone_glDf_sce$School.Name %in% input$zone_8_sch,]
      zone_8_gldf_sce <- zone_8_gldf_sce[order(zone_8_gldf_sce$Order),]
      zone_8_new_subgroups <- schLvl$new_subgroup_shares_all[schLvl$new_subgroup_shares_all$School.Name %in% input$zone_8_sch,]
      
      #Grades Served
      zone_8_grades <-  unique(trimws(str_split(paste(zone_8_schlvloutputs$"Grades_Scenario", collapse = ","), ",")[[1]]))
      zone_8_grades[zone_8_grades == "alt_ed"] <- "Alt_Ed"
      zone_8_grades[zone_8_grades == "ell"] <- "ELL"
      zone_8_grades[zone_8_grades == "sp_ed"] <- "Sp_Ed"
      grade_order <-  c("PK","K","1","2","3","4","5","6","7","8","9","10","11","12","Alt_Ed","ELL","Sp_Ed")
      zone_8_grades <- zone_8_grades[zone_8_grades != 0]
      zone_8_grades <- paste(zone_8_grades[order(match(zone_8_grades[zone_8_grades != "0"],grade_order))], collapse = ", ")
      
      
      ## Zone School Characterisitics table update-------------------------------------------------------------------------------------------------
      
      #Grades
      zone_sch_char[1,2] <- zone_1_grades
      zone_sch_char[1,3] <- zone_2_grades
      zone_sch_char[1,4] <- zone_3_grades
      zone_sch_char[1,5] <- zone_4_grades
      zone_sch_char[1,6] <- zone_5_grades
      zone_sch_char[1,7] <- zone_6_grades
      zone_sch_char[1,8] <- zone_7_grades
      zone_sch_char[1,9] <- zone_8_grades
      
      #Capacity
      zone_sch_char[2,2] <- sum(zone_1_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,3] <- sum(zone_2_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,4] <- sum(zone_3_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,5] <- sum(zone_4_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,6] <- sum(zone_5_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,7] <- sum(zone_6_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,8] <- sum(zone_7_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[2,9] <- sum(zone_8_schlvl$Capacity, na.rm = TRUE)
      
      #Utilization
      zone_sch_char[3,2] <- sum(zone_1_schlvl$enrollment, na.rm =TRUE) / sum(zone_1_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,3] <- sum(zone_2_schlvl$enrollment, na.rm =TRUE) / sum(zone_2_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,4] <- sum(zone_3_schlvl$enrollment, na.rm =TRUE) / sum(zone_3_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,5] <- sum(zone_4_schlvl$enrollment, na.rm =TRUE) / sum(zone_4_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,6] <- sum(zone_5_schlvl$enrollment, na.rm =TRUE) / sum(zone_5_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,7] <- sum(zone_6_schlvl$enrollment, na.rm =TRUE) / sum(zone_6_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,8] <- sum(zone_7_schlvl$enrollment, na.rm =TRUE) / sum(zone_7_schlvl$Capacity, na.rm = TRUE)
      zone_sch_char[3,9] <- sum(zone_8_schlvl$enrollment, na.rm =TRUE) / sum(zone_8_schlvl$Capacity, na.rm = TRUE)
      
      #Combined Score
      zone_sch_char[4,2] <- ceiling(mean(as.numeric(zone_1_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,3] <- ceiling(mean(as.numeric(zone_2_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,4] <- ceiling(mean(as.numeric(zone_3_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,5] <- ceiling(mean(as.numeric(zone_4_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,6] <- ceiling(mean(as.numeric(zone_5_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,7] <- ceiling(mean(as.numeric(zone_6_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,8] <- ceiling(mean(as.numeric(zone_7_schlvl$Combined.Score), na.rm = TRUE))
      zone_sch_char[4,9] <- ceiling(mean(as.numeric(zone_8_schlvl$Combined.Score), na.rm = TRUE))
      
      #HQ Schools Count by Zone
      zone_sch_char[5,2] <- sum(zone_1_schlvl$Combined.Score >= 80 & !(zone_1_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,3] <- sum(zone_2_schlvl$Combined.Score >= 80 & !(zone_2_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,4] <- sum(zone_3_schlvl$Combined.Score >= 80 & !(zone_3_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,5] <- sum(zone_4_schlvl$Combined.Score >= 80 & !(zone_4_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,6] <- sum(zone_5_schlvl$Combined.Score >= 80 & !(zone_5_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,7] <- sum(zone_6_schlvl$Combined.Score >= 80 & !(zone_6_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,8] <- sum(zone_7_schlvl$Combined.Score >= 80 & !(zone_7_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[5,9] <- sum(zone_8_schlvl$Combined.Score >= 80 & !(zone_8_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      
      #LQ Schools Count by Zone
      zone_sch_char[6,2] <- sum(zone_1_schlvl$Combined.Score <= 60 & !(zone_1_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,3] <- sum(zone_2_schlvl$Combined.Score <= 60 & !(zone_2_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,4] <- sum(zone_3_schlvl$Combined.Score <= 60 & !(zone_3_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,5] <- sum(zone_4_schlvl$Combined.Score <= 60 & !(zone_4_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,6] <- sum(zone_5_schlvl$Combined.Score <= 60 & !(zone_5_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,7] <- sum(zone_6_schlvl$Combined.Score <= 60 & !(zone_6_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,8] <- sum(zone_7_schlvl$Combined.Score <= 60 & !(zone_7_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      zone_sch_char[6,9] <- sum(zone_8_schlvl$Combined.Score <= 60 & !(zone_8_schlvl$School.Name.SL %in% schLvl$omit_new_sch), na.rm = TRUE)
      
      #Num of High Needs
      zone_sch_char[7,2] <- sum(zone_1_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,3] <- sum(zone_2_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,4] <- sum(zone_3_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,5] <- sum(zone_4_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,6] <- sum(zone_5_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,7] <- sum(zone_6_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,8] <- sum(zone_7_schlvl$High.Needs == "Yes", na.rm = TRUE)
      zone_sch_char[7,9] <- sum(zone_8_schlvl$High.Needs == "Yes", na.rm = TRUE)
      
      #Average School Acct Metric
      zone_sch_char[8,2] <- round(mean(as.numeric(zone_1_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,3] <- round(mean(as.numeric(zone_2_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,4] <- round(mean(as.numeric(zone_3_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,5] <- round(mean(as.numeric(zone_4_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,6] <- round(mean(as.numeric(zone_5_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,7] <- round(mean(as.numeric(zone_6_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,8] <- round(mean(as.numeric(zone_7_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      zone_sch_char[8,9] <- round(mean(as.numeric(zone_8_schlvl$School.Accountability.Metric), na.rm = TRUE), digits = 2)
      
      zone_sch_char[zone_sch_char == ""] <- 0
      zone_sch_char[zone_sch_char == "NaN"] <- 0
      
      #Download
      
      zone_sch_char_sce$sce <- zone_sch_char
      
      format_util_sce <-  zone_sch_char_sce$sce %>% 
        slice(n()) %>%
        mutate(order = row_number()) %>% 
        gather(metric, value, -c(Metrics, order)) %>%
        mutate(Metrics = as.character(Metrics)) %>%
        mutate(value = as.numeric(value)) %>%
        mutate(value = case_when(Metrics == 'Average Utilization' ~ scales::percent(value), TRUE ~ as.character(prettyNum(round(value), big.mark = ',')))) %>% 
        spread(metric, value)
      
      zone_sch_char_sce$sce[zone_sch_char_sce$sce$Metrics == "Average Utilization",] <- format_util_sce[,-2] 
      
      #Output
      
      output$zone_sch_char <- output$zone_sch_char <- renderDT({
        format_util <-  zone_sch_char %>% 
          slice(3) %>%
          mutate(order = row_number()) %>% 
          gather(metric, value, -c(Metrics, order)) %>%
          mutate(Metrics = as.character(Metrics)) %>%
          mutate(value = as.numeric(value)) %>%
          mutate(value = case_when(Metrics == 'Average Utilization' ~ scales::percent(value), TRUE ~ as.character(prettyNum(round(value, digits = 2), big.mark = ',')))) %>% 
          spread(metric, value)
        
        zone_sch_char[zone_sch_char$Metrics == "Average Utilization",] <- format_util[,-2] 
        
        zone_sch_char %>% datatable(options = list(sort = FALSE, info = FALSE, searching = FALSE, paginate = FALSE), rownames = FALSE)
      })
      
      ## Enrollment by Zone by School table update-------------------------------------------------------------------------------------------------
      
      zone_gr_lvl_enr[,2] <- ifelse(nrow(zone_1_x_df) > 0, aggregate(me_change ~ Grade+Order, data = zone_1_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,3] <- ifelse(nrow(zone_2_x_df) > 0, aggregate(me_change ~ Grade+Order, data = zone_2_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,4] <- ifelse(nrow(zone_3_x_df) > 0, aggregate(me_change ~ Grade+Order, data = zone_3_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,5] <- ifelse(nrow(zone_4_x_df) > 0, aggregate(me_change ~ Grade+Order, data = zone_4_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,6] <- ifelse(nrow(zone_5_x_df) > 0, aggregate(me_change ~ Grade+Order, data = zone_5_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,7] <- ifelse(nrow(zone_6_x_df) > 0, aggregate(me_change ~ Grade+Order, data = zone_6_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,8] <- ifelse(nrow(zone_7_x_df) > 0, aggregate(me_change ~ Grade+Order, data = zone_7_x_df, sum)[3], 0)
      zone_gr_lvl_enr[,9] <- ifelse(nrow(zone_8_x_df) > 0, aggregate(me_change ~ Grade+Order, data = zone_8_x_df, sum)[3], 0)
      
      zone_gr_lvl_enr[,10] <- rowSums(zone_gr_lvl_enr[2:9]) 
      
      zone_gr_lvl_enr_sce$sce <- zone_gr_lvl_enr
      
      output$zone_gr_lvl_enr <- renderDT(zone_gr_lvl_enr, 
                                         filter = "none",
                                         selection = 'none',
                                         options = list( paging = FALSE, searching = FALSE), rownames = FALSE)
      
      ## Subgroup Pcts by Zone ---------------------------------------------------------------------------
      ## Grouped by Calculation Type
      
      #Recreate Default Table
      
      zone_subgroups <- data.frame("Demographic" = c("SWD","ELL","FRL","Asian", "Black", "Latinx", "White", "Multiethnic"), stringsAsFactors = FALSE)
      
      zone_subgroups$Zone_1 <- 0
      zone_subgroups$Zone_2 <- 0
      zone_subgroups$Zone_3 <- 0
      zone_subgroups$Zone_4 <- 0
      zone_subgroups$Zone_5 <- 0
      zone_subgroups$Zone_6 <- 0
      zone_subgroups$Zone_7 <- 0
      zone_subgroups$Zone_8 <- 0
      
      names(zone_subgroups) <- c("Demographic","Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5", "Zone 6", "Zone 7", "Zone 8")
      
      ### Sp_Ed/ELL ----
      
      zone_subgroups[1,2] <- ifelse(nrow(zone_1_x_df) > 0, sum(zone_1_x_df[zone_1_x_df$Grade == "Sp_Ed",7]) / sum(zone_1_x_df[,7]), 0)
      zone_subgroups[1,3] <- ifelse(nrow(zone_2_x_df) > 0, sum(zone_2_x_df[zone_2_x_df$Grade == "Sp_Ed",7]) / sum(zone_2_x_df[,7]), 0)
      zone_subgroups[1,4] <- ifelse(nrow(zone_3_x_df) > 0, sum(zone_3_x_df[zone_3_x_df$Grade == "Sp_Ed",7]) / sum(zone_3_x_df[,7]), 0)
      zone_subgroups[1,5] <- ifelse(nrow(zone_4_x_df) > 0, sum(zone_4_x_df[zone_4_x_df$Grade == "Sp_Ed",7]) / sum(zone_4_x_df[,7]), 0)
      zone_subgroups[1,6] <- ifelse(nrow(zone_5_x_df) > 0, sum(zone_5_x_df[zone_5_x_df$Grade == "Sp_Ed",7]) / sum(zone_5_x_df[,7]), 0)
      zone_subgroups[1,7] <- ifelse(nrow(zone_6_x_df) > 0, sum(zone_6_x_df[zone_6_x_df$Grade == "Sp_Ed",7]) / sum(zone_6_x_df[,7]), 0)
      zone_subgroups[1,8] <- ifelse(nrow(zone_7_x_df) > 0, sum(zone_7_x_df[zone_7_x_df$Grade == "Sp_Ed",7]) / sum(zone_7_x_df[,7]), 0)
      zone_subgroups[1,9] <- ifelse(nrow(zone_8_x_df) > 0, sum(zone_8_x_df[zone_8_x_df$Grade == "Sp_Ed",7]) / sum(zone_8_x_df[,7]), 0)
      
      zone_subgroups[2,2] <- ifelse(nrow(zone_1_x_df) > 0, sum(zone_1_x_df[zone_1_x_df$Grade == "ELL",7]) / sum(zone_1_x_df[,7]), 0)
      zone_subgroups[2,3] <- ifelse(nrow(zone_2_x_df) > 0, sum(zone_2_x_df[zone_2_x_df$Grade == "ELL",7]) / sum(zone_2_x_df[,7]), 0)
      zone_subgroups[2,4] <- ifelse(nrow(zone_3_x_df) > 0, sum(zone_3_x_df[zone_3_x_df$Grade == "ELL",7]) / sum(zone_3_x_df[,7]), 0)
      zone_subgroups[2,5] <- ifelse(nrow(zone_4_x_df) > 0, sum(zone_4_x_df[zone_4_x_df$Grade == "ELL",7]) / sum(zone_4_x_df[,7]), 0)
      zone_subgroups[2,6] <- ifelse(nrow(zone_5_x_df) > 0, sum(zone_5_x_df[zone_5_x_df$Grade == "ELL",7]) / sum(zone_5_x_df[,7]), 0)
      zone_subgroups[2,7] <- ifelse(nrow(zone_6_x_df) > 0, sum(zone_6_x_df[zone_6_x_df$Grade == "ELL",7]) / sum(zone_6_x_df[,7]), 0)
      zone_subgroups[2,8] <- ifelse(nrow(zone_7_x_df) > 0, sum(zone_7_x_df[zone_7_x_df$Grade == "ELL",7]) / sum(zone_7_x_df[,7]), 0)
      zone_subgroups[2,9] <- ifelse(nrow(zone_8_x_df) > 0, sum(zone_8_x_df[zone_8_x_df$Grade == "ELL",7]) / sum(zone_8_x_df[,7]), 0)
      
      ### FRL ----
      zone_subgroups[3,2] <- ifelse(nrow(zone_1_schlvl) > 0, sum(zone_1_schlvl[,"Direct.Cert"] * zone_1_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_1_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,3] <- ifelse(nrow(zone_2_schlvl) > 0, sum(zone_2_schlvl[,"Direct.Cert"] * zone_2_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_2_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,4] <- ifelse(nrow(zone_3_schlvl) > 0, sum(zone_3_schlvl[,"Direct.Cert"] * zone_3_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_3_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,5] <- ifelse(nrow(zone_4_schlvl) > 0, sum(zone_4_schlvl[,"Direct.Cert"] * zone_4_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_4_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,6] <- ifelse(nrow(zone_5_schlvl) > 0, sum(zone_5_schlvl[,"Direct.Cert"] * zone_5_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_5_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,7] <- ifelse(nrow(zone_6_schlvl) > 0, sum(zone_6_schlvl[,"Direct.Cert"] * zone_6_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_6_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,8] <- ifelse(nrow(zone_7_schlvl) > 0, sum(zone_7_schlvl[,"Direct.Cert"] * zone_7_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_7_schlvl$enrollment, na.rm = TRUE), 0)
      zone_subgroups[3,9] <- ifelse(nrow(zone_8_schlvl) > 0, sum(zone_8_schlvl[,"Direct.Cert"] * zone_8_schlvl[,"enrollment"],na.rm = TRUE) / sum(zone_8_schlvl$enrollment, na.rm = TRUE), 0)
      
      ### Ethnicity Subgroups ----
      zone_subgroups[4,2] <- ifelse(nrow(zone_1_schlvl) > 0, sum(zone_1_new_subgroups[zone_1_new_subgroups$Race == "Asian","Students"], na.rm = TRUE)/sum(zone_1_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[4,3] <- ifelse(nrow(zone_2_schlvl) > 0, sum(zone_2_new_subgroups[zone_2_new_subgroups$Race == "Asian","Students"], na.rm = TRUE)/sum(zone_2_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[4,4] <- ifelse(nrow(zone_3_schlvl) > 0, sum(zone_3_new_subgroups[zone_3_new_subgroups$Race == "Asian","Students"], na.rm = TRUE)/sum(zone_3_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[4,5] <- ifelse(nrow(zone_4_schlvl) > 0, sum(zone_4_new_subgroups[zone_4_new_subgroups$Race == "Asian","Students"], na.rm = TRUE)/sum(zone_4_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[4,6] <- ifelse(nrow(zone_5_schlvl) > 0, sum(zone_5_new_subgroups[zone_5_new_subgroups$Race == "Asian","Students"], na.rm = TRUE)/sum(zone_5_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[4,7] <- ifelse(nrow(zone_6_schlvl) > 0, sum(zone_6_new_subgroups[zone_6_new_subgroups$Race == "Asian","Students"], na.rm = TRUE)/sum(zone_6_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[4,8] <- ifelse(nrow(zone_7_schlvl) > 0, sum(zone_7_new_subgroups[zone_7_new_subgroups$Race == "Asian","Students"], na.rm = TRUE)/sum(zone_7_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[4,9] <- ifelse(nrow(zone_8_schlvl) > 0, sum(zone_8_new_subgroups[zone_8_new_subgroups$Race == "Asian","Students"], na.rm = TRUE)/sum(zone_8_new_subgroups$Students, na.rm =TRUE), 0)
      
      zone_subgroups[5,2] <- ifelse(nrow(zone_1_schlvl) > 0,sum(zone_1_new_subgroups[zone_1_new_subgroups$Race == "Black","Students"], na.rm = TRUE)/sum(zone_1_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[5,3] <- ifelse(nrow(zone_2_schlvl) > 0,sum(zone_2_new_subgroups[zone_2_new_subgroups$Race == "Black","Students"], na.rm = TRUE)/sum(zone_2_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[5,4] <- ifelse(nrow(zone_3_schlvl) > 0,sum(zone_3_new_subgroups[zone_3_new_subgroups$Race == "Black","Students"], na.rm = TRUE)/sum(zone_3_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[5,5] <- ifelse(nrow(zone_4_schlvl) > 0,sum(zone_4_new_subgroups[zone_4_new_subgroups$Race == "Black","Students"], na.rm = TRUE)/sum(zone_4_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[5,6] <- ifelse(nrow(zone_5_schlvl) > 0,sum(zone_5_new_subgroups[zone_5_new_subgroups$Race == "Black","Students"], na.rm = TRUE)/sum(zone_5_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[5,7] <- ifelse(nrow(zone_6_schlvl) > 0,sum(zone_6_new_subgroups[zone_6_new_subgroups$Race == "Black","Students"], na.rm = TRUE)/sum(zone_6_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[5,8] <- ifelse(nrow(zone_7_schlvl) > 0,sum(zone_7_new_subgroups[zone_7_new_subgroups$Race == "Black","Students"], na.rm = TRUE)/sum(zone_7_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[5,9] <- ifelse(nrow(zone_8_schlvl) > 0,sum(zone_8_new_subgroups[zone_8_new_subgroups$Race == "Black","Students"], na.rm = TRUE)/sum(zone_8_new_subgroups$Students, na.rm =TRUE), 0)
      
      zone_subgroups[6,2] <- ifelse(nrow(zone_1_schlvl) > 0,sum(zone_1_new_subgroups[zone_1_new_subgroups$Race == "Latinx","Students"], na.rm = TRUE)/sum(zone_1_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[6,3] <- ifelse(nrow(zone_2_schlvl) > 0,sum(zone_2_new_subgroups[zone_2_new_subgroups$Race == "Latinx","Students"], na.rm = TRUE)/sum(zone_2_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[6,4] <- ifelse(nrow(zone_3_schlvl) > 0,sum(zone_3_new_subgroups[zone_3_new_subgroups$Race == "Latinx","Students"], na.rm = TRUE)/sum(zone_3_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[6,5] <- ifelse(nrow(zone_4_schlvl) > 0,sum(zone_4_new_subgroups[zone_4_new_subgroups$Race == "Latinx","Students"], na.rm = TRUE)/sum(zone_4_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[6,6] <- ifelse(nrow(zone_5_schlvl) > 0,sum(zone_5_new_subgroups[zone_5_new_subgroups$Race == "Latinx","Students"], na.rm = TRUE)/sum(zone_5_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[6,7] <- ifelse(nrow(zone_6_schlvl) > 0,sum(zone_6_new_subgroups[zone_6_new_subgroups$Race == "Latinx","Students"], na.rm = TRUE)/sum(zone_6_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[6,8] <- ifelse(nrow(zone_7_schlvl) > 0,sum(zone_7_new_subgroups[zone_7_new_subgroups$Race == "Latinx","Students"], na.rm = TRUE)/sum(zone_7_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[6,9] <- ifelse(nrow(zone_8_schlvl) > 0,sum(zone_8_new_subgroups[zone_8_new_subgroups$Race == "Latinx","Students"], na.rm = TRUE)/sum(zone_8_new_subgroups$Students, na.rm =TRUE), 0)
      
      zone_subgroups[7,2] <- ifelse(nrow(zone_1_schlvl) > 0,sum(zone_1_new_subgroups[zone_1_new_subgroups$Race == "White","Students"], na.rm = TRUE)/sum(zone_1_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[7,3] <- ifelse(nrow(zone_2_schlvl) > 0,sum(zone_2_new_subgroups[zone_2_new_subgroups$Race == "White","Students"], na.rm = TRUE)/sum(zone_2_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[7,4] <- ifelse(nrow(zone_3_schlvl) > 0,sum(zone_3_new_subgroups[zone_3_new_subgroups$Race == "White","Students"], na.rm = TRUE)/sum(zone_3_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[7,5] <- ifelse(nrow(zone_4_schlvl) > 0,sum(zone_4_new_subgroups[zone_4_new_subgroups$Race == "White","Students"], na.rm = TRUE)/sum(zone_4_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[7,6] <- ifelse(nrow(zone_5_schlvl) > 0,sum(zone_5_new_subgroups[zone_5_new_subgroups$Race == "White","Students"], na.rm = TRUE)/sum(zone_5_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[7,7] <- ifelse(nrow(zone_6_schlvl) > 0,sum(zone_6_new_subgroups[zone_6_new_subgroups$Race == "White","Students"], na.rm = TRUE)/sum(zone_6_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[7,8] <- ifelse(nrow(zone_7_schlvl) > 0,sum(zone_7_new_subgroups[zone_7_new_subgroups$Race == "White","Students"], na.rm = TRUE)/sum(zone_7_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[7,9] <- ifelse(nrow(zone_8_schlvl) > 0,sum(zone_8_new_subgroups[zone_8_new_subgroups$Race == "White","Students"], na.rm = TRUE)/sum(zone_8_new_subgroups$Students, na.rm =TRUE), 0)
      
      zone_subgroups[8,2] <- ifelse(nrow(zone_1_schlvl) > 0,sum(zone_1_new_subgroups[zone_1_new_subgroups$Race == "Multiethnic","Students"], na.rm = TRUE)/sum(zone_1_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[8,3] <- ifelse(nrow(zone_2_schlvl) > 0,sum(zone_2_new_subgroups[zone_2_new_subgroups$Race == "Multiethnic","Students"], na.rm = TRUE)/sum(zone_2_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[8,4] <- ifelse(nrow(zone_3_schlvl) > 0,sum(zone_3_new_subgroups[zone_3_new_subgroups$Race == "Multiethnic","Students"], na.rm = TRUE)/sum(zone_3_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[8,5] <- ifelse(nrow(zone_4_schlvl) > 0,sum(zone_4_new_subgroups[zone_4_new_subgroups$Race == "Multiethnic","Students"], na.rm = TRUE)/sum(zone_4_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[8,6] <- ifelse(nrow(zone_5_schlvl) > 0,sum(zone_5_new_subgroups[zone_5_new_subgroups$Race == "Multiethnic","Students"], na.rm = TRUE)/sum(zone_5_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[8,7] <- ifelse(nrow(zone_6_schlvl) > 0,sum(zone_6_new_subgroups[zone_6_new_subgroups$Race == "Multiethnic","Students"], na.rm = TRUE)/sum(zone_6_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[8,8] <- ifelse(nrow(zone_7_schlvl) > 0,sum(zone_7_new_subgroups[zone_7_new_subgroups$Race == "Multiethnic","Students"], na.rm = TRUE)/sum(zone_7_new_subgroups$Students, na.rm =TRUE), 0)
      zone_subgroups[8,9] <- ifelse(nrow(zone_8_schlvl) > 0,sum(zone_8_new_subgroups[zone_8_new_subgroups$Race == "Multiethnic","Students"], na.rm = TRUE)/sum(zone_8_new_subgroups$Students, na.rm =TRUE), 0)
      
      
      zone_subgroups[zone_subgroups == "NaN"] <- 0
      
      #Transpose columns and rows and convert to numerics
      zone_subgroups <- as.data.frame(t(zone_subgroups), row.names = FALSE)
      zone_subgroups <- header.true(zone_subgroups)
      zone_subgroups$Zone <- c("Zone 1", "Zone 2", "Zone 3", "Zone 4"," Zone 5", "Zone 6", "Zone 7", "Zone 8")
      zone_subgroups <- zone_subgroups %>%
        select(Zone, everything())
      zone_subgroups[,2] <- as.numeric(as.character(zone_subgroups[,2]))
      zone_subgroups[,3] <- as.numeric(as.character(zone_subgroups[,3]))
      zone_subgroups[,4] <- as.numeric(as.character(zone_subgroups[,4]))
      zone_subgroups[,5] <- as.numeric(as.character(zone_subgroups[,5]))
      zone_subgroups[,6] <- as.numeric(as.character(zone_subgroups[,6]))
      zone_subgroups[,7] <- as.numeric(as.character(zone_subgroups[,7]))
      zone_subgroups[,8] <- as.numeric(as.character(zone_subgroups[,8]))
      zone_subgroups[,9] <- as.numeric(as.character(zone_subgroups[,9]))
      names(zone_subgroups[,1]) <- "Zone"
      
      zone_subgroups_sce$sce <- zone_subgroups
      
      ## Output ----
      output$zone_subgroups <-   renderDT({datatable(zone_subgroups, 
                                                     options = list(paginate = FALSE, searching = FALSE, info = FALSE), rownames = FALSE) %>% 
          formatPercentage(names(zone_subgroups)[!names(zone_subgroups) == 'Zone'])
      })
      
      ### Funding Subgroups ----
      zone_fin$'Total Baseline' <- 0 
      zone_fin$'Total SBA' <- 0
      zone_fin$'Total Funding' <- 0
      
      zone_fin[1,"Total SBA"] <- sum(zone_1_schlvloutputs$SBA_Scenario)
      zone_fin[1,"Total Baseline"] <- sum(zone_1_schlvloutputs$Baseline_Scenario)
      zone_fin[1,"Total Funding"] <- zone_fin[1,"Total SBA"] + zone_fin[1,"Total Baseline"]
      
      zone_fin[2,"Total SBA"] <- sum(zone_2_schlvloutputs$SBA_Scenario)
      zone_fin[2,"Total Baseline"] <- sum(zone_2_schlvloutputs$Baseline_Scenario)
      zone_fin[2,"Total Funding"] <- zone_fin[2,"Total SBA"] + zone_fin[2,"Total Baseline"]
      
      zone_fin[3,"Total SBA"] <- sum(zone_3_schlvloutputs$SBA_Scenario)
      zone_fin[3,"Total Baseline"] <- sum(zone_3_schlvloutputs$Baseline_Scenario)
      zone_fin[3,"Total Funding"] <- zone_fin[3,"Total SBA"] + zone_fin[3,"Total Baseline"]
      
      zone_fin[4,"Total SBA"] <- sum(zone_4_schlvloutputs$SBA_Scenario)
      zone_fin[4,"Total Baseline"] <- sum(zone_4_schlvloutputs$Baseline_Scenario)
      zone_fin[4,"Total Funding"] <- zone_fin[4,"Total SBA"] + zone_fin[4,"Total Baseline"]
      
      zone_fin[5,"Total SBA"] <- sum(zone_5_schlvloutputs$SBA_Scenario)
      zone_fin[5,"Total Baseline"] <- sum(zone_5_schlvloutputs$Baseline_Scenario)
      zone_fin[5,"Total Funding"] <- zone_fin[5,"Total SBA"] + zone_fin[5,"Total Baseline"]
      
      zone_fin[6,"Total SBA"] <- sum(zone_6_schlvloutputs$SBA_Scenario)
      zone_fin[6,"Total Baseline"] <- sum(zone_6_schlvloutputs$Baseline_Scenario)
      zone_fin[6,"Total Funding"] <- zone_fin[6,"Total SBA"] + zone_fin[6,"Total Baseline"]
      
      zone_fin[7,"Total SBA"] <- sum(zone_7_schlvloutputs$SBA_Scenario)
      zone_fin[7,"Total Baseline"] <- sum(zone_7_schlvloutputs$Baseline_Scenario)
      zone_fin[7,"Total Funding"] <- zone_fin[7,"Total SBA"] + zone_fin[7,"Total Baseline"]
      
      zone_fin[8,"Total SBA"] <- sum(zone_8_schlvloutputs$SBA_Scenario)
      zone_fin[8,"Total Baseline"] <- sum(zone_8_schlvloutputs$Baseline_Scenario)
      zone_fin[8,"Total Funding"] <- zone_fin[8,"Total SBA"] + zone_fin[8,"Total Baseline"]
      
      zone_fin[,2] <- dollar(zone_fin[,2])
      zone_fin[,3] <- dollar(zone_fin[,3])
      zone_fin[,4] <- dollar(zone_fin[,4])
      
      zone_fin_sce$sce <- zone_fin
      
      output$zone_fin <- renderDT(zone_fin, 
                                  filter = "none",
                                  selection = 'none',
                                  options = list( paging = FALSE, searching = FALSE), rownames = FALSE)
      
      
      ##Schools Mapped by Zone---------------------------------------------------------------------------
      
      schLvl$Zmap_Counter <- schLvl$Zmap_Counter + 1
      
      ## Zone Assignment Dataframe Setup ----
      
      zone_1_sch_label <- as.data.frame(ifelse(nrow(zone_1_gldf_sce) > 0, lapply(subset(zone_1_schlvl, select = c(School.Name.SL)), unlist), NA), stringsAsFactors = FALSE)
      zone_1_sch_label$Zone <- ifelse(nrow(zone_1_gldf_sce) > 0, 1, NA)
      names(zone_1_sch_label) <- c("School.Name.SL", "Zone")
      
      zone_2_sch_label <- as.data.frame(ifelse(nrow(zone_2_gldf_sce) > 0, lapply(subset(zone_2_schlvl, select = c(School.Name.SL)), unlist), NA), stringsAsFactors = FALSE)
      zone_2_sch_label$Zone <- ifelse(nrow(zone_2_gldf_sce) > 0, 2, NA)
      names(zone_2_sch_label) <- c("School.Name.SL", "Zone")
      
      zone_3_sch_label <- as.data.frame(ifelse(nrow(zone_3_gldf_sce) > 0, lapply(subset(zone_3_schlvl, select = c(School.Name.SL)), unlist), NA), stringsAsFactors = FALSE)
      zone_3_sch_label$Zone <- ifelse(nrow(zone_3_gldf_sce) > 0, 3, NA)
      names(zone_3_sch_label) <- c("School.Name.SL", "Zone")
      
      zone_4_sch_label <- as.data.frame(ifelse(nrow(zone_4_gldf_sce) > 0, lapply(subset(zone_4_schlvl, select = c(School.Name.SL)), unlist), NA), stringsAsFactors = FALSE)
      zone_4_sch_label$Zone <- ifelse(nrow(zone_4_gldf_sce) > 0, 4, NA)
      names(zone_4_sch_label) <- c("School.Name.SL", "Zone")
      
      zone_5_sch_label <- as.data.frame(ifelse(nrow(zone_5_gldf_sce) > 0, lapply(subset(zone_5_schlvl, select = c(School.Name.SL)), unlist), NA), stringsAsFactors = FALSE)
      zone_5_sch_label$Zone <- ifelse(nrow(zone_5_gldf_sce) > 0, 5, NA)
      names(zone_5_sch_label) <- c("School.Name.SL", "Zone")
      
      zone_6_sch_label <- as.data.frame(ifelse(nrow(zone_6_gldf_sce) > 0, lapply(subset(zone_6_schlvl, select = c(School.Name.SL)), unlist), NA), stringsAsFactors = FALSE)
      zone_6_sch_label$Zone <- ifelse(nrow(zone_6_gldf_sce) > 0, 6, NA)
      names(zone_6_sch_label) <- c("School.Name.SL", "Zone")
      
      zone_7_sch_label <- as.data.frame(ifelse(nrow(zone_7_gldf_sce) > 0, lapply(subset(zone_7_schlvl, select = c(School.Name.SL)), unlist), NA), stringsAsFactors = FALSE)
      zone_7_sch_label$Zone <- ifelse(nrow(zone_7_gldf_sce) > 0, 7, NA)
      names(zone_7_sch_label) <- c("School.Name.SL", "Zone")
      
      zone_8_sch_label <- as.data.frame(ifelse(nrow(zone_8_gldf_sce) > 0, lapply(subset(zone_8_schlvl, select = c(School.Name.SL)), unlist), NA), stringsAsFactors = FALSE)
      zone_8_sch_label$Zone <- ifelse(nrow(zone_8_gldf_sce) > 0, 8, NA)
      names(zone_8_sch_label) <- c("School.Name.SL", "Zone")
      
      #zone_gr_lvl_enr[,2] <- ifelse(nrow(zone_1_gldf_sce) > 0, aggregate(Count ~ Grade+Order, data = zone_1_gldf_sce, sum)[3], 0)
      
      all_zones_schools <- rbind(zone_1_sch_label, zone_2_sch_label, zone_3_sch_label, zone_4_sch_label, zone_5_sch_label, zone_6_sch_label, zone_7_sch_label, zone_8_sch_label)
      
      na.omit(all_zones_schools)
      schLvl$zMDf_Scenario <- schLvl$mDf %>% inner_join(all_zones_schools, by = "School.Name.SL")
      
    }
    
  }
  
  ## School Zone Map Default to Scenario Conditional----
  sub_zMDf <- reactive({
    if (schLvl$Zmap_Counter > 0) {
      zMDf <- schLvl$zMDf_Scenario
    } else {
      zMDf <- zMDf_Default
    }
    return(zMDf)
  })
  
  ## School Zone Map Output----
  output$Zmap<-renderLeaflet({
    
    ## Create map and set defaut zoom
    m<-leaflet(data=sub_zMDf(), options = leafletOptions(minZoom = 11))%>%
      
      ## Specify background layout
      addProviderTiles('OpenStreetMap.Mapnik') %>% 
      ## Add print function
      onRender(
        "function(el, x) {
                                            L.easyPrint({
                                              sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
                                              filename: 'Map',
                                              exportOnly: true,
                                              hideControlContainer: true
                                            }).addTo(this);
                    }"
      )
    
  })
  
  
  observe({
    
    pal <- colorFactor(c("grey30","yellow","orange","red","purple","blue","green","turquoise","hotpink" ), domain= c(0,1,2,3,4,5,6,7,8))
    colorBy<-unique(sub_zMDf()$Zone)
    
    req(input$display_tab == "Enrollment Zones")
    
    leafletProxy("Zmap",data = sub_zMDf())%>%
      
      setMaxBounds(~min(Longitude,na.rm=TRUE), 
                   ~min(Latitude,na.rm=TRUE), 
                   ~max(Longitude,na.rm=TRUE), 
                   ~max(Latitude,na.rm=TRUE))%>%
      
      clearGroup('schoolPoints') %>%
      clearControls()%>%
      
      addCircleMarkers(
        group = "schoolPoints",
        ~Longitude,
        ~Latitude,
        
        ##Sets fill and stroke color(the same)
        color = ~pal(sub_zMDf()$Zone),
        stroke=TRUE,
        
        ##Sets opacity and weight
        opacity=1,
        weight=1,
        fillOpacity=0.7,
        
        popup= ~paste("<i>",School.Name.SL,"</i>", "<br>", 
                      "<strong>","Zone:","</strong>", Zone,"<br>",
                      "<strong>","Enrollment:","</strong>", enrollment,"<br>", 
                      "<strong>","Utilization:","</strong>", scales::percent(round(Utilization, digits = 1)),"<br>",
                      "<strong>","Direct Cert:","</strong>", scales::percent(round(Direct.Cert, digits = 3)), "<br>",
                      "<strong>","School Performance:","</strong>", School.Performance,"<br>",
                      "<strong>","Combined Facility Score:","</strong>", scales::percent(round(Combined.Score/100, digits = 3))))%>%            
      
      addLegend("topright", pal = pal, values = ~Zone,
                title = "Zones",
                opacity = 1)
    
    
    
  })
  
  #Enrollment Zone Event Update------------------------------------------------------------------------------------
  
  observeEvent(input$zone_submit,{
    trigger_zone()
  })
  
  
  #Input Table reactive dataframes---------------------------------------------------------------------------------
  x <- reactiveValues(df = enrInputMstr, df1 = enrInputMstr, df2 = enrInputMstr, df3 = enrInputMstr, df4 = enrInputMstr, df5 = enrInputMstr, df6 = enrInputMstr, df7 = enrInputMstr,
                      df8 = enrInputMstr, df9 = enrInputMstr, df10 = enrInputMstr, df11 = enrInputMstr, df_changes = tibble(), df_all = tibble(), df_allCh = tibble(), 
                      df_attrition = NULL, net_scenario_change_table = NULL, titleDescription = NULL, inact_schools = NULL, zone_inputSchList = NULL, closed_schools = NULL,
                      inputSchList = inputSchList, mDf = mDf)
  
  seg_sce <- reactiveValues(all = school_enr_gr, all2 = school_enr_gr_2, to = NULL)
  
  schLvl_df_default <- subset(slDf, select = -c(School.Name.M, IPS.ID.M, Building.Name)) %>% 
    filter(!is.na(School.Name.SL))
  
  schLvl <- reactiveValues(df = schLvl_df_default, dfO = NULL, dfC = NULL, enr = NULL, accTCntS = NULL, accTCnt1 = NULL, accTCnt2 = NULL, schOP = NULL, schOPMerge = NULL, schOPTable = NULL, 
                           utl_sum = NULL, funding = NULL, fundingMerge = NULL, tc = NULL, tck = NULL, tc1 = NULL,tc2 = NULL, tc4 = NULL,dlschLvlFile = NULL, mDf = NULL, zMDf = tibble(), 
                           zMDf_Scenario = tibble(), Zmap_Counter = 0, Impact_Master = NULL, sch_lvl_outputs = sch_lvl_outputs,  grdLvlSubGrp = grdLvlSubGrp, omit_new_sch = NULL, new_subgroup_shares_all = NULL)
  
  counter <- reactiveValues(counterValue = 0)
  
  zone <- reactiveValues(df = 0)
  
  existing_scenarios <- reactiveValues(scenarios = get_scenarios_from_s3())
  
  #Download file reactive dataframes--------------------------------------------------------------------------------
  
  cntCls <- reactiveValues(close = 0)
  
  sch_count_sce <- reactiveValues(sce = NULL)
  
  sch_cost_sce <- reactiveValues(sce = NULL)
  
  avg_school_char_sce <- reactiveValues(sce = NULL)
  
  school_enr_gr_sce <- reactiveValues(sce = NULL)
  
  subgroups_sce <- reactiveValues(sce = NULL) 
  
  schLvl_utl_sum <- reactiveValues(sce = NULL)
  
  cost_ps_sce <- reactiveValues(sce = NULL)
  
  zone_fin_sce <- reactiveValues(sce = NULL)
  
  zone_gr_lvl_enr_sce <- reactiveValues(sce = NULL)
  
  zone_subgroups_sce <- reactiveValues(sce = NULL)
  
  zone_sch_char_sce <- reactiveValues(sce = NULL)
  
  ## Delete Button
  
  observeEvent(input$delete_scenario_modal, {
    file_to_remove <- existing_scenarios$scenarios$filename[input$existing_scenarios_datatable_rows_selected]
    delete_object(object = str_glue('data/scenarios/{file_to_remove}.rds'), bucket = Primary_Bucket, region = 'us-east-2')
    existing_scenarios$scenarios <- slice(existing_scenarios$scenarios, -input$existing_scenarios_datatable_rows_selected)
    tmp <- tempfile()
    write.csv(existing_scenarios$scenarios, tmp, row.names = FALSE)
    put_object(tmp, object = 'scenario_metadata.csv', bucket = Primary_Bucket, region = 'us-east-2')
    unlink(tmp)
  })
  
  ## Save Button ----------------------------
  
  observeEvent(input$save, {
    
    withBusyIndicatorServer('save', {
      existing_scenarios$scenarios <- get_scenarios_from_s3()
      
      scenario <- list(
        title = input$scenario_title,
        description = input$scenario_description,
        timestamp = as.character(Sys.time()),
        x = x,
        schLvl = schLvl,
        cntCls = cntCls,
        inputs = reactiveValuesToList(input),
        counter = counter
      )
      
      filename <- str_glue('scenario_{UUIDgenerate()}')
      s3saveRDS(x = scenario, bucket = Primary_Bucket, object = str_glue('data/scenarios/{filename}.rds'), region = 'us-east-2')
      
      metadata <- tibble(
        title = scenario$title,
        description = scenario$description,
        timestamp = scenario$timestamp,
        filename = filename
      )
      
      combined_metadata <- existing_scenarios$scenarios %>% 
        rbind(metadata)
      
      tmp <- tempfile()
      write.csv(combined_metadata, tmp, row.names = FALSE)
      put_object(tmp, object = 'scenario_metadata.csv', bucket = Primary_Bucket, region = 'us-east-2')
      unlink(tmp)
      
      existing_scenarios$scenarios <- combined_metadata
      
    })
    
    # shinyalert(
    #   title = 'Success',
    #   text = str_glue('{metadata$title} scenario saved successfully'),
    #   closeOnEsc = TRUE,
    #   closeOnClickOutside = FALSE,
    #   html = FALSE,
    #   type = 'success',
    #   showConfirmButton = TRUE,
    #   showCancelButton = FALSE,
    #   confirmButtonText = "OK",
    #   confirmButtonCol = "#AEDEF4",
    #   timer = 0,
    #   imageUrl = '',
    #   animation = TRUE
    # )
    
  })
  ## Load Button---------------------------
  observeEvent(input$load, {
    existing_scenarios <- get_scenarios_from_s3()
  })
  
  output$scenario_load_ui <- renderUI({
    tagList(
      dataTableOutput('existing_scenarios_datatable'),
      uiOutput('scenario_load_button_ui')
    )
  })
  
  output$scenario_load_button_ui <- renderUI({
    req(input$existing_scenarios_datatable_rows_selected)
    div(style = 'display:inline-block;',
        div(style = 'display:inline-block;', withBusyIndicatorUI(actionButton('load_scenario_modal', str_glue('Load scenario "{selected_scenario()$title}"'), icon = icon('upload')))),
        br(),br(),br(),br(),
        p("Deleting a scenario is a permanent action. Please be sure you are certain you want to delete a scenario."),
        div(style = 'display:inline-block;', actionButton('delete_scenario_modal', str_glue('Delete scenario "{selected_scenario()$title}"'), icon = icon('trash')))
        
    )
  })
  
  selected_scenario <- reactive({
    existing_scenarios$scenarios %>% 
      slice(input$existing_scenarios_datatable_rows_selected)
  })
  
  output$existing_scenarios_datatable <- renderDataTable({
    existing_scenarios$scenarios %>% 
      select(-filename) %>% 
      set_names(c('Title', 'Description', 'Created at')) %>% 
      datatable(selection = 'single', rownames = FALSE)
  })
  
  
  observeEvent(input$load_scenario_modal, {
    withBusyIndicatorServer('load_scenario_modal', {
      trigger_reset()
      new_globals <- s3readRDS(object = str_glue('data/scenarios/{selected_scenario()$filename}.rds'), bucket = Primary_Bucket, region = 'us-east-2')
      x$mDf <- new_globals$x$mDf
      x$inputSchList <- new_globals$x$inputSchList
      cntCls$close <- new_globals$cntCls$close
      counter$counterValue <- new_globals$counter$counterValue
      
      #Conditional for Old Scenarios
      if ( sum(new_globals$x$df11$me_delta) == 0) {
        x$df1 <- new_globals$x$df
        map(2:11, function(n) {
          x[[str_glue('df{n}')]] <- new_globals$x[[str_glue('df{n-1}')]]
        })
        map(1:8, function(n) {
          x[[str_glue('zone_{n}_sch')]] <- new_globals$input[[str_glue('zone_{n}_sch')]]
        })
      } else {
        x$df1 <- new_globals$x$df1
        map(1:11, function(n) {
          x[[str_glue('df{n}')]] <- new_globals$x[[str_glue('df{n}')]]
        })
        map(1:8, function(n) {
          x[[str_glue('zone_{n}_sch')]] <- new_globals$input[[str_glue('zone_{n}_sch')]]
        }) }
      
      schLvl$df <- new_globals$schLvl$df
      schLvl$sch_lvl_outputs <- new_globals$schLvl$sch_lvl_outputs
      schLvl$dfC <- new_globals$schLvl$dfC
      schLvl$mDf <- new_globals$schLvl$mDf
      schLvl$grdLvlSubGrp <- new_globals$schLvl$grdLvlSubGrp
      schLvl$new_subgroup_shares_all <- new_globals$schLvl$new_subgroup_shares_all
      schLvl$omit_new_sch <- new_globals$schLvl$omit_new_sch
      updateTextInput(session, 'scenario_title', value = new_globals$title)
      updateTextInput(session, 'scenario_description', value = new_globals$description)
      trigger_scenario()
      # trigger_zone()
      # trigger_tracker_1()
      # trigger_tracker_2()
      toggleModal(session, 'scenario_load_modal')
    })
    
    
  })
  
  #Submit Scenario Changes Button--------------------------------------------------------------------------------
  observeEvent(input$submit,{
    trigger_scenario()
  })
  
  # Close School Button ----------------------------------------------------------------------------------
  observeEvent(input$closeSchool,{
    cntCls$close <- 1
    
    x$df1[x$df1$School.Name == isolate(input$subSchool),][["me_change"]] <-0
    x$df1[x$df1$School.Name == isolate(input$subSchool),]["me_delta"] <- x$df1[x$df1$School.Name == isolate(input$subSchool),]["me_change"] - x$df1[x$df1$School.Name == isolate(input$subSchool),]["2021"]
    
    trigger_tracker_1()
  })
  #Reset Button---------------------------------------------------------------------------------------
  observeEvent(input$reset,{
    trigger_reset()
  })
  
  #Defines output table for IPS School District Attrition Table-------------------
  setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("name", "age", "gender"))
  
  #Defines output table for SUBJECT SCHOOL input table and refines table formatting-----------------
  output$x0 <- renderDT({DT::datatable(
    x$df1 %>% filter(School.Name == input$subSchool) %>% 
      select(-c(Order, School.Name)) ,
    colnames = c("Grade", "2018-19", "2019-20", "Default 2020-21", "Scenario 2020-21", "Delta"), 
    filter = "none",
    selection = 'none', 
    editable = list(target = "cell", disable = list(columns = c(0,1,2,3,5))), #disables editing in all columns but 20-21 scenario
    options = list(pageLength = 17, paging = FALSE, searching = FALSE), #ensures all rows are visible, removes paging and search box from datatable output
    rownames=FALSE) #%>%
    #formatStyle(
    #  columns = 6,
    #  color = styleInterval(-1, c('red', 'black')),
    #  backgroundColor = styleInterval(-1, c('white', 'white'))
    #)
  })
  
  proxy <- dataTableProxy('x0')
  
  observeEvent(input$x0_cell_edit, {
    info = input$x0_cell_edit
    i = info$row
    j = info$col
    v = as.numeric(info$value)
    x$df1[x$df1$School.Name == isolate(input$subSchool),][[i, "me_change"]] <- DT::coerceValue(v, x$df1[x$df1$School.Name == isolate(input$subSchool),][i, j])
    x$df1[x$df1$School.Name == isolate(input$subSchool),][i, "me_delta"] <- x$df1[x$df1$School.Name == isolate(input$subSchool),][i,"me_change"] - x$df1[x$df1$School.Name == isolate(input$subSchool),][i,"2021"]
    
    trigger_tracker_1()
  })
  
  #Subject School Subgroup Table: Scenario-----------------------------------------------------------------------------------------
  
  
  
  #Subject School Subgroup Table reactive dataframe--------------------------------------------------------------------------------
  output$sub_SGTbl<- renderTable({sub_SG()}, 
                                 include.rownames = TRUE, 
                                 colnames = FALSE)
  
  
  sub_SG <- reactive ({
    sgDf <- subGroupMstr
    sgDf <- sgDf[sgDf$School.Name == input$subSchool,]
    sgDf <- subset(sgDf, select = c(Asian, Black, Latinx, Multiethnic, White, Direct.Cert, Mobility.Rate))
    setnames(sgDf , "Direct.Cert", "Direct Cert %")
    setnames(sgDf , "Mobility.Rate", "Mobility Rate %")
    sgDf[1,] <- sprintf("%.0f %%", 100*sgDf[1,]) 
    sgDf <- t(sgDf)
    sgDf <- as.data.frame(sgDf)
  })
  
  #Define Outuput Table for Impact Schools ----------------------------------
  
  proxy <- dataTableProxy('x1')
  
  map(2:11, function(num) {
    observeEvent(input[[str_glue('x{num}_cell_edit')]], {
      info = input[[str_glue('x{num}_cell_edit')]]
      i = info$row
      j = info$col
      v = as.numeric(info$value)
      x[[str_glue('df{num}')]][x[[str_glue('df{num}')]]$School.Name == isolate(input[[str_glue('iSch{num}')]]),][[i, "me_change"]] <- as.numeric(DT::coerceValue(v, x[[str_glue('df{num}')]][x[[str_glue('df{num}')]]$School.Name == isolate(input[[str_glue('iSch{num}')]]),][i, j]))
      x[[str_glue('df{num}')]][x[[str_glue('df{num}')]]$School.Name == isolate(input[[str_glue('iSch{num}')]]),][i,"me_delta"] <- x[[str_glue('df{num}')]][x[[str_glue('df{num}')]]$School.Name == isolate(input[[str_glue('iSch{num}')]]),][i,"me_change"] - x[[str_glue('df{num}')]][x[[str_glue('df{num}')]]$School.Name == isolate(input[[str_glue('iSch{num}')]]),][i,"2021"]
      
      trigger_tracker_2()
    })
    
    output[[str_glue('x{num}')]] <- renderDT({
      DT::datatable(
        x[[str_glue('df{num}')]] %>% filter(School.Name == input[[str_glue('iSch{num}')]]) %>% select(-c(0,1,2,4,5)),
        colnames = c("Grade", "Default 2020-21", "Scenario 2020-21", "Delta"), 
        filter = "none",
        selection = 'none', 
        editable = list(target = "cell", disable = list(columns = c(0,1))), #disables editing in all columns but 20-21 scenario
        options = list(pageLength = 17, paging = FALSE, searching = FALSE), #ensures all rows are visible, removes paging and search box from datatable output
        rownames=FALSE) #%>%
      #formatStyle(
      #  columns = 4,
      #  color = styleInterval(-1, c('red', 'black')),
      #  backgroundColor = styleInterval(-1, c('white', 'white'))
      # )
    })
    
    proxy <- dataTableProxy(str_glue('x{num}'))
    
    assign(str_glue('sub_SG{num}'), {
      reactive({
        sgDf <- subGroupMstr
        sgDf <- sgDf[sgDf$School.Name == input[[str_glue('iSch{num}')]],]
        sgDf <- subset(sgDf, select = c(Asian, Black, Latinx, Multiethnic, White, Direct.Cert, Mobility.Rate))
        colnames(sgDf)[which(names(sgDf) == "Direct.Cert")] <- "Direct Cert %"
        colnames(sgDf)[which(names(sgDf) == "Mobility.Rate")] <- "Mobility Rate %"
        sgDf[1,] <- sprintf("%.0f %%", 100*sgDf[1,]) 
        sgDf <- t(sgDf)
        as.data.frame(sgDf)
      })
    })
    
    output[[str_glue('sub_SGTbl{num}')]] <- renderTable({do.call(str_glue('sub_SG{num}'), list())}, include.rownames = TRUE, colnames = FALSE)
    
  })
  
  #Download Button--------------------------------------------------------------------------------
  
  output$download_Data <- downloadHandler(
    filename = function(){paste('Data_Export.xlsx')},
    content = function(file){
      
      # Create Excel workbook
      writeWorksheetToFile(file, 
                           # Add dataframes to populate the data in each sheet (1 through 11)
                           data = list(i1 = x$titleDescription, 
                                       i2 = schLvl$dlschLvlFile,
                                       i3 = x$net_scenario_change_table,
                                       i4 = schLvl$accTCntS,
                                       i5 = avg_school_char_sce$sce,
                                       i6 = school_enr_gr_sce$sce,
                                       i7 = subgroups_sce$sce,
                                       i8 = sch_count_sce$sce,
                                       i9 = sch_cost_sce$sce,
                                       i10 = schLvl_utl_sum$sce,
                                       i11 = schLvl$df,
                                       i12 = x$df1,
                                       i13 = zone_sch_char_sce$sce,
                                       i14 = zone_gr_lvl_enr_sce$sce,
                                       i15 = zone_subgroups_sce$sce,
                                       i16 = zone_fin_sce$sce
                           ),
                           # Add names to each sheet, corresponding to the data inputs above
                           sheet = c('Title & Description',
                                     'Sch Lvl Outputs',
                                     'Net Scnr Chgs',
                                     'Sch Cnts by Acc Tp',
                                     'Avg Sch Char by Sch Lvl',
                                     'Schs & Enr by Grd',
                                     'SbGrps by Fac Cndtn',
                                     'Sch Allo Sum - Sch Cnt',
                                     'Sch Allo Sum - Sch Cst',
                                     'Facilities Sum',
                                     'All Sch Lvl Scnr Data',
                                     'All Grd Lvl Scnr Data',
                                     'Sch Char by Zone',
                                     'Grd Lvl Enr by Zone',
                                     'SbGrps by Zone',
                                     'Financials by Zone'
                           ))
      
    })
  
  output$download_Data1 <- downloadHandler(
    filename = function(){paste('Data_Export.xlsx')},
    content = function(file){
      
      # Create Excel workbook
      writeWorksheetToFile(file, 
                           # Add dataframes to populate the data in each sheet (1 through 11)
                           data = list(i1 = x$titleDescription, 
                                       i2 = schLvl$dlschLvlFile,
                                       i3 = x$net_scenario_change_table,
                                       i4 = schLvl$accTCntS,
                                       i5 = avg_school_char_sce$sce,
                                       i6 = school_enr_gr_sce$sce,
                                       i7 = subgroups_sce$sce,
                                       i8 = sch_count_sce$sce,
                                       i9 = sch_cost_sce$sce,
                                       i10 = schLvl_utl_sum$sce,
                                       i11 = schLvl$df,
                                       i12 = x$df1,
                                       i13 = zone_sch_char_sce$sce,
                                       i14 = zone_gr_lvl_enr_sce$sce,
                                       i15 = zone_subgroups_sce$sce,
                                       i16 = zone_fin_sce$sce
                           ),
                           # Add names to each sheet, corresponding to the data inputs above
                           sheet = c('Title & Description',
                                     'Sch Lvl Outputs',
                                     'Net Scnr Chgs',
                                     'Sch Cnts by Acc Tp',
                                     'Avg Sch Char by Sch Lvl',
                                     'Schs & Enr by Grd',
                                     'SbGrps by Fac Cndtn',
                                     'Sch Allo Sum - Sch Cnt',
                                     'Sch Allo Sum - Sch Cst',
                                     'Facilities Sum',
                                     'All Sch Lvl Scnr Data',
                                     'All Grd Lvl Scnr Data',
                                     'Sch Char by Zone',
                                     'Grd Lvl Enr by Zone',
                                     'SbGrps by Zone',
                                     'Financials by Zone'
                           ))
      
    })
  
  # output$download_sl <- downloadHandler(
  #   filename = function(){paste('data-', Sys.Date(), '.csv', sep='')},
  #   content = function(fname){
  #     write.csv(schLvl$df, fname)
  #   })
  
  #Map reactive dataframe--------------------------------------------------------------------------------  
  output$map <- renderLeaflet({
    
    m <- leaflet(data = mDf, options = leafletOptions(minZoom = 12))%>%
      
      addProviderTiles('OpenStreetMap.Mapnik') %>%
      ## Add print function
      onRender(
        "function(el, x) {
                                            L.easyPrint({
                                              sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
                                              filename: 'Map',
                                              exportOnly: true,
                                              hideControlContainer: true
                                            }).addTo(this);
                    }"
      )
    
  })
  
  sub_mDf <- reactive({
    
    
    if (input$scenarioType == 'Default') {
      mDf<-x$mDf%>%
        filter(School.Level %in% input$schoolLevel)
      if(input$accessType!="All"){mDf<- mDf[mDf$Access.Type == input$accessType,]}
      mDf[rowSums(is.na(mDf))!=ncol(mDf),] 
    } else if (input$scenarioType == 'Scenario') {
      req(schLvl$mDf)
      mDf<-schLvl$mDf%>%
        filter(School.Level %in% input$schoolLevel)
      if(input$accessType!="All"){mDf<- mDf[mDf$Access.Type == input$accessType,]}
      mDf[rowSums(is.na(mDf))!=ncol(mDf),] 
    } else {
      stop('Error: invalid Map Scenario Type')
    }
    
    
  })
  
  
  view_lat  <- 39.765094
  view_lon  <- -86.154785
  view_zoom <- 10
  
  observe({
    
    req(input$display_tab == 'Map')
    proxy <- sub_mDf() %>% 
      left_join(icon_ids, by = c('Access.Type', 'School.Level')) %>% 
      leafletProxy("map", data = .) %>%
      
      setView(lat = view_lat, lng = view_lon, zoom = view_zoom) %>%
      
      # setMaxBounds(~min(Longitude,na.rm=TRUE), 
      # ~min(Latitude,na.rm=TRUE), 
      # ~max(Longitude,na.rm=TRUE), 
      # ~max(Latitude,na.rm=TRUE)) %>%
      
      clearGroup('schoolPoints') %>%
      clearControls()
    
    make_shape <- function(color, shape, size) {
      if (shape == 'Triangle') {
        paste0("transparent; width: 0; height: 0; border-left: ", size/2, "px solid transparent; border-right: ", size/2, "px solid transparent; border-bottom: ", size, "px solid ", color, ";")
      } else if (shape == 'TriangleDown') {
        paste0("transparent; width: 0; height: 0; border-left: ", size/2, "px solid transparent; border-right: ", size/2, "px solid transparent; border-top: ", size, "px solid ", color, ";") 
      } else if (shape == 'Square') {
        paste0(color, "; width:", size, "px; height:", size, "px; border:3px solid transparent; border-radius:0%")
      } else if (shape == 'Circle') {
        paste0(color, "; width:", size, "px; height:", size, "px; border:3px solid transparent; border-radius:50%")
      }
    }
    
    req(input$accessType)
    
    unique_school_icon_types <- sub_mDf() %>% 
      mutate(school_icon_type = paste0(Access.Type, '-', School.Level)) %>% 
      unique() %>% 
      pull()
    
    icon_ids_filtered <- icon_ids %>% 
      arrange(Access.Type, School.Level) %>% 
      rowwise() %>% 
      mutate(shapes = make_shape(color, names(shape_ids[shape_ids == shape]), size = 30),
             labels = paste0("<div style='display: inline-block;height: 30px;margin-top: 4px;line-height: 30px;'>", School.Level, ', ', Access.Type, "</div>"),
             school_icon_type = paste0(Access.Type, '-', School.Level)) %>% 
      filter(school_icon_type %in% unique_school_icon_types)
    
    if (nrow(sub_mDf()) > 0) {
      proxy <- proxy %>% 
        addMarkers(
          group = "schoolPoints",
          ~Longitude,
          ~Latitude,
          popup = ~paste("<i>",School.Name.SL,"</i>", "<br>", 
                         "<strong>","Enrollment:","</strong>", enrollment,"<br>", 
                         "<strong>","Utilization:","</strong>", scales::percent(round(Utilization, digits = 1)),"<br>",
                         "<strong>","Direct Cert:","</strong>", scales::percent(round(Direct.Cert, digits = 3)), "<br>",
                         "<strong>","School Performance:","</strong>", School.Performance,"<br>",
                         "<strong>","Combined Facility Score:","</strong>", scales::percent(round(Combined.Score/100, digits = 3))),
          icon = ~icons(
            iconUrl = iconFiles[icon_id],
            popupAnchorX = 20, popupAnchorY = 0
          )
        ) %>% addLegend(colors = icon_ids_filtered$shapes, labels = icon_ids_filtered$labels, opacity = 1)
    } 
    
    return(proxy)
  })
  
  # ### Loading Zone Variables In-------------------------------------------
  observeEvent(input$load_scenario_modal, {
    
    if (!is.null(x$zone_1_sch)) {
      school_zones <- map(1:8, function(zone) {
        x[[str_glue('zone_{zone}_sch')]]
      })
      schools_to_choose <- x$inputSchList[!x$inputSchList %in% unlist(school_zones)]
    } else {
      school_zones <- map(1:8, list(character(0)))
      schools_to_choose <- x$inputSchList
    }
    
    output$bucketlist_ui <- renderUI({
      
      #Create Zone Buckets
      zone_buckets <- map(1:8, function(zone) {
        zone_bucket <- list(
          text = str_glue('Zone {zone}'),
          labels = school_zones[[zone]],
          input_id = str_glue('zone_{zone}_sch'),
          options= sortable_options(height = 250)
        )
        class(zone_bucket) <- c('add_rank_list', 'list')
        zone_bucket
      })
      
      #Create "Schools Currently in Zones"
      zone_1_bucket <- zone_buckets[[1]]
      zone_2_bucket <- zone_buckets[[2]]
      zone_3_bucket <- zone_buckets[[3]]
      zone_4_bucket <- zone_buckets[[4]]
      zone_5_bucket <- zone_buckets[[5]]
      zone_6_bucket <- zone_buckets[[6]]
      zone_7_bucket <- zone_buckets[[7]]
      zone_8_bucket <- zone_buckets[[8]]
      
      zoned_schs <- c(zone_1_bucket$labels, zone_2_bucket$labels, zone_3_bucket$labels, zone_4_bucket$labels,
                      zone_5_bucket$labels, zone_6_bucket$labels, zone_7_bucket$labels, zone_8_bucket$labels)
      
      bucketlist_options <- list(text = HTML('Drag from here'),
                                 labels = x$inputSchList[!(x$inputSchList %in% zoned_schs)],
                                 input_id = str_glue('zone_sch_list'),
                                 options = sortable_options(height = 250),
                                 css_id = 'bucketlist_options')
      
      class(bucketlist_options) <- c('add_rank_list', 'list')
      
      bucket_args <- list(
        header = 'Drag the items in any desired bucket',
        group_name = 'bucket_list_group',
        orientation = 'horizontal',
        bucketlist_options
      )
      
      
      for (i in 1:length(zone_buckets)) {
        bucket_args <- c(bucket_args, zone_buckets[i])
      }
      
      do.call(bucket_list, args = bucket_args)
      
      
    })
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$load_googlesheets, {
    waiter_show(html = div(spin_loaders(42), h4('Loading new data. The app will restart when complete.')))
    save_google_sheet_data_to_s3()
    waiter_hide()
    js$refresh()
  })
  
}
shinyApp(ui, server)
