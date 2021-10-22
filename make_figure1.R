##########################################
### Make nice and shiny plot for paper ###
##########################################

# Purpose: To create Figure 1 for the paper.

# Libraries
library(data.table) # For data wrangling
library(lubridate) # To deal with date information
library(patchwork) # To join plots together
library(ggplot2) # For visualisations
library(viridis) # To allow for colour-blind friendly colours
library(readxl) # To load in excel files


### Set up data ###


## Load and tidy GP data ##

# Load data
files <- fs::dir_ls("./Data/CCG monthly consultations/") # Get a list of all files in the folder (each CCG is seperate)
l <- lapply(files, fread, sep = ",") # Load in monthly data on GP interactions (save as list)
ccg_consult_month <- rbindlist(l) # Convert to single table
rm(l, files) # Tidy

# Tidy up dates (some inconsistent coding)
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Apr-20"] <- "APR2020"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Apr-21"] <- "APR2021"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Aug-20"] <- "AUG2020"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Aug-21"] <- "AUG2021"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Dec-20"] <- "DEC2020"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Jan-21"] <- "JAN2021"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Feb-21"] <- "FEB2021"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Jul-20"] <- "JUL2020"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Jul-21"] <- "JUL2021"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Jun-20"] <- "JUN2020"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Jun-21"] <- "JUN2021"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Mar-20"] <- "MAR2020"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Mar-21"] <- "MAR2021"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "May-20"] <- "MAY2020"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "May-21"] <- "MAY2021"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Nov-20"] <- "NOV2020"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Oct-20"] <- "OCT2020"
ccg_consult_month$Appointment_Month[ccg_consult_month$Appointment_Month == "Sep-20"] <- "SEP2020"


## Load and tidy IMD data ##

# Load data
imd2019 <- read_excel("Data/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx", sheet = "IMD2019") # Load IMD data
lkup <- fread("./Data/LSOA_(2011)_to_Clinical_Commissioning_Groups_to_Sustainability_and_Transformation_Partnerships_(April_2021)_Lookup_in_England.csv") # Load LSOA to STP lookup
lkup <- merge(lkup, imd2019, by.x = "LSOA11CD", by.y = "LSOA code (2011)", all.x = T) # Join IMD onto lookup

# Create outcome measure
lkup$bottomq <- 0
lkup$bottomq[lkup$`Index of Multiple Deprivation (IMD) Decile` < 3] <- 1 # Define if in bottom quintile

# Aggregate data to CCGs
stp_imd <- lkup[, list(lsoas = .N, bottomq = sum(bottomq, na.rm=T), mean_rank = mean(`Index of Multiple Deprivation (IMD) Rank`, na.rm=T)), by = "STP21CD"] # Aggregate to STP
stp_imd$bottomq_pc <- (stp_imd$bottomq / stp_imd$lsoas) * 100 # Calculate measure


## Wrangle data into necessary format for overall remote plot ##

# Reshape data
ccg_consult_gp <- ccg_consult_month[ccg_consult_month$HCP_TYPE == "GP",] # Keep only GP interactions
hold <- ccg_consult_gp[, list(freq = sum(COUNT_OF_APPOINTMENTS, na.rm=T)), by = c("APPT_STATUS", "APPT_MODE", "Appointment_Month", "STP_ONS")] # Aggregate by whether attended or not, mode of appointment, date, and CCG area
ccg_consult_gp_wide <- dcast(hold, Appointment_Month + STP_ONS ~ APPT_STATUS + APPT_MODE, value.var = "freq", fill = 0) # Convert to wide (stay long ~ go wide) and fill NAs with 0s

# Create variables
ccg_consult_gp_wide$telephone_online_pc <- NA # Percentage of attended consultations that were online or telephone
ccg_consult_gp_wide$telephone_online_pc <- ((ccg_consult_gp_wide$'Attended_Video/Online' + ccg_consult_gp_wide$'Attended_Telephone') / (ccg_consult_gp_wide$'Attended_Face-to-Face' + ccg_consult_gp_wide$'Attended_Home Visit' + ccg_consult_gp_wide$'Attended_Video/Online' + ccg_consult_gp_wide$'Attended_Telephone')) * 100

ccg_consult_gp_wide$dna_in_person <- NA # Percentage of missed appointments face-to-face or at home
ccg_consult_gp_wide$dna_in_person <- ((ccg_consult_gp_wide$'DNA_Face-to-Face' + ccg_consult_gp_wide$'DNA_Home Visit') / (ccg_consult_gp_wide$'Attended_Face-to-Face' + ccg_consult_gp_wide$'Attended_Home Visit' + ccg_consult_gp_wide$'DNA_Face-to-Face' + ccg_consult_gp_wide$'DNA_Home Visit')) * 100

ccg_consult_gp_wide$dna_phone_online <- NA # Percentage of missed appointments that were online or telephone
ccg_consult_gp_wide$dna_phone_online <- ((ccg_consult_gp_wide$'DNA_Telephone' + ccg_consult_gp_wide$'DNA_Video/Online') / (ccg_consult_gp_wide$'DNA_Telephone' + ccg_consult_gp_wide$'DNA_Video/Online' + ccg_consult_gp_wide$'Attended_Video/Online' + ccg_consult_gp_wide$'Attended_Telephone')) * 100


## Create monthly average for England ##

# Create England aggregate
ccg_consult_gp_eng <- ccg_consult_gp[, list(freq = sum(COUNT_OF_APPOINTMENTS, na.rm=T)), by = c("APPT_STATUS", "APPT_MODE", "Appointment_Month")] # Aggregate by whether attended or not, mode of appointment, and date for England
ccg_consult_gp_eng_wide <- dcast(ccg_consult_gp_eng, Appointment_Month ~ APPT_STATUS + APPT_MODE, value.var = "freq", fill = 0) # Convert to wide (stay long ~ go wide) and fill NAs with 0s

# Create variables
ccg_consult_gp_eng_wide$telephone_online_pc <- NA # Percentage of attended consultations that were online or telephone
ccg_consult_gp_eng_wide$telephone_online_pc <- ((ccg_consult_gp_eng_wide$'Attended_Video/Online' + ccg_consult_gp_eng_wide$'Attended_Telephone') / (ccg_consult_gp_eng_wide$'Attended_Face-to-Face' + ccg_consult_gp_eng_wide$'Attended_Home Visit' + ccg_consult_gp_eng_wide$'Attended_Video/Online' + ccg_consult_gp_eng_wide$'Attended_Telephone')) * 100

ccg_consult_gp_eng_wide$dna_in_person <- NA # Percentage of missed appointments face-to-face or at home
ccg_consult_gp_eng_wide$dna_in_person <- ((ccg_consult_gp_eng_wide$'DNA_Face-to-Face' + ccg_consult_gp_eng_wide$'DNA_Home Visit') / (ccg_consult_gp_eng_wide$'Attended_Face-to-Face' + ccg_consult_gp_eng_wide$'Attended_Home Visit' + ccg_consult_gp_eng_wide$'DNA_Face-to-Face' + ccg_consult_gp_eng_wide$'DNA_Home Visit')) * 100

ccg_consult_gp_eng_wide$dna_phone_online <- NA # Percentage of missed appointments that were online or telephone
ccg_consult_gp_eng_wide$dna_phone_online <- ((ccg_consult_gp_eng_wide$'DNA_Telephone' + ccg_consult_gp_eng_wide$'DNA_Video/Online') / (ccg_consult_gp_eng_wide$'DNA_Telephone' + ccg_consult_gp_eng_wide$'DNA_Video/Online' + ccg_consult_gp_eng_wide$'Attended_Video/Online' + ccg_consult_gp_eng_wide$'Attended_Telephone')) * 100


# Define month as date variable #
ccg_consult_gp_wide$date <- floor_date(dmy(paste("01", ccg_consult_gp_wide$Appointment_Month)), unit = "month") + 14 # Fix date at middle of each month
ccg_consult_gp_wide <- ccg_consult_gp_wide[order(ccg_consult_gp_wide$date)] # Order by date
ccg_consult_gp_eng_wide$date <- floor_date(dmy(paste("01", ccg_consult_gp_eng_wide$Appointment_Month)), unit = "month") + 14
ccg_consult_gp_eng_wide <- ccg_consult_gp_eng_wide[order(ccg_consult_gp_eng_wide$date)] # Order by date

# Join on IMD data
ccg_consult_gp_wide <- merge(ccg_consult_gp_wide, stp_imd, by.x = "STP_ONS", by.y = "STP21CD", all.x = T)


## Tidy data to examine time to appointment ##

# Reshape data
ccg_consult_gp_att <- ccg_consult_month[ccg_consult_month$HCP_TYPE == "GP" & ccg_consult_month$APPT_STATUS == "Attended",] # Keep only attended GP interactions
ccg_consult_gp_att$remote <- NA
ccg_consult_gp_att$remote[ccg_consult_gp_att$APPT_MODE == "Telephone" | ccg_consult_gp_att$APPT_MODE == "Video/Online"] <- "Remote"
ccg_consult_gp_att$remote[ccg_consult_gp_att$APPT_MODE == "Face-to-Face" | ccg_consult_gp_att$APPT_MODE == "Home Visit"] <- "Face-to-Face"

hold <- ccg_consult_gp_att[, list(freq = sum(COUNT_OF_APPOINTMENTS, na.rm=T)), by = c("TIME_BETWEEN_BOOK_AND_APPT", "remote", "Appointment_Month", "STP_ONS")] # Aggregate by whether attended or not, mode of appointment, date, and CCG area
ccg_consult_gp_att_wide <- dcast(hold, Appointment_Month + STP_ONS ~ TIME_BETWEEN_BOOK_AND_APPT + remote, value.var = "freq", fill = 0) # Convert to wide (stay long ~ go wide) and fill NAs with 0s

# Create variables for mapping
ccg_consult_gp_att_wide$total_remote <- ccg_consult_gp_att_wide$`Same Day_Remote` + ccg_consult_gp_att_wide$`1 Day_Remote` + ccg_consult_gp_att_wide$`2 to 7 Days_Remote` + ccg_consult_gp_att_wide$`8  to 14 Days_Remote` + ccg_consult_gp_att_wide$`15  to 21 Days_Remote` + ccg_consult_gp_att_wide$`22  to 28 Days_Remote` + ccg_consult_gp_att_wide$`More than 28 Days_Remote`
ccg_consult_gp_att_wide$pc_same_day_remote <- (ccg_consult_gp_att_wide$`Same Day_Remote` / ccg_consult_gp_att_wide$total_remote) * 100

ccg_consult_gp_att_wide$total_f2f <- ccg_consult_gp_att_wide$`Same Day_Face-to-Face` + ccg_consult_gp_att_wide$`1 Day_Face-to-Face` + ccg_consult_gp_att_wide$`2 to 7 Days_Face-to-Face` + ccg_consult_gp_att_wide$`8  to 14 Days_Face-to-Face` + ccg_consult_gp_att_wide$`15  to 21 Days_Face-to-Face` + ccg_consult_gp_att_wide$`22  to 28 Days_Face-to-Face` + ccg_consult_gp_att_wide$`More than 28 Days_Face-to-Face`
ccg_consult_gp_att_wide$pc_same_day_f2f <- (ccg_consult_gp_att_wide$`Same Day_Face-to-Face` / ccg_consult_gp_att_wide$total_f2f) * 100

## Calculate England average ##

# Aggregate to England
ccg_consult_gp__att_eng <- ccg_consult_gp_att[, list(freq = sum(COUNT_OF_APPOINTMENTS, na.rm=T)), by = c("TIME_BETWEEN_BOOK_AND_APPT", "remote", "Appointment_Month")] # Aggregate by whether time for appointment, mode of appointment, and date for England
ccg_consult_gp_att_eng_wide <- dcast(ccg_consult_gp__att_eng, Appointment_Month ~ TIME_BETWEEN_BOOK_AND_APPT + remote, value.var = "freq", fill = 0) # Convert to wide (stay long ~ go wide) and fill NAs with 0s

# Create variables
ccg_consult_gp_att_eng_wide$total_remote <- ccg_consult_gp_att_eng_wide$`Same Day_Remote` + ccg_consult_gp_att_eng_wide$`1 Day_Remote` + ccg_consult_gp_att_eng_wide$`2 to 7 Days_Remote` + ccg_consult_gp_att_eng_wide$`8  to 14 Days_Remote` + ccg_consult_gp_att_eng_wide$`15  to 21 Days_Remote` + ccg_consult_gp_att_eng_wide$`22  to 28 Days_Remote` + ccg_consult_gp_att_eng_wide$`More than 28 Days_Remote`
ccg_consult_gp_att_eng_wide$pc_same_day_remote <- (ccg_consult_gp_att_eng_wide$`Same Day_Remote` / ccg_consult_gp_att_eng_wide$total_remote) * 100 # same day remote appointment

ccg_consult_gp_att_eng_wide$total_f2f <- ccg_consult_gp_att_eng_wide$`Same Day_Face-to-Face` + ccg_consult_gp_att_eng_wide$`1 Day_Face-to-Face` + ccg_consult_gp_att_eng_wide$`2 to 7 Days_Face-to-Face` + ccg_consult_gp_att_eng_wide$`8  to 14 Days_Face-to-Face` + ccg_consult_gp_att_eng_wide$`15  to 21 Days_Face-to-Face` + ccg_consult_gp_att_eng_wide$`22  to 28 Days_Face-to-Face` + ccg_consult_gp_att_eng_wide$`More than 28 Days_Face-to-Face`
ccg_consult_gp_att_eng_wide$pc_same_day_f2f <- (ccg_consult_gp_att_eng_wide$`Same Day_Face-to-Face` / ccg_consult_gp_att_eng_wide$total_f2f) * 100 # same day f2f appointment

ccg_consult_gp_att_eng_wide$total_same_day <- ccg_consult_gp_att_eng_wide$total_f2f + ccg_consult_gp_att_eng_wide$total_remote
ccg_consult_gp_att_eng_wide$pc_same_da <- ((ccg_consult_gp_att_eng_wide$`Same Day_Face-to-Face` + ccg_consult_gp_att_eng_wide$`Same Day_Remote`) / ccg_consult_gp_att_eng_wide$total_same_day) * 100 # same day appointment total


## Define month as date variable ##

ccg_consult_gp_att_wide$date <- floor_date(dmy(paste("01", ccg_consult_gp_att_wide$Appointment_Month)), unit = "month") + 14 # Fix date at middle of each month
ccg_consult_gp_att_wide <- ccg_consult_gp_att_wide[order(ccg_consult_gp_att_wide$date)] # Order by date
ccg_consult_gp_att_eng_wide$date <- floor_date(dmy(paste("01", ccg_consult_gp_att_eng_wide$Appointment_Month)), unit = "month") + 14
ccg_consult_gp_att_eng_wide <- ccg_consult_gp_att_eng_wide[order(ccg_consult_gp_att_eng_wide$date)] # Order by date

# Join on IMD data
ccg_consult_gp_att_wide <- merge(ccg_consult_gp_att_wide, stp_imd, by.x = "STP_ONS", by.y = "STP21CD", all.x = T)


### Make plots ###

## Figure 1a ##

# Add labels for national lockdowns as reference lines
text_plot <- data.frame(text = c("Lockdown 1", "Lockdown 2", "Lockdown 3"), date = as.Date(c("2020-03-23", "2020-11-05", "2021-01-06")), stringsAsFactors = FALSE) 

# Plot
figure1a <- ggplot() +
  geom_path(data = ccg_consult_gp_wide, aes(x = date, y = telephone_online_pc, group = STP_ONS, color = bottomq_pc), alpha = 0.4) + # Plot CCGs
  geom_path(data = ccg_consult_gp_eng_wide, aes(x = date, y = telephone_online_pc), size=1) + # Plot England average
  geom_vline(mapping = aes(xintercept = date), data = text_plot, show.legend = FALSE, linetype = "dotted") + # Add in reference lines
  geom_text(mapping = aes(x = date, y = 0, label = text), data = text_plot, angle = 90, vjust = 1.5, hjust = 0, size = 3) + # Add labels for reference lines
  scale_x_date(date_breaks = "5 months", date_labels =  "%b %Y") + # Define x-axis as date format
  scale_color_viridis() + # Make colour-blind friendly
  xlab("Date") + # Add labels
  ylab("Remote consultations (%)") +
  ylim(0, 100) + # Restrict range of values to be plot
  theme(legend.position="bottom") +
  labs(title = "Percent of appointments which are remote",
       color = "Percentage of LSOAs in bottom quintile")
figure1a # Print

## Figure 1b ##

# Save plot for in-person consultations
figure1b <- ggplot() +
  geom_path(data = ccg_consult_gp_att_wide, aes(x = date, y = pc_same_day_f2f, group = STP_ONS, color = bottomq_pc), alpha = 0.4) +
  geom_path(data = ccg_consult_gp_att_eng_wide, aes(x = date, y = pc_same_day_f2f), size=1) +
  geom_vline(mapping = aes(xintercept = date), data = text_plot, show.legend = FALSE, linetype = "dotted") +
  geom_text(mapping = aes(x = date, y = 0, label = text), data = text_plot, angle = 90, vjust = 1.5, hjust = 0, size = 3) +
  scale_x_date(date_breaks = "5 months", date_labels =  "%b %Y") +
  scale_color_viridis() + # Make colour-blind friendly
  xlab("Date") +
  ylab("Same day (%)") +
  ylim(0, 100) +
  theme(legend.position="bottom") +
  labs(title = "Same day: face-to-face",
       color = "Percentage of LSOAs in bottom quintile")
figure1b

## Figure 1c ##

# Save plot for remote consultations
figure1c <- ggplot() +
  geom_path(data = ccg_consult_gp_att_wide, aes(x = date, y = pc_same_day_remote, group = STP_ONS, color = bottomq_pc), alpha = 0.4) +
  geom_path(data = ccg_consult_gp_att_eng_wide, aes(x = date, y = pc_same_day_remote), size=1) +
  geom_vline(mapping = aes(xintercept = date), data = text_plot, show.legend = FALSE, linetype = "dotted") +
  geom_text(mapping = aes(x = date, y = 0, label = text), data = text_plot, angle = 90, vjust = 1.5, hjust = 0, size = 3) +
  scale_x_date(date_breaks = "5 months", date_labels =  "%b %Y") +
  scale_color_viridis() + # Make colour-blind friendly
  xlab("Date") +
  ylab("Same day (%)") +
  ylim(0, 100) +
  theme(legend.position="bottom") +
  labs(title = "Same day: remote",
       color = "Percentage of LSOAs in bottom quintile")
figure1c

## Join together into Figure 1 ##

# Join plots together
figure1_plot <- figure1a / (figure1b + figure1c) + # Join these plots
  plot_annotation(tag_levels = 'A') + # Add in labels to plots
  plot_layout(guides = 'collect') & theme(legend.position="bottom") # Use same legend for all and place at bottom
figure1_plot # Print

# Save
ggsave(plot = figure1_plot, filename = "./figure1_highres.tiff", dpi = 300) # High res version
ggsave(plot = figure1_plot, filename = "./figure1_lowres.jpeg") # Low res version



