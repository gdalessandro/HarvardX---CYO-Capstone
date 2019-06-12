########################################################
#
# HARVARDX CAPSTONE PROJECT II
#
# SURVIVING THE TITANIC
#
# BY GUIDO D'ALESSANDRO
#
########################################################



## ----global-r-options----------------------------------------------------

DBG_LEVEL_RLS <- 0 # Release
DBG_LEVEL_FST <- 1 # FAST Training but NO print outs
DBG_LEVEL_PRT <- 2 # FAST Training and print outs
R___DEBUG <- DBG_LEVEL_RLS

# Turn script warning off (markdown
# handles its own in chunck options)
saved_warn_level <- getOption("warn")
if ( R___DEBUG == DBG_LEVEL_RLS )
	options( warn = -1 )

# Save the script start time
start_time <- Sys.time()

# Save the session information
session_info <- sessionInfo()

options(str=strOptions(vec.len = 1), width=80)
options(repos=structure(c(CRAN="https://cran.mtu.edu/")))
knitr::opts_chunk$set(fig.align="center", echo=FALSE, eval=TRUE, highlight=TRUE, 
  message=FALSE, warning=FALSE, comment="", include=TRUE, tidy=TRUE, size="small", results="markup")


## ----libraries-----------------------------------------------------------

# Check if needed libraries are installed
installed_packages <- rownames( installed.packages() )
checkInstallPackage <- function(pckg) {
  if ( !(pckg %in% installed_packages) ) {
    install.packages(pckg)
  }
}
	
# dslabs
checkInstallPackage("dslabs")
checkInstallPackage("dslabs")

# ggrepel
checkInstallPackage("gbm")

# ggrepel
checkInstallPackage("ggrepel")

# gridExtra
checkInstallPackage("gridExtra")

# kableExtra
checkInstallPackage("kableExtra")

# knitr
checkInstallPackage("knitr")

# lattice
checkInstallPackage("lattice")

# lubridate
checkInstallPackage("lubridate")

# MASS
checkInstallPackage("MASS")

# rpart
checkInstallPackage("rpart")

# stringr
checkInstallPackage("stringr")

# tidyverse
checkInstallPackage("tidyverse")

# tinytex
if ( !("tinytex" %in% installed_packages) ) {
  # Requires a set path or reboot
  install.packages("tinytex")
  tinytex::install_tinytex()
}


## ----global-variables----------------------------------------------------
# Add Libraries
library(tidyverse)

# Save the script start time
script_start <- proc.time()

# Save the session information
session_info <- sessionInfo()

# Working directory
raw_work_dir <- "~/titanic/"
work_dir     <- normalizePath(raw_work_dir, winslash="/")

local_data_dir <- normalizePath(paste(raw_work_dir, "data", sep=""),     winslash="/")
local_csv_dir  <- normalizePath(paste(raw_work_dir, "data/csv", sep=""), winslash="/")
local_rds_dir  <- normalizePath(paste(raw_work_dir, "data/rds", sep=""), winslash="/")

local_rel_csv_dir  <- "./data/csv"
local_rel_rds_dir  <- "./data/rds"

# Used data files
dataset_files_dataset <- c("data/train.rds", "data/test.rds")
dataset_files_working <- c("data/train.rds", "data/test.rds", "data/control.rds", "data/submit.csv")
train_set_ndx   <- 1
test_set_ndx    <- 2
control_set_ndx <- 3
submit_set_ndx  <- 4


# Kaggle Data description
# From: https://www.kaggle.com/c/titanic/overview
#   and https://www.kaggle.com/c/titanic/data 
#
kaggle_login_url <- "https://www.kaggle.com/account/login"

kaggle_man_url   <- "https://www.kaggle.com/c/titanic/data"
#kaggle_zip_url   <- "https://www.kaggle.com/c/3136/download-all"

kaggle_train_url <- "https://www.kaggle.com/c/titanic/download/train.csv"
kaggle_test_url  <- "https://www.kaggle.com/c/titanic/download/test.csv"

known_surv_rate    <- 1502/2224

kaggle_train_rows  <- 891
kaggle_train_cols  <- 12
kaggle_test_rows   <- 418
kaggle_test_cols   <- 11

kaggle_train_col_names <- c("PassengerId", "Survived", "Pclass", "Name",
                            "Sex",         "Age",      "SibSp",  "Parch",
                            "Ticket",      "Fare",     "Cabin",  "Embarked")

kaggle_test_col_names <- c("PassengerId",  "Pclass",   "Name",    "Sex",
                            "Age",         "SibSp",    "Parch",   "Ticket",
							              "Fare",        "Cabin",    "Embarked")

# Age group descriptions
age_group_descripts  <- c("0-12 mos", "1-9 yrs",   "10-19 yrs", "20-29 yrs", "30-39 yrs",
                          "40-49 yrs", "50-59 yrs", "60-69 yrs", "70-79 yrs", "80-89 yrs")

# First 11 Fare group descriptions
fare_group_descripts  <- c("$0-49", "$50-99",   "$100-149", "$150-199", "$200-249",
                          "$250-299", "$300-349", "$350-399", "$400-449", "$450-499", "$500-549")

# Floating precisions
rnd_detail <- 4
rnd_quick  <- 2


## ----global-functions----------------------------------------------------

# Check for RDS files w/o path hassle
f_rdsFileExists <- function(file) {
  
  # Create a Windows full path name
  f  <- paste(local_rds_dir, "/", file, sep="")
  fp <- normalizePath(f, winslash="\\")
  
  return( file.exists(fp) )
}


# Check for CSV files w/o path hassle
f_csvFileExists <- function(file) {
  
  # Create a Windows full path name
  f  <- paste(local_csv_dir, "/", file, sep="")
  fp <- normalizePath(f, winslash="\\")
  
  return( file.exists(fp) )
}


# Create sub-directory if not present
# Check exists folder and create if not present
f_checkCreatePath <- function(pd, sd)
{
  nd <- normalizePath(pd, winslash="/")
	if ( !file.exists(sd) )
	{
		dir.create( file.path(nd, sd) )
	}
}


# Create the data directories if needed
f_CreateDataDir <- function() 
{
	# Save working directory
	sd <- getwd()

	# set wd with currwent working dir
	wd <-normalizePath(raw_work_dir, winslash="\\")

	# create sub-dir "data of wd if does not exists
	f_checkCreatePath(wd, "data")
	
	dd <- normalizePath(local_data_dir, winslash="\\")
	setwd(dd)

	# create sub-dir "data of wd if does not exists
	f_checkCreatePath(dd, "csv")

	# create sub-dir "data of wd if does not exists
	f_checkCreatePath(dd, "rds")
	
	setwd(sd)
}


# Save given file as RDS files w/o path hassle
f_rdsSave <- function(tbl, file_name) {
  
  # Create a Windows full path name
  f  <- paste(local_rds_dir, "/", file_name, sep="")
  fp <- normalizePath(f, winslash="\\")
  
  saveRDS(tbl, file=fp)
}


# Read RDS file w/o path hassle
f_rdsRead <- function(file) {
  
  # Create a Windows full path name
  f  <- paste(local_rds_dir, "/", file, sep="")
  fp <- normalizePath(f, winslash="\\")
  
  readRDS(fp)
}


# Read CVS file w/o path hassle
f_csvRead <- function(file) {
  
  # Create a Windows full path name
  f  <- paste(local_csv_dir, "/", file, sep="")
  fp <- normalizePath(f, winslash="\\")
  
  read.csv(fp, sep=",", quote="\"")
}


f_prepEmptyTable <- function(tbl, filler) {

	t <- tbl

	lstr <- seq(nrow(t), 2)
	lstc <- seq(ncol(t), 2)
	track <- sapply(lstr, function(r) {
		for (c in lstc) {
			if ( !is.na(t[r, c]) & (t[r, c]!="") & ((t[r, c] == t[r-1, c]) & (t[r, 1] == t[r-1, 1])) ) {
				t[r, c] <<- filler
			}
		}
		if ( !is.na(t[r, 1]) & (t[r, 1]!="") & (t[r, 1] == t[r-1, 1]) ) {
			t[r, 1] <<- filler
		}
		r
	})
	return( t )
}


#################################
# Call this early to set project
# directory structure and turn
# script warning off
#

# Create data directories
f_CreateDataDir()
#


## ----remote-read-files---------------------------------------------------
# Read URL files

# Training dataset
if ( !f_csvFileExists("train.csv") )
  train_set <- read.csv(url(kaggle_train_url), sep=",", quote="\"")

# Testing dataset
if ( !f_csvFileExists("test.csv") )
  test_set  <- read.csv(url(kaggle_test_url),  sep=",", quote="\"")


## ----save-files----------------------------------------------------------

# For the training dataset
if ( !f_rdsFileExists("train.rds") ) {
  
  # Read the CSV
  train_set <- f_csvRead("train.csv")
  
  # Save as compact RDS
  f_rdsSave(train_set, "train.rds")
  
} else {  # RDS Exists
  
  # Read the RDS
  train_set <- f_rdsRead("train.rds")
}

# Do the same for the testing dataset
if ( !f_rdsFileExists("test.rds") ) {
  test_set <- f_csvRead("test.csv")
  f_rdsSave(test_set, "test.rds")
} else {
  test_set <- f_rdsRead("test.rds")
}


## ----data-struct-inspections--------------------------------------------------

# For the Training set
str(train_set)

# For the Testing set
str(test_set)


## ----check-dimensions----------------------------------------------------
# Verify the read sets match the kaggle documented dimensions:

# For training
if ( kaggle_train_rows != nrow(train_set) |
     kaggle_train_cols != ncol(train_set) ) {
  noquote("The dimensions of the downloaded training file do not match Kaggle's....")
  noquote("Aborting, repair this issue and try again")
} else {
  noquote( 
    paste("Downloaded train file and Kaggle's file dimensions (", 
          kaggle_train_rows, " rows x ", kaggle_train_cols, " columns) match!", sep=""))
}

# For testing
if ( kaggle_test_rows != nrow(test_set) |
     kaggle_test_cols != ncol(test_set) ) {
  noquote("The dimensions of the downloaded testing file do not match Kaggle's....")
  noquote("Aborting, repair this issue and try again")
} else {
  noquote( 
    paste("Downloaded test file and Kaggle's file dimensions (",
          kaggle_test_rows, " rows x ", kaggle_test_cols, " columns)match!", sep=""))
}


## ----check-col-names-----------------------------------------------------

# Verify the column names in the read sets match the kaggle documented names

# For training
if ( sum(kaggle_train_col_names != colnames(train_set)) > 0 ) {
  noquote("The column names of the downloaded test file do not match Kaggle's...")
  noquote("Aborting, repair this issue and try again")
} else {
  noquote("Downloaded train file column names match Kaggle's!")
}

# For testing (if here, col_match still TRUE)
if ( sum(kaggle_test_col_names != colnames(test_set)) > 0 ) {
  noquote("The column names of the downloaded test file do not match Kaggle's...")
  noquote("Aborting, repair this issue and try again")
} else {
  noquote("Downloaded test file column names match Kaggle's!")
}

## ------------------------------------------------------------------------
## ----check-na-values-----------------------------------------------------

# In training file
train_na <- colnames(train_set)[colSums(is.na(train_set)) > 0]
train_na

# In testing file
test_na <- colnames(test_set)[colSums(is.na(test_set)) > 0]
test_na


## ----check-empty-values--------------------------------------------------

# MAKE SURE NA.RM = TRUE
# colSums return NA if found and na.rm = FALSE

# For training file 
train_em <- colnames(train_set)[colSums(train_set=="", na.rm=TRUE) > 0]
train_em

# In testing file
test_em <- colnames(test_set)[colSums(test_set=="", na.rm=TRUE) > 0]
test_em


## ----missing-values-summary----------------------------------------------

library(dplyr)

tbl <- data.frame(Table = "train_set", "NA" = train_na, Empty = train_em)
tbl <- bind_rows(tbl, 
       data.frame(Table = "_____",  "NA" = "",  Empty = ""))
tbl <- bind_rows(tbl, 
       data.frame(Table = "test_set",  "NA" = test_na,  Empty = test_em))

tbl <- f_prepEmptyTable(tbl, "")
tbl %>% knitr::kable()


## ----duplicated-rows-----------------------------------------------------

# train_set
tr_dup <- nrow( train_set[duplicated(train_set["PassengerId"]),] )

# test_set
ts_dup <- nrow( test_set[duplicated(test_set["PassengerId"]),] )

bind_rows(data.frame(Table="train_set", Duplicates=tr_dup),
          data.frame(Table="train_set", Duplicates=ts_dup)) %>% 
  knitr::kable(caption="Duplicated Rows")

if ( tr_dup | ts_dup ) {
  noquote("There are duplicates, a determination of row removal is needed.")
} else {
  noquote("There are no duplicates, so a determination of row removal is not needed.")
}


## ----cross-duplication---------------------------------------------------

tbl <- inner_join(test_set, train_set, by="PassengerId")
nr <- nrow(tbl)
noquote(
  paste("There are ", nr, " passengers whose fate is known",
        ifelse(nr>0,
               "--these can be excluded from prediction.",
               "--nothing left to do here."),  sep=""))


## ----data-summary--------------------------------------------------------

noquote("-- TRAINING DATASET SUMMARY:")
# For the training dataset
train_set %>% summary()
noquote("-- TESTING DATASET SUMMARY:")
# For the test dataset
test_set %>% summary()


data.frame(First=1, Second=2, Third=3) %>% knitr::kable(caption="Ticket Class Values")

## ----check-pclass-column-------------------------------------------------

temp <- train_set %>%  
  group_by(Pclass) %>% 
  mutate(pc_avg=mean(Survived))

first <- round(100*temp$pc_avg[temp$Pclass==1][1], 0)
secnd <- round(100*temp$pc_avg[temp$Pclass==2][1], 0)
third <- round(100*temp$pc_avg[temp$Pclass==3][1], 0)

temp %>% 
  ggplot(aes(Pclass, pc_avg)) + 
  geom_point(color="#5050ff", size=1, shape=15)+
  geom_smooth(color="darkgray", size=0.5, method="glm") +
  labs(x = "Ticket Class", y = "Survival Rate") +
	ggtitle("Survival Rate  vs. Ticket Class")


## ----pclass-correlation--------------------------------------------------

noquote(
  paste("Correlation =",
        round(cor(x=temp$Pclass, y=temp$pc_avg), rnd_detail)))


## ----overall-survival-by-class-------------------------------------------

nr = nrow(train_set)
temp <- train_set %>%  
  group_by(Pclass) %>% 
  mutate(pc_avg = sum(Survived) / nr)

first <- round(100*temp$pc_avg[temp$Pclass==1][1], 1)
secnd <- round(100*temp$pc_avg[temp$Pclass==2][1], 1)
third <- round(100*temp$pc_avg[temp$Pclass==3][1], 1)

temp %>% 
  ggplot(aes(Pclass, pc_avg)) + 
  geom_point(color="#5050ff", size=1, shape=15)+
  geom_smooth(color="darkgray", size=0.5, method="glm") +
  labs(x = "Ticket Class", y = "Survival Rate") +
	ggtitle("Overall Survival Rate  vs. Ticket Class")

noquote(
  paste("Survival Rates: First=",first,"%, second=", secnd, "%, and third=", third, "%.", sep="" ))


## ----select-table--------------------------------------------------------

sel_table <- data.frame(Column = "Pclass", Type = "Integer", "Use As" = "Factor", 
                        "Req.Work" = "Y", "Work Type" = "TC", "Description" = "To factor", "Req Alt"="N")
sel_table %>% knitr::kable(caption="Variable Analysis Table")


## ----check-sex-column----------------------------------------------------

temp <- train_set %>%  
  group_by(Sex) %>% 
  mutate(sex_avg=mean(Survived))

mp <- round(100*temp$sex_avg[temp$Sex =="male"  ][1], 0)
fp <- round(100*temp$sex_avg[temp$Sex =="female"][1], 0)

temp %>% 
  ggplot(aes(Sex, sex_avg)) + 
  geom_point(color="#5050ff", size=1, shape=15)+
  geom_smooth(aes(x=as.integer(Sex)), color="darkgray", size=0.5, method="glm") +
  labs(x = "Passenger Sex", y = "Survival Rate") +
	ggtitle("Survival Rate  vs. Passenger Sex")


## ----sex-correlation-----------------------------------------------------

noquote( 
  paste("Correlation =", 
        round(cor(x=as.integer(temp$Sex), y=temp$sex_avg), rnd_detail)))


## ----overall-sex-survival------------------------------------------------

temp <- train_set %>%  
  group_by(Sex) %>% 
  mutate(sex_avg=sum(Survived)/nr)

mp <- round(100*temp$sex_avg[temp$Sex =="male"  ][1], 0)
fp <- round(100*temp$sex_avg[temp$Sex =="female"][1], 0)

temp %>% 
  ggplot(aes(Sex, sex_avg)) + 
  geom_point(color="#5050ff", size=1, shape=15)+
  geom_smooth(aes(x=as.integer(Sex)), color="darkgray", size=0.5, method="glm") +
  labs(x = "Passenger Sex", y = "Survival Rate") +
	ggtitle("Overall Survival Rate  vs. Passenger Sex")

noquote( 
  paste("Correlation =", 
        round(cor(x=as.integer(temp$Sex), y=temp$sex_avg), rnd_detail)))


## ----add-sex-to-selection-table------------------------------------------

sel_table <- bind_rows(sel_table, 
              data.frame(Column = "Sex", Type = "Factor", "Use As" = "As is", 
                         "Req Work"="N", "Work Type" = "", Description = "", "Req Alt"="N"))
                     
sel_table %>% knitr::kable(caption = "Updated Variable Analysis Table")


## ----check-age-column----------------------------------------------------
tr_na <- nrow(train_set %>% filter( is.na(Age) ))
tr_em <- nrow(train_set %>% filter( Age == "" ))
ts_na <- nrow(test_set %>% filter( is.na(Age) ))
ts_em <- nrow(test_set %>% filter( Age == "" ))
data.frame("Train NA"=tr_na, "Train Empty"=tr_em, "Train Total"=tr_na+tr_em,
           "Test NA"=ts_na, "Test Empty"=ts_em, "Test Total"=ts_na+ts_em) %>%
  knitr::kable(caption="Age Missing Values")


## ----reporting-age-groups------------------------------------------------

temp <- train_set %>%  
  filter( !is.na(Age) ) %>% 
  mutate(age_group = as.factor( ifelse(Age<1, 0, floor(Age/10) + 1)) ) %>%
  group_by(age_group) %>% 
  mutate( avg_rate=mean(Survived) )

grps <- levels( temp$age_group )
surv <- sapply(grps, function(group){
  ndx <- as.integer(group) + 1
  return( round( 100 * mean(temp$avg_rate[temp$age_group == grps[ndx]]), rnd_quick) )
})

data.frame("Age Group"=grps, Age=age_group_descripts, "Survival Rate"=surv, row.names=1:10) %>% knitr::kable(caption = "Age Groups in train_set")

infants <- nrow( temp %>% filter(age_group == 0) )
old     <- nrow( temp %>% filter(age_group == 8) )
oldest  <- nrow( temp %>% filter(age_group == 9) )


## ----plotting-age-groups-------------------------------------------------

temp %>% 
  ggplot(aes(age_group, avg_rate)) + 
  geom_point(color="#5050ff", size=1, shape=15)+
  geom_smooth(aes(x=as.integer(age_group)), color="darkgray", size=0.5, method="glm") +
  labs(x = "Passenger Age Group", y = "Survival Rate") +
	ggtitle("Survival vs. Passenger Age (All Groups)")

temp2 <- train_set %>%  
  filter( !is.na(Age) & Age>1 & Age < 70) %>% 
  mutate(age_group = as.factor(ifelse(Age <= 1, 0, floor(Age/10) + 1))) %>%
  group_by(age_group) %>% 
  mutate( avg_rate=mean(Survived) )

temp2 %>% 
  ggplot(aes(age_group, avg_rate)) + 
  geom_point(color="#5050ff", size=1, shape=15)+
  geom_smooth(aes(x=as.integer(age_group)), color="darkgray", size=0.5, method="glm") +
  labs(x = "Passenger Age Group", y = "Survival Rate") +
	ggtitle("Survival vs. Passenger (1 < Age < 70)")

temp3 <- train_set %>%  
  filter( !is.na(Age) & Age > 1 ) %>% 
  mutate(age_group = as.factor( ifelse(Age>=70, 4, floor(Age/20) + 1))) %>%
  group_by(age_group) %>% 
  mutate( avg_rate=mean(Survived) )

temp3 %>% 
  ggplot(aes(age_group, avg_rate)) + 
  geom_point(color="#5050ff", size=1, shape=15)+
  geom_smooth(aes(x=as.integer(age_group)), color="darkgray", size=0.5, method="glm") +
  labs(x = "Passenger Age Group", y = "Survival Rate") +
	ggtitle("Survival vs. Passenger Age (x20yrs)")

## ----age-groups-correlation-with-survival-rates--------------------------

noquote( 
  paste("Correlation =", 
        round(cor(x=as.integer(temp3$age_group), y=temp3$avg_rate), rnd_detail)))

#noquote( 
  #paste("Covariance =", 
        #round(cov(x=as.integer(temp3$age_group), y=temp3$avg_rate), rnd_detail)))


## ----adding-age-to-selection-table---------------------------------------

sel_table <- bind_rows(sel_table, 
          data.frame(Column = "Age", Type = "Integer", "Use As" = "As is", 
                     "Req Work"="Y", "Work Type" = "CL", "Description" = "To bins", "Req Alt"=""))
sel_table <- bind_rows(sel_table, 
          data.frame(Column = "", Type = "Integer", "Use As" = "As factor", 
                     "Req Work"="Y", "Work Type" = "TC", "Description" = "To factor", "Req Alt"=""))
sel_table <- bind_rows(sel_table, 
          data.frame(Column = "", Type = "Factor", "Use As" = "As is", 
                     "Req Work"="Y", "Work Type" = "VE", "Description" = "Exclude group", "Req Alt"="Y"))
					 
sel_table %>% knitr::kable(caption="Updated Variable Analysis Table")


## ----table-with-random-siblings-spouse-----------------------------------

set.seed(11, sample.kind = "Rounding")
ndx <- caret::createDataPartition(y=train_set$PassengerId, times=1, p=0.01, list=FALSE)
train_set[ndx, ] %>% dplyr::select(PassengerId, Name, SibSp) %>%
  knitr::kable(caption="Random sample of rows to show SibSp values")

## ----check-sibsp-column--------------------------------------------------

# Survival rates for SipSp
temp <- train_set %>% 
  mutate(ss_group = as.factor(SibSp)) %>%
  group_by(ss_group) %>% 
  mutate( avg_rate=mean(Survived) )

# Plot SipSp vs survival rate
temp %>% 
  ggplot(aes(ss_group, avg_rate)) + 
  geom_point(color="#5050ff", size=1, shape=15)+
  geom_smooth(aes(x=as.integer(ss_group)), color="darkgray", size=0.5, method="glm") +
  geom_smooth(aes(x=as.integer(ss_group)), color="#800000", size=0.25, method="gams") +
  labs(x = "Number of Siblings/Spouse Aboard", y = "Survival Rate") +
	ggtitle("Survival vs. Number of Siblings/Spouse")

# Exclude passengers traveling alone
temp2 <- train_set %>% 
  filter(SibSp > 0) %>%
  mutate(ss_group = as.factor(SibSp)) %>%
  group_by(ss_group) %>% 
  mutate( avg_rate=mean(Survived) )

# Plot accompanied passengers vs survival rate
temp2 %>% 
  ggplot(aes(ss_group, avg_rate)) + 
  geom_point(color="#5050ff", size=1, shape=15)+
  geom_smooth(aes(x=as.integer(ss_group)), color="darkgray", size=0.5, method="glm") +
  labs(x = "Number of Siblings/Spouse", y = "Survival Rate") +
	ggtitle("Survival vs. SibSp Accompanied Passengers")


## ----correlation-between-SipSp-and-survival-rate-------------------------

noquote( 
  paste("Correlation =", 
        round(cor(x=as.integer(temp2$ss_group), y=temp2$avg_rate), rnd_detail)))


## ----add-sibsp-column-to-selection-table---------------------------------

sel_table <- bind_rows(sel_table, 
          data.frame(Column = "SibSp", Type = "Integer", "Use As" = "As factor", 
                     "Req Work"="Y", "Work Type" = "TC", "Description" = "To factor", "Req Alt"=""))
sel_table <- bind_rows(sel_table, 
          data.frame(Column = "", Type = "Factor", "Use As" = "As is", 
                     "Req Work"="Y", "Work Type" = "VE", "Description" = "Exclude group", "Req Alt"="N"))
sel_table %>% knitr::kable(caption="Updated Variable Analysis Table")

## ----check-parent-children-column----------------------------------------

# Survival rates for Parch
temp <- train_set %>% 
  mutate(pch_group = as.factor(Parch)) %>%
  group_by(pch_group) %>% 
  mutate( avg_rate=mean(Survived) )

# Plor survival rates vs Parch
temp %>% 
  ggplot(aes(pch_group, avg_rate)) + 
  geom_point(color="#5050ff", size=1, shape=15)+
  geom_smooth(aes(x=as.integer(pch_group)), color="darkgray", size=0.5, method="glm") +
  geom_smooth(aes(x=as.integer(pch_group)), color="#800000", size=0.25) +
  labs(x = "Number of Parents/Children Aboard", y = "Survival Rate") +
	ggtitle("Survival vs. Number of Parent/Children")

# Exclude passengers traveling alone
temp2 <- train_set %>% 
  filter(Parch > 0) %>%
  mutate(pch_group = as.factor(Parch)) %>%
  group_by(pch_group) %>% 
  mutate( avg_rate=mean(Survived) )

# Plot for accompanied passengers only
temp2 %>% 
  ggplot(aes(pch_group, avg_rate)) + 
  geom_point(color="#5050ff", size=1, shape=15)+
  geom_smooth(aes(x=as.integer(pch_group)), color="darkgray", size=0.5, method="glm") +
  labs(x = "Number of Parents/Children Aboard", y = "Survival Rate") +
	ggtitle("Survival vs. Parch Accompanied Passengers Only")


## ----correlation-between-Parch-and-survival-rate-------------------------

noquote( 
  paste("Correlation =", 
        round(cor(x=as.integer(temp2$Pclass), y=temp2$avg_rate), rnd_detail)))


## ----Parch-vs-age----------------------------------------------

train_set %>%  
  filter( !is.na(Age) ) %>% 
  mutate(pch_group = as.factor(Parch), age_group = as.factor(ifelse(Age < 1, 0, floor(Age/10) + 1))) %>%
  group_by(pch_group)%>% 
  mutate(grp_count = n()) %>%
  ggplot(aes(pch_group, Age, fill=grp_count, color="khaki"))+ 
  geom_boxplot(outlier.colour = "#404040")+
  guides(color=FALSE)+
  labs(x = "Number of Parents/Children Aboard", y = "Age", fill = "Group Count\n(Passengers)")+
	ggtitle("Number of Parents/Children vs. Age")

## ----adding-parch-to-selection-------------------------------------------

sel_table <- bind_rows(sel_table, 
  data.frame(Column = "Parch", Type = "Integer", "Use As" = "As factor",
             "Req.Work" = "", "Work Type" = "TC", "Description" = "To factor", "Req Alt"=""))
sel_table <- bind_rows(sel_table, 
  data.frame(Column = "", Type = "Factor", "Use As" = "As is",
             "Req.Work" = "", "Work Type" = "VE", "Description" = "Exclude group", "Req Alt"="N"))
sel_table %>% knitr::kable(caption="Updated Variable Analysis Table")

## ----check-ticket-number-------------------------------------------------

temp <- train_set %>% 
  mutate(tk_group = as.factor(Ticket)) %>%
  group_by(tk_group) %>% 
  mutate( avg_rate=mean(Survived) )

temp %>% 
  ggplot(aes(tk_group, avg_rate)) + 
  geom_point(color="lightgray", size=1, shape=0)+
  geom_smooth(aes(x=as.integer(tk_group)), color="darkred", size=0.5, method="glm") +
  labs(x = "Ticket Number", y = "Survival Rate") +
	ggtitle("Survival vs. Ticket Number")

## ----missing-fare-rows---------------------------------------------------

# Count missing value rows in test area
mv <- test_set %>% 
  filter( is.na(Fare) ) %>% nrow()

## ----checking-fare-vs-survival-------------------------------------------

# Count missing value rows in test area
mv <- test_set %>% 
  filter( is.na(Fare) ) %>% nrow()

temp <- train_set %>% 
  filter( !is.na(Fare) )

avg_fare <- mean( temp$Fare )

less_pct <- round(100 * nrow(temp %>% filter(Fare < avg_fare ))/nrow(temp), rnd_quick)

temp %>% 
  ggplot(aes(Fare, fill=as.factor(Pclass))) + 
  geom_histogram(binwidth=10)+
  geom_vline(xintercept=avg_fare, color="darkred") +
  labs(x = "Fare", y = "Count", fill = "Ticket Class") +
	ggtitle("Count vs. Fare and Ticket Class")

temp %>% 
  ggplot(aes(Fare)) + 
  geom_histogram(binwidth=10, aes(fill=..count..)) +
  geom_vline(xintercept=35, color="darkred") +
  labs(x = "Fare", y = "Count", fill = "Fare No. Payers") +
	ggtitle("Count vs. Fare and Count")


## ----grouping-fare-in-10-dollar-increments-------------------------------

temp <- train_set %>% 
  filter( !is.na(Fare) ) %>%
  mutate(fare_group = as.factor( floor(Fare/10) )) %>%
  group_by(fare_group) %>%
  mutate(avg_rate=mean(Survived), fare_count=n())

temp %>% 
  ggplot(aes(as.integer(fare_group), avg_rate)) + 
  geom_point(color="darkgray", size=1, shape=15)+
  geom_smooth(color="darkred", size=0.5, method="glm") +
  #geom_smooth(color="darkblue", size=0.5) +
  labs(x = "Fare", y = "Survival Rate") +
	ggtitle("Survival Rate vs. Fare")

# Number of Fare missing values in the test dataset
fare_mv <- nrow( test_set %>% filter( is.na(Fare) ) %>% 
  dplyr::select(PassengerId, Pclass, Name, Fare) )
noquote(paste("Missing Fare values in the test dataset =", fare_mv))

## ----add-fare-column-to-selection----------------------------------------

# Add this column and the work to be done in the selection table
sel_table <- bind_rows(sel_table, 
  data.frame(Column = "Fare", Type = "Number", "Use As" = "As is", 
    "Req.Work" = "", "Work Type" = "CL", "Description" = "To bins", "Req Alt"=""))
sel_table <- bind_rows(sel_table, 
  data.frame(Column = "", Type = "", "Use As" = "As factor", 
    "Req.Work" = "", "Work Type" = "TC", "Description" = "To factor", "Req Alt"="Y"))
sel_table %>% knitr::kable(caption="Updated Variable Analysis Table")


## ----checking-empty-cabin-columns----------------------------------------

# For Training set, count missing values in Cabin column
tmp1 <- train_set %>% filter( is.na(Cabin) | Cabin == "" )
mv1 <- nrow(tmp1)
noquote(paste("Train set missing values in Cabin column =", mv1))

# For testing set, count missing values in Cabin column
tmp2 <- test_set  %>% filter( is.na(Cabin) | Cabin == "" )
mv2 <- nrow(tmp2)
noquote(paste("Test set missing values in Cabin column =", mv2))

## ----sample-cabin-values-------------------------------------------------

# For Training set, sample Cabin values
tmp1 <- train_set %>% 
  filter( !is.na(Cabin) & Cabin != "" & str_length(toString(Cabin)) > 4 )
tmp1 %>% dplyr::select(PassengerId, Name, Cabin) %>%
  head(n=10) %>% knitr::kable(caption="Train set: Sample Cabin Values")

# For testing set, sample Cabin values
tmp2 <- test_set %>% filter( !is.na(Cabin) & Cabin != "" & str_length(toString(Cabin)) > 4 )
tmp2 %>% dplyr::select(PassengerId, Name, Cabin) %>%
  head(n=10) %>% knitr::kable(caption="Test set: Sample Cabin Values")


## ----checking-embarked-missing-values------------------------------------

temp <- train_set %>% 
  filter( is.na(Embarked) | Embarked == "" )
mv <- nrow(temp)

# Report
noquote(paste("Embarked Column Missing Values in Training Set =", mv))

## ----plotting-embarkment-vs-survival-------------------------------------

# Get survival rate for embarkment ports
temp <- train_set %>% 
  filter( !is.na(Embarked) & Embarked != "" ) %>%
  mutate( em_no = ifelse(Embarked=='C', 1, ifelse(Embarked=='Q',2,3)) ) %>%
  group_by( em_no ) %>% 
  mutate( avg_rate=mean(Survived) )

# Plot it
temp %>% 
  ggplot(aes(em_no, avg_rate)) + 
  geom_point(color="darkgray", size=1, shape=15)+
  geom_smooth(size=0.5, method="glm") +
  labs(x = "Embarkation Port", y = "Survival Rate") +
	ggtitle("Survival vs. Embarkation Port")

## ----adding-embarkment-to-selection--------------------------------------

# Add this column and the work to be done in the selection table
sel_table <- bind_rows(sel_table, 
  data.frame(Column = "Embarked", Type = "Factor", "Use As" = "As is", 
    "Req.Work" = "N", "Work Type" = "", "Description" = "", "Req Alt"="Y"))
sel_table %>% knitr::kable(caption="Updated Variable Analysis Table")


## ----print-selection-----------------------------------------------------

sel_table %>% knitr::kable(caption="Final Variable Analysis Table Results")

## ----save-the-worksets---------------------------------------------------

# The training set workset 
tr_workset <- train_set
f_rdsSave(tr_workset, "tr_workset.rds")

# The test set workset 
ts_workset <- test_set
f_rdsSave(ts_workset, "ts_workset.rds")

## ----calc-number-of-infant-passengers------------------------------------

# In the test dataset
ts_infants <- ts_workset %>% filter(Age <= 1)
ts_infants %>% dplyr::select(PassengerId, Pclass, Sex, 
                      Age, SibSp, Parch, Embarked)
no_ts_infants <- nrow(ts_infants)

# In the training dataset
tr_infants <- tr_workset %>% filter(Age <= 1)
tr_infants %>% dplyr::select(PassengerId, Pclass, Sex, 
                      Age, SibSp, Parch, Embarked)
no_tr_infants <- nrow(tr_infants)

## ----retrieve-seventy-plus-age-passengers--------------------------------

tr70 <- tr_workset %>% filter(Age >69) %>%arrange(desc(Age),Pclass,Embarked)
tr70 %>% dplyr::select(PassengerId, Pclass, Sex,
                 Age, SibSp, Parch, Embarked)
				 
# Print list of passenger ids and their names (age>69yrs)
tr70 %>% dplyr::select(PassengerId, Name)

## ----seventy-plus-age-group-by-sex-report--------------------------------

# 70+-years-old in test set
ts70 <- ts_workset %>% filter(Age >69) %>%
  arrange(desc(Age),Pclass,Embarked)

ts70 %>% dplyr::select(PassengerId, Pclass, Sex,
                Age, SibSp, Parch, Embarked)

# Print their Id, name and age 70+-year-ols in test set
ts70 %>% dplyr::select(PassengerId, Age, Name)

# Create variables for text reporting
ts70_cnt <- nrow(ts70)
ts70_age <- "NA"
ts70_sex <- "NA"
if ( ts70_cnt > 1 ) {
  ts70_age <- "70+"
  if ( ("male"%in%ts70$Sex) & ("female"%in%ts70$Sex) ) {
    ts70_sex <- "mixed-sex"
  } else if ( "male"%in%ts70$Sex ) {
    ts70_sex <- "male(s)"
  } else {
    ts70_sex <- "female(s)"
  }
} else if ( ts70_cnt == 1 ) {
  ts70_age <- ts70$Age[1]
  ts70_sex <- ts70$Sex[1]
}

# full-join of 70+-years-olds... looking
# for the husband (or sibling) of
# Mrs Tyrell William Cavendish (Julia Florence Siegel) SibSp
# only female surviver > 69 yrs

full <- ts_workset %>% full_join(tr_workset) 
full %>% filter("Cavendish" %in% Name)
full %>% filter("Siegel" %in% Name)


## ----age-groups-creation-------------------------------------------------

## Notice we are placing Age NA's into 10yr bins

# For the training set
tr_workset <- tr_workset %>%
  mutate(age_group = 
           as.factor(ifelse(is.na(Age), 10, ifelse(Age < 1, 0, floor(Age/10) + 1))))

#And save it
f_rdsSave(tr_workset, "tr_workset.rds")

# For the test set
ts_workset <- ts_workset %>%
  mutate(age_group = 
           as.factor(ifelse( is.na(Age), 10, ifelse(Age < 1, 0, floor(Age/10) + 1))))

#And save it
f_rdsSave(ts_workset, "ts_workset.rds")

## ----plotting-fare-without-missing-values--------------------------------

ts_workset %>% 
  filter( !is.na(Fare) & Fare != "") %>%
  ggplot(aes(PassengerId, Fare)) + geom_point(size=1, shape=1)

## ----plot-fare-bins------------------------------------------------------

# Mean group value
temp <- ts_workset %>% filter( !is.na(Fare) & Fare != "")
mu_fare <- mean(floor(temp$Fare/10))

# $10 binned groups
ts_workset %>% mutate(fare_group=ifelse(is.na(Fare) | Fare=="", 100, floor(Fare/10))) %>%
  group_by(fare_group) %>% ggplot( aes(fare_group, fill=Pclass)) +
  geom_histogram(binwidth=1) + geom_vline(xintercept=mu_fare, color="darkkhaki")

## ----bin-classes-for-fare-groups-----------------------------------------

# -- Train set binning Fare into $10 bins
tr_workset <- tr_workset %>%
  mutate(fare_group =
           as.factor(ifelse(is.na(Fare), 100, floor(Fare/10))))

#And save it
f_rdsSave(tr_workset, "tr_workset.rds")

# -- Test set$10 binned groups
ts_workset <- ts_workset %>%
  mutate(fare_group =
           as.factor(ifelse(is.na(Fare), 100, floor(Fare/10))))

#And save it
f_rdsSave(ts_workset, "ts_workset.rds")


## ----datasets-structural-report------------------------------------------

noquote("-- TRAINING WORKSET STRUCTURE:")

# For the training dataset
str(tr_workset)

noquote("-- TESTING WORKSET STRUCTURE:")

# For the test dataset
str(ts_workset)
noquote("---------- END OF STRUCTURES -")


## ----dataset-summary-reports---------------------------------------------

noquote("-- TRAIN WORKSET SUMMARY:")

# For the training dataset
train_set %>% summary()

noquote("-- TEST WORKSET SUMMARY:")

# For the test dataset
test_set %>% summary()
noquote("---------- END OF SUMMARIES -")


## ----create-age-and-fare-groups------------------------------------------

# The Cabin Column has so many empty fields
# in training data that we have to remove
# them from a copy of the training dataset

# Training set max and (NA or Empty)
# variables to show in text

tr        <- tr_workset
tr$Cabin  <- NULL
tr_mv_vec <- apply(tr, 1, function(x) sum(is.na(x) | x==""))
tr_max    <- max( tr_mv_vec )
tr_na_set <- tr[tr_mv_vec > 1,]
tr_rows   <- nrow( tr_na_set )

# Same for Testing...
ts        <- ts_workset
ts$Cabin  <- NULL
ts_mv_vec <- apply(ts, 1, function(x) sum(is.na(x) | x==""))
ts_max    <- max( ts_mv_vec )
ts_na_set <- ts[ts_mv_vec > 1,]
ts_rows   <- nrow( ts_na_set )


## ----display-missing-value-for-age-and-fare------------------------------

# Calculation for Test set only (remember we stuck na ages in group 10, and na fares in group 100)
head( ts %>% filter( (is.na(Age) & is.na(Fare)) | (age_group == 10 & fare_group == 100) ) )


## ----remove-unused-columns-----------------------------------------------

# For the training set
tr_final        <- tr_workset
tr_final$Name   <- NULL
tr_final$Ticket <- NULL
tr_final$Cabin  <- NULL

# And Save it
f_rdsSave(tr_final, "tr_final.rds")

# For the testing set
ts_final        <- ts_workset
ts_final$Name   <- NULL
ts_final$Ticket <- NULL
ts_final$Cabin  <- NULL

# And Save it
f_rdsSave(ts_final, "ts_final.rds")


## ----prepare-model-specific-training-datasets----------------------------

# Remove rows with empty values for training main model
## We have to remove embarkment because it is one of the selected
## predictors for main, but it is not missing values in the test set

# Create the set for training the main model
tr_main <- tr_final %>% 
  mutate(lived=as.factor(Survived)) %>% 
  filter(!is.na(Age) & !is.na(Fare) & (Embarked != ""))

# Report the number of rows for training the main model
noquote(paste("After removing missing value rows, the main set was left with", nrow(tr_main), "rows."))

# Remove rows with empty values for Fare to train age model
# have to remove empty fare because it is used by age model
# (and Embarked)

tr_age <- tr_final %>% 
  mutate(lived=as.factor(Survived)) %>% 
  filter( !is.na(Fare) & (Embarked != ""))
  
# Report number of rows available to train age model
noquote(paste("After removing missing value rows, the age set was left with", nrow(tr_age), "rows for training."))

# Remove rows with empty values for Fare to train fare model
# have to remove empty ages because it is used by fare model
# (and Embarked)

tr_fare <- tr_final %>% 
  mutate(lived=as.factor(Survived)) %>% 
  filter( !is.na(Age) & (Embarked != "") )
  
# Report number of rows available to train fare model
noquote(paste("After removing missing value rows, the fare set was left with", nrow(tr_fare), "rows for training."))

# And Save them
f_rdsSave(tr_main, "tr_main.rds")
f_rdsSave(tr_age, "tr_age.rds")
f_rdsSave(tr_fare, "tr_fare.rds")

## ----main-knn-validation-------------------------------------------------

library(caret)

# define training control (this control will be used by all models)
t_ctrl<- trainControl(method="repeatedcv", number=10, repeats=10, savePredictions = TRUE)

# Quicker version for debug
if ( R___DEBUG > 0 ) {
  # Quicker version for debuging
  t_ctrl<- trainControl(method="repeatedcv", number=3, repeats=1, savePredictions = TRUE)
}

# seed and train the model 
library(gbm)
set.seed(11, sample.kind = "Rounding")
Model_knn <- train(lived ~ Pclass+Sex + Age + SibSp + Parch + Fare +
                       Embarked + age_group + fare_group, 
                   data=tr_main, trControl = t_ctrl, method="knn")


## ----main-gbm-validation-------------------------------------------------

library(gbm)

# seed and train the model
set.seed(11, sample.kind = "Rounding")
Model_gbm <- train(lived ~ Pclass+Sex + Age + SibSp + Parch + Fare +
                       Embarked + age_group + fare_group, 
                   data=tr_main, trControl = t_ctrl, method="gbm", verbose=FALSE)

## ----main-glm-validation-------------------------------------------------

# seed and train the model
set.seed(11, sample.kind = "Rounding")
Model_glm <- train(lived ~ Pclass+Sex + Age + SibSp + Parch + Fare +
                       Embarked + age_group + fare_group, 
                    data=tr_main, trControl=t_ctrl, 
                    method="glm", family="binomial")


## ----main-nn-validation--------------------------------------------------

library(MASS)

# seed the model
set.seed(11, sample.kind = "Rounding")

# train the model
if (R___DEBUG > 0 ) {
  # Quicker Debug
  Model_nn <- train(lived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                   Embarked + age_group + fare_group,
                  data=tr_main, trControl=t_ctrl, method = "nnet", maxit = 100, 
                  trace=FALSE)
} else {
  # Slower release
  Model_nn <- train(lived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + age_group + fare_group,
                    data=tr_main, trControl=t_ctrl, method = "nnet", maxit = 1000, 
                    trace=FALSE)
}


## ----main-rpart-validation-----------------------------------------------

library(rpart)

# seed and train the model 
set.seed(11, sample.kind = "Rounding")
Model_rpart <- train(lived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + age_group + fare_group,
                     data=tr_main, trControl=t_ctrl, method="rpart")


## ----age-knn-validation--------------------------------------------------

library(caret)

# seed and train the model
set.seed(11, sample.kind = "Rounding")
Model_aknn <- train(lived ~ Pclass + Sex + SibSp + Parch + Fare +
                       Embarked + fare_group,
                    data=tr_age, trControl=t_ctrl, method="knn")


## ----age-gbm-validation--------------------------------------------------

library(gbm)

# seed and train the model
set.seed(11, sample.kind = "Rounding")
Model_agbm <- train(lived ~ Pclass + Sex + SibSp + Parch + Fare +
                       Embarked + fare_group, 
                   data=tr_age, trControl = t_ctrl,
                   method="gbm", verbose=FALSE)


## ----age-glm-validation--------------------------------------------------

# train the model 
set.seed(11, sample.kind = "Rounding")
Model_aglm <- train(lived ~ Pclass + Sex + SibSp + Parch + Fare +
                       Embarked + fare_group, 
                     data=tr_age, trControl=t_ctrl, 
                     method="glm", family="binomial")


## ----age-nn-validation---------------------------------------------------

library(MASS)

# seed the model 
set.seed(11, sample.kind = "Rounding")

# train the model
if (R___DEBUG > 0) {
Model_ann <- train(lived ~ Pclass + Sex + SibSp + Parch + Fare +
                    Embarked + fare_group,
                   data=tr_age, trControl=t_ctrl, method = "nnet", maxit = 100, 
                   trace=FALSE)
} else {
  Model_ann <- train(lived ~ Pclass + Sex + SibSp + Parch + Fare +
                       Embarked + fare_group,
                     data=tr_age, trControl=t_ctrl, method = "nnet", maxit = 1000, 
                     trace=FALSE)
}


## ----age-rpart-validation------------------------------------------------

library(rpart)

# seed and train the model
set.seed(11, sample.kind = "Rounding")
Model_arpart <- train(lived ~ Pclass + Sex + SibSp + Parch + Fare +
                       Embarked + fare_group,
                      data=tr_age, trControl=t_ctrl, method="rpart")


## ----fare-knn-validation-------------------------------------------------

library(caret)

# seed and train the model
set.seed(11, sample.kind = "Rounding")
Model_fknn <- train(lived ~ Pclass + Sex + SibSp + Parch + Age +
                       Embarked + age_group, 
                    data=tr_fare, trControl=t_ctrl, method="knn")


## ----fare-gbm-validation-------------------------------------------------

library(gbm)

# seed and train the model
set.seed(11, sample.kind = "Rounding")
Model_fgbm <- train(lived ~ Pclass + Sex + SibSp + Parch + Age +
                       Embarked + age_group, 
                   data=tr_fare, trControl = t_ctrl,
                   method="gbm", verbose=FALSE)


## ----fare-glm-validation-------------------------------------------------

# seed and train the model
set.seed(11, sample.kind = "Rounding")
Model_fglm <- train(lived ~ Pclass + Sex + SibSp + Parch + Age +
                       Embarked + age_group, 
                     data=tr_fare, trControl = t_ctrl, 
                     method="glm", family="binomial")


## ----fare-nn-validation--------------------------------------------------

library(MASS)

# seed the model
set.seed(11, sample.kind = "Rounding")

# train the model
if ( R___DEBUG > 0 ) {
Model_fnn <- train(lived ~ Pclass + Sex + SibSp + Parch + Age +
                    Embarked + age_group,
                   data=tr_fare, trControl=t_ctrl, method = "nnet", maxit = 100, 
                   trace=FALSE)
} else {
  Model_fnn <- train(lived ~ Pclass + Sex + SibSp + Parch + Age +
                       Embarked + age_group,
                     data=tr_fare, trControl=t_ctrl, method = "nnet", maxit = 1000, 
                     trace=FALSE)
}


## ----fare-rpart-validation-----------------------------------------------

library(rpart)

# seed and train the model 
set.seed(11, sample.kind = "Rounding")
Model_frpart <- train(lived ~ Pclass + Sex + SibSp + Parch + Age +
                       Embarked + age_group, 
                      data=tr_fare, trControl=t_ctrl, method="rpart")


## ----main-models-resample------------------------------------------------

# We are going to benchmark all methods for each model
# by running the resamples() function
# Here we benchmark the main methods

# collect resamples
m_list <- list(KNN   = Model_knn, 
               GBM   = Model_gbm,
               GLM   = Model_glm,
               NN    = Model_nn,
               RPART = Model_rpart)

# Report performances                         
m_res <- resamples( m_list )

# summarize the resample results
#noquote("---- Summary:")
summary(m_res)

# boxplots of results
bwplot(m_res)


## ----age-models-resample-------------------------------------------------

# We are going to benchmark all methods for each model
# by running the resamples() function
# Here we benchmark the age methods

# collect resamples
a_list <- list(KNN   = Model_aknn, 
               GBM   = Model_agbm,
               GLM   = Model_aglm, 
               NN    = Model_ann, 
               RPART = Model_arpart )

# Report performances                
a_res <- resamples( a_list )

# summarize the resample results
#noquote("---- Summary:")
summary(a_res)

# boxplots of results
bwplot(a_res)


## ----fare-models-resample------------------------------------------------

# We are going to benchmark all methods for each model
# by running the resamples() function
# Here we benchmark the fare methods

# collect resamples
f_list <- list(KNN   = Model_fknn,  
               GBM   = Model_fgbm,
               GLM   = Model_fglm,
               NN    = Model_fnn,
               RPART = Model_frpart)

# Report performances
f_res <- resamples( f_list )

# summarize the resample results
#noquote("---- Summary:")
summary(f_res)

# boxplots of results
bwplot(f_res)


## ----best-methods-by-highest-mean-accuracy-------------------------------

# Find best model methods

methods  <- c("KNN", "GBM", "GLM", "NN", "RPART")

m_models <- c("Model_knn",  "Model_gbm",  "Model_glm",  "Model_nn",  "Model_rpart")
a_models <- c("Model_aknn", "Model_agbm", "Model_aglm", "Model_ann", "Model_arpart")
f_models <- c("Model_fknn", "Model_fgbm", "Model_fglm", "Model_fnn", "Model_frpart")

# Function to retrieve best method
# (complicated navigation of S# objects)

f_bestMethod <- function(res) {

  acc <- sapply(methods, function(m){
    stat <- paste("res$values$\"", m, "~", "Accuracy\"", sep="")
    return( mean(eval(parse(text=stat))) )
  })
  
  # Sort accuracy descending so highest in first row for easy return
  df <- data.frame(Method=methods, Accuracy=acc) %>% arrange(desc(Accuracy))
  return(df[1,]) # returning first row, which is the method with highest mean Accuracy
}

# Calculate for each model
m_best <- f_bestMethod(m_res)
a_best <- f_bestMethod(a_res)
f_best <- f_bestMethod(f_res)

# Retrieve each model's best method by name
m_name <- m_models[which(methods == m_best$Method[1])]
a_name <- a_models[which(methods == a_best$Method[1])]
f_name <- f_models[which(methods == f_best$Method[1])]


## ----display-best-methods------------------------------------------------

# Show the best methods in a nice kable table
sry_methods <- bind_rows(bind_rows(m_best, a_best), f_best)
rownames(sry_methods) <- c("Main Model", "Age Model", "Fare Model")
sry_methods %>% knitr::kable(caption="Best Performing Methods")


## ----print-the-best-methods-details--------------------------------------

eval(parse(text=paste(m_name,"$finalModel",sep="")))
eval(parse(text=paste(a_name,"$finalModel",sep="")))
eval(parse(text=paste(f_name,"$finalModel",sep="")))


## ----overall-accuracy----------------------------------------------------

# The prediction data sets -- we need these numbers
ts_main <- (ts_final %>% filter( !is.na(Age) & !is.na(Fare)))
ts_age  <- (ts_final %>% filter(  is.na(Age)))
ts_fare <- (ts_final %>% filter(  is.na(Fare)))

# nrow(ts_main) pedicted with main model, 
# nrow(ts_age) with the age model and 
# nrow(ts_fare) with the fare model
# such that nrow(test_set) == nrow(ts_main) + 
# nrow(ts_age) + nrow(ts_fare)

# Overall accuracy is weighted depending on how many
# many rows we are going to predict with each model

sys_accuracy <- (nrow(ts_main)*mean( eval(parse(text=paste(m_name,"$results$Accuracy",sep=""))) ) + 
                 nrow(ts_age )*mean( eval(parse(text=paste(a_name,"$results$Accuracy",sep=""))) ) + 
                 nrow(ts_fare)*mean( eval(parse(text=paste(f_name,"$results$Accuracy",sep=""))) )) / nrow(test_set)

# Report overall weighted accuracy
noquote(paste("Expected overall accuracy predicting the test dataset = ", round(sys_accuracy,rnd_detail)))


## ----predict-the test_dataset----------------------------------------------

# The predictions
p_main <- as.data.frame( predict(Model_gbm,  ts_main) )
p_age  <- as.data.frame( predict(Model_agbm, ts_age ) )
p_fare <- as.data.frame( predict(Model_fgbm, ts_fare) )

# For Main
ts_main$Survived <- p_main[,1]
if ( R___DEBUG > 1 )  {
  summarize(ts_main)
  head( ts_main %>% dplyr::select(PassengerId, Survived) )
}

# For Age
ts_age$Survived <- p_age[,1]
if ( R___DEBUG > 1 ) {
  summarize(ts_age)
  head( ts_age %>% dplyr::select(PassengerId, Survived) )
}

# For Fare
ts_fare$Survived <- p_fare[,1]
if ( R___DEBUG > 1 ) {
  summarize(ts_fare)
  head( ts_fare %>% dplyr::select(PassengerId, Survived) )
}

# Final result
ts_res <- full_join(ts_main, ts_age) %>% 
  full_join(ts_fare) %>% 
  dplyr::select(PassengerId, Survived)

# Nice table for first 10 predictions (in total 418, wont print that!)
head( ts_res, n=10 ) %>% knitr::kable(caption="First 10 predictions")

# SHow structure of frame that it coincides
# in size with what we have to predict

str(ts_res)


## ----dump-system-info----------------------------------------------------

# Dump system (R) information
session_info


## ----print-execution-time------------------------------------------------

## Create a small function for this
f_lapsedTime <- function(start) {

	# find time difference (in seconds)
	elapsed <- difftime(Sys.time(),start, units="secs")

	# select correct units
	t_unit <- "secs."
	if ( elapsed >= 3600 ) {
		elapsed <- round(elapsed/3600, 2)
		units   <- "hours"
	} else if ( elapsed > 60 ) {
	  elapsed <- round(elapsed/60, 2)
	  t_unit <- "mins."
	} else {
	  elapsed <- round(elapsed, 0)
	}
	
	# Retun a formated string with units
	return( paste(elapsed, t_unit) )
}

# report
noquote( paste("Total execution time:", f_lapsedTime(start_time)) )


## ----restore-warning-level-----------------------------------------------

# Restore warning level
options( warn = saved_warn_level )

## -----------------------------------------------------------------------

