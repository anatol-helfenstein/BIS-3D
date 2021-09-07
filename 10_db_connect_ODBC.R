#------------------------------------------------------------------------------
# Name:     10_db_connect_ODBC.R
#
# Content:  - connect to BIS (currently version 7.4) Oracle database (DB) using
#             oracle client basic and odbc driver
#           - query DB using dbplyr* syntax
#
# Inputs:   - none
#
# Output:   - none
#
# Project   BIS+ 
# Author:   Anatol Helfenstein
# Date:     February 2020

# * dbplyr pkg: A 'dplyr' back end for databases that allows you to work with
# remote database tables as if they are in-memory data frames. Basic features
# works with any database that has a 'DBI' back end; more advanced features
# require 'SQL' translation to be provided by the package author.

# [...] removed for data privacy reasons



### install & load required packages ---------------------------------------

pkgs <- c("tidyverse", "here", "DBI", "odbc", "dbplyr") #"ROracle"
lapply(pkgs, library, character.only = TRUE)



### connect to BIS database (DB) --------------------------------------------

# Overview of how to access DB in R:
# https://db.rstudio.com/getting-started/
# DBI is an interface, ODBC (or JDBC, ROracle) is the implementation
# see difference btw. JDBC and ODBC here:
# https://techdifferences.com/difference-between-jdbc-and-odbc.html

# setting up oracle client basic and odbc driver (carefully follow instructions):
# oracle.com/nl/database/technologies/releasenote-odbc-ic.html
# Load oracle odbc drivers one of two ways:

# 1) Assuming oracle client basic and odbc driver (these need to be installed in
# the same directory!) were unzipped in /opt/oracle/ (on Ubuntu OS),
# run /opt/oracle/odbc_update_ini.sh / (this is because default directory is in /etc/,
# so argument ODBCDM_Home = / recognizes that the ini file is indeed in /etc/)
# open code of odbc_update_ini.sh script and adjust and name parameters accordingly

# 2) Do not use odbc_update_ini.sh and just set up /etc/odbc.ini, /etc/odbcinst.ini
# and ~/.odbc.ini yourself; then check drivers in Rscript (below)

# Verify that odbc recognizes the installed drivers
drv <- unique(odbcListDrivers()$name)[1]

# check if R session is running using R i386 (32bit) or 64 bit & choose Oracle client accordingly
# in Win OS, there are oracle clients for 32 bit and others for 64 bit
Sys.info()
Sys.getenv("R_ARCH")

# Test if we can connect using a complete connection string
# Details of connection are in tnsnames.ora file (host, port, etc.)
DBI::dbCanConnect(odbc::odbc(),
                  .connection_string = paste0("Driver={", drv, "};DBQ=[...];UID=[...];PWD=[...]"),
                  timeout = 10)

# Connect to BIS DB using login credentials
odbc_con <- DBI::dbConnect(odbc::odbc(),
                           Driver = drv,
                           DBQ = "[...]",
                           UID = rstudioapi::askForPassword("Database user"),
                           PWD = rstudioapi::askForPassword("Database password"))



### Explore DB & query data -------------------------------------------------

dbListTables(odbc_con) # or db_list_tables(odbc_con) for dplyr version

# Write code in dplyr syntax, and dplyr will translate your code into SQL
# dplyr syntax is easy to read, but can always inspect SQL translation with show_query() fnc
tbl(odbc_con, "BEP") %>% 
  show_query()

# one of the huge advantages of dplyr is that it does all the work on the DB memory
# and only pulls it into R if you explicitly ask for it (e.g. when printing an object)
# But even then it only pulls part of the data (see example above)
# this can be seen in that nrow() is always NA and tail() gives an error
tbl(odbc_con, "BODEM") %>% 
  nrow()

tbl(odbc_con, "BODEM") %>% 
  tail()

# can also ask DB how it plans to execute query using explain()
tbl(odbc_con, "BODEM") %>% 
  select(BODEM_C) %>% 
  explain()

# dbReadTable() (not dplyr) will read a full table into an R data.frame().
dbReadTable(odbc_con, "BEP") %>% 
  as_tibble()

# dbWriteTable() writes R data.frame() to SQL table (not sure if I have writable rights)
# data <- dbWriteTable(odbc_con, "iris", iris)

# there are several schemata besides the default schema "BISTEST" we've worked in so far
# these can be accessed using in_schema() fnc from the "dbplyr" pkg
tbl(odbc_con, in_schema("BRO", "LANDUSE"))



### Close DB connection -----------------------------------------------------

dbDisconnect(odbc_con)


