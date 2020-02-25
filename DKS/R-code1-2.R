#Packages needed for R-Code-1#

library(RPostgreSQL)
library(devtools)
library(remotes)
library(RPostgres)


#PACKAGES needed for R-CODE-2 #

library(sodium)


# R-CODE-1 #

# Establishing basic connection with the database
#1. Defining the database password
pw<- {"@Yogya55252"}


#2. Defining the driver and set up the connection
drv<- dbDriver("PostgreSQL")

db_name <- "AKSARA"  #provide the name of your db
host_db <- "localhost" #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  
db_port <- "5432"  # or any other port specified by the DBA
db_user <- "postgres" #your db username 
db_password <- "@Yogya55252"  #your personal password

con <- dbConnect(RPostgres::Postgres(), dbname = "AKSARA", host="localhost",
                 port="5432", user="postgres", password="@Yogya55252")



#3. Checking the existance of our tables
dbExistsTable(con, "user_profile")


#4. Reading the table from postgre into R
user_prof <- dbReadTable(con,c("user_profile"))
attach(user_prof)


#5. Disconnect DB
dbDisconnect(con)


#R-CODE-2 #


#1. Login page confirmation #
credentials = data.frame(
  username_id = c("dks"),
  passod   = sapply(c("cute123"),password_store), 
  stringsAsFactors = F
)


#Version 1#
#https://www.listendata.com/2019/06/how-to-add-login-page-in-shiny-r.html#


login=FALSE

if(login == FALSE) {
  if(!is.null(login)) {
    if(login > 0) {
      Username <- isolate(username)
      Password <- isolate(password)
      if(length(which(credentials$username_id==username))==1) { 
        pasmatch  <- credentials["passod"][which(credentials$username_id==username),]
        pasverify <- password_verify(pasmatch, password)
        if(pasverify) {
        login <- TRUE
        }else{
          print("Login unsuccessfull")
        }
        }
      }
    }
}



# 2. Creating a new folder that will be containing users' activities, scenarios and intervention points


# 3. List of users' activities #
userid.log<-data.frame(Action="Connect database", Message="Database conncection successful", Sys.Date())
userid.log1<-rbind(userid.log, data.frame(Action="Connect database ",Message= "Database connection failed ", Sys.Date()))
userid.log2<-rbind(userid.log1, data.frame(Action="Disconnect database ",Message= "Database disconnected ", Sys.Date()))
userid.log3<-rbind(userid.log2, data.frame(Action="Input username and password ", Message= " ", Sys.Date()))

saveRDS(userid.log3, file="D:/MRV/Aksara/user.id/user_activities.rds")


#4. List of users'scenarios
user.scenarios<- data.frame(Scenario_names=" ", intervention_point=" ", year=" ", Sys.Date(), Sector=" ")
saveRDS(user.scenarios,file="D:/MRV/Aksara/user.id/list_scenarios.rds")



#5. List of users' intervention points
user.int<-data.frame(Sektor=" ", Findem=" ", Sat.Energi=" ", Sat.Limbah=" ", Sat.Lahan=" ", Added_value=" ")
saveRDS(user.int,file="D:/MRV/Aksara/user.id/intervention_points.rds")


#6. Reading dummy tables from RDS files

user.int1<-readRDS("user.int1")
user.int2<-readRDS("user.int2")
user.int3<-readRDS("user.int3")
user.scen<-readRDS("user.scen")

