getfromSQL<-function(mydbname,mytablename){
  library(data.table)
  library(RMySQL)
  mydb = dbConnect(MySQL(), user='root', password='notapassword', host='192.168.8.190', dbname = mydbname)
  dbGetQuery(mydb, "SET NAMES utf8")
  importTable <- data.table(dbReadTable(mydb, mytablename))
  dbDisconnect(mydb)
  return(importTable)
}

savetoSQL<-function(mydbname,mytablename,mytable){
  library(data.table)
  library(RMySQL)
  mydb = dbConnect(MySQL(), user='root', password='notapassword', host='192.168.8.190', dbname = mydbname)
  #dbListTables(mydb)
  dbWriteTable(mydb, mytablename, mytable,overwrite = TRUE)
  cat(" Data colums:","\n  ",dbListFields(mydb, mytablename),"\n Success:")
  dbDisconnect(mydb)
}

readMongoTable <- function(tablename){
  library(data.table)
  library(jsonlite)
  system(paste("mongoexport -h 192.168.8.190 -d uniteddata -c ", tablename, " -o ", tablename, ".json", sep=""))
  mydata <- stream_in(file(paste(tablename, ".json", sep="")))
  mydata$`_id` <- NULL
  mydata$holdday <- NULL
  mydata <- encode(mydata, "unknown")
  mydata <- data.table(mydata)
  saveRDS(mydata, (paste(tablename, ".RDS", sep="")))
  return((mydata))
}

exportMongo <- function(df, db, name){
  library(rjson)
  library(rmongodb)
  df_list <- lapply(split(df, 1:nrow(df)), function(x) mongo.bson.from.JSON(toJSON(x)))
  mongo <- mongo.create(host = '192.168.8.190')
  print(mongo.is.connected(mongo))
  mongo.get.databases(mongo)
  if (mongo.is.connected(mongo) == TRUE) {
    icoll <- paste(db, name, sep=".")
    mongo.insert.batch(mongo, icoll, df_list)          # insert into the MongoDB
  }
}

jsonToDataframe <- function(json_file){
  json_file <- fromJSON(json_file)
  json_file <- lapply(json_file, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  return(do.call("rbind", json_file))
}

encode <- function(dt, enc){
  for(i in names(dt)){
    if(class(dt[[as.character(i)]])=="character"){
      Encoding(dt[[as.character(i)]]) <- enc
    }
  }
  return(dt)
}
