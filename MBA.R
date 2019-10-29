#--------------------------------------------
#title: "Market Basket Analysis"
#author: "Ashiqur"
#date: "10/9/2019"
#--------------------------------------------

#*******************************import three Table from Server*******************************************

library(RMySQL)

##Import df 'line_item'

# 2. Settings
db_user <- 'data_student_berlin'
db_password <- 'waai_has_shitty_internet'
db_name <- 'pricehub'
db_table <- 'line_item'
db_host <- '34.89.228.59' # for local access
db_port <- 3306

# 3. Read data from db
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)
s <- paste0("select * from ", db_table)
rs <- dbSendQuery(mydb, s)
line_item <-  fetch(rs, n = -1)
on.exit(dbDisconnect(mydb))

##Import df 'orders'

# 2. Settings
db_user <- 'data_student_berlin'
db_password <- 'waai_has_shitty_internet'
db_name <- 'pricehub'
db_table <- 'orders'
db_host <- '34.89.228.59' # for local access
db_port <- 3306

# 3. Read data from db
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)
s <- paste0("select * from ", db_table)
rs <- dbSendQuery(mydb, s)
orders <-  fetch(rs, n = -1)
on.exit(dbDisconnect(mydb))

##Import df 'products'

# 2. Settings
db_user <- 'data_student_berlin'
db_password <- 'waai_has_shitty_internet'
db_name <- 'pricehub'
db_table <- 'products'
db_host <- '34.89.228.59' # for local access
db_port <- 3306

# 3. Read data from db
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)
s <- paste0("select * from ", db_table)
rs <- dbSendQuery(mydb, s)
products <-  fetch(rs, n = -1)
on.exit(dbDisconnect(mydb))


#*************************************************************************************************************





