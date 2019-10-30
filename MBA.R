#--------------------------------------------
#title: "Market Basket Analysis"
#author: "Ashiqur"
#date: "10/9/2019"
#--------------------------------------------

#*******************************import three Table from Server*******************************************

library(RMySQL)
library(ggplot2)

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
library(dplyr)


#Check if Data Frame is NULL
sum(is.null(line_item))
sum(is.null(products))
sum(is.null(orders))


#There are 218 missing values in products data frame. We will remove these rows in next step and check again

#products <- products[complete.cases(products), ]
#sum(is.na(products))

#Show head and summary of line_item
head(line_item)
summary(line_item)
str(line_item)
glimpse(line_item)
dim(line_item)


#Show head and summary of orders
head(orders)
summary(orders)
str(orders)
glimpse(orders)
dim(orders)

#Show head and summary of products
head(products)
summary(products)
str(products)
glimpse(products)
dim(products)
colnames(products)


#line_item$order_paid<-line_item$product_quantity*line_item$unit_price
head(line_item)
dim(line_item)
line_price <- line_item %>% 
        group_by(id_order) %>% 
        summarise(count_price = sum(product_quantity * unit_price))
dim(line_price)
head(line_price)
sum(is.null(line_price))

#View(line_price)

compare_total<-inner_join(line_price,orders,by="id_order")
head(compare_total)
dim(compare_total)

compare_total$price_diff<-compare_total$total_paid-compare_total$count_price
#comp_total$price_diff<-comp_total$count_price-comp_total$total_paid
head(compare_total)
dim(compare_total)



compare_total %>% 
        filter(price_diff < 10, price_diff > -10) %>% 
        ggplot(aes(price_diff)) +
        geom_histogram()

#To explore individual id order 
filter(compare_total,id_order==299552)
        

comp_10<-filter(compare_total,price_diff<10)
count(comp_10)#that might be the delivery

comp_10<-filter(compare_total,price_diff>10)
count(comp_10)

correct_price<-filter(compare_total,price_diff==0) 
head(correct_price)
dim(correct_price)



incorrect_price<-filter(compare_total,price_diff!=0) 
head(incorrect_price)
dim(incorrect_price)
summary(incorrect_price)
#count(unique(incorrect_price$id_order))

compare_total %>% 
        filter(price_diff < 10, price_diff > -10) %>% 
        ggplot(aes(correct_price)) +
        geom_histogram()


# Check that all orders in line_item are present in our orders dataset. 
# Exclude from line_item any rows that do not meet that condition.

#using inner_join
new_Line_item<-inner_join(line_item,orders,by="id_order")
dim(new_Line_item)
head(new_Line_item)

#list of the value of id_order which is present in order dataset
line_item[line_item$id_order %in% new_Line_item$id_order,]
count(line_item[line_item$id_order %in% new_Line_item$id_order,])

#list of the value of id_order which is not present  in order table
count(line_item[!(line_item$id_order %in% new_Line_item$id_order),])

#############################################################################################################
testdf <- data.frame(
        colA = c("A", "A", "B", "C", "D", "E"),
        value = c(1,2,3,4,5,6)
)
testdf$colA <- as.character(testdf$colA)

testdf2 <- data.frame(
        colA = c("A", "B", "C", "F", "X"),
        value2 = c(1,2,3,4,5)
)
testdf2$colA <- as.character(testdf2$colA)


jointest <- inner_join(testdf, testdf2, by = "colA")

testdf[testdf$colA %in% testdf2$colA]
testdf[testdf$colA %in% testdf2$colA,]
unique(testdf[testdf$colA %in% testdf2$colA,])

a = c(1,2,3,4,5)
b = c(3,4,5,6,7)


a %in% b

testdf["colA"]


#using merge
new_Line_item <- merge(orders, line_item, by="id_order")
dim(new_Line_item)
head(new_Line_item)

#val = c()
#for (item in orders$id_order){ if (item %in% line_item$id_order)   {val = append(val, item)}}
##############################################################################################################


#t=distinct(select(line_item,id_order))
#r=distinct(select(orders,id_order))


#Exclude from line_item any rows from orders that are not “Completed”.
#Filter only completed orders

#list of the panding item in line_item table
new_Line_item_pan <- filter(new_Line_item, state == "Pending")
dim(new_Line_item_pan)
head(new_Line_item_pan)

#list of the Cancelled item in line_item table
new_Line_item_can <- filter(new_Line_item, state == "Cancelled")
dim(new_Line_item_can)
head(new_Line_item_can)

#list of the Completed item in line_item table
new_Line_item_com <- filter(new_Line_item, state == "Completed")
dim(new_Line_item_com)
head(new_Line_item_com)

##############dim(setdiff(line_item,orders))

##########y=dim(line_item)-dim(orders)
# Check that all products in line_item are present in the products dataset. 
# Exclude from line_item any rows that do not meet that condition.

#new_Line_item2 <- inner_join(line_item, products, by="sku")
#dim(new_Line_item2)
#head(new_Line_item2)

new_Line_item2 <- inner_join(new_Line_item_com, products, by="sku")
dim(new_Line_item2)
head(new_Line_item2)



#list of the value of sku which is present in order dataset
line_item[line_item$sku %in% 
     new_Line_item2$sku,]
count(line_item[line_item$sku %in% 
         new_Line_item2$sku,])

#list of the value of sku which is not present  in order table
count(line_item[!(line_item$sku %in% new_Line_item2$sku),])

dim(new_Line_item_com)
colnames(new_Line_item_com)
dim(new_Line_item2)
colnames(new_Line_item2)
head(new_Line_item2)

#Explore the relationship between prices in line_item and order:


#??what is the price in product table

new_Line_item2_price<-select(new_Line_item2,id_order,unit_price,product_quantity,total_paid)
head(new_Line_item2_price)

new_Line_item2_price %>% group_by(id_order) %>% summarise(cd=count(id_order))
#final <- inner_join(new_Line_item_com, new_Line_item2)
#dim(final)
#glimpse(final)
#head(final)

# Remove unnecessary columns

final$id <- NULL
final$date <- NULL
final$created_date <- NULL
final$state <- NULL
final$salable <- NULL
final$stock_qty <- NULL
final$purchasable <- NULL

dim(final)
head(final)
# Remove outliers from final

final <- filter(final, unit_price >= 0  & product_quantity < 999)

dim(final)

final %>% 
        id_order <- as.factor(id_order) %>% 
        final <- group_by(id_order) %>% mutate(id = row_number())
head(final)
dim(final)

# Explore the relationship between prices in line_item and order:
# Do the prices of the orders and the sum of the prices of each item in the order match? 
# If not, can you make some assumptions that explain the differences?
#Exclude from line_items the rows with differences that you cannot explain.

