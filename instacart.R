# Refered to code from kernels



library(dplyr)

setwd("C:/Users/mnave/Documents/instacart")


memory.size(max = TRUE)
aisles = read.csv("aisles.csv")
head(aisles)

departments = read.csv("departments.csv")
head(departments)

train2 = read.csv("order_products__train.csv")
head(train)
str(train)

# contains orderid,useridmevalset,ordernumber
orders = read.csv("orders.csv")
head(orders)
# prior 
prior = read.csv("order_products__prior.csv")

head(prior)
products = read.csv("products.csv")

head(products)
prior = read.csv("order_products__prior.csv")

head(prior)

# it is created by joining order and prder_prior data frames


head(order_prods)

#order_prods$days_since_prior_order[is.na(order_prods$days_since_prior_order)==TRUE]=0

# Number of times the product is added by user (product order frequency)
order_prods['countcolumn']=1
order_prods = mutate(order_prods,product_time = row_number())

order_prods=order_prods %>% group_by(user_id, product_id) %>% mutate(product_time = row_number()) %>% ungroup()
order_prods = as.data.frame(order_prods)

# Number of times product is ordered (prods)
prodfreq = aggregate(formula= countcolumn ~ product_id,data= order_prods,FUN=sum)

# Number of times product is re-ordered (prreods)
prodrefreq = aggregate(formula= reordered ~ product_id,data= order_prods,FUN=sum)
temp = merge(prodfreq,prodrefreq,by="product_id")
colnames(temp)= c("product_id","prodfreq","prodrefreq")

# Number of times product ordered first time (prodfirst)
prodfirst = aggregate(formula= countcolumn ~ product_id,data= order_prods[order_prods$product_time==1,],FUN=sum) 
temp = merge(temp,prodfirst,by="product_id")
colnames(temp)[4]= "prodfirst"
# Number of times product ordered second time 
prodsecond = aggregate(formula= countcolumn ~ product_id,data= order_prods[order_prods$product_time==1,],FUN=sum) 
temp = merge(temp,prodsecond,by="product_id")
colnames(temp)[5]= "prodsecond"


temp['prodreorderprob'] = temp$prodsecond/temp$prodfirst
temp['reorderratio'] = temp$prodrefreq/temp$prodfreq
temp$reorderratio = round(temp$reorderratio,digits = 2)

# product features created 
productfeatures = select(temp,-prodrefreq,-prodfirst,-prodsecond)


# creating user features
# data set without train features
temp = orders[orders$eval_set != 'test',]
orderfreq = aggregate(formula = order_number ~ user_id,data=temp,FUN=max)
colnames(orderfreq)[2] = "orderfreq"

# average days between user orders
avgdaysbworders = aggregate(formula = days_since_prior_order ~ user_id,data = temp,FUN=mean) 
colnames(avgdaysbworders)[2] =   "avgdaysbworders"                           
userfeatures = merge(orderfreq,avgdaysbworders,by="user_id")

# total products ordered by the users

totalprodsuser = aggregate(formula = countcolumn~user_id,data = order_prods,FUN = sum)
colnames(totalprodsuser)[2]="totalprodsuser"
userfeatures = merge(userfeatures,totalprodsuser,by="user_id")


#re order ratio of the user
totalreorderedprods = aggregate(formula = countcolumn~user_id,data = order_prods[order_prods$reordered==1,],FUN = sum)
colnames(totalreorderedprods)[2]= "totalreorderedprods"
head(totalreorderedprods)
userfeatures = merge(userfeatures,totalreorderedprods,by="user_id")
userfeatures['reorderratio'] = userfeatures$totalreorderedprods/userfeatures$totalprodsuser
userfeatures$reorderratio = round(userfeatures$reorderratio,2)

# unique prods ordered by user
temp = order_prods[,c("user_id","product_id")]
temp = unique(temp)
temp['countcolumn']=1
distinctprods = aggregate(formula = countcolumn~user_id,data=temp,FUN=sum)
colnames(distinctprods)[2]="distinctprods"

#userfeatures['distinctprods']= userfeatures$totalprodsuser-userfeatures$totalreorderedprods
userfeatures = merge(userfeatures,distinctprods,by="user_id")
userfeatures['averageprodsinbasket']= userfeatures$totalprodsuser/userfeatures$orderfreq

# selecting train and test data
temp = orders[orders$eval_set!="prior",]
userfeatures = merge(userfeatures,temp[,c("user_id","order_id","eval_set","days_since_prior_order")],by="user_id")
userfeatures$averageprodsinbasket = round(userfeatures$averageprodsinbasket,0)


# combining alla columns together

temp = order_prods[,c("user_id","product_id","add_to_cart_order","order_number","countcolumn")]

# userprod per order

usrprodorders = aggregate(formula = countcolumn~ user_id+product_id,data=temp,FUN=sum)
colnames(usrprodorders)[3]= "usrprodorders"

# first and last order of user product

usrprodfirst = aggregate(formula = order_number~ user_id+product_id,data=temp,FUN=min)
colnames(usrprodfirst)[3] = "usrprodfirst"

usrprodorders = merge(usrprodorders,usrprodfirst,by=c("user_id","product_id"))
rm(usrprodfirst)

usrprodlast = aggregate(formula = order_number~ user_id+product_id,data=temp,FUN=max)
colnames(usrprodlast)[3]="usrprodlast"
usrprodorders = merge(usrprodorders,usrprodlast,by=c("user_id","product_id"))
rm(usrprodlast)

usravgcartpost = aggregate(formula = add_to_cart_order ~ user_id+product_id,data=temp,FUN=mean)
colnames(usravgcartpost)[3]="usravgcartpost"
usrprodorders = merge(usrprodorders,usravgcartpost,by=c("user_id","product_id"))
write.csv(usrprodorders,"usrprodorders.csv")

usrprodorders = read.csv("usrprodorders.csv")
productfeatures= read.csv("productfeatures.csv")
userfeatures = read.csv("userfeatures.csv")

rm(usrprodorders)
rm(productfeatures)

mydata = merge(usrprodorders,productfeatures,by="product_id")

mydata = merge(mydata,userfeatures,by="user_id")
--------------# redo
  

mydata['usrprodorderrate'] = mydata$usrprodorders/mydata$orderfreq





# seperating train and test data set

train= mydata[mydata$eval_set=="train",]

train = merge(train,train2[,c("order_id","product_id","reordered")],by=c("order_id","product_id"))


test= mydata[mydata$eval_set=="test",]

train$eval_set=NULL
train$order_id=NULL
train$user_id=NULL
train$product_id=NULL
train$reordered[is.na(train$reordered)]=0

test$eval_set=NULL
test$user_id=NULL
test$reordered = NULL

write.csv(train,"train.csv")
write.csv(test,"test.csv")


