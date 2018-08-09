#1.

adult_db <- read.table(file.choose(), header = FALSE, sep= ",",na.strings = "?" , colClasses = NA, strip.white = TRUE, stringsAsFactors = FALSE )

names(adult_db) = c("age",
                    "workclass",
                    "fnlwgt",
                    "education",
                    "education_num",
                    "marital_status",
                    "occupation",
                    "relationship",
                    "race",
                    "sex",
                    "capital_gain",
                    "capital_loss",
                    "hours_per_week",
                    "native_country",
                    "class")

View(adult_db)

adult_db$class[adult_db$class==">50K"] <- 1
adult_db$class[adult_db$class=="<=50K"] <- 0



#2.

apply(adult_db, 2, function(x){sum(is.na(x))})


row.na <- apply(adult_db, 1, function(x){any(is.na(x))})
sum(row.na) #number of row wants to remove
adult_db_nomiss <- adult_db[!row.na,]

#or adult_db_nomiss <- na.omit(adult_db)


#3.

set.seed(1013)
idx = sample(1:nrow(adult_db_nomiss),1000)
adult_db_lim = adult_db_nomiss[idx,]
row.names(adult_db_lim) <- NULL

View(adult_db_lim)

#3a.

No_adult_db<- (adult_db_lim$class== 0)
Yes_adult_db<- (adult_db_lim$class== 1)

hist(adult_db_lim$age[No_adult_db], breaks = 50, main = "Age of adults", xlab = "Age", ylab = "Frequency", col = "red")

hist(adult_db_lim$age[Yes_adult_db], breaks = 50, xlab = "age", ylab = "frequency", col = "blue", add=T)

legend(x = 50, y = 30, legend = c(">50K", "<=50K"),
       col=c( "blue" , "red"), pch = 20, cex = 0.75)

#3b.

par(mar=c(5,5,2,2))

height_of_bar <- table(adult_db_lim$race)

barplot(height_of_bar, col=c("black", "red", "green", "blue" , "cyan"),
        main = "Race of adults", 
        names.arg = c("Amer-Indian-Eskimo", "Asian-Pac-Islander", "Black", "other" , "White"),
        cex.names = 0.8, xlab = "Race")

legend(x=0.5, y=580, legend = c("Amer-Indian-Eskimo", "Asian-Pac-Islander", "Black", "other" , "White"),col=c("black", "red", "green", "blue" , "cyan"), pch = 20, cex = 0.50)



#3c.

boxplot(adult_db_lim$age, pch=20, col="red", main = "Age of adults", ylab="Age")

boxplot.stats(adult_db_lim$age)$out


#4.

adult_db_numeric <- adult_db_lim[,c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")]
class_val <- as.numeric(adult_db_lim[,c("class")])

adult_db_num_std <- scale(adult_db_numeric)

# or 
#m <- apply(adult_db_numeric,2,mean)
#std <- apply(adult_db_numeric,2,sd)

#for (j in 1:6) { # each specified col for (j in cols) 
#adult_db_num_std[,j] <-  sapply(adult_db_numeric[,j] , function(x) (x- m[j]) / std[j])
#}


apply(adult_db_num_std, 2, mean)
apply(adult_db_num_std, 2, sd)


#5.

#5.a

pr.out <- prcomp(adult_db_num_std[,c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")], retx=TRUE, center=TRUE, scale=TRUE)

names(pr.out)

head(pr.out$x)

principal_components <- pr.out$x

plot(principal_components[,1:2], col = (class_val + 2), pch = 20, main = "First two Principal Components")

legend(x = -8, y = 4, legend = c( "<=50K", ">50K"),
       col=c("red", "green"), pch = 20, cex = 0.75)


#5.b

pr.var <- (pr.out$sdev)^2
pve <- pr.var/sum(pr.var)

par(mfrow=c(1,2), oma=c(0,0,2,0))

plot(pve, xlab = "Principal Components", ylab = "Variance", type = "b", ylim = c(0,1), col="red")

plot(cumsum(pve), xlab = "Principal Components", ylab = "Cumulative Variance", type = "b", ylim = c(0,1), col="red")


#5.c

#cumsum(pve)
#0.2289799 0.4037900 0.5654989 0.7243586 0.8731598 1.0000000
#Based on Q.5b for at least 50% of the total variance which is 0.5654989 we need three(3) principal components and for at least 90% of the total variance which is between 0.8731598 and 1.0000000 we need six(6) principal components.



