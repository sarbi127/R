# Part 1: Simulating node splitting in decision tree
#A: Entropy calculation

load("entropy_data.RData")
head(entropy.data)
table(entropy.data$buys)

getEntropy <- function(class.freq){
  
  # First convert class frequency into class probabilities (p) by dividing 
  # frequencies by total number of observations
  p <- class.freq/sum(class.freq)
  
  # Use formula to compute entropy using class probabilities "p"
  # *** ATTENTION !!! *** #
  entropy <- sapply(1:length(class.freq), function(i) {
    (-(p[[i]] * log2(p[[i]])))}
    )
  
  # Probability p = 0 creates NaN value
  # we need to remove such value before computing sum
  # is.nan() returns TRUE if value is NaN
  entropy_final <- sum(entropy[!is.nan(entropy)])
  
  return(entropy_final)
}
getEntropy(table(entropy.data$buys))

#B: Information gain

getInfoGain <- function(split_var, class_var){
  
  class.freq <- as.matrix(table(class_var))
  # Calculate entropy of this parent node using class.freq
  entropy_parent <- getEntropy(class.freq)
  # We want to split this parent node using an attribute (e.g. income).
  # Create contingency table using the attribute and class label
  attr.class.freq <- as.matrix(table(split_var, class_var))
  #print(attr.class.freq)
  
   # Intialize the overall entropy of split to 0.
    entropy_split <- 0
    num_observations <- length(split_var)
    #print( num_observations)
    
    for(i in 1:nrow(attr.class.freq)){
      
      # each row in attr.class.freq is class frequency of one node
      # *** ATTENTION !!! *** #
      child.node.freq <- (attr.class.freq[i,])
      print(child.node.freq)
      
      # In order to find entropy of the split, 
      # first calculate entropy in each child node
      child.node.entropy <- getEntropy(child.node.freq)
      
      # number of examples in this child node
      num_obs_child.node <- sum(child.node.freq)
      # Add child node entropies weighted by fraction of examples they receive from parent node.
      
      # *** ATTENTION !!! *** #
      entropy_split <- entropy_split + (child.node.entropy * (num_obs_child.node / num_observations ))
      
    }
      
    # print(entropy_split)
    # Information gain is difference between parent node entropy and split entropy
    information_gain <- (entropy_parent - entropy_split)
    
    return(information_gain)
    
}
                               
getInfoGain(entropy.data$income, entropy.data$buys)

getInfoGain(entropy.data$student, entropy.data$buys)

getInfoGain(entropy.data$credit, entropy.data$buys)


#C: Information gain for numeric attributes

findBestAttribute <- function(data.X, data.Y){
  
  number_of_columns <- ncol(data.X)
  attribute_names <- colnames(data.X)
  info_gains <- numeric(number_of_columns)
  best_cutoffs <- rep(NA, number_of_columns)
  
  for(i in 1:number_of_columns){
    
    # If an attribute is numeric, it has to be discretized (changed into categorical)
    if(class(data.X[,i]) == "numeric"){
      
      # find all the numeric values, order them in ascending order and select unique values
      x_sort_unique <- unique(sort(data.X[,i]))
      
      # for any two consecutive numbers, find the midpoint and use this midpoint to discretize
      # All observations with value for this attribute less than this midpoint gets "YES" category while rest gets "NO" 
      # Once all observations have value either "YES" or "NO", it can be treated as categorical
      
      max_info_gain_this_var <- 0
      best_cutoff_point <- NA
      
      for(j in 1:(length(x_sort_unique)-1)){
        
        # Calculate midpoint betweent two consecutive numeric values of numeric variable
        
        # *** ATTENTION !!! *** #
        cutoff_point <- (x_sort_unique[j] + x_sort_unique[j + 1])/2
        
        # Categorize all observations with numeric value less than cutoff as "YES" else "NO" 
        lessthan_cutoff <- ifelse(data.X[,i] <= cutoff_point, "YES", "NO")
        # Compute information gain on descritized numeric variable (now categorical) "lessthan_cutoff"
        
        # *** ATTENTION !!! *** #
        info_gain_for_this_split <- getInfoGain(lessthan_cutoff , data.Y)
        
        # If information gain is bettr than previous information gain, set it as new best
        if(info_gain_for_this_split > max_info_gain_this_var){
          max_info_gain_this_var <- info_gain_for_this_split
          best_cutoff_point <- cutoff_point
        }
      }
      info_gains[i] <- max_info_gain_this_var
      best_cutoffs[i] <- best_cutoff_point
      
    }else{ 
      # If attribute is categorical, simply use contingency table to calculate info gain calling getInfoGain function
      info_gains[i] <- getInfoGain(data.X[,i], data.Y)
    }
  }
  
  return_df <- data.frame("Attributes"=attribute_names, "Info.Gain" = info_gains, "Cutoff.at" = best_cutoffs)
  
  return(return_df) 
}

attr_info_gains <- findBestAttribute(data.X = entropy.data[,1:4], data.Y = entropy.data[,5])
print(attr_info_gains)

#Part 2: Perceptron Learning Algorithm

load("Perceptron_data.RData")
head(perceptron.data)

plot(perceptron.data[,1:2], xlim = c(-1,1), ylim = c(-1,1), col = ifelse(perceptron.data$y == -1, "red", "green"))

my.perceptron <- function(data.X, data.Y, max.iter = 500){
  
  # Add X0, which is 1 for all examples
  X.new <- cbind(1, data.X) 
  
  # Initial weight vector with all 0 elements, note that the first element is w0
  w <- matrix(0, 1, ncol(X.new))
  
  # track itertaions, counter for number of iterations
  iterations <- 0
  
  # matrix to keep track of weight changes
  weight.trace <- X.new[numeric(0),]
  
  while(TRUE){
    # use matrix multiplication of X.new and w to calculate the hypothesis.
    # Hint: Make sure both are matrix and try to see how to get cross product of two matrices 
    
    # *** ATTENTION !!! *** #
    hypothesis <- crossprod(t(w), t(as.matrix(X.new)))
    dim(hypothesis)
    
    # use the sign of hypothesis to predict new class label for each observation
    # if hypothesis is positive (greater or equal to zero) assign it a class 1 else -1
    label.new <- ifelse(hypothesis >= 0, 1, -1) 
    
    # if the new class labels from hypothesis are same as the true class labels (data.Y) 
    # for all observations, then stop iterating
    if(all(label.new == data.Y)){ 
      
      #return(list("final.weight" = w, "weight.trace" = weight.trace))
      # return(w)
      break
      
      # if number of iterations exceeds maximum limit, stop iterating
    }else if(iterations >= max.iter){
      
      # return(w)
      break 
      
    }else{ 
      # if the new class labels from hypothesis are not the same with the true class label, 
      # update the weight vector and continue the iteration. 
      
      # index for those that had incorrect predictions
      row.index.wrong.pred <- (label.new != data.Y)
      
      # randomly select one misclassified point
      set.seed(5)
      wrong.predicted.idx <- which(row.index.wrong.pred == TRUE)
      rand.sel.index <- sample(wrong.predicted.idx, 1)
      
      # update the weight vector using this randomly selected misclassified point
      iterations <- iterations + 1
      
      # *** ATTENTION !!! *** #
      w <- (w + (X.new[rand.sel.index,]* data.Y[rand.sel.index] ) )
      
      # save the intermediate weights to see the changes in weight vector after each iteration 
      weight.trace <- rbind(weight.trace, w)
    }   
  }
  
  # remove the row names
  rownames(weight.trace) <- NULL
  
  return(list("final.weight" = w, "iterations" = iterations, "weight.trace" = weight.trace))
}

model <- my.perceptron(data.X = perceptron.data[,1:2], data.Y = perceptron.data[,3])
print(model$final.weight)

# plot the final splitting hyperplane discovered by the algorithm
plot(perceptron.data[,1:2], xlim = c(-1,1), ylim = c(-1,1), col = ifelse(perceptron.data$y == -1, "red", "green"), pch = 20)
intercept.final <- -(model$final.weight[1])/model$final.weight[3]
slope.final <- -(model$final.weight[2])/model$final.weight[3]
abline(as.numeric(intercept.final), as.numeric(slope.final), lwd = 2)

# plot split planes and see how the plane adjust at each iteration
# and gets closer and closer to the correct spliting hyperplane
plot(perceptron.data[,1:2], xlim = c(-1,1), ylim = c(-1,1), col = ifelse(perceptron.data$y == -1, "red", "green"), pch = 20)
for(i in 1:model$iterations){
  intercept.i <- -(model$weight.trace[i,1])/model$weight.trace[i,3]
  slope.i <- -(model$weight.trace[i,2])/model$weight.trace[i,3]
  abline(intercept.i, slope.i, col = "blue")
  Sys.sleep(1)
  if(i == model$iterations)
    abline(intercept.i, slope.i,lwd = 3)
}

# Part 3: Cross Validation

heart.disease.db <- read.table(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",
                               header = FALSE, sep = ",", na.strings = "?", strip.white = TRUE, 
                               stringsAsFactors = FALSE)

head(heart.disease.db)
colnames(heart.disease.db) <- c("age", "sex", "cp", "trestbps", "chol",
                                "fbs", "restecg", "thalach", "exang",
                                "oldpeak", "slope", "ca", "thal", "class")

# Check for missing values in each column and remove the examples with missing values
apply(heart.disease.db, 2, function(x) sum(is.na(x)))
heart.disease.db <- na.omit(heart.disease.db)

# Format data properly
heart.disease.db$sex <- ifelse(heart.disease.db$sex == 1, "Male", "Female")
heart.disease.db$sex <- as.factor(heart.disease.db$sex)

heart.disease.db$cp <- as.factor(heart.disease.db$cp)

heart.disease.db$fbs <- ifelse(heart.disease.db$fbs == 1, ">120", "<120")
heart.disease.db$fbs <- as.factor(heart.disease.db$fbs)

heart.disease.db$exang <- ifelse(heart.disease.db$exang == 1, "yes", "no")
heart.disease.db$exang <- as.factor(heart.disease.db$exang)

heart.disease.db$slope <- as.factor(heart.disease.db$slope)

heart.disease.db$thal <- as.factor(heart.disease.db$thal)

# for all the patients with heart diseases, change the class label to 1 (combine 1,2,3 and 4 to 1)
# 1 = Heart Disease
# 0 = No heart Disease
index.heart.disease <- (heart.disease.db$class != 0) 
heart.disease.db$class[index.heart.disease] <- 1
heart.disease.db$class <- as.factor(heart.disease.db$class)

#part A

library("caret")
library("e1071")

set.seed(105)
num.folds <- 10

# create 10 fold using createFolds() function from caret package

# *** ATTENTION !!! *** #
k.fold.data.idx <- createFolds(heart.disease.db$class , k = num.folds)

# cost parameters to vary
cost.parameter <- c(0.1, 1, 10, 100)

# average cross validation accuracy for each cost value
cv.accuracy.cost <- numeric(length(cost.parameter))

for(i in 1:length(cost.parameter)){
  
  # vector to hold accuracy for each fold
  fold.accuracy <- numeric(num.folds)
  for(j in 1:num.folds){
    
    test.set.idx <- k.fold.data.idx[[j]]
    test.set = heart.disease.db[test.set.idx,]
    train.set = heart.disease.db[-test.set.idx,]
    
    # use svm() function in e1071 package to train SVM
    
    # *** ATTENTION !!! *** #
    svm.model <- svm(formula = class ~ ., data = train.set, kernel = "radial", cost=cost.parameter[i], probability = TRUE)
    
    svm.pred <- predict(svm.model, newdata = test.set)
    # accuracy on a test fold
    fold.accuracy[j] <- mean(svm.pred == test.set$class)
  }
  
  # average accuracy for a given cost value
  cv.accuracy.cost[i] <- mean(fold.accuracy)
}

# print average accuracy for varying cost parameter
print(cbind("cost.par" = cost.parameter, "avg.accuracy" = cv.accuracy.cost))

# Write in comments what value of cost parameter resulted in best SVM model.
#cost.par avg.accuracy
#[1,]      0.1    0.8050575
#[2,]      1.0    0.8352874
#[3,]     10.0    0.8051724
#[4,]    100.0    0.7642529

# the second value(1.0) give the best SVM model because the accuracy(0.8352874) is higher than the other.


#Part B

# For this part, randomly select 80% of data for training and test on remaining data
# Plot ROC curve with AUC value for positive class (i.e. 1)

set.seed(113)
train.idx <- sample(1:nrow(heart.disease.db), size = round(0.8 * nrow(heart.disease.db)), replace = FALSE)

train.set <- heart.disease.db[train.idx,]
test.set <- heart.disease.db[-train.idx,]

svm.model <- svm(formula = class ~ ., data = train.set, kernel = "radial", cost = 1, probability = TRUE)

# plotting ROC
library("ROCR")

# predict on test examples using trained SVM model

# *** ATTENTION !!! *** #
svm.pred <- predict(svm.model, newdata = test.set, probability = TRUE)

class.probs <- attr(svm.pred, "probabilities")
pred2 <- prediction(class.probs[,2], test.set$class)

roc.curve <- performance(pred2, "tpr", "fpr")
plot(roc.curve)
lines(x = c(0,1), y = c(0,1))

# AUC (Area Under ROC)
auc <- unlist(attr(performance(pred2, "auc"), "y.values"))
print(auc)
legend("bottomright", sprintf("%.3f",auc), title = "AUC")







