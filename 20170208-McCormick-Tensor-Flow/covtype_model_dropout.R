Sys.setenv(TENSORFLOW_PYTHON = "/usr/bin/python2")

library(tensorflow)
library(ggplot2)

train <- read.csv('train.cover') #, colClasses = c(rep("factor", 8), rep("numeric", 10), rep("factor", 44)))

#head(train)

train.mat <- data.matrix(train)
#head(train.mat)
#str(train.mat)
#colnames(train)

train_input <- train.mat[, 9:62]
train_labels <- train.mat[, 2:8]

val <- read.csv('val.cover') #, colClasses = c(rep("factor", 8), rep("numeric", 10), rep("factor", 44)))

#head(train)

val.mat <- data.matrix(val)
#head(train.mat)
#str(train.mat)
#colnames(train)

val_input <- val.mat[, 9:62]
val_labels <- val.mat[, 2:8]

learning_rate = .05
batch_size = 100
momentum = .5

# I tried to do it this way but it wouldn't work unless they were specifically 
#stated in the placeholders
#n_hidden = 120L  # nodes in hidden layer
#n_input = 54L  # input variables 
#n_classes = 7L # number of classes of cover type

#tf graph input

# Python
# x=tf.placeholder("float", [None,  n_input])
# y=tf.placeholder("float", [None, n_classes])

x = tf$placeholder(tf$float32, shape(NULL, 54L))
y_ = tf$placeholder(tf$float32, shape(NULL, 7L))
pkeep = tf$placeholder("float")

W1 = tf$Variable(tf$truncated_normal(shape(54L, 120L), stddev = 0.1))
B1 = tf$Variable(tf$zeros(shape(120L)))

W2 = tf$Variable(tf$truncated_normal(shape(120L, 7L), stddev = 0.1))
B2 = tf$Variable(tf$zeros(shape(7L)))

layer1 <- tf$nn$sigmoid(tf$matmul(x,W1) + B1)
l1drop = tf$nn$dropout(layer1, pkeep)
y <- tf$nn$softmax(tf$matmul(l1drop, W2) + B2)

cross_entropy <- tf$reduce_mean(-tf$reduce_sum(y_*tf$log(y), reduction_indices = 1L))

optimizer <- tf$train$MomentumOptimizer(learning_rate, momentum)
#optimizer <- tf$train$AdamOptimizer(learning_rate = .05, beta1 = 0.9, beta2 = 0.999)
train_step <- optimizer$minimize(cross_entropy)

correct_prediction <- tf$equal(tf$argmax(y, 1L), tf$argmax(y_, 1L))
accuracy <- tf$reduce_mean(tf$cast(correct_prediction, tf$float32))

init_op <- tf$global_variables_initializer()
sess <- tf$Session()
sess$run(init_op)

acc <- NULL
acc_val <- NULL

for( i in 1:1000) {  
  batch_index <- sample(seq_len(nrow(train_input)), batch_size)
  batch_x <- train_input[batch_index,]
  batch_y <- train_labels[batch_index,]
  sess$run(train_step,
           feed_dict = dict(x=batch_x, y_ = batch_y, pkeep = 0.75))
  train_accuracy <- sess$run(accuracy, feed_dict = dict(x = batch_x, y_ = batch_y, pkeep = 1.0))
  val_accuracy <- sess$run(accuracy, feed_dict = dict(x = val_input, y_ = val_labels, pkeep = 1.0))
  acc <- c(acc, train_accuracy)
  acc_val <- c(acc_val, val_accuracy)
  if (i%%100 == 0){
    cat(sprintf("step %d, training accuracy %g\n", i, train_accuracy))
    }
}


acc <- as.data.frame(cbind(1:1000, acc))
qplot(V1, acc, data=acc, geom = c("point", "smooth"))

acc_val <- as.data.frame(cbind(1:100, acc_val))
qplot(V1, acc_val, data=acc_val, geom = c("point", "smooth"))

accuracy_train<- sess$run(accuracy, feed_dict=dict(x = train_input, y_ = train_labels, pkeep=1.0))
print(accuracy_train)
print(val_accuracy)
#don't run this stuff it takes too long
#can put in test data to evaluate model on that
#test <- read.csv('test.cover') #, colClasses = c(rep("factor", 8), rep("numeric", 10), rep("factor", 44)))

#test.mat <- data.matrix(test)
#rm(test)
#head(train.mat)
#str(train.mat)
#colnames(train)

#test_input <- test.mat[, 9:62]
#test_labels <- test.mat[, 2:8]
#rm(test.mat)

#accuracy_test <- sess$run(accuracy, feed_dict=dict(x = test_input, y_ = test_labels))
# Only 64% accuracy on the test set :(


