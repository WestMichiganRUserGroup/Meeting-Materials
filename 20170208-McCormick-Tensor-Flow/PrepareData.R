
# read in data, make sure binary and ordinal variables are read in correctly

coverdata <- read.csv('covtype.data', header=FALSE, 
                      colClasses = c(rep("numeric", 10), rep("factor", 45)))
head(coverdata)

# put in some column names
colnames(coverdata)[1:14] <- c('elevation', 'aspect', 'slope', 'horizontal_dist_to_hydrology',
                    'vertical_dist_to_hyrdology', 'horizontal_dist_to_road', 
                    'hillshade_9am', 'Hilllshade_noon', 'Hillshade_3pm', 
                    'horizontal_dist_to_firepoints', 'Rawah_WA', 'Neota_WA',
                    'Comanche_Peak_WA', 'CachelaPoudre_WA')
new_names <- paste('soil', 1:40, sep = '')
colnames(coverdata)[15:54] <- new_names
colnames(coverdata)[55] <- 'covertype'

dim(coverdata)

str(coverdata)

levels(coverdata[,55])

levels(coverdata$covertype)<- c('Spruce/Fir', 'Lodgepole Pine', 'Ponderosa Pine', 
                                 'Cottonwood/Willow', 'Aspen', 'Douglas-fir', 'Krummholz')
levels(coverdata$covertype)

# One hot encoding the variables we want to classify

coverdata1hot <- data.frame(model.matrix(~covertype-1,coverdata), coverdata[1:54])

head(coverdata1hot[,1:7])

# explore predictor variables

summary(coverdata1hot)

hist(coverdata1hot$elevation)
hist(coverdata1hot$slope)

# Normalize quantitative predictors - easier for training Neural Network

coverdatanormal <- coverdata1hot
#not sure if I really want it one hot coded first, trying without
#coverdatanormal <- coverdata

colnames(coverdatanormal)
coverdatanormal[,8:17] <- scale(coverdatanormal[,8:17])
#coverdatanormal[,1:10] <- scale(coverdatanormal[,1:10])


hist(coverdatanormal$elevation)
hist(coverdatanormal$slope)

# need to select a sample that has a the same number of all the types in it! 
# Figure out how to do without breaking down into separate data sets and then sample
#and then reconcatonating?

#unbalanced categories
table(coverdata[,55])

2747/(nrow(coverdata))

#subset data to sample from, then put all together

Spruce.Fir <- subset(coverdatanormal, covertypeSpruce.Fir == 1)
Lodgepole.Pine <- subset(coverdatanormal, covertypeLodgepole.Pine == 1)
Ponderosa.Pine <- subset(coverdatanormal, covertypePonderosa.Pine == 1)
Cottonwood.Willow <- subset(coverdatanormal, covertypeCottonwood.Willow == 1)
Aspen <- subset(coverdatanormal, covertypeAspen == 1)
Douglas.fir <- subset(coverdatanormal, covertypeDouglas.fir == 1)
Krummholz <- subset(coverdatanormal, covertypeKrummholz == 1)

#make sure we have enough space
rm(coverdata1hot)

#create sample and test data sets - you should really write a function to do all this...

create_samples <- function(dataframe, dataframe.train, dataframe.test, dataframe.val){
    index <- 1:nrow(dataframe)
    train_index <- sample(index, size = 1620)
    dataframe.train <- dataframe[train_index,]
    dataframe.test <- dataframe[-train_index,]
    index2 <- 1:nrow(dataframe.test)
    val_index <- sample(index2, size=540)
    dataframe.val <- dataframe.test[val_index,]
    dataframe.test <- dataframe.test[-val_index,]
    return(list(a = dataframe.train, b = dataframe.val))

}

list <- create_samples(Spruce.Fir, Spruce.Fir.Train, Spruce.Fir.Test, Spruce.Fir.val)
Spruce.Fir.Train <- list$a
Spruce.Fir.Val <- list$b

list <- create_samples(Lodgepole.Pine, Lodgepole.Pine.Train, Lodgepole.Pine.Test, Lodgepole.Pine.val)
Lodgepole.Pine.Train <- list$a
Lodgepole.Pine.Val <- list$b

list <- create_samples(Ponderosa.Pine, Ponderosa.Pine.Train, Ponderosa.Pine.Test, Ponderosa.Pine.val)
Ponderosa.Pine.Train <- list$a
Ponderosa.Pine.Val <- list$b

list <- create_samples(Cottonwood.Willow, Cottonwood.Willow.Train, Cottonwood.Willow.Test, Cottonwood.Willow.val)
Cottonwood.Willow.Train <- list$a
Cottonwood.Willow.Val <- list$b

list <- create_samples(Douglas.fir, Douglas.fir.Train, Douglas.fir.Test, Douglas.fir.val)
Douglas.fir.Train <- list$a
Douglas.fir.Val <- list$b

list <- create_samples(Aspen, Aspen.Train, Aspen.Test, Aspen.val)
Aspen.Train <- list$a
Aspen.Val <- list$b

list <- create_samples(Krummholz, Krummholz.Train, Krummholz.Test, Krummholz.val)
Krummholz.Train <- list$a
Krummholz.Val <- list$b



train <- rbind(Spruce.Fir.Train, Lodgepole.Pine.Train, Ponderosa.Pine.Train, Cottonwood.Willow.Train,
               Aspen.Train, Douglas.fir.Train, Krummholz.Train)

val <- rbind(Spruce.Fir.Val, Lodgepole.Pine.Val, Ponderosa.Pine.Val, Cottonwood.Willow.Val,
              Aspen.Val, Douglas.fir.Val, Krummholz.Val)


write.csv(train, file='train.cover')
write.csv(val, file='val.cover')

