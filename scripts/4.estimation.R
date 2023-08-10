# This script will collect all of the grid-level features together

library(tidyverse)
library(povmap)
library(glmnet) # this is for lasso

# preliminaries --------------------------------------------
# load grid data (features)
grid <- read_csv("data/grid_clean.csv")
# here, we might normally make decisions on transforming variables, but for now we will just log transform the 
# distance variables as an example (none are equal to zero since they are defined from the grid CENTROID)
for (i in 1:10){ # ten distance variables, let's do this in a loop to save space
    grid[[paste0("distance", i)]] <- log(grid[[paste0("distance", i)]])
}
# verify no missings
sum(is.na(grid)) # 0, great!
# you could also consider adding fixed effects of, for example, admin1s. We skip that for parsimony here.
# create a vector with the names of the FEATURES
featurevec <- colnames(grid)[3:ncol(grid)]


# load household coordinates
hh <- read_csv("data/coords.csv")

# let's estimate a subarea-level model
# so we first need to aggregate households to grid level
hh <- hh %>%
        group_by(id) %>%
        mutate(insecure = weighted.mean(insecure, hhweight*hhsize)) %>% # this is a weighted mean, with weights equal to PEOPLE
        filter(row_number()==1) %>%
        ungroup()

# now add features to hh data
hh <- hh %>%
        left_join(grid, by = "id")

# first, let's look at the distribution of our outcome variable
ggplot(hh) +
    geom_density(aes(x = insecure)) # not very normal
ggplot(hh) +
    geom_density(aes(x = asin(sqrt(insecure)))) # definitely better! still not perfect

# let's go with asin(sqrt(insecure)) as our outcome variable
hh <- hh %>%
        mutate(insecure_transformed = asin(sqrt(insecure)))



# lasso --------------------------------------------
# we have 70 features. This isn't that many, actually. We didn't create a lot of different possible combinations of the predictors.
# We also don't have any fixed effects. This is just to fix ideas.
# Nonetheless, let's try lasso!
# we use the glmnet package to implement lasso. It also allows ridge, but we want to make sure to use lasso.
# how do we do this? we want to allocate grid cells across different "folds". However, an important point is that we want to allocate
# based on natural geospatial clustering. We won't allocate grid cells... instead, we'll allocate admin2s across grids!
admin2s <- unique(hh$admin2Pcod)
length(admin2s)     # there are 76 of these. Let's do this across FIVE folds, instead of ten.
# give each admin2 a random integer between 1 and 5.
folds <- sample(x = 1:5, size = length(admin2s), replace = TRUE)
# create a matrix with admin2s and folds
admin2s <- as_tibble(cbind(admin2s, folds))
colnames(admin2s)
colnames(admin2s) <- c("admin2Pcod", "folds")
hh <- hh %>%
        left_join(admin2s, by = c("admin2Pcod"))

# each grid cell now has a fold that it shares with all other grid cells in the same admin2
# The way we choose the "best" fit is by choosing a value, called lambda, that minimizes the 
# error between values in the sample and predictions
# We are going to iterate over different values of lambda. Let's allow for a wide range here.
lambdas <- seq(from = 0.001, to = 1, by = 0.002) # this will give 500 different values of lambda
# let's create a matrix where we can store the prediction errors. rows equal to length of lambdas, and columns equal to folds
errormat <- matrix(data = NA, nrow = length(lambdas), ncol = 5)
# set seed so we always get the SAME results
set.seed(30194856)
# iterate over each lambda AND each fold
for (l in 1:length(lambdas)){
    for (f in 1:5){
        # fit lasso for ONE value of lambda and leaving out ONE fold
        lasso <- glmnet(x = hh[hh$folds!=f, featurevec],            # x is the rows of hh that are NOT in fold f and just the features
                       y = hh$insecure_transformed[hh$folds!=f],    # this is the vector of outcomes for rows of hh that are NOT in fold f
                       alpha = 1,                               # this fits lasso. Anything other than 1 will also include ridge regression
                       lambda = lambdas[l])                     # only one value of lambda        
        # this is going to store in the errormat the MEAN SQUARED PREDICTION ERROR for a lambda of l and in fold f   
        errormat[l, f] <- mean((as.matrix(hh$insecure_transformed[hh$folds==f]) -
                                predict(lasso, newx = as.matrix(hh[hh$folds==f, featurevec])))^2)
    }
}
# now we have a matrix, and we want to find the mean squared prediction error for each value of lambda
# we can do this by taking the mean of each row
errormat <- apply(errormat, 1, mean)
# optimal value of lambda based on the minimum error
optlambda <- lambdas[which.min(errormat)]

# now do lasso again with optimal value of lambda
lasso <- glmnet(x = hh[,featurevec],
                y = hh$insecure_transformed,
                alpha = 1,
                lambda = optlambda)
# we need to extract the non-zero coefficients to figure out which features lasso selected
lasso_coef <- as.matrix(coef(lasso))
# which are not zero?
lasso_coef <- lasso_coef[lasso_coef[,1]!=0,]
names(lasso_coef)
# the first column is the intercept, so we don't care about that. drop it.
lasso_coef <- lasso_coef[-1]
optfeatures <- names(lasso_coef)
optfeatures # here are the features we will use!

# for povmap, we need to turn this into a FORMULA. We can do this with paste0.
# we need to add the outcome variable to the front of the formula
# we will not use the transformed version of insecure because povmap will automatically transform it for us!
optformula <- paste0("insecure ~ ", paste0(optfeatures, collapse = " + "))
optformula <- as.formula(optformula)
# let's first just look at a linear regression and see how it looks
summary(lm(optformula, data = hh))
# now we see a problem that will give us an issue with povmap: some of the selected distance variables are perfectly collinear!
# let's remove those. they are 4, 5, 6, and 7 positions
optfeatures <- optfeatures[-c(4, 5, 6, 7)]

# let's try again.
optformula <- paste0("insecure ~ ", paste0(optfeatures, collapse = " + "))
optformula <- as.formula(optformula)
# let's first just look at a linear regression and see how it looks
summary(lm(optformula, data = hh))
# nothing percetly collinear! okay great.


# now EBP
ebp <- povmap::ebp(fixed = optformula, # this is the formula
                    pop_data = grid, # this is the POPULATION DATA, so all the grid cells
                    pop_domains = "admin2Pcod", # this is the variable that identifies the admin2s
                    smp_data = hh, # sample data
                    smp_domains = "admin2Pcod", # sample domains
                    transformation = "arcsin", # transformation
                    seed = 41093658,    # random seed
                    MSE = TRUE)     # we want it to give us the uncertainty

# let's look at the results
summary(ebp) # the error skewness and kurtosis actually look really good! They aren't quite normal but they aren't far off either

# we can extract the estimated values for the domains:
# this will put together the domain, point estimate, and MSE of the point estimate
results <- as_tibble(cbind(ebp$ind$Domain, ebp$ind$Mean, ebp$MSE$Mean))
# let's give them names
colnames(results) <- c("admin2Pcod", "insecure", "se")
# let's turn the mean and MSE into numeric values as well as take the square root of MSE for the standard error
results$insecure <- as.numeric(results$insecure)
results$se <- sqrt(as.numeric(results$se))

# can also calculate the coefficient of variation:
results$cv <- (results$se/results$insecure)*100
summary(results$cv)     # these look really good! This is probably partly because ALL domains have survey estimates. This helps a lot!


# what do the direct estimates look like?
direct <- povmap::direct(y = "insecure", # this is the formula
                    smp_data = hh, # sample data
                    smp_domains = "admin2Pcod", # sample domains
                    seed = 41093658,
                    var = TRUE) # get uncertainty
directresults <- as_tibble(cbind(direct$ind$Domain, direct$ind$Mean, direct$MSE$Mean))
# let's give them names
colnames(directresults) <- c("admin2Pcod", "insecure", "se")
# let's turn the mean and MSE into numeric values as well as take the square root of MSE for the standard error
directresults$insecure <- as.numeric(directresults$insecure)
directresults$se <- sqrt(as.numeric(directresults$se))

# can also calculate the coefficient of variation:
directresults$cv <- (directresults$se/directresults$insecure)*100
summary(directresults$cv)  # we already see why direct estimates can be misleading. Lots of 0 because of no variation in the survey domain!
# However, 75th percentile is 17.4, while max for EBP was 12.5. Already clear improvements.
# Unfortunately, we don't have a census so we can't check accuracy!

