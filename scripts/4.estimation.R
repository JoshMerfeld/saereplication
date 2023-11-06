# This script will collect all of the grid-level features together

library(tidyverse)
library(povmap)
library(glmnet) # this is for lasso

# preliminaries --------------------------------------------
# load grid data (features)
grid <- read_csv("data/grid_clean.csv")
# here, we might normally make decisions on transforming variables, but for now we will just log transform the 
# distance variables as an example (none are equal to zero since they are defined from the grid CENTROID)
#find colnames with "distance" in them
for (i in colnames(grid)[grep("distance", colnames(grid))]){ # ten distance variables, let's do this in a loop to save space
    grid[[i]] <- log(grid[[i]])
}
# verify no missings
sum(is.na(grid)) # 0, great!
# you could also consider adding fixed effects of, for example, admin1s. We skip that for parsimony here.
# create a vector with the names of the FEATURES
# I've purposefully set up the dataset so that the first two columns are identifiers and the rest are features
# so take 3rd column to the end
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

# let's also create a 

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
# we have ~60 features. This isn't that many, actually. We didn't create a lot of different possible combinations of the predictors.
# We also don't have any fixed effects. This is just to fix ideas.
# Nonetheless, let's try lasso!
# we use the glmnet package to implement lasso. It also allows ridge, but we want to make sure to use lasso.
# how do we do this? we want to allocate grid cells across different "folds". 
# In the paper, we argue for using EAs as the way to assign. Since we don't have EAs, we'll use the individual grid cells, which are
# similar to allocating by EA. If you want to use something different, just replace "id" with your preference
ids <- unique(hh$id)
length(ids)     # there are 642 of these. Let's do this across ten folds.

# Starting from here, we are using some randomness. We want to set.seed so we get the same results.
set.seed(30194856)
# give each id a random integer between 1 and 10.
folds <- sample(x = 1:10, size = length(ids), replace = TRUE)
# create a matrix with ids and folds
ids <- as_tibble(cbind(ids, folds))
colnames(ids)
colnames(ids) <- c("id", "folds")
# since we are doing this by grid cells, we do'nt really nee dto add this to hh.
# However, if you did this with a higher level of aggregation, you'd want to add this to hhs.
# So, adding it to keep this general.
hh <- hh %>%
        left_join(ids, by = c("id"))

# We now need to set up glmnet to do lasso. We need to specify the features, the outcome, and the folds.
# note how we need to specify the features as a matrix. We do this with as.matrix
cvfit <- cv.glmnet(x = as.matrix(hh[, featurevec]),  # features
                    y = hh$insecure_transformed,     # y
                    type.measure = "mse",            # use MSE
                    foldid = hh$folds,               # here are the folds we created above
                    alpha = 1)                       # alpha = 1 means lasso

# get optimal lambda (just in case you want it later)
optlambda <- cvfit$lambda.min

# get the non-zero coefficients
(lasso_coef <- labels(coef(cvfit))[[1]][as.numeric(coef(cvfit))!=0])
# the first column is the intercept, so we don't care about that. drop it.
lasso_coef <- lasso_coef[-1]


# for povmap, we need to turn this into a FORMULA. We can do this with paste0.
# we need to add the outcome variable to the front of the formula
# we will not use the transformed version of insecure because povmap will automatically transform it for us!
optformula <- paste0("insecure ~ ", paste0(lasso_coef, collapse = " + "))
optformula <- as.formula(optformula)
# let's first just look at a linear regression and see how it looks
(lmresult <- lm(optformula, data = hh))
# now we see a problem that will give us an issue with povmap: some of the selected distance variables are perfectly collinear!
# let's remove those.
(optfeatures <- coefficients(lmresult)[!is.na(coefficients(lmresult))])
# remove the intercept again
(optfeatures <- optfeatures[-1])

# let's try again.
optformula <- paste0("insecure ~ ", paste0(names(optfeatures), collapse = " + "))
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
# small note: Domain is stored as a factor variable. using levels() will extract the actual admin2 name
results <- as_tibble(cbind(levels(ebp$ind$Domain), ebp$ind$Mean, ebp$MSE$Mean))
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
directresults <- as_tibble(cbind(levels(direct$ind$Domain), direct$ind$Mean, direct$MSE$Mean))
# let's give them names
colnames(directresults) <- c("admin2Pcod", "insecure", "se")
# let's turn the mean and MSE into numeric values as well as take the square root of MSE for the standard error
directresults$insecure <- as.numeric(directresults$insecure)
directresults$se <- sqrt(as.numeric(directresults$se))

# can also calculate the coefficient of variation:
directresults$cv <- (directresults$se/directresults$insecure)*100
summary(directresults$cv)  # we already see why direct estimates can be misleading. Lots of 0 because of no variation in the survey domain!
# However, 75th percentile is 17.4, while max for EBP was ~15. Already clear improvements.
# Unfortunately, we don't have a census so we can't check accuracy!

