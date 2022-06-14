library(mdsr)
library(tidyverse)
library(tidymodels)
library(dplyr)
library(yardstick)
library(GGally)
library(moderndive)
#library(parsnip)
#library(rsample)
#library(rminer)
#library(purrr)

filename <- read.csv("student-mat.csv",header = TRUE,sep = ';')

student_math <- filename %>%
  select(-c(school))%>%
  mutate(Medu = factor(Medu, levels = c(0, 1, 2, 3, 4), labels = c('None', 'primary', 'middle', 'secondary', 'higher')),
         Fedu = factor(Fedu, levels = c(0, 1, 2, 3, 4), labels = c('None', 'primary', 'middle', 'secondary', 'higher')),
         traveltime = factor(traveltime, levels = c(1, 2, 3, 4), labels = c('< 15', '15 to 30', '30 to hour', '> hour')),
         studytime = factor(studytime, levels = c(1, 2, 3, 4), labels = c('2 hrs', '2 to 5 hrs', '5 to 10 hrs', '> 10 hrs')),
         sex = factor(sex),
         address = factor(address),
         famsize = factor(famsize),
         Pstatus = factor(Pstatus),
         Mjob = factor(Mjob),
         Fjob = factor(Fjob),
         famrel = factor(famrel),
         freetime = factor(freetime),
         goout = factor(goout),
         Dalc = factor(Dalc),
         Walc = factor(Walc),
         health = factor(health),
         
         G1 = as.numeric(G1),
         G2 = as.numeric(G2),
         G3 = as.numeric(G3),
         age = as.numeric(age),
         absences = as.numeric(absences))%>%
  na.omit()

student_math

glimpse(student_math)

length(names(student_math))
#You need summarry stuff and visuals right here!!!!!
summary(student_math)

#eda Report
#library(dlookr)

#student_math$G3 = as.factor(student_math$G3)
#student_math %>%
  #eda_paged_report(target = "G3", subtitle = "Student Final Grade",
                   #output_dir = "./", output_file = "EDA.pdf", theme = "blue")



#ggpairs(student_math, title="correlogram with ggpairs()") 

library(dplyr)
student_math %>% summarise_if(is.numeric, var)

#corr.
dfnum <- select_if(student_math, is.numeric) 
ggcorr(dfnum, method =  "pairwise.complete.obs")


#scatterplots 
ggpairs(dfnum, title="correlogram with ggpairs()") 


#Feature selection
library(earth)
Model <- earth(G3 ~ ., data=student_math) # build model
ev <- evimp(Model) 
ev
plot(ev)



#Training testing/split
set.seed(364)

n <- nrow(student_math)
student_math_parts <- student_math %>%
  initial_split(prop = 0.8)


train <- student_math_parts %>%
  training()

test <- student_math_parts %>%
  testing()

list(train, test)

names(train)


#choosing variables for model 1
train1 = train %>% 
  select(G3, G2, absences, famrel,  failures, activities, health, Fedu)
train1
test1 = test %>% 
  select(G3, G2, absences, famrel,  failures, activities, health, Fedu)
test1

#A1
#multiple linear regression
model <- lm(G3 ~ ., data = train1)
tidy(model)
glance(model)
glance(model)$r.squared  # R^2
glance(model)$adj.r.squared  # adjusted R^2
summary(model)

model1predict = predict(model, newdata = test1)
model1predict
mean((model1predict - test$G3)^2)

regression_points <- get_regression_points(model)
regression_points


#choosing variables for model 2
train2 = train %>% 
  select(!c(G2, absences, famrel,  failures, activities, health, Fedu))
train2
test2 = test %>% 
  select(!c(G2, absences, famrel,  failures, activities, health, Fedu))
test2

#multiple linear regression 2
model2 <- lm(G3 ~ ., data = train2)
tidy(model2)
glance(model2)
glance(model2)$r.squared  # R^2
glance(model2)$adj.r.squared  # adjusted R^2
summary(model2)
summary(model2)$coefficient

model2predict = predict(model2, newdata = test2)
model2predict
mean((model2predict - test2$G3)^2)

#############################################################################
#A2
student_math$g3_binary = ifelse(student_math$G3>=10,'pass','fail')
student_math



#new Training testing/split
student_math$g3_binary = as.factor(student_math$g3_binary)
set.seed(364)

n <- nrow(student_math)
student_math_parts <- student_math %>%
  initial_split(prop = 0.8)


train <- student_math_parts %>%
  training()

test <- student_math_parts %>%
  testing()

list(train, test)

names(train)




#form
train_A2 = train %>% 
  select(g3_binary, G2, absences, famrel,  failures, activities, health, Fedu)
train_A2

test_A2 = test %>% 
  select(g3_binary, G2, absences, famrel,  failures, activities, health, Fedu)
test_A2

form <- as.formula(
  "g3_binary ~ ."
)


mod_tree <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>%
  fit(form, data = train_A2)
mod_tree

pred <- train_A2 %>%
  select(g3_binary) %>%
  bind_cols(
    predict(mod_tree, new_data = train_A2, type = "class")
  ) %>%
  rename(g3_dtree = .pred_class)

# Naive Bayes
library(discrim)
library(klaR)
mod_nb <- naive_Bayes(mode = "classification") %>%
  set_engine("klaR") %>%
  fit(form, data = train_A2)

pred <- pred %>%  
  bind_cols(
    predict(mod_nb, new_data = train_A2, type = "class")
  ) %>%
  rename(g3_nb = .pred_class)

accuracy(pred, g3_binary, g3_nb)

# Anns
set.seed(1)
library(nnet)
mod_nn <- mlp(mode = "classification", hidden_units = 9,activation = 'relu') %>%
  set_engine("nnet") %>%
  fit(form, data = train_A2)

NeuralNetTools::plotnet(mod_nn$fit, circle_cex = 3, cex_val = 0.7, alpha_val = 0.3)


pred <- pred %>%
  bind_cols(
    predict(mod_nn, new_data = train_A2, type = "class")
  ) %>%
  rename(g3_nn = .pred_class)

accuracy(pred, g3_binary, g3_nn)



# Model comparison
mods <- tibble(
  type = c(
    "neural_net", "naive_bayes"
  ),
  mod = list(
    mod_nn, mod_nb
  )
)

mods <- mods %>%
  mutate(
    y_train = list(pull(train_A2, g3_binary)),
    y_test = list(pull(test_A2, g3_binary)),
    y_hat_train = map(
      mod, 
      ~pull(predict(.x, new_data = train_A2, type = "class"), .pred_class)
    ),
    y_hat_test = map(
      mod, 
      ~pull(predict(.x, new_data = test_A2, type = "class"), .pred_class)
    )
  )
mods



mods <- mods %>%
  mutate(
    accuracy_train = map2_dbl(y_train, y_hat_train, accuracy_vec),
    accuracy_test = map2_dbl(y_test, y_hat_test, accuracy_vec),
    sens_test = map2_dbl(
      y_test,
      y_hat_test,
      sens_vec,
      event_level = "second"
    ),
    spec_test = map2_dbl(y_test,
                         y_hat_test,
                         spec_vec,
                         event_level = "second"
    )
  )
mods
mods %>%
  select(-mod, -matches("^y_")) %>%
  arrange(desc(accuracy_test))



pred_class_nb = predict(mod_nb, test_A2, type = 'class')
pred_class_nn = predict(mod_nn, test_A2, type = 'class')



#Creating confusion matrix of testing data
library(caret)
nb_cf <- confusionMatrix(data=factor(pred_class_nb$.pred_class), reference = factor(test_A2$g3_binary))
nb_cf

nn_cf <- confusionMatrix(data=factor(pred_class_nn$.pred_class), reference = factor(test_A2$g3_binary))
nn_cf


################################################################################
#A3 without G2
library(dplyr)
train_A3 = train %>% 
  select(g3_binary, absences, famrel,  failures, activities, health, Fedu)
train_A3

test_A3 = test %>% 
  select(g3_binary, absences, famrel,  failures, activities, health, Fedu)
test_A3

mod_tree <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>%
  fit(form, data = train_A3)
mod_tree

pred <- train_A3 %>%
  select(g3_binary) %>%
  bind_cols(
    predict(mod_tree, new_data = train_A3, type = "class")
  ) %>%
  rename(g3_dtree = .pred_class)

# Naive Bayes
mod_nb <- naive_Bayes(mode = "classification") %>%
  set_engine("klaR") %>%
  fit(form, data = train_A3)

pred <- pred %>%  
  bind_cols(
    predict(mod_nb, new_data = train_A3, type = "class")
  ) %>%
  rename(g3_nb = .pred_class)

accuracy(pred, g3_binary, g3_nb)

# Anns
mod_nn <- mlp(mode = "classification", hidden_units = 9,activation = 'relu') %>%
  set_engine("nnet") %>%
  fit(form, data = train_A3)

NeuralNetTools::plotnet(mod_nn$fit, circle_cex = 3, cex_val = 0.7, alpha_val = 0.3)


pred <- pred %>%
  bind_cols(
    predict(mod_nn, new_data = train_A3, type = "class")
  ) %>%
  rename(g3_nn = .pred_class)

accuracy(pred, g3_binary, g3_nn)



# Model comparison
mods <- tibble(
  type = c(
    "neural_net", "naive_bayes"
  ),
  mod = list(
    mod_nn, mod_nb
  )
)

mods <- mods %>%
  mutate(
    y_train = list(pull(train_A3, g3_binary)),
    y_test = list(pull(test_A3, g3_binary)),
    y_hat_train = map(
      mod, 
      ~pull(predict(.x, new_data = train_A3, type = "class"), .pred_class)
    ),
    y_hat_test = map(
      mod, 
      ~pull(predict(.x, new_data = test_A3, type = "class"), .pred_class)
    )
  )
mods



mods <- mods %>%
  mutate(
    accuracy_train = map2_dbl(y_train, y_hat_train, accuracy_vec),
    accuracy_test = map2_dbl(y_test, y_hat_test, accuracy_vec),
    sens_test = map2_dbl(
      y_test,
      y_hat_test,
      sens_vec,
      event_level = "second"
    ),
    spec_test = map2_dbl(y_test,
                         y_hat_test,
                         spec_vec,
                         event_level = "second"
    )
  )
mods %>%
  select(-mod, -matches("^y")) %>%
  arrange(desc(accuracy_test))



pred_class_nb = predict(mod_nb, test_A3, type = 'class')
pred_class_nn = predict(mod_nn, test_A3, type = 'class')



#Creating confusion matrix of testing data
nb_cf <- confusionMatrix(data=factor(pred_class_nn$.pred_class), reference = factor(test_A3$g3_binary))
nb_cf

nn_cf <- confusionMatrix(data=factor(pred_class_nb$.pred_class), reference = factor(test_A3$g3_binary))
nn_cf



