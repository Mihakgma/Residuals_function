# Удалить ВСЕ объекты из глобального окружения!
# remove EVERYTHING from Environment!
rm(list=ls(all=TRUE))

# take mtcars DF
# в качестве датафрейма берем данные из встроенного mtcars
my_mtcars <- mtcars

View(my_mtcars)

which_residual <- function(x){
  qu_vec <- quantile(x, na.rm = T)
  nizh_qu <- qu_vec[2]
  verh_qu <- qu_vec[4]
  ind_n <- which(x < nizh_qu - 1.5*IQR(x, na.rm = T))
  ind_v <- which(x > verh_qu + 1.5*IQR(x, na.rm = T))
  car_model <- as.character(rownames(my_mtcars))
  return(c("Нижние выбросы (Lower residuals): ", car_model[ind_n], 
           "Верхние выбросы (Upper residuals): ", car_model[ind_v]))
} 

# Прогоняем полученную функцию определения выбросов по количественным показателям!
# roll this function around our dataframe columns!

my_residuals <- sapply(my_mtcars, which_residual)

# Смотрим выбросы
# let's look on residuals!
View(my_residuals)