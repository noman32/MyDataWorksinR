#==========================
##### Basic Operations ####
#==========================
5 + 7 # like a calculator
x <- 5 + 7 # now becoming more than a calculator
x

x = 5
x <- 5

p <-  4


1:20

x <- 1:20
x
print(x)

min(x)


max(x)
mean(x)
median(x)
sd(x)
range(x)

y <- x - 3
y

# a collection of numbers called a vector. Any object that contains data is called a data structure and numeric vectors are the simplest type of data structure in R. In fact, even a single number is considered a vector of length one.
z <- c(1.1, 9, 3.14) # a numeric vector with concatenate/combine c() cunction
?c # help on the c function
z

v_testMark <- c(z, 555, z)

z * 2 + 100 # arithmatic op on vector, 
# behind the scene its doing z * c(2, 2, 2) + c(100, 100, 100). This is called vectorization.


my_sqrt <- sqrt(z - 1)
my_sqrt
my_div <- z / my_sqrt
my_div

# another try
c(1, 2, 3, 4) + c(10, 2, 3, 100)

# If the length of the shorter vector does not divide evenly into the length of the longer vector, R will still do vectorization, but will throw a warning to let you know something fishy might be going on
c(1, 2, 3, 4) + c(0, 10, 100) 

#try autocompletion by pressing tab key after typin first few characters of any var name
my_div


### OPTIONAL ###
#===================================
#### FIle and folder operations ####
#===================================

# just get familiarized with getwd() and setwd() funcitions, rest is optional.
"""
Examining local workspace in R and begin to explore the relationship between your workspace and the file system of your machine.
"""

getwd()

ls()
x <- 9
ls()
list.files()
?list.files

args(list.files)

old.dir <- getwd()
dir.create("testdir")

setwd("testdir")
file.create("mytest.R")
list.files()

file.exists("mytest.R")

file.info("mytest.R")

# We can also use the $ operator --- e.g., file.info("mytest.R")$mode --- to grab specific items.

file.rename("mytest.R", "mytest2.R")
file.remove('mytest.R')

file.copy("mytest2.R", "mytest3.R")
file.path("mytest3.R")

file.path("folder1", "folder2")
?dir.create

dir.create(file.path("testdir2", "testdir3"), recursive = TRUE)
unlink("testdir2", recursive = TRUE)

setwd(old.dir)

unlink("testdir", recursive = TRUE)

# =========================================
#### Creating sequences of numbers in R ###
# =========================================
1:20

pi:10

15:1

?seq
seq(1, 20)
seq(0, -10, by = - 0.5)

my_seq <- seq(5, 10, length=30)

length(my_seq)

1:length(my_seq)

#seq(along.with = my_seq)
#seq_along(my_seq)

rep(0, times = 40)

rep(c(0, 1, 2), times = 10)

rep(c(0, 1, 2), each = 10)

my_sequence <- seq(1, 10, by = 0.5)
# ========================================================
### Vectors, simplest and common-most data struct in R ###
# ========================================================
 
# Vectors come in two diff types: atomic vectors and lists. 
# Atomic vector: contains exactly one data type (numeric, logical, character, integer, complex)
# List: may contain multiple data types
# Logical vectors can contain the values TRUE, FALSE, and NA (for 'not available'). These values are generated as the result of logical 'conditions'. 

# First, create a numeric vector num_vect that contains the values 0.5, 55, -10, and 6.
num_vect <- c(0.5, 55, -10, 6)
class(num_vect)

p <- c(0.5, "noman" , -10, 6)
class(p)

class(c(5, 55, -10, 6))

p < 7

num_vect < 1

num_vect >= 6

rm()
x <- 5
print(x)

p <- 4 # NO
4 = 4

p = 4


(3 > 5) || (4 == 4)

#(TRUE == FALSE) || (TRUE == TRUE)
#((111 >= 111) || !(TRUE)) & ((4 + 1) == 5)

# character vector
my_char <- c("My", "name", "is")
my_char

class(my_char)


paste(my_char, collapse = " ")

#my_name <- c(my_char, "Swirl")
#my_name
#paste(my_name, collapse = " ")

paste("Hello", "world!", sep = " ")

#paste(1:3, c("X", "Y", "Z"), sep = "")

#paste(LETTERS, 1:4, sep = "-")

#===============================
### Missing values handling ###
#===============================
# In R, NA is used to represent any value that is 'not available' or 'missing' (in the statistical sense). Lets explore missing values further. Any operation involving NA generally yields NA as the result. 
# missing values must not be ignored, but rather they should be carefully studied to see if there's an underlying pattern or cause for their missingness


x <- c(44, NA, 5, NA)
x * 3

y <- rnorm(1000)

z <- rep(NA, 1000)
z

my_data <- sample(c(y, z), 100)

my_na <- is.na(my_data)
my_na

# my_data == NA
# returns a vector of the same length as my_data that contains all NAs, coz, NA is not really a value, but just a placeholder for a quantity that is not available. Therefore the logical expression is incomplete and R has no choice but to return all NAs.

sum(my_na)

my_data

# NaN - second type of missing value, which stands for 'not a number'. To generate NaN, try dividing (using a forward slash) 0 by 0 now.

0/0

Inf - Inf

#=========================================================
### Extract elements from a vector based on conditions ###
#=========================================================
x<- sample(c(y, z), 20)
x
# 'subset' from a vector by placing an 'index vector' in []
x[1:10]

x[0]
x[1]

# Index vectors come in four different flavors -- logical vectors, vectors of positive integers, vectors of negative integers, and vectors of character strings.
# Let's start by indexing with logical vectors. One common scenario when working with real-world data is that we want to extract all elements of a vector that are not NA (i.e. missing data).

x[is.na(x)]

y <- x[!is.na(x)]
y

y[y > 0] 
# this different to
x[x > 0]

# Since NA is not a value, but rather a placeholder for an unknown quantity, the expression NA > 0 evaluates to NA. Hence we get a bunch of NAs mixed in with our positive numbers when we do this.

# but it works, i mean only returns positive values
x[!is.na(x) & x > 0]

# R uses 'one-based indexing', unlike python etc

x[c(3, 5, 7)]

x[0]
x[1]
x[3000]

#R accepts negative integer indexes. 
x[c(-2, -10)]
x[-c(2, 10)]

# So far logical, positive integer, and negative integer type index vectors are covered. Rremaining is the concept of \'named\' elements.

p<-c(11, 2, NA)
class(p)
p[3]

vect <- c(foo = 11, bar = 2, norf = NA)
vect
vect[1]
vect["bar"]

names(vect)
vect2 <- c(11, 2, NA)
names(vect2) <- c("foo", "bar", "norf")
#identical(vect, vect2)

vect["bar"]
vect[2]
vect["2"]
vect[c("foo", "bar")]

#=============================
### Metrics and dataframes ###
#=============================

# They represent \'rectangular\' data types, meaning that they are used to store tabular data, with rows and columns. The main difference is that matrices can only contain a single class of data, while data frames can consist of many different classes of data.

my_vector <- 1:20
my_vector
class(my_vector)

dim(my_vector) # doesn't have a `dim` attribute (so it's just NULL)
length(my_vector)

# but we can assign dim attribute to it
dim(my_vector) <- c(4, 5)
my_vector

dim(my_vector)
attributes(my_vector)

# It is not a vector any more. It's a matrix. View the contents of my_vector now to see what it looks like.

my_vector
class(my_vector)

my_matrix <- my_vector

# So, matrix is simply an atomic vector with a dimension attribute. A more direct method of creating the same matrix uses the matrix() function.

my_matrix2 <- matrix(1:20, nrow=4, ncol=5)
# identical(my_matrix, my_matrix2)
class(my_matrix2)
# patients is a character vector
patients <- c("Bill", "Gina", "Kelly", "Sean") 
cbind(patients, my_matrix2)

# Combining the character vector with our matrix of numbers caused everything to be enclosed in double quotes. This means we're left with a matrix of character strings.
# As we already know, matrices can only contain ONE class of data.  Therefore, when we tried to combine a character vector with a numeric matrix, R was forced to 'coerce' the numbers to characters, hence the double quotes. This is called 'implicit coercion'.

# Now, how to include the names of our patients in the table without destroying the integrity of our numeric data?

data.frame(patients, my_matrix2)
my_data <- data.frame(patients, my_matrix2)
my_data

# data.frame() function takes any number of arguments and returns a single object of class `data.frame` that is composed of the original objects.

class(my_data)

cnames <- c("patient", "age", "weight", "bp", "rating", "test")
colnames(my_data) <- cnames

my_data

x<- 1:20
(x)

#===============
###   EDA    ###
#===============

# Whenever you're working with a new dataset, the first thing you should do is look at it! What is the format of the data? What are the dimensions? What are the variable names? How are the variables stored? Are there missing data? Are there any flaws in the data?	

# PLANTS Database (http://plants.usda.gov/adv_search.html).	

setwd("C:/Noman/MIT_dataAnalysisForSocialScience/14_310x_Intro_to_R")
plants <- read.csv("plants.csv")

class(plants) #default class for data read using eg. read.csv() and read.table()

dim(plants)

nrow(plants)	

ncol(plants)	

object.size(plants)	

names(plants)	
 
head(plants)

head(plants, 10)

tail(plants, 15)	

summary(plants)
#provides different output for each variable, depending on its class. For numeric data, it displays the min, 1st Q, median, mean, 3rd quartile, and max. For categorical variables (called 'factor' variables in R), it shows the count.

summary(plants$Active_Growth_Period)
summary(plants$Temp_Min_F)

table(plants$Duration)
table(plants$Active_Growth_Period)

str(plants)	
#str() can be used on most objects in R. Any time you want to understand the structure of something (a dataset, function, etc.), str() is a good function.

View(plants)
#View() is a function that opens a spreadsheet-like viewer of the data

#=======================
### Base graphics ###
#=======================

# A greatest strengths of R, relative to other programming languages, is the ease with which we can	create publication-quality graphics 
# Advanced graphics approaches in R include lattice, ggplot2 and ggvis.	
# Lets start with Base graphics in R.	
#There is a school of thought that this approach is backwards, that we should learn ggplot2 first: 
#http://varianceexplained.org/r/teach_ggplot2_to_beginners/ for an #outline of this view.	

# Load the included data frame cars with data(cars).	

data(cars)
?cars	
	
head(cars)	

# Lets get a sense of the data with eg. dim(), names(), head(), tail() and summary()

# now lts plot the data.
plot(cars)

# it has just two columns, so it assumes that you want to plot one column versus the other.	
# 'plot' is short for scatterplot.
?plot

plot(cars$speed, cars$dist)
plot(x = cars$speed, y = cars$dist)

# There are other ways to call the plot command, i.e., using the "formula" interface, eg., plot(dist ~ speed, cars).
# The formula interface is useful when you have many variables in your dataset and you want to plot one versus another.

plot(x = cars$dist, y = cars$speed)	
plot(x = cars$speed, y = cars$dist, xlab = "Speed")
plot(x = cars$speed, y = cars$dist, ylab = "Stopping Distance")
plot(x = cars$speed, y = cars$dist, xlab = "Speed", ylab = "Stopping Distance")
plot(cars, main = "My Plot")
plot(cars, sub = "My Plot Subtitle")
plot(cars, col = 2)
plot(cars, xlim = c(10, 15))
plot(cars, pch = 2)

# Arguments like "col" and "pch" may not seem very intuitive. So many prefer modern packages like ggplot2, for creating their graphics in R. However, its useful to have an introduction to base graphics because many of the idioms in lattice and ggplot2 are modeled on them.

data(mtcars)
str(cars)
str(mtcars)

# boxplot(), like many R functions, also takes a "formula" argument, generally an expression with a tilde ("~")	
#plot(formula = mpg ~ cyl, data = mtcars)
#boxplot(formula = mpg ~ cyl, data = mtcars)
plot(formula = mpg ~ cyl, data = mtcars)
boxplot(formula = mpg ~ cyl, data = mtcars)

hist(mtcars$mpg)	
# (http://www.ling.upenn.edu/~joseff/rstudy/week4.html) provides a useful overview.	

#=======================
### Dplyr package ####
#=======================
install.packages("dplyr")
library(dplyr)
packageVersion("dplyr")

# dplyr is a fast and powerful R package written by Hadley Wickham and Romain Francois that provides a consistent and concise grammar for manipulating tabular data.
# One unique aspect of dplyr is that the same set of tools allow you to work with tabular data from a variety of sources, including data frames, data tables, databases and multidimensional arrays. Here our focus is on data frames, but everything you learn will apply equally to other formats.

# Our dataset now:
# "CRAN is a network of ftp and web servers around the world that store identical, up-to-date, versions of code and documentation for R" (http://cran.rstudio.com/). RStudio maintains one of these so-called 'CRAN mirrors'  and they generously make their download logs publicly available (http://cran-logs.rstudio.com/). We'll be working with the log from July 8, 2014, which contains information on roughly 225,000 package downloads.
setwd("G:/My Drive/ABP/ABP DSBA Batch 03/Session 03")
getwd()
mydf <- read.csv("2014-07-08.csv") # default stringsAsFactors = FALSE
View(mydf)
dim(mydf)
head(mydf,10)

# The first step of working with data in dplyr is to load the data into what the package authors call a 'data frame tbl' or 'tbl_df'.

# cran <- tbl_df(mydf) # this deprecated function is now called as_tibble()
library(dplyr)

cran <- as_tibble(mydf)
rm("mydf")

# main advantage to using a tbl_df over a regular data frame is the printing."
cran

# "The dplyr philosophy is to have small functions that each do one thing well." 
# Specifically, dplyr supplies five 'verbs' that cover most fundamental data manipulation tasks: select(), filter(), arrange(), mutate(), and summarize().

# first select() to select a set of columns from a data frame.
?select

select(cran, ip_id, package, country)

select(cran, r_arch:country)

select(cran, country:r_arch)

select(cran, -time)

select(cran, -(date:size))

# Next: "How do I select a subset of rows?" That's where the filter() function comes in.

filter(cran, package == "swirl")

filter(cran, package == "swirl", r_version == "3.1.1", country == "US")

filter(cran, r_version <= "3.0.2", country == "IN")

filter(cran, country == "US" | country == "IN")

filter(cran, size > 100500, r_os == "linux-gnu")

#is.na(c(3, 5, NA, 10))
#!is.na(c(3, 5, NA, 10))

filter(cran, !is.na(r_version))

# Now arrange() to reorder the rows of a data frame according to one of the variables/columns.

cran2 <- select(cran, size:ip_id)

arrange(cran2, ip_id)

arrange(cran2, desc(ip_id))

print(arrange(cran2, package, ip_id), n=100)

arrange(cran2, country, desc(r_version), ip_id)

# mutate() to create new variables based on transformations of existing variables.

cran3 <- select(cran, ip_id, package, size)
cran3

mutate(cran3, size_mb = size / 2^20)

mutate(cran3, size_mb = size / 2^20, size_gb = size_mb / 2^10)

# lets assume, there was a glith in size col
mutate(cran3, correct_size = size + 1000)

# summarize() to collapse each group into a single-row summary of that group.

summarize(cran, avg_bytes = mean(size))
summarize(cran, mean(size))

# summarize() is most useful when working with data that has been grouped by the values of a particular variable.

?group_by
cran
by_package <- group_by(cran, package)
by_package

# At the top of the output above, we see 'Groups: package', which tells us that this tbl has been grouped by the package variable. Everything else looks the same, but now any operation we apply to the grouped data will take place on a per package basis.
# When we applied mean(size) to the original tbl_df via summarize(), it returned a single number. but wouldn't it be so much more interesting to look at the mean download size for each unique package?

summarize(by_package, mean(size))

pack_sum <- summarize(by_package, n(), n_distinct(ip_id), n_distinct(country), mean(size))
pack_sum

pack_sum <- summarize(by_package, count = n(), unique = n_distinct(ip_id), countries = n_distinct(country), avg_bytes = mean(size))
pack_sum

# now, lets try to know the value of 'count' that splits the data into the top 1% and bottom 99% of packages based on total downloads. In statistics, this is called the 0.99, or 99%, sample quantile. 

quantile(pack_sum$count, probs = 0.99)

# Lets isolate those packages which had more than 679 total downloads.
top_counts <- filter(pack_sum, count > 679)
top_counts
View(top_counts)

# Lets sort the table by the count column in descending order.
top_counts_sorted <- arrange(top_counts, desc(count))
View(top_counts_sorted)

# now lets see the top packages by country having more than 60 country count
top_countries <- filter(pack_sum, countries > 60)
result1 <- arrange(top_countries, desc(countries), avg_bytes)
print(result1)


# now lest use a special chaining operator, %>%, which was originally introduced in the magrittr R package and has now become a key component of dplyr. The benefit of %>% is that it allows us to chain the function calls in a linear fashion. The code to the right of %>% operates on the result from the code to the left of %>%.
 

# liets print the top countries as we did above using %>% operator
result3 <-
  cran %>%
  group_by(package) %>%
  summarize(count = n(), unique = n_distinct(ip_id), countries = n_distinct(country), avg_bytes = mean(size)
  ) %>%
  filter(countries > 60) %>%
  arrange(desc(countries), avg_bytes)

print(result3)

# let's work through a few more simple examples of chaining.
cran %>%   
  select( ip_id, country, package, size) %>%   
  print

cran %>%  
  select(ip_id, country, package, size) %>%   
  mutate(size_mb = size / 2^20)

cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%
  filter(size_mb <= 0.5)

cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%
  filter(size_mb <= 0.5) %>%
  arrange(desc(size_mb))


### OPTIONAL except get basic understanding of gather(), separate(), and spread() functions ###
#=======================================
### Tidying data with tidyr package ####
#=======================================

library(readr)
library(tidyr)
library(dplyr)

# The author of tidyr, Hadley Wickham, discusses his philosophy of tidy data in his 'Tidy Data' paper: http://vita.had.co.nz/papers/tidy-data.pdf
 
# tidy data satisfies three conditions:
# 1) Each variable forms a column
# 2) Each observation forms a row
# 3) Each type of observational unit forms a table

# Any dataset that doesn't satisfy these conditions is considered 'messy' data. 

# The first problem is when we have column headers that are values, not variable names. 

students <- read.csv("students.txt")
students

# This dataset actually has three variables: grade, sex, and count. The first variable, grade, is already a column, so that should remain as it is. The second variable, sex, is captured by the second and third column headings. The third variable, count, is the number of students for each combination of grade and sex.

?gather
gather(
  data,
  key = "key",
  value = "value",
  ..., #A selection of columns. If empty, all variables are selected. You can supply bare variable names, select all variables between x and z with x:z, exclude y with -y.

)

gather(data = students, key = "gender", value = "count", -grade)
gather(students, key = gender, value = count, -grade)

p<-gather(students, gender, count, -grade)
p
spread(p, gender, count)
# Note the minus sign before grade, which says we want to gather all columns EXCEPT grade.

# It's important to understand what each argument to gather() means. The data argument, students, gives the name of the original dataset. The key and value arguments -- sex and count, respectively -- give the column names for our tidy dataset. The final argument, -grade, says that we want to gather all columns EXCEPT the grade column (since grade is already a proper column variable.)

# The second messy data case we'll look at is when multiple variables are stored in one column. 

students2 <- read.csv("students2.txt")
students2

# This dataset is similar to the first, except now there are two separate classes, 1 and 2, and we have total counts for each sex within each class. students2 suffers from the same messy data problem of having column headers that are values (male_1, female_1, etc.) and not variable names (sex, class, and count).

# However, it also has multiple variables stored in each column (sex and class), which is another common symptom of messy data. Tidying this dataset will be a two step process.

#Step 1:
res <- gather(students2, gender_class, count, -grade)
res

?separate
# arguments: data = res, col = sex_class, into = c("sex", "class"). ng
# We don't have to provide the argument names as long as they are in the correct order.

#Step 2
separate(res, gender_class, c("gender", "class"))

# Conveniently, separate() was able to figure out on its own how to separate the sex_class column. Unless you request otherwise with the 'sep' argument, it splits on non-alphanumeric values. In other words, it assumes that the values are separated by something other than a letter or number (in this case, an underscore.)

# Now the two steps in one go with %>% operator
students2 %>%
  gather(gender_class, count, -grade) %>%
  separate(gender_class, c("gender", "class")) %>%
  print

# A third symptom of messy data is when variables are stored in both rows and columns. 
students3 <- read.csv("students3.txt")
students3

# In students3, we have midterm and final exam grades for five students, each of whom were enrolled in exactly two of five possible classes.

# The first variable, name, is already a column and should remain as it is. The headers of the last five columns, class1 through class5, are all different values of what should be a class variable. The values in the test column, midterm and final, should each be its own variable containing the respective grades for each student.

# This will require multiple steps, which we will build up gradually using %>%.

# Step 01
students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  print

# Step 02
?spread

students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  print

# Step 03
# readr is required for certain data manipulations, such as parse_number(), which will be used in the next step.

library(readr)
parse_number("class5")

students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  mutate(class = parse_number(class)) %>%
  print

# The fourth messy data problem we'll look at occurs when multiple observational units are stored in the same table.

students4<- read.csv("students4.txt")
students4
# students4 is almost the same as our tidy version of students3. The only difference is that students4 provides a unique id for each student, as well as his or her sex (M = male; F = female).

# At first glance, there doesn't seem to be much of a problem with students4. All columns are variables and all rows are observations. However, notice that each id, name, and sex is repeated twice, which seems quite redundant. This is a hint that our data contains multiple observational units in a single table.

# Our solution will be to break students4 into two separate tables -- one containing basic student information (id, name, and sex) and the other containing grades (id, class, midterm, final).

student_info <- students4 %>%
  select(id, name, gender) %>%
  print

student_info <- students4 %>%
  select(id, name, gender) %>%
  unique %>%
  print

gradebook <- students4 %>%
  select(id, class, midterm, final) %>%
  print

# The fifth and final messy data scenario that we'll address is when a single observational unit is stored in multiple tables. It's the opposite of the fourth problem.

passed <- read.csv("passed.txt")
passed

failed <- read.csv("failed.txt")
failed

# Teachers decided to only take into consideration final exam grades in determining whether students passed or failed each class. As you may have inferred from the data, students passed a class if they received a final exam grade of A or B and failed otherwise.

# The name of each dataset actually represents the value of a new variable that we will call 'status'. Before joining the two tables together, we'll add a new column to each containing this information so that it's not lost when we put everything together.

# Use dplyr's mutate() to add a new column to the passed table. The column should be called status and the value, "passed" (a character string), should be the same for all students. 'Overwrite' the current version of passed with the new one.

passed <- passed %>% mutate(status = "passed")
failed <- failed %>% mutate(status = "failed")

passed
failed

bind_rows(passed, failed)

# Of course, we could arrange the rows however we wish at this point, but the important thing is that each row is an observation, each column is a variable, and the table contains a single observational unit. Thus, the data are tidy.

#=====================================================
#### A complete example of tidying a real dataset ####
#=====================================================
# The dataset we'll be working with is called 'Total Group Report 2013' and can be found here: http://research.collegeboard.org/programs/sat/data/cb-seniors-2013

#https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwiy-frdgeb-AhULTGwGHYucADMQFnoECB0QAQ&url=https%3A%2F%2Fsecure-media.collegeboard.org%2FdigitalServices%2Fpdf%2Fresearch%2F2013%2FGA_13_03_03_01.pdf&usg=AOvVaw3RnUJsOcvcWB7ebPriwBCf

sat<-read.csv("sat.txt")
sat

sat %>%
  select(-contains("total")) %>%
  gather(part_gender, count, -score_range) %>%
  separate(part_gender, c("part", "gender")) %>%
  print

sat %>%
  select(-contains("total")) %>%
  gather(part_gender, count, -score_range) %>%
  separate(part_gender, c("part", "gender")) %>%
  group_by(part, gender) %>%
  mutate(total = sum(count),
  ) %>% 
  print
