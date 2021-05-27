library(sqldf)
library(ggplot2)
library(vcd)
# import dataset with MATH 119 enrollment data:
class = read.csv(file = "Math119_enrollment.csv")

# find dimensions of dataset
dim(class)

# changing the grades to simplified grades so the transfer and utk grades match
# first creating an empty column called location
class_empty = rep(NA,length(class$grade))
class$location = class_empty
# then changing the grades to simplified grades and creating a new column to ID whether the course is utk or transfer
for (i in seq_along(class$grade))
if (!is.na(class$grade[i]) && class$grade[i] == "A" || class$grade[i] =="A-") {
   class$grade[i] = "A"
   class$location[i] = "UTK"
} else if (!is.na(class$grade[i]) && class$grade[i] == "A.") {
   class$grade[i] = "A"
   class$location[i] = "transfer"
} else if (!is.na(class$grade[i]) && class$grade[i] == "B" || class$grade[i] == "B-" || class$grade[i] == "B+") {
    class$grade[i] = "B"
    class$location[i] = "UTK"
} else if (!is.na(class$grade[i]) && class$grade[i] == "B.") {
    class$grade[i] = "B"
    class$location[i] = "transfer"
} else if (!is.na(class$grade[i]) && class$grade[i] == "C" || class$grade[i] == "C+"){
    class$grade[i] = "C"
    class$location[i] = "UTK"
} else if (!is.na(class$grade[i]) && class$grade[i] == "C." || class$grade[i] == "C+."){
    class$grade[i] = "C"
    class$location[i] = "transfer"
} else if (!is.na(class$grade[i]) && class$grade[i] == "D"){
    class$location[i] = "UTK"
} else if (!is.na(class$grade[i]) && class$grade[i] == "D."){
    class$grade[i] = "D"
    class$location[i] = "transfer"
} else if (!is.na(class$grade[i]) && class$grade[i] == "F"){
    class$location[i] = "UTK"
} else if (!is.na(class$grade[i]) && class$grade[i] =="F.") {
    class$grade[i] = "F"
    class$location[i] = "transfer"
} else if (!is.na(class$grade[i]) && class$grade[i] == "NC" || class$grade[i] == "NR") {
    class$grade[i] = "NC"
    class$location[i] = "UTK"
} else if (!is.na(class$grade[i]) && class$grade[i] == "NC."){
    class$grade[i] = "NC"
    class$location[i] = "transfer"
} else if (!is.na(class$grade[i]) && class$grade[i] == "S") {
    class$location[i] = "UTK"
} else if (!is.na(class$grade[i]) && class$grade[i] == "W" || class$grade[i] == "WF" || class$grade[i] == "WP"){
    class$grade[i] = "W"
    class$location[i] = "UTK"
} else if (!is.na(class$grade[i]) && class$grade[i] =="WP." || class$grade[i] == "W."){
    class$grade[i] = "W"
    class$location[i] = "transfer"
} else {print("you missed something")
}

# separate out the transfer grades and utk grades into separate tables using SQL
transfer_grades = droplevels(sqldf('SELECT * FROM class WHERE location == "transfer"'))
utk_grades = droplevels(sqldf('SELECT * FROM class WHERE location == "UTK"'))
# look at the frequency of final grades
grade_freq_utk = table(utk_grades$grade)
grade_freq_transfer = table(transfer_grades$grade)
# converting the grade frequency tables into a dataframe
grade_freq_utk = as.data.frame(grade_freq_utk)
colnames(grade_freq_utk) = c("grade", "freq")
grade_freq_transfer = as.data.frame(grade_freq_transfer)
colnames(grade_freq_transfer) = c("grade", "freq")
# # separating the dataframe into utk grades and transfer grades
#     # changing the column names to something more meaningful
#     colnames(grade_freq) = c("grade", "freq")
#     # removing the transfer grades from the dataset to create a utk gradeset and removing the levels of the removed rows
#     utk_grades = droplevels(grade_freq[-c(3, 6, 9, 11, 13, 15, 17, 21, 24),])
#         # resetting the rownumbers
#         rownames(utk_grades) = 1:nrow(utk_grades)
#     # removing the utk grades to create a transfer dataset
#     transfer_grades = droplevels(grade_freq[c(3, 6, 9, 11, 13, 15, 17, 21, 24),])
#         # resetting the rownumbers
#         rownames(transfer_grades) = 1:nrow(transfer_grades)

# adding a new column that calculates the percent of students achieving that grade to transfer and utk grade dataframes
    # creating an empty vector and adding it to the utk dataframe
    utk_empty = rep(NA,length(grade_freq_utk$freq))
    grade_freq_utk$perc = utk_empty
    # calculating the percent of students that scored each grade in the class
    grade_freq_utk$perc = (grade_freq_utk$freq/sum(as.numeric(grade_freq_utk$freq))) * 100
    # repeating with transfer dataframe
    transfer_empty = rep(NA,length(grade_freq_transfer$freq))
    grade_freq_transfer$perc = transfer_empty
    grade_freq_transfer$perc = (grade_freq_transfer$freq/sum(as.numeric(grade_freq_transfer$freq))) * 100

# # reordering the rows so that the grades are in the correct order for plots
# utk_grades = utk_grades[c(1,2,5,3,4,7,6,8,9,10,11,12,13,14,15),]
# transfer_grades = transfer_grades[c(1,2,4,3,5,6,7,8,9),]
# # changing the order of the levels bc the bar chart labels will be out of order if you don't
# levels(utk_grades$grade) = utk_grades$grade
# levels(transfer_grades$grade) = transfer_grades$grade
# # also changing the grades in the transfer dataframe to remove the "."
# transfer_grades$grade = c("A", "B", "C+", "C", "D", "F", "NC", "W", "WP")

# creating jpegs of the barplots of the percents of each final grade for utk and transfer students
# cex.names changes the size of the font of the labels so they all fit under their bars
jpeg("utk_grade_perc_bar.jpeg")
barplot(grade_freq_utk$perc ~ grade_freq_utk$grade, col = "orange", xlab = "Grade", ylab = "Percent of Students", main = "Distribution of UTK MATH119 Final Grades", cex.names = .7)
dev.off()

jpeg("transfer_grade_perc_bar.jpeg")
barplot(grade_freq_transfer$perc ~ grade_freq_transfer$grade, col = "gray", xlab = "Grade", ylab = "Percent of Students", main = "Distribution of Transfer MATH119 Final Grades", cex.names = .7)
dev.off()

# defining the utk_grades and transfer_grades that the rest of my code uses so as not to 1. break it and 2. have to go through and change everything
utk_grades = grade_freq_utk
transfer_grades = grade_freq_transfer
# create a new data frame with the total students who took the class at utk and who took it as a transfer
    # calculating the sums of students that took the course at utk and who transferred the course
    utk_student_num = sum(as.numeric(utk_grades$freq))
    transfer_student_num = sum(as.numeric(transfer_grades$freq))
    # creating a list of student types
    st_type = c("transfer", "utk")
    # creating a list of student totals
    total = c(transfer_student_num, utk_student_num)
    # creating a dataframe with columns st_type and total with the values in the lists
    student_dist = data.frame(st_type, total)

# creating a barplot that displays the total students that took the class at utk and transfer with totals labels on top of each bar
jpeg("student_dist_bar.jpeg")
ggplot(student_dist,aes(x=reorder(st_type, -total),y=total))+geom_bar(stat="identity",fill=c("gray", "orange"), colour="gray")+ylim(c(0,25000)) + 
    ylab("Total Students") + xlab("Student Type") + ggtitle("UTK vs. Transfer Student Distribution") + geom_text(aes(label=total), position = position_dodge(width = 1.0), vjust=-0.25)
dev.off()

#sorting the class dataframe by student ID and term in SQL in R:
sorted_class = sqldf('SELECT * FROM class ORDER BY studentID, e_term')

# creating a table of the students who took the course multiple times and giving them a rank
repeat_st = droplevels(sqldf('SELECT *
,ROW_NUMBER() OVER(PARTITION BY studentID ORDER BY studentID, e_term) AS rank
FROM class
WHERE studentID IN (
SELECT studentID
FROM class
GROUP BY studentID
HAVING COUNT(*) > 1)
ORDER BY studentID, e_term'))

# getting the number of unique student IDs to see how many students took the course multiple times
distinct_repeat_st = sqldf('SELECT DISTINCT studentID FROM repeat_st')

#getting the number of unique student IDs in the table to see how many total individual students took the course
distinct_class = sqldf('SELECT DISTINCT studentID FROM class')

# getting the dimensions of the previous two to see the total number of indiv studetns taking the course and the total number who retook it
dim(distinct_repeat_st) # = 3615
dim(distinct_class) # = 20008

# so can say that 3615 out of 20008 students retook the class of the students that retook it....
# looking at the rank values to see how many times students retook the course
table(repeat_st$rank)
# ... they retook it anywhere from 2 to 7 times with 1 retaking it 7 times, 3 6 times, 7 5 times, 68 4 times, 463 3 times, and 3073 2 times
# with the average student retaking it 2.2 times
# creating a dataframe of the accurate rep values
repeat_rank = c(2:7)
repeat_total = c(3073, 463, 68, 7, 3, 1)
repeat_values = data.frame(repeat_rank, repeat_total)   # creating dataframe called repeat_values
colnames(repeat_values) = c("rank", "total")    # changing colnames to something more streamlined

# to get the first time they took the course:
repeat_first = sqldf('SELECT * FROM repeat_st WHERE rank == 1')

# to get the last time they took the course:
# Have to put DESC in the second line to get the ranks to be distributed in reverse so the last course taken will have a 1 and you can use your code again...
repeat_rev_rank = droplevels(sqldf('SELECT *
,ROW_NUMBER() OVER(PARTITION BY studentID ORDER BY studentID, e_term DESC) AS rank
FROM class
WHERE studentID IN (
SELECT studentID
FROM class
GROUP BY studentID
HAVING COUNT(*) > 1)
ORDER BY studentID, e_term'))
#.... for returning the rows with rank of 1. Except this time it is your last course taken
repeat_last = sqldf('SELECT * FROM repeat_rev_rank WHERE rank == 1')



# adding a column to the repeat_first and repeat_last df with the terms first and last, respectively and removing the rank column in each
add_rep_fir = rep("first", length(repeat_first$rank))
repeat_first$class_id = add_rep_fir
add_rep_las = rep("last", length(repeat_last$rank))
repeat_last$class_id = add_rep_las
repeat_first$rank = NULL
repeat_last$rank = NULL

# Join the two tables together:
repeat_first_last = sqldf('SELECT * FROM repeat_first UNION ALL SELECT * FROM repeat_last ')

# creating a graph showing the changes in student grades from first to last classes
jpeg("repeat_first_last_dist_hist.jpeg")
ggplot(repeat_first_last, aes(x = grade)) +
  geom_histogram(aes(color = rep, fill = rep), binwidth = 50, stat = "count") +
  facet_grid(rep ~ .) + 
  scale_color_manual(values = c("#00AFBB", "#FCE205")) + 
  scale_fill_manual(values = c("#00AFBB", "#FCE205")) + 
  labs(title = "Grade Distribution Changes", x = "Grade", y = "Total Students") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


###### Do a percent increase in passing grades and percent decrease in failing grades analysis
# creating lists of the simplified grades and combined totals of the grades
repeat_first_freq = as.data.frame(table(repeat_first$grade))
repeat_last_freq = as.data.frame(table(repeat_last$grade))
colnames(repeat_first_freq) = c("grade", "freq")
colnames(repeat_last_freq) = c("grade", "freq")
first_list = rep("first",length(repeat_first_freq$grade))
last_list = rep("last",length(repeat_last_freq$grade))
repeat_first_freq$rep = first_list
repeat_last_freq$rep = last_list
# joining the two dataframes using SQL
repeat_grade_freq = sqldf('SELECT * FROM grade_freq_first_df UNION ALL SELECT * FROM grade_freq_last_df ')
colnames(repeat_grade_freq) = c("grade", "freq", "rep")

# defining passing grades as C and above and failing grades as everything else
# first course had 10 passing grades and 3605 failing grades
# last course had 1845 passing grades and 1770 failing grades
# this shows a 18350% increase in passing grades and a 50.9% decrease in failing grades
sqldf('SELECT SUM(freq) from repeat_grade_freq where rep == "first" AND (grade == "A" OR grade == "B" OR grade == "C")')
sqldf('SELECT SUM(freq) from repeat_grade_freq where rep == "first" AND (grade == "D" OR grade == "F" OR grade == "NC" OR grade == "W")')
sqldf('SELECT SUM(freq) from repeat_grade_freq where rep == "last" AND (grade == "A" OR grade == "B" OR grade == "C")')
sqldf('SELECT SUM(freq) from repeat_grade_freq where rep == "last" AND (grade == "D" OR grade == "F" OR grade == "NC" OR grade == "W")')


# want to look at grade distributions of students who took the course only once
# subset out the students who took it only once
once_st = droplevels(sqldf('SELECT *
,ROW_NUMBER() OVER(PARTITION BY studentID ORDER BY studentID, e_term) AS rank
FROM class
WHERE studentID IN (
SELECT studentID
FROM class
GROUP BY studentID
HAVING COUNT(*) = 1)
ORDER BY studentID, e_term'))
# 16393 students took it once
dim(once_st)

# creating a hist of grade distributions
jpeg("once_dist_hist.jpeg")
ggplot(once_st, aes(x = grade)) +
  geom_histogram(fill = "#FCE205", colour = "black", binwidth = 50, stat = "count")
dev.off() 

# creating a df of grades for students taking the course once
once_freq = as.data.frame(table(once_st$grade))
colnames(once_freq) = c("grade", "freq")

# totaling the number that passed and failed
sqldf('SELECT SUM(freq) from once_freq where grade == "A" OR grade == "B" OR grade == "C"')
sqldf('SELECT SUM(freq) from once_freq where grade == "D" OR grade == "F" OR grade == "NC" OR grade == "W" OR grade = "S"')
# 11967 students passed the first time they took it and 4426 failed when taking the course only once
# 73% of students passed the first time they took it and 27% failed when taking the course only once

# whereas those who had to repeat it had 0.3% of students pass the course and 99.7% of students failed it the first time they took the course
# by the last time they took the course, the grades had improved to 51.0% passing the course and 49.0% failing the course. 
# so they improved, but were not as successful as those who took it just once


# can we use number of times student repeated the course to predict ultimate outcome?
# subsetting the students into those that took the course
# two times
repeat_st2 = droplevels(sqldf('SELECT *
,ROW_NUMBER() OVER(PARTITION BY studentID ORDER BY studentID, e_term) AS rank
FROM class
WHERE studentID IN (
SELECT studentID
FROM class
GROUP BY studentID
HAVING COUNT(*) = 2)
ORDER BY studentID, e_term'))

# three times
repeat_st3 = droplevels(sqldf('SELECT *
,ROW_NUMBER() OVER(PARTITION BY studentID ORDER BY studentID, e_term) AS rank
FROM class
WHERE studentID IN (
SELECT studentID
FROM class
GROUP BY studentID
HAVING COUNT(*) = 3)
ORDER BY studentID, e_term'))

# four times
repeat_st4 = droplevels(sqldf('SELECT *
,ROW_NUMBER() OVER(PARTITION BY studentID ORDER BY studentID, e_term) AS rank
FROM class
WHERE studentID IN (
SELECT studentID
FROM class
GROUP BY studentID
HAVING COUNT(*) = 4)
ORDER BY studentID, e_term'))

# five times
repeat_st5 = droplevels(sqldf('SELECT *
,ROW_NUMBER() OVER(PARTITION BY studentID ORDER BY studentID, e_term) AS rank
FROM class
WHERE studentID IN (
SELECT studentID
FROM class
GROUP BY studentID
HAVING COUNT(*) = 5)
ORDER BY studentID, e_term'))

# six times
repeat_st6 = droplevels(sqldf('SELECT *
,ROW_NUMBER() OVER(PARTITION BY studentID ORDER BY studentID, e_term) AS rank
FROM class
WHERE studentID IN (
SELECT studentID
FROM class
GROUP BY studentID
HAVING COUNT(*) = 6)
ORDER BY studentID, e_term'))

# and seven times
repeat_st7 = droplevels(sqldf('SELECT *
,ROW_NUMBER() OVER(PARTITION BY studentID ORDER BY studentID, e_term) AS rank
FROM class
WHERE studentID IN (
SELECT studentID
FROM class
GROUP BY studentID
HAVING COUNT(*) = 7)
ORDER BY studentID, e_term'))

# define "passed" as those who ultimately ended up passing the class and "failed" as those who ultimately ended up failing the course
# bc apparent utk uses the latest grade to calculate gpa so if you passed it, retook it, then failed it. The failed grade is what counts
# for those who took it twice. Looking at the final grade given
repeat2_passed = sqldf('SELECT studentID from repeat_st2 where rank = 2 AND grade in ("A", "B", "C")')
repeat2_failed = sqldf('SELECT studentID from repeat_st2 where rank = 2 AND grade in ("D", "F", "S", "NC", "W")')
dim(repeat2_passed)
dim(repeat2_failed)
# 1597 students who took it twice ultimately passed
# 1476 students who took it twice ultimately failed

# for those that took it three times
repeat3_passed = sqldf('SELECT studentID from repeat_st3 where rank = 3 AND grade in ("A", "B", "C")')
repeat3_failed = sqldf('SELECT studentID from repeat_st3 where rank = 3 AND grade in ("D", "F", "S", "NC", "W")')
dim(repeat3_passed)
dim(repeat3_failed)
# 222 students ultimately passed
# 241 ultimately failed

# for those that took it four times
repeat4_passed = sqldf('SELECT studentID from repeat_st4 where rank = 4 AND grade in ("A", "B", "C")')
repeat4_failed = sqldf('SELECT studentID from repeat_st4 where rank = 4 AND grade in ("D", "F", "S", "NC", "W")')
dim(repeat4_passed)
dim(repeat4_failed)
# 29 students ultimately passed
# 39 ultimately failed

# for those that took it five times
repeat5_passed = sqldf('SELECT studentID from repeat_st5 where rank = 5 AND grade in ("A", "B", "C")')
repeat5_failed = sqldf('SELECT studentID from repeat_st5 where rank = 5 AND grade in ("D", "F", "S", "NC", "W")')
dim(repeat5_passed)
dim(repeat5_failed)
# 2 students ultimately passed
# 5 ultimately failed

# for those that took it six times
repeat6_passed = sqldf('SELECT studentID from repeat_st6 where rank = 6 AND grade in ("A", "B", "C")')
repeat6_failed = sqldf('SELECT studentID from repeat_st6 where rank = 6 AND grade in ("D", "F", "S", "NC", "W")')
dim(repeat6_passed)
dim(repeat6_failed)
# 1 students ultimately passed
# 2 ultimately failed

# for those that took it seven times
repeat7_passed = sqldf('SELECT studentID from repeat_st7 where rank = 7 AND grade in ("A", "B", "C")')
repeat7_failed = sqldf('SELECT studentID from repeat_st7 where rank = 7 AND grade in ("D", "F", "S", "NC", "W")')
dim(repeat7_passed)
dim(repeat7_failed)
# 0 students ultimately passed
# 1 ultimately failed

# because such low sample numbers for 4+, will combine?
# if they took it four or more times, 32 ultimately passed and 47 ultimately failed


once_st_pass = sqldf('SELECT studentID from once_st where grade == "A" OR grade == "B" OR grade == "C"')
once_st_fail = sqldf('SELECT studentID from once_st where grade == "D" OR grade == "F" OR grade == "NC" OR grade == "W" OR grade = "S"')
once_pass_empty = rep("pass",length(once_st_pass$studentID))
once_reps_empty = rep(1, length(once_st_pass$studentID))
once_st_pass$outcome = once_pass_empty
once_st_pass$reps = once_reps_empty

# creating new columns to ID "reps" ie how many times the student took the course and whether they ultimately passed or failed
# for those who took it once
# to get a better distribution for the chi squared test, I lumped together the ones that took it 3 or more times since they had similar patterns to proportions of pass vs failed
once_pass_empty = rep("pass",length(once_st_pass$studentID))
once_reps_pass_empty = rep("1", length(once_st_pass$studentID))
once_st_pass$outcome = once_pass_empty
once_st_pass$reps = once_reps_pass_empty

once_fail_empty = rep("fail",length(once_st_fail$studentID))
once_reps_fails_empty = rep("1", length(once_st_fail$studentID))
once_st_fail$outcome = once_fail_empty
once_st_fail$reps = once_reps_fails_empty

# for those who took it twice
repeat2_passed_empty = rep("pass",length(repeat2_passed$studentID))
repeat2_reps_empty = rep("2", length(repeat2_passed$studentID))
repeat2_passed$outcome = repeat2_passed_empty
repeat2_passed$reps = repeat2_reps_empty

repeat2_failed_empty = rep("fail",length(repeat2_failed$studentID))
repeat2_reps_failed_empty = rep("2", length(repeat2_failed$studentID))
repeat2_failed$outcome = repeat2_failed_empty
repeat2_failed$reps = repeat2_reps_failed_empty

# three times
repeat3_passed_empty = rep("pass",length(repeat3_passed$studentID))
repeat3_reps_empty = rep("3+", length(repeat3_passed$studentID))
repeat3_passed$outcome = repeat3_passed_empty
repeat3_passed$reps = repeat3_reps_empty

repeat3_failed_empty = rep("fail",length(repeat3_failed$studentID))
repeat3_reps_failed_empty = rep("3+", length(repeat3_failed$studentID))
repeat3_failed$outcome = repeat3_failed_empty
repeat3_failed$reps = repeat3_reps_failed_empty

# four times
repeat4_passed_empty = rep("pass",length(repeat4_passed$studentID))
repeat4_reps_empty = rep("3+", length(repeat4_passed$studentID))
repeat4_passed$outcome = repeat4_passed_empty
repeat4_passed$reps = repeat4_reps_empty

repeat4_failed_empty = rep("fail",length(repeat4_failed$studentID))
repeat4_reps_failed_empty = rep("3+", length(repeat4_failed$studentID))
repeat4_failed$outcome = repeat4_failed_empty
repeat4_failed$reps = repeat4_reps_failed_empty

# five times
repeat5_passed_empty = rep("pass",length(repeat5_passed$studentID))
repeat5_reps_empty = rep("3+", length(repeat5_passed$studentID))
repeat5_passed$outcome = repeat5_passed_empty
repeat5_passed$reps = repeat5_reps_empty

repeat5_failed_empty = rep("fail",length(repeat5_failed$studentID))
repeat5_reps_failed_empty = rep("3+", length(repeat5_failed$studentID))
repeat5_failed$outcome = repeat5_failed_empty
repeat5_failed$reps = repeat5_reps_failed_empty

# six times
repeat6_passed_empty = rep("pass",length(repeat6_passed$studentID))
repeat6_reps_empty = rep("3+", length(repeat6_passed$studentID))
repeat6_passed$outcome = repeat6_passed_empty
repeat6_passed$reps = repeat6_reps_empty

repeat6_failed_empty = rep("fail",length(repeat6_failed$studentID))
repeat6_reps_failed_empty = rep("3+", length(repeat6_failed$studentID))
repeat6_failed$outcome = repeat6_failed_empty
repeat6_failed$reps = repeat6_reps_failed_empty

# seven times
repeat7_passed_empty = rep("pass",length(repeat7_passed$studentID))
repeat7_reps_empty = rep("3+", length(repeat7_passed$studentID))
repeat7_passed$outcome = repeat7_passed_empty
repeat7_passed$reps = repeat7_reps_empty

repeat7_failed_empty = rep("fail",length(repeat7_failed$studentID))
repeat7_reps_failed_empty = rep("3+", length(repeat7_failed$studentID))
repeat7_failed$outcome = repeat7_failed_empty
repeat7_failed$reps = repeat7_reps_failed_empty

# Join all tables together:
pass_fail_reps_stID = sqldf('SELECT * FROM once_st_fail UNION ALL SELECT * FROM once_st_pass UNION ALL SELECT * from repeat2_failed UNION ALL select * from repeat2_passed UNION ALL SELECT * from repeat3_failed UNION ALL select * from repeat3_passed UNION ALL SELECT * from repeat4_failed UNION ALL select * from repeat4_passed UNION ALL SELECT * from repeat5_failed UNION ALL select * from repeat5_passed UNION ALL SELECT * from repeat6_failed UNION ALL select * from repeat6_passed UNION ALL SELECT * from repeat7_failed UNION ALL select * from repeat7_passed')

# chi sq test for those that passed and failed based on number of times they repeated the class
test = chisq.test(pass_fail_reps_stID$outcome, pass_fail_reps_stID$reps)
# results of the chi squared test
#         Pearson's Chi-squared test

# data:  pass_fail_reps_stID$outcome and pass_fail_reps_stID$reps
# X-squared = 664.25, df = 2, p-value < 2.2e-16
# low pvalue means there is a relationship between number of times you  take the course and passing or failing it

# can apparently graph contingency tables too
library(vcd)
jpeg("grade_outcome_x_reps.jpeg")
mosaic(~ reps + outcome,
       direction = c("v", "h"),
       data = pass_fail_reps_stID,
       shade = TRUE)
dev.off()


## Looking at grade distribution per semester
#whoops... need to drop levesls from the class df
class = droplevels(class)
grade_dist_per_sem = table(class$grade, class$e_term)

# creating a new column to determine whether the grade was pass or fail

class_outcome_empty = rep(NA, length(class$grade))
class$outcome = class_outcome_empty
for (i in seq_along(class$grade))
if (!is.na(class$grade[i]) && class$grade[i] == "A" || class$grade[i] == "B" || class$grade[i] == "C") {
   class$outcome[i] = "pass"
} else { class$outcome[i] = "fail"
}

# looking at pass vs fail distributions per semester
grade_outcome_per_sem = table(class$outcome, class$e_term)
outcome_perc_per_sem = prop.table(grade_outcome_per_sem, margin = 2)
#calculating the sums of the rows so I can do math to see avg percent pass and avg percent fail so I can look at semesters higher or lower than that
sumr_outcome_perc=rowSums(outcome_perc_per_sem)

# need to know how many semesters to divide it by
sem_num = sqldf('select distinct e_term from class ORDER BY e_term')
# 37 semesters in the dataset
avg_perc = sumr_outcome_perc/37
# the average semester has 40% students passing and 60% of students failing
# making a dataframe that has columns for semester number, perc_pass, and perc_fail
# turning the prop table into a df
outcome_perc_per_sem = as.data.frame(outcome_perc_per_sem)
outcome_perc_pass = sqldf('select Freq from outcome_perc_per_sem where Var1 = "pass"')
outcome_perc_fail = sqldf('select Freq from outcome_perc_per_sem where Var1 = "fail"')
# checking the dimensions to make sure they are 37 units long (37 = # semesters)
dim(outcome_perc_pass)
dim(outcome_perc_fail)
# connecting the df
outcome_perc_per_sem = data.frame(sem_num, outcome_perc_pass, outcome_perc_fail)
# changing column names
colnames(outcome_perc_per_sem) = c("term", "perc_pass", "perc_fail")
# calculating the summary stats of percent of students that pass each semester and fail each semester
summary(outcome_perc_per_sem$perc_pass)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3580  0.5199  0.5970  0.6006  0.6636  0.8384 
summary(outcome_perc_per_sem$perc_fail)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1616  0.3364  0.4030  0.3994  0.4801  0.6420 

# Plot on box plot to see if any pass outliers
jpeg("outcome_perc_pass_box.jpeg")
boxplot(outcome_perc_per_sem$perc_pass, main="Percent of Students Passing per Semester")
boxplot(outcome_perc_per_sem$perc_pass, main="Perc Pass Per Sem Dist", add=TRUE)
stripchart(outcome_perc_per_sem$perc_pass, vertical=TRUE, add=TRUE, method="jitter", col="blue", pch=16)
dev.off()

# I know it's essentially the same thing, but wasn't sure if outliers would be more easily seen this way
jpeg("outcome_perc_fail_box.jpeg")
boxplot(outcome_perc_per_sem$perc_fail, main="Perc Fail Per Sem Dist")
boxplot(outcome_perc_per_sem$perc_fail, main="Perc Fail Per Sem Dist", add=TRUE)
stripchart(outcome_perc_per_sem$perc_fail, vertical=TRUE, add=TRUE, method="jitter", col="green", pch=16)
dev.off()


# did an ifelse to determine whether there were outliers
outlier_empty = rep(NA, length(outcome_perc_per_sem$perc_pass))
outcome_perc_per_sem$outlier = outlier_empty
for (i in seq_along(outcome_perc_per_sem$perc_pass))
if (!is.na(outcome_perc_per_sem$perc_pass[i]) && abs(.6 - outcome_perc_per_sem$perc_pass[i]) >= 0.2155117)  {
   outcome_perc_per_sem$outlier[i] = "yes"
} else { outcome_perc_per_sem$outlier[i] = "no"
}

# there were only two so deciding to go with the fourth quartile and first quartile as high and low passing rates
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3580  0.5199  0.5970  0.6006  0.6636  0.8384 
# so if less than the 1st quartile passing = low, if greater than the third quartile = high, if between mean and 1st quartile = below_avg, if between mean and 3rd quartile = above_avg.
for (i in seq_along(outcome_perc_per_sem$perc_pass))
if (outcome_perc_per_sem$perc_pass[i] < 0.5199) {
    outcome_perc_per_sem$st_success[i] = "low"
} else if (outcome_perc_per_sem$perc_pass[i] > 0.6636) {
    outcome_perc_per_sem$st_success[i] = "high"
} else if (outcome_perc_per_sem$perc_pass[i] <= 0.6636 && outcome_perc_per_sem$perc_pass[i] >= 0.6006) {
    outcome_perc_per_sem$st_success[i] = "above_avg"
} else { outcome_perc_per_sem$st_success[i] = "below_avg"
}

# making a new dataframe with columns being the semesters in each category
high = sqldf('select term from outcome_perc_per_sem where st_success = "high"') 
above_avg = sqldf('select term from outcome_perc_per_sem where st_success = "above_avg"') 
below_avg = sqldf('select term from outcome_perc_per_sem where st_success = "below_avg"') 
low = sqldf('select term from outcome_perc_per_sem where st_success = "low"') 
sem_st_success = data.frame(low, below_avg, above_avg, high)

# making new columns called semester and year
semester_empty = rep(NA, length(outcome_perc_per_sem$term))
outcome_perc_per_sem$semester = semester_empty
year_empty = rep(NA, length(outcome_perc_per_sem$term))
outcome_perc_per_sem$year = year_empty
# filling those columns out based on the values in the term column
# for (i in seq_along(outcome_perc_per_sem$term))
# if (outcome_perc_per_sem$term[i] == ".*20$") {
#     outcome_perc_per_sem$semester[i] = "spring"
# } else if (outcome_perc_per_sem$term[i] == ".*30$") {
#     outcome_perc_per_sem$semester[i] = "summer"
# } else { outcome_perc_per_sem$semester[i] = "fall"
# }

# # shortening the name bc so much to type out
# outcome = outcome_perc_per_sem
# for (i in seq_along(outcome$term))
# if (outcome$term[i] =="2000.*") {
#     outcome$year[i] = "2000"
# } else if (outcome$term[i] == "2001.*") {
#     outcome$year[i] = "2001"
# } else if (outcome$term[i] == "2002.*") {
#     outcome$year[i] = "2002"
# } else if (outcome$term[i] == "2003.*") {
#     outcome$year[i] = "2003"
# } else if (outcome$term[i] == "2004.*") {
#     outcome$year[i] = "2004"
# } else if (outcome$term[i] == "2005.*") {
#     outcome$year[i] = "2005"
# } else if (outcome$term[i] =="2006.*") {
#     outcome$year[i] = "2006"
# } else if (outcome$term[i] == "2007.*") {
#     outcome$year[i] = "2007"
# } else if (outcome$term[i] == "2008.*") {
#     outcome$year[i] = "2008"
# } else if (outcome$term[i] == "2009.*") {
#     outcome$year[i] = "2009"
# } else if (outcome$term[i] == "2010.*") {
#     outcome$year[i] = "2010"
# } else if (outcome$term[i] =="2011.*") {
#     outcome$year[i] = "2011"
# } else { outcome$year[i] = "2012"
# }

# except those didn't work so just manually did it...
outcome$semester = c("spring", "summer", "fall", "spring", "summer", "fall","spring", "summer", "fall","spring", "summer", "fall","spring", "summer", "fall", "spring", "summer", "fall", "spring", "summer", "fall", "spring", "summer", "fall", "spring", "summer", "fall", "spring", "summer", "fall", "spring", "summer", "fall", "spring", "summer", "fall", "spring")
outcome$year = c("2000", "2000", "2000", "2001", "2001", "2001", "2002", "2002", "2002", "2003", "2003", "2003", "2004", "2004", "2004", "2005", "2005", "2005", "2006", "2006", "2006", "2007", "2007", "2007", "2008", "2008", "2008", "2009", "2009", "2009", "2010", "2010", "2010", "2011", "2011", "2011", "2012")

# looking at st_success based on semester and year
table(outcome$st_success, outcome$semester)
table(outcome$st_success, outcome$year) # doesn't really seem to be a year affect so going with semester effects
sem_outcome = table(outcome$st_success, outcome$semester)
chisq.test(sem_outcome)
# results
#         Pearson's Chi-squared test

# data:  sem_outcome
# X-squared = 23.072, df = 6, p-value = 0.0007726
# does seem to be a semester effect on grades
# the around average semesters seem to be evenly distributed, but the high scoring semesters are almost exclusively in the summer and the low scoring semesters are almost exclusively in the spring
# possibly diff teachers in spring vs summer? possibly more motivated students taking it in the summer? possibly burnt out students in the spring?



# look at semester dist in the table that has students first times taking it
# were they possibly set up for success or failure bc they took it at the wrong time?
# adding the number of students who repeated the course and it was their first time taking it to the table
first = as.data.frame(table(repeat_first$e_term))
outcome$first_rep = first$Freq
# repeating with the last time they took it
last = as.data.frame(table(repeat_last$e_term))

# finding the average number of students that repeated the course after taking it as their first semester and stopped taking it as their last semester based on st_success
sqldf('select term, st_success, AVG(first_rep), AVG(last_rep) from outcome group by st_success')
# results
#     term st_success AVG(first_rep) AVG(last_rep)
# 1 200030  above_avg       92.88889      85.55556
# 2 200120  below_avg      159.70000      94.90000
# 3 200230       high       41.44444      38.00000
# 4 200020        low       89.88889     172.66667

# repeating with totals
sqldf('select term, st_success, SUM(first_rep), SUM(last_rep) from outcome group by st_success')
#     term st_success SUM(first_rep) SUM(last_rep)
# 1 200030  above_avg            836           770
# 2 200120  below_avg           1597           949
# 3 200230       high            373           342
# 4 200020        low            809          1554

# repeating with percents
sqldf('select term, st_success, (SUM(first_rep)/3615), (SUM(last_rep)/3615) from outcome group by st_success')
#     term st_success (SUM(first_rep)/3615) (SUM(last_rep)/3615)
# 1 200030  above_avg             0.2312586           0.21300138
# 2 200120  below_avg             0.4417704           0.26251729
# 3 200230       high             0.1031812           0.09460581
# 4 200020        low             0.2237898           0.42987552

# creating a df called ultimate grades to show the students final grades in the course
once = once_st
once$rank = NULL
rep_last = repeat_last
rep_last$class_id = NULL
ult_grades = sqldf('select * from once UNION ALL select * from rep_last')

# importing the student major data
majors = read.csv(file = "Student_Majr.csv")
# changing column names
colnames(majors) = c("studentID", "m_term", "maj", "conc", "level")

# not doing distributions of majors that took the course bc TOO MANY




# selecting students from majors who appear more than once aka who have changed their major
changed_majors = droplevels(sqldf('SELECT *
,ROW_NUMBER() OVER(PARTITION BY studentID ORDER BY studentID, m_term) AS rank
FROM majors
WHERE studentID IN (
SELECT studentID
FROM majors
GROUP BY studentID
HAVING COUNT(*) > 1)
ORDER BY studentID, m_term'))

# reversing the rankings so we can look at what they started doing and ultimately ended doing
changed_majors_rev = droplevels(sqldf('SELECT *
,ROW_NUMBER() OVER(PARTITION BY studentID ORDER BY studentID, m_term DESC) AS rank
FROM majors
WHERE studentID IN (
SELECT studentID
FROM majors
GROUP BY studentID
HAVING COUNT(*) > 1)
ORDER BY studentID, m_term'))

# total student number
distinct_majors = sqldf('SELECT DISTINCT studentID FROM majors')
dim(distinct_majors)
# 26358 students who have changed their majors
# total number who have changed their major
distinct_changed_majors = sqldf('SELECT DISTINCT studentID FROM changed_majors')
dim(distinct_changed_majors)
# 19175 students have changed their major at least once

# deleting students who have changed their major and have not taken math 119
changed_majors_in = sqldf('select * from changed_majors where studentID in (select studentID from distinct_class)')
distinct_changed_in = sqldf('select distinct studentID from changed_majors_in')
# 14481 students have changed their major who have taken the math class

# also was curious about the whole majors list - removing any students that haven't taken the math class whether or not they changed majors
majors_in = sqldf('select * from majors where studentID in (select studentID from distinct_class)')
distinct_majors_in = sqldf('select distinct studentID from majors_in')
# 19905 students total ...so I guess missing some students....?....bc 20008 took the math class


# adding grade classifications to my list of first and last times repeating the course
repeat_first_outcome_empty = rep(NA, length(repeat_first$grade))
repeat_first$outcome = repeat_first_outcome_empty
for (i in seq_along(repeat_first$grade))
if (!is.na(repeat_first$grade[i]) && repeat_first$grade[i] == "A" || repeat_first$grade[i] == "B" || repeat_first$grade[i] == "C") {
   repeat_first$outcome[i] = "pass"
} else { repeat_first$outcome[i] = "fail"
}

repeat_last_outcome_empty = rep(NA, length(repeat_last$grade))
repeat_last$outcome = repeat_last_outcome_empty
for (i in seq_along(repeat_last$grade))
if (!is.na(repeat_last$grade[i]) && repeat_last$grade[i] == "A" || repeat_last$grade[i] == "B" || repeat_last$grade[i] == "C") {
   repeat_last$outcome[i] = "pass"
} else { repeat_last$outcome[i] = "fail"
}

# do the students who took it multiple times change their majors? does it differ bw if they ultimately passed and ultimately failed?



# do the students who only took the course once change their major if they passed it vs if they failed it?
# now, do the students who had to repeat it and stopped taking it in the hard semesters switch their majors? in the easy?

# tables to pull from: outcome, once_st - for st, repeat_first, repeat_last, once_st_pass, once_st_fail, repeat_first, repeat_last

# making a graph to depict the grade distributions of students taking it once vs taking it multiple times. as well as a graph of student numbers.
rep_type = c("single", "repeat")
total = c(16393, 3615)
perc = c(82,12)

single_v_repeat = data.frame(rep_type, total, perc)

jpeg("repeat_vs_single.jpeg")
ggplot(single_v_repeat,aes(x=reorder(rep_type, -total),y=total))+geom_bar(stat="identity",fill=c("dark green", "#028A0F"), colour="gray")+ylim(c(0,25000)) + 
    ylab("Total Students") + xlab("Student Type") + ggtitle("Single vs. Repeat Student Totals") + geom_text(aes(label=total), position = position_dodge(width = 1.0), vjust=-0.25) + 
    theme(plot.title = element_text(hjust = 0.5))
dev.off()

jpeg("repeat_vs_single_perc.jpeg")
ggplot(single_v_repeat,aes(x=reorder(rep_type, -perc),y=perc))+geom_bar(stat="identity",fill=c("dark green", "#028A0F"), colour="gray")+ylim(c(0,100)) + 
    ylab("Percent of Students") + xlab("Student Type") + ggtitle("Single vs. Repeat Student Percents") + geom_text(aes(label=perc), position = position_dodge(width = 1.0), vjust=-0.25) + 
    theme(plot.title = element_text(hjust = 0.5))
dev.off()

single_grade_dist = table(once_st$grade)
repeat_grade_dist = table(repeat_st$grade)
# this code may not be complete so commenting it out when done
single_v_repeat_grades = data.frame(single_grade_dist)
colnames(single_v_repeat_grades) = c("grade", "single", "rept")
repeat_grades = data.frame(repeat_grade_dist)
single_v_repeat_grades$rept = repeat_grades$total
single_v_repeat_grades$perc_rep = (single_v_repeat_grades$rept/sum(single_v_repeat_grades$rept))*100


jpeg("single_grades.jpeg")
    barplot(single_v_repeat_grades$perc_sing ~ single_v_repeat_grades$grade, col = "dark green", xlab = "Grade", ylab = "Percent of Single Students", 
        main = "Single Enrollment Student Grade Distribution")
dev.off()

jpeg("repeat_grades.jpeg")
    barplot(single_v_repeat_grades$perc_rep ~ single_v_repeat_grades$grade, col = "#028A0F", xlab = "Grade", ylab = "Percent of Repeat Students", 
        main = "Repeat Enrollment Student Grade Distribution")
dev.off()

jpeg("repeat_first_grades.jpeg")
    barplot(rep_fir$perc ~ rep_fir$grades, col = "#028A0F", xlab = "Grade", ylab = "Grade", 
        main = "First Grade Given to Repeat Students")
dev.off()




x = c(0,7,2,2,3,4,3,2,5,8,0,1)
dim(x) = c(3,4)
dimnames(x) = list(sem = c("20", "30", "40"),diff = c("High", "Above", "Below", "Low"))

jpeg("success_per_Sem.jpeg")
mosaicplot(x, xlab = "Semester", ylab = "Student Success", main = "Student Success per Semester", color = c("#32612D", "#028A0F", "#E3242B", "#A91B0D"))
dev.off()

jpeg("success_per_rep.jpeg")
mosaicplot(x, xlab = "Semester", ylab = "Student Success", main = "Student Success per Semester", color = c("#32612D", "#028A0F", "#E3242B", "#A91B0D"))
dev.off()


x = c(12.3,10.3,28.2,23.1,38.1,44.2,21.5,22.4)
dim(x) = c(2,4)
dimnames(x) = list(sem = c("Once", "Repeat,\nFirst Time"),diff = c("High", "Above", "Below", "Low"))

jpeg("success_per_rep.jpeg")
mosaicplot(x, xlab = "Student Enrollment", ylab = "Student Success", main = "Student Success per Student Enrollment Type", color = c("#32612D", "#028A0F", "#E3242B", "#A91B0D"))
dev.off()

# do the students who took it multiple times change their majors? does it differ bw if they ultimately passed and ultimately failed?
# do the students who only took the course once change their major if they passed it vs if they failed it?
# categories: repeated and failed, repeated and passed, once and passed, once and failed.
# tables to pull from: outcome, once_st - for st, repeat_first, repeat_last, once_st_pass, once_st_fail, repeat_first, repeat_last

once_majors
once_changed
# 72% changed
once_pass_changed
# 9126 of passed and changed
# 11967 passed total
#76%
once_fail_changed
#2639 of failed changed
#4426 failed total
#59%
rep_majors
rep_changed
#75% changed
rep_fail_changed
# 1250 failed and changed
#1770 total failed
71%
rep_pass_changed
# 1466 passed and changed
#1845 passed total
79%


x = c(76, 79, 59, 71)
dim(x) = c(2,2)
dimnames(x) = list(sem = c("Once", "Repeat"),diff = c("Pass", "Fail"))

jpeg("changed_per_rep.jpeg")
mosaicplot(x, xlab = "Student Enrollment", ylab = "Student Success", main = "Major Changes per Student Enrollment Type", color = c("#32612D", "#028A0F"))
dev.off()


# selecting distinct studentID and major pairs to see which students actually changed their major vs just changing their concentration
# filtering out those who are flagged as "changed their major" because they specified a concentration
maj_no_conc = sqldf('select distinct studentID,  maj from majors')
# finding those who occur more than once in the table aka changed their major
changed_majors = sqldf('select * from maj_no_conc where studentID in (select studentID from maj_no_conc group by studentID having count(*) >1) order by studentID')


# the number of students who took the course once and passed it and changed their major
once_passed = sqldf('select studentID from once_st_pass where studentID in (select studentID from changed_majors)')

dim(once_passed)
# 9035 passed and changed
dim(once_st_pass)
# 11967 passed total
# = 75.5% took it once, passed, and changed their major
dim(once_failed)
# 2611
dim(once_st_fail)
# 4426
# = 59% took it once, failed, and changed their major
dim(rep_passed)
# 1442
# 1845
# = 78% repeated it, passed, and changed their major
dim(rep_failed)
# 1247
# 1770
# = 70.4% repeated it, failed, and changed their major



