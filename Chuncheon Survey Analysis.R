library(readxl)

survey <- read_excel('C:/Users/seongjun/Desktop/서베이.xlsx')
survey <- survey[c(-1,-2), c(-1,-2)]
head(survey)
#View(survey)
################################################################################
# 2.일반적 특성

#1) 성별 (1_남자, 2_여자)
q1_3 <- table(survey[,1])
q1_3
q1_3_p <- round(prop.table(q1_3) * 100, 1)
q1_3_p

#2) 나이대 ("1_20-29세, 2_30-39세, 3_40-49세, 4_50-59세, 5_60세 이상")
q1_4 <- table(survey[,2])
q1_4
q1_4_p <- round(prop.table(q1_4) * 100, 1)
q1_4_p

16.4+37.3
#3) 거주지역
q1_5 <- table(survey[,3])
q1_5
sort(q1_5)
q1_5_p <- round(prop.table(q1_5) * 100, 1)
q1_5_p
sort(q1_5_p)

#4) 학력 (3.중졸, 4.고졸, 5.대졸, 6.대학원이상)
q1_6 <- table(survey[,4])
q1_6
q1_6_p <- round(prop.table(q1_6) * 100, 1)
q1_6_p

#5) 혼인상태 
q1_7 <- table(survey[,5])
q1_7
q1_7_p <- round(prop.table(q1_7) * 100, 1)
q1_7_p
48.5+5.2

#6) 가구구성
q1_8 <- table(survey[,6])
q1_8
q1_8_p <- round(prop.table(q1_8) * 100, 1)
q1_8_p
14 +65  +4  +3

10.4 +48.5  +3.0  +2.2 
#7) 직업
q1_9 <- table(survey[,7])
q1_9
sort(q1_9)
q1_9_p <- round(prop.table(q1_9) * 100, 1)
q1_9_p
sort(q1_9_p)
#8) 소득
q2_1 <- table(survey[,9])
q2_1
q2_1_p <- round(prop.table(q2_1) * 100, 1)
q2_1_p

#8) 소득
q2_1 <- table(survey[,9])
q2_1
q2_1_p <- round(prop.table(q2_1) * 100, 1)
q2_1_p

#9) 수급자유무
q2_1_1 <- table(survey[,10])
q2_1_1
q2_1_1p <- round(prop.table(q2_1_1) * 100, 1)
q2_1_1p
10+358
2.6+94.2 
#10) 만성질환 유무
q2_2 <- table(survey[,11])
q2_2
q2_2_p <- round(prop.table(q2_2) * 100, 1)
q2_2_p

#11) 장애진단 유무
q2_3 <- table(survey[,13])
q2_3
q2_3_p <- round(prop.table(q2_3) * 100, 1)
q2_3_p

#11-1) 장애진단시 종류
q2_3_1 <- table(survey[,14])
q2_3_1
q2_3_1p <- round(prop.table(q2_3_1) * 100, 1)
q2_3_1p

################################################################################
# 3. 주관적 건강평가
df3 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6],h = survey[,20])
head(df3)
df3[,4] <- ifelse(df3[,4] == 1, 2, ifelse(df3[,4] == 5, 4, df3[,4]))
df3[,2] <- ifelse(df3[,2] == 1, 2, df3[,2])
df3[,3] <- ifelse(df3[,3] == 2, 2, ifelse(df3[,3] == 3,2, ifelse(df3[,3] == 4, 2, ifelse(df3[,3]==5,2, df3[,3]))))

# 전체(계) 2.나쁨, 3. 보통, 4. 좋음
table(df3[,4])
round(table(df3[,4])/ 380 * 100 , 1)
# 5점 만점 평균
round((2*76+3*188+4*116)/380,2)
# 성별과 건강평가 1.남자 2.여자
q3_1 <- table(df3[,1],df3[,4])
q3_1
row1_sum <- sum(table(df3[,1], df3[,4])[1,])
ratio_row1 <- round((table(df3[,1], df3[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df3[,1], df3[,4])[2,])
ratio_row1 <- round((table(df3[,1], df3[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 나이와 건강평가
q3_2 <- table(df3[,2],df3[,4])
q3_2
row1_sum <- sum(table(df3[,2], df3[,4])[1,])
ratio_row1 <- round((table(df3[,2], df3[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df3[,2], df3[,4])[2,])
ratio_row1 <- round((table(df3[,2], df3[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df3[,2], df3[,4])[3,])
ratio_row1 <- round((table(df3[,2], df3[,4])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df3[,2], df3[,4])[4,])
ratio_row1 <- round((table(df3[,2], df3[,4])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 가구원수와 건강평가
df3[,3] <- ifelse(df3[,3] != 1, 2, df3[,3])
table(df3[,3], df3[,4])
row1_sum <- sum(table(df3[,3], df3[,4])[1,])
ratio_row1 <- round((table(df3[,3], df3[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df3[,3], df3[,4])[2,])
ratio_row1 <- round((table(df3[,3], df3[,4])[2,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)

# 건강평가 평균비교(성별)
group1 <- as.numeric(df3[df3[,1] == 1, 4])
group2 <- as.numeric(df3[df3[,1] == 2, 4])

sum(table(group1)) #남자수
sum(table(group2)) #여자수
mean(group1) # 남자평균
sd(group1) # 남자표편
mean(group2) # 여자평균
sd(group2) # 여자표편

t_test_result <- t.test(group1, group2)
print(t_test_result)

# 건강평가 평균비교(연령)
group2 <- as.numeric(df3[df3[,2] == 2, 4])
group3 <- as.numeric(df3[df3[,2] == 3, 4])
group4 <- as.numeric(df3[df3[,2] == 4, 4])
group5 <- as.numeric(df3[df3[,2] == 5, 4])

sum(table(group2)) #2030수
sum(table(group3)) #40수
sum(table(group4)) #50수
sum(table(group5)) #60수

mean(group2) # 2030평균
sd(group2) # 2030표편
mean(group3) # 40평균
sd(group3) # 40표편
mean(group4) # 50평균
sd(group4) # 50표편
mean(group5) # 60평균
sd(group5) # 60표편

anova_data <- data.frame(
  value = c(group2, group3, group4, group5),
  group = factor(c(rep(2, length(group2)), 
                   rep(3, length(group3)), 
                   rep(4, length(group4)), 
                   rep(5, length(group5))))
)

anova_result <- aov(value ~ group, data = anova_data)
summary(anova_result)

# 건강평가 평균비교(가구원 수)
group1 <- as.numeric(df3[df3[,3] == 1, 4])
group2 <- as.numeric(df3[df3[,3] == 2, 4])

sum(table(group1)) #1인가구수
sum(table(group2)) #1인이상수

mean(group1) # 1인가구 평균
sd(group1) # 1인가구 표편
mean(group2) # 1인 이상 평균
sd(group2) # 1인 이상 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)
################################################################################
# 4. 삶의 만족
df4 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6], life = survey[,15],city = survey[,16],work = survey[,17], happy = survey[,18],unhappy = survey[,19])
head(df4)
df4[,2] <- ifelse(df4[,2] == 1, 2, df4[,2])
df4[,3] <- ifelse(df4[,3] == 2, 2, ifelse(df4[,3] == 3,2, ifelse(df4[,3] == 4, 2, ifelse(df4[,3]==5,2, df4[,3]))))

# 삶 만족 평균비교(성별)
group1 <- as.numeric(df4[df4[,1] == 1, 4])
group2 <- as.numeric(df4[df4[,1] == 2, 4])

mean(group1) # 남자 평균
sd(group1) # 남자 표편
mean(group2) # 여자 평균
sd(group2) # 여자 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)

# 거주지역 평균비교(성별)
group1 <- as.numeric(df4[df4[,1] == 1, 5])
group2 <- as.numeric(df4[df4[,1] == 2, 5])

mean(group1) # 남자 평균
sd(group1) # 남자 표편
mean(group2) # 여자 평균
sd(group2) # 여자 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)

# 일가치감 평균비교(성별)
group1 <- as.numeric(df4[df4[,1] == 1, 6])
group2 <- as.numeric(df4[df4[,1] == 2, 6])

mean(group1) # 남자 평균
sd(group1) # 남자 표편
mean(group2) # 여자 평균
sd(group2) # 여자 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)

# 행복 평균비교(성별)
group1 <- as.numeric(df4[df4[,1] == 1, 7])
group2 <- as.numeric(df4[df4[,1] == 2, 7])

mean(group1) # 남자 평균
sd(group1) # 남자 표편
mean(group2) # 여자 평균
sd(group2) # 여자 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)

# 걱정 평균비교(성별)
group1 <- as.numeric(df4[df4[,1] == 1, 8])
group2 <- as.numeric(df4[df4[,1] == 2, 8])

mean(group1) # 남자 평균
sd(group1) # 남자 표편
mean(group2) # 여자 평균
sd(group2) # 여자 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)

##############################################
# 삶 만족 평균비교(연령)
group2 <- as.numeric(df4[df4[,2] == 2, 4])
group3 <- as.numeric(df4[df4[,2] == 3, 4])
group4 <- as.numeric(df4[df4[,2] == 4, 4])
group5 <- as.numeric(df4[df4[,2] == 5, 4])

mean(group2) # 2030평균
sd(group2) # 2030표편
mean(group3) # 40평균
sd(group3) # 40표편
mean(group4) # 50평균
sd(group4) # 50표편
mean(group5) # 60평균
sd(group5) # 60표편

anova_data <- data.frame(
  value = c(group2, group3, group4, group5),
  group = factor(c(rep(2, length(group2)), 
                   rep(3, length(group3)), 
                   rep(4, length(group4)), 
                   rep(5, length(group5))))
)

anova_result <- aov(value ~ group, data = anova_data)
summary(anova_result)

# 거주지역 만족 평균비교(연령)
group2 <- as.numeric(df4[df4[,2] == 2, 5])
group3 <- as.numeric(df4[df4[,2] == 3, 5])
group4 <- as.numeric(df4[df4[,2] == 4, 5])
group5 <- as.numeric(df4[df4[,2] == 5, 5])

mean(group2) # 2030평균
sd(group2) # 2030표편
mean(group3) # 40평균
sd(group3) # 40표편
mean(group4) # 50평균
sd(group4) # 50표편
mean(group5) # 60평균
sd(group5) # 60표편

anova_data <- data.frame(
  value = c(group2, group3, group4, group5),
  group = factor(c(rep(2, length(group2)), 
                   rep(3, length(group3)), 
                   rep(4, length(group4)), 
                   rep(5, length(group5))))
)

anova_result <- aov(value ~ group, data = anova_data)
summary(anova_result)

# 일가치감 평균비교(연령)
group2 <- as.numeric(df4[df4[,2] == 2, 6])
group3 <- as.numeric(df4[df4[,2] == 3, 6])
group4 <- as.numeric(df4[df4[,2] == 4, 6])
group5 <- as.numeric(df4[df4[,2] == 5, 6])

mean(group2) # 2030평균
sd(group2) # 2030표편
mean(group3) # 40평균
sd(group3) # 40표편
mean(group4) # 50평균
sd(group4) # 50표편
mean(group5) # 60평균
sd(group5) # 60표편

anova_data <- data.frame(
  value = c(group2, group3, group4, group5),
  group = factor(c(rep(2, length(group2)), 
                   rep(3, length(group3)), 
                   rep(4, length(group4)), 
                   rep(5, length(group5))))
)

anova_result <- aov(value ~ group, data = anova_data)
summary(anova_result)

# 행복 평균비교(연령)
group2 <- as.numeric(df4[df4[,2] == 2, 7])
group3 <- as.numeric(df4[df4[,2] == 3, 7])
group4 <- as.numeric(df4[df4[,2] == 4, 7])
group5 <- as.numeric(df4[df4[,2] == 5, 7])

mean(group2) # 2030평균
sd(group2) # 2030표편
mean(group3) # 40평균
sd(group3) # 40표편
mean(group4) # 50평균
sd(group4) # 50표편
mean(group5) # 60평균
sd(group5) # 60표편

anova_data <- data.frame(
  value = c(group2, group3, group4, group5),
  group = factor(c(rep(2, length(group2)), 
                   rep(3, length(group3)), 
                   rep(4, length(group4)), 
                   rep(5, length(group5))))
)

anova_result <- aov(value ~ group, data = anova_data)
summary(anova_result)

# 걱정 평균비교(연령)
group2 <- as.numeric(df4[df4[,2] == 2, 8])
group3 <- as.numeric(df4[df4[,2] == 3, 8])
group4 <- as.numeric(df4[df4[,2] == 4, 8])
group5 <- as.numeric(df4[df4[,2] == 5, 8])

mean(group2) # 2030평균
sd(group2) # 2030표편
mean(group3) # 40평균
sd(group3) # 40표편
mean(group4) # 50평균
sd(group4) # 50표편
mean(group5) # 60평균
sd(group5) # 60표편

anova_data <- data.frame(
  value = c(group2, group3, group4, group5),
  group = factor(c(rep(2, length(group2)), 
                   rep(3, length(group3)), 
                   rep(4, length(group4)), 
                   rep(5, length(group5))))
)

anova_result <- aov(value ~ group, data = anova_data)
summary(anova_result)

##############################################
# 삶 만족 평균비교(가구원 수)

group1 <- as.numeric(df4[df4[,3] == 1, 4])
group2 <- as.numeric(df4[df4[,3] == 2, 4])

mean(group1) # 1인 평균
sd(group1) # 1인 표편
mean(group2) # 1인이상 평균
sd(group2) # 1인이상 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)

# 거주지역 평균비교(가구원 수)
group1 <- as.numeric(df4[df4[,3] == 1, 5])
group2 <- as.numeric(df4[df4[,3] == 2, 5])

mean(group1) # 1인 평균
sd(group1) # 1인 표편
mean(group2) # 1인이상 평균
sd(group2) # 1인이상 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)

# 일 가치감 평균비교(가구원 수)
group1 <- as.numeric(df4[df4[,3] == 1, 6])
group2 <- as.numeric(df4[df4[,3] == 2, 6])

mean(group1) # 1인 평균
sd(group1) # 1인 표편
mean(group2) # 1인이상 평균
sd(group2) # 1인이상 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)

# 행복 평균비교(가구원 수)
group1 <- as.numeric(df4[df4[,3] == 1, 7])
group2 <- as.numeric(df4[df4[,3] == 2, 7])

mean(group1) # 1인 평균
sd(group1) # 1인 표편
mean(group2) # 1인이상 평균
sd(group2) # 1인이상 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)

# 걱정 평균비교(가구원 수)
group1 <- as.numeric(df4[df4[,3] == 1, 8])
group2 <- as.numeric(df4[df4[,3] == 2, 8])

mean(group1) # 1인 평균
sd(group1) # 1인 표편
mean(group2) # 1인이상 평균
sd(group2) # 1인이상 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)


mean(as.numeric(df4[,4]))
sd(as.numeric(df4[,4]))
mean(as.numeric(df4[,5]))
sd(as.numeric(df4[,5]))
mean(as.numeric(df4[,6]))
sd(as.numeric(df4[,6]))
mean(as.numeric(df4[,7]))
sd(as.numeric(df4[,7]))
mean(as.numeric(df4[,8]))
sd(as.numeric(df4[,8]))
################################################################################
# 5. 정신건강 인식도 및 서비스 욕구

# 1번문항
table(survey[,21])
round(prop.table(table(survey[,21])) * 100,1)

# 2번문항
table(survey[,22])
round(prop.table(table(survey[,22])) * 100,1)

# 3번문항-정신질환이 있는 사람은 행동을 예측할 수 없거나 위험한 행동을 한다
table(survey[,23])
round(prop.table(table(survey[,23])) * 100,1)

# 4번문항
table(survey[,24])
round(prop.table(table(survey[,24])) * 100,1)

# 5번문항-1
table(survey[,25])
round(prop.table(table(survey[,25])) * 100,1)

# 6번문항-2
table(survey[,26])
round(prop.table(table(survey[,26])) * 100,1)

# 7번문항-정신질환에 걸린 사람과 대화하면 나는 불편함을 느낄 것이다
table(survey[,27])
round(prop.table(table(survey[,27])) * 100,1)

# 8번문항
table(survey[,28])
round(prop.table(table(survey[,28])) * 100,1)

# 9번문항
table(survey[,29])
round(prop.table(table(survey[,29])) * 100,1)

# 10번문항
table(survey[,30])
round(prop.table(table(survey[,30])) * 100,1)

# 11번문항
table(survey[,31])
round(prop.table(table(survey[,31])) * 100,1)

# 12번문항-우리사회에서는 여전히 정신질환이 있는 사람들에게 낙인과 차별이 존재한다
table(survey[,32])
round(prop.table(table(survey[,32])) * 100,1)

# 13번문항-정신질환자 이용 시설이 우리 동네에 들어와도 받아들일 수 있다
table(survey[,33])
round(prop.table(table(survey[,33])) * 100,1)

#########################################
# 시급하게 다루어야 할 정신건강문제
# 1순위
sort(table(survey[,34]))
sort(round(prop.table(table(survey[,34])) * 100,1))

# 2순위
sort(table(survey[,35]))
sort(round(prop.table(table(survey[,35])) * 100,1))

# 3순위
sort(table(survey[,36]))
sort(round(prop.table(table(survey[,36])) * 100,1))

#########################################
# 정신건강 서비스 욕구
df5 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6], need = survey[,38], use =  survey[,39] , plan = survey[,40])
head(df5)
df5[,2] <- ifelse(df5[,2] == 1, 2, df5[,2])
df5[,3] <- ifelse(df5[,3] == 2, 2, ifelse(df5[,3] == 3,2, ifelse(df5[,3] == 4, 2, ifelse(df5[,3]==5,2, df5[,3]))))

# 필요성(성별)
table(df5[,1], df5[,4])
row1_sum <- sum(table(df5[,1], df5[,4])[1,])
ratio_row1 <- round((table(df5[,1], df5[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,1], df5[,4])[2,])
ratio_row1 <- round((table(df5[,1], df5[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 이용경험(성별)
table(df5[,1], df5[,5])
row1_sum <- sum(table(df5[,1], df5[,5])[1,])
ratio_row1 <- round((table(df5[,1], df5[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,1], df5[,5])[2,])
ratio_row1 <- round((table(df5[,1], df5[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 이용의향(성별)
table(df5[,1], df5[,6])
row1_sum <- sum(table(df5[,1], df5[,6])[1,])
ratio_row1 <- round((table(df5[,1], df5[,6])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,1], df5[,6])[2,])
ratio_row1 <- round((table(df5[,1], df5[,6])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
#####################################################
# 필요성(연령)
table(df5[,2], df5[,4])
row1_sum <- sum(table(df5[,2], df5[,4])[1,])
ratio_row1 <- round((table(df5[,2], df5[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,2], df5[,4])[2,])
ratio_row1 <- round((table(df5[,2], df5[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,2], df5[,4])[3,])
ratio_row1 <- round((table(df5[,2], df5[,4])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,2], df5[,4])[4,])
ratio_row1 <- round((table(df5[,2], df5[,4])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 이용경험(연령)
table(df5[,2], df5[,5])
row1_sum <- sum(table(df5[,2], df5[,5])[1,])
ratio_row1 <- round((table(df5[,2], df5[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,2], df5[,5])[2,])
ratio_row1 <- round((table(df5[,2], df5[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,2], df5[,5])[3,])
ratio_row1 <- round((table(df5[,2], df5[,5])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,2], df5[,5])[4,])
ratio_row1 <- round((table(df5[,2], df5[,5])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 이용의향(연령)
table(df5[,2], df5[,6])
row1_sum <- sum(table(df5[,2], df5[,6])[1,])
ratio_row1 <- round((table(df5[,2], df5[,6])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,2], df5[,6])[2,])
ratio_row1 <- round((table(df5[,2], df5[,6])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,2], df5[,6])[3,])
ratio_row1 <- round((table(df5[,2], df5[,6])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,2], df5[,6])[4,])
ratio_row1 <- round((table(df5[,2], df5[,6])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)
###############################################
# 필요성(가구수)
table(df5[,3], df5[,4])
row1_sum <- sum(table(df5[,3], df5[,4])[1,])
ratio_row1 <- round((table(df5[,3], df5[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,3], df5[,4])[2,])
ratio_row1 <- round((table(df5[,3], df5[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 이용경험(가구수)
table(df5[,3], df5[,5])
row1_sum <- sum(table(df5[,3], df5[,5])[1,])
ratio_row1 <- round((table(df5[,3], df5[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,3], df5[,5])[2,])
ratio_row1 <- round((table(df5[,3], df5[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 이용의향(가구수)
table(df5[,3], df5[,6])
row1_sum <- sum(table(df5[,3], df5[,6])[1,])
ratio_row1 <- round((table(df5[,3], df5[,6])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df5[,3], df5[,6])[2,])
ratio_row1 <- round((table(df5[,3], df5[,6])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

table(df5[,4])
round(table(df5[,4])/380 * 100 , 1)

table(df5[,5])
round(table(df5[,5])/380 * 100 , 1)

table(df5[,6])
round(table(df5[,6])/380 * 100 , 1)
###########################################
# 도움요청
# 1순위
sort(table(survey[,43]))
sort(round(prop.table(table(survey[,43])) * 100,1))

# 2순위
sort(table(survey[,44]))
sort(round(prop.table(table(survey[,44])) * 100,1))

# 3순위
sort(table(survey[,45]))
sort(round(prop.table(table(survey[,45])) * 100,1))

###########################################
# 정신건강서비스 욕구
# 1순위
sort(table(survey[,47]))
sort(round(prop.table(table(survey[,47])) * 100,1))

# 2순위
sort(table(survey[,48]))
sort(round(prop.table(table(survey[,48])) * 100,1))

# 3순위
sort(table(survey[,49]))
sort(round(prop.table(table(survey[,49])) * 100,1))
################################################################################
# 6. 자살
# 전체 자살생각
table(survey[,51])
round(prop.table(table(survey[,51])) * 100,1)

# 전체 시기
table(survey[,52])
round(prop.table(table(survey[,52])) * 100,1)

# 데이터 생성
df6 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6], thn = survey[,51], time = survey[,52])
head(df6)
df6[,2] <- ifelse(df6[,2] == 1, 2, df6[,2])
df6[,3] <- ifelse(df6[,3] == 2, 2, ifelse(df6[,3] == 3,2, ifelse(df6[,3] == 4, 2, ifelse(df6[,3]==5,2, df6[,3]))))
#####################################
# 자살생각(성별)
table(df6[,1], df6[,4])
row1_sum <- sum(table(df6[,1], df6[,4])[1,])
ratio_row1 <- round((table(df6[,1], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,4])[2,])
ratio_row1 <- round((table(df6[,1], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 시기(성별)
table(df6[,1], df6[,5])
row1_sum <- sum(table(df6[,1], df6[,5])[1,])
ratio_row1 <- round((table(df6[,1], df6[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,5])[2,])
ratio_row1 <- round((table(df6[,1], df6[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
################################################
# 자살생각(연령)
table(df6[,2], df6[,4])
row1_sum <- sum(table(df6[,2], df6[,4])[1,])
ratio_row1 <- round((table(df6[,2], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[2,])
ratio_row1 <- round((table(df6[,2], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[3,])
ratio_row1 <- round((table(df6[,2], df6[,4])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[4,])
ratio_row1 <- round((table(df6[,2], df6[,4])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 시기(연령)
table(df6[,2], df6[,5])
row1_sum <- sum(table(df6[,2], df6[,5])[1,])
ratio_row1 <- round((table(df6[,2], df6[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,5])[2,])
ratio_row1 <- round((table(df6[,2], df6[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,5])[3,])
ratio_row1 <- round((table(df6[,2], df6[,5])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,5])[4,])
ratio_row1 <- round((table(df6[,2], df6[,5])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)
##############################################
# 이용의향(가구)
table(df6[,3], df6[,4])
row1_sum <- sum(table(df6[,3], df6[,4])[1,])
ratio_row1 <- round((table(df6[,3], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,4])[2,])
ratio_row1 <- round((table(df6[,3], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 시기(성별)
table(df6[,3], df6[,5])
row1_sum <- sum(table(df6[,3], df6[,5])[1,])
ratio_row1 <- round((table(df6[,3], df6[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,5])[2,])
ratio_row1 <- round((table(df6[,3], df6[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
27/47
##############################################
# 자살 생각 이유
table(survey[,53])
round(table(survey[,53])/102*100,1) #1 가정생활
round(table(survey[,54])/102*100,1) #...
round(table(survey[,55])/102*100,1)
round(table(survey[,56])/102*100,1)
round(table(survey[,57])/102*100,1)
round(table(survey[,58])/102*100,1)
round(table(survey[,59])/102*100,1)
round(table(survey[,60])/102*100,1)
round(table(survey[,61])/102*100,1)
round(table(survey[,62])/102*100,1)
round(table(survey[,63])/102*100,1) #11 기타
##############################################
# 자살 계획 및 시도
# 전체 계획
table(survey[,65])

# 전체 시도
table(survey[,66])

# 데이터 생성
df6 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6], plan = survey[,65], try = survey[,66])
head(df6)
df6[,2] <- ifelse(df6[,2] == 1, 2, df6[,2])
df6[,3] <- ifelse(df6[,3] == 2, 2, ifelse(df6[,3] == 3,2, ifelse(df6[,3] == 4, 2, ifelse(df6[,3]==5,2, df6[,3]))))

# 계획(성별)
table(df6[,1], df6[,4])
row1_sum <- sum(table(df6[,1], df6[,4])[1,])
ratio_row1 <- round((table(df6[,1], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,4])[2,])
ratio_row1 <- round((table(df6[,1], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 시도(성별)
table(df6[,1], df6[,5])
row1_sum <- sum(table(df6[,1], df6[,5])[1,])
ratio_row1 <- round((table(df6[,1], df6[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,5])[2,])
ratio_row1 <- round((table(df6[,1], df6[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
################################################
# 계획(연령)
table(df6[,2], df6[,4])
row1_sum <- sum(table(df6[,2], df6[,4])[1,])
ratio_row1 <- round((table(df6[,2], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[2,])
ratio_row1 <- round((table(df6[,2], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[3,])
ratio_row1 <- round((table(df6[,2], df6[,4])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[4,])
ratio_row1 <- round((table(df6[,2], df6[,4])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 시도(연령)
table(df6[,2], df6[,5])
row1_sum <- sum(table(df6[,2], df6[,5])[1,])
ratio_row1 <- round((table(df6[,2], df6[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,5])[2,])
ratio_row1 <- round((table(df6[,2], df6[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,5])[3,])
ratio_row1 <- round((table(df6[,2], df6[,5])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,5])[4,])
ratio_row1 <- round((table(df6[,2], df6[,5])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)
##############################################
# 계획(가구)
table(df6[,3], df6[,4])
row1_sum <- sum(table(df6[,3], df6[,4])[1,])
ratio_row1 <- round((table(df6[,3], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,4])[2,])
ratio_row1 <- round((table(df6[,3], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 시도(성별)
table(df6[,3], df6[,5])
row1_sum <- sum(table(df6[,3], df6[,5])[1,])
ratio_row1 <- round((table(df6[,3], df6[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,5])[2,])
ratio_row1 <- round((table(df6[,3], df6[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
###############################################
# 상담 경험 및 연계 의향
# 전체 상담 경험
table(survey[,67])

# 전체 상담 의향
table(survey[,70])

# 전체 연계의향
table(survey[,71])

# 데이터 생성
df6 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6], plan = survey[,67], try = survey[,70], yy = survey[,71])
head(df6)
df6[,2] <- ifelse(df6[,2] == 1, 2, df6[,2])
df6[,3] <- ifelse(df6[,3] == 2, 2, ifelse(df6[,3] == 3,2, ifelse(df6[,3] == 4, 2, ifelse(df6[,3]==5,2, df6[,3]))))
############################################
# 상담경험(성별)
table(df6[,1], df6[,4])
row1_sum <- sum(table(df6[,1], df6[,4])[1,])
ratio_row1 <- round((table(df6[,1], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,4])[2,])
ratio_row1 <- round((table(df6[,1], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 상담의향(성별)
table(df6[,1], df6[,5])
row1_sum <- sum(table(df6[,1], df6[,5])[1,])
ratio_row1 <- round((table(df6[,1], df6[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,5])[2,])
ratio_row1 <- round((table(df6[,1], df6[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 연계의향(성별)
table(df6[,1], df6[,6])
row1_sum <- sum(table(df6[,1], df6[,6])[1,])
ratio_row1 <- round((table(df6[,1], df6[,6])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,6])[2,])
ratio_row1 <- round((table(df6[,1], df6[,6])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
################################################
# 상담경험(연령)
table(df6[,2], df6[,4])
row1_sum <- sum(table(df6[,2], df6[,4])[1,])
ratio_row1 <- round((table(df6[,2], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[2,])
ratio_row1 <- round((table(df6[,2], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[3,])
ratio_row1 <- round((table(df6[,2], df6[,4])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[4,])
ratio_row1 <- round((table(df6[,2], df6[,4])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 상담의향(연령)
table(df6[,2], df6[,5])
row1_sum <- sum(table(df6[,2], df6[,5])[1,])
ratio_row1 <- round((table(df6[,2], df6[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,5])[2,])
ratio_row1 <- round((table(df6[,2], df6[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,5])[3,])
ratio_row1 <- round((table(df6[,2], df6[,5])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,5])[4,])
ratio_row1 <- round((table(df6[,2], df6[,5])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 연계의향(연령)
table(df6[,2], df6[,6])
row1_sum <- sum(table(df6[,2], df6[,6])[1,])
ratio_row1 <- round((table(df6[,2], df6[,6])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,6])[2,])
ratio_row1 <- round((table(df6[,2], df6[,6])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,6])[3,])
ratio_row1 <- round((table(df6[,2], df6[,6])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,6])[4,])
ratio_row1 <- round((table(df6[,2], df6[,6])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)
##############################################
# 상담경험(가구)
table(df6[,3], df6[,4])
row1_sum <- sum(table(df6[,3], df6[,4])[1,])
ratio_row1 <- round((table(df6[,3], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,4])[2,])
ratio_row1 <- round((table(df6[,3], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 상담의향(성별)
table(df6[,3], df6[,5])
row1_sum <- sum(table(df6[,3], df6[,5])[1,])
ratio_row1 <- round((table(df6[,3], df6[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,5])[2,])
ratio_row1 <- round((table(df6[,3], df6[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 연계의향(성별)
table(df6[,3], df6[,6])
row1_sum <- sum(table(df6[,3], df6[,6])[1,])
ratio_row1 <- round((table(df6[,3], df6[,6])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,6])[2,])
ratio_row1 <- round((table(df6[,3], df6[,6])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
########################################
# 자살예방서비스 이용 장애요인
# 전체 장애요인
table(survey[,68])

# 데이터 생성
df6 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6], plan = survey[,68])
head(df6)
df6[,2] <- ifelse(df6[,2] == 1, 2, df6[,2])
df6[,3] <- ifelse(df6[,3] == 2, 2, ifelse(df6[,3] == 3,2, ifelse(df6[,3] == 4, 2, ifelse(df6[,3]==5,2, df6[,3]))))
############################################
# 장애요인(성별)
table(df6[,1], df6[,4])
row1_sum <- sum(table(df6[,1], df6[,4])[1,])
ratio_row1 <- round((table(df6[,1], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,4])[2,])
ratio_row1 <- round((table(df6[,1], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
################################################
# 상담경험(연령)
table(df6[,2], df6[,4])
row1_sum <- sum(table(df6[,2], df6[,4])[1,])
ratio_row1 <- round((table(df6[,2], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[2,])
ratio_row1 <- round((table(df6[,2], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[3,])
ratio_row1 <- round((table(df6[,2], df6[,4])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[4,])
ratio_row1 <- round((table(df6[,2], df6[,4])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

##############################################
# 상담경험(가구)
table(df6[,3], df6[,4])
row1_sum <- sum(table(df6[,3], df6[,4])[1,])
ratio_row1 <- round((table(df6[,3], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,4])[2,])
ratio_row1 <- round((table(df6[,3], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

################################################################################
# 7. 사회적 지원
# 전체 도움받을 사람
table(survey[,73])

# 전체 친인척 명 수
# 데이터 생성
df6 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6], plan = survey[,73], cin = survey[,74], fr = survey[,75])
head(df6)
df6[,2] <- ifelse(df6[,2] == 1, 2, df6[,2])
df6[,3] <- ifelse(df6[,3] == 2, 2, ifelse(df6[,3] == 3,2, ifelse(df6[,3] == 4, 2, ifelse(df6[,3]==5,2, df6[,3]))))

# 전체 친인척 명수 평균
mean(as.numeric(df6[,5]))
sd(as.numeric(df6[,5]))

# 전체 지인 명수 평균
mean(as.numeric(df6[,6]))
sd(as.numeric(df6[,6]))
###############################################
# 성별별 도움받을 사람
table(df6[,1],df6[,4])
row1_sum <- sum(table(df6[,1], df6[,4])[1,])
ratio_row1 <- round((table(df6[,1], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,4])[2,])
ratio_row1 <- round((table(df6[,1], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 연령병 도움받을 사람
table(df6[,2], df6[,4])
row1_sum <- sum(table(df6[,2], df6[,4])[1,])
ratio_row1 <- round((table(df6[,2], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[2,])
ratio_row1 <- round((table(df6[,2], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[3,])
ratio_row1 <- round((table(df6[,2], df6[,4])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[4,])
ratio_row1 <- round((table(df6[,2], df6[,4])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 가구원별 도움받을 사람
table(df6[,3], df6[,4])
row1_sum <- sum(table(df6[,3], df6[,4])[1,])
ratio_row1 <- round((table(df6[,3], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,4])[2,])
ratio_row1 <- round((table(df6[,3], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
#######################################
# 남성 친인척 수 평균
mean(as.numeric(df6[df6[,1] == 1,5]))
sd(as.numeric(df6[df6[,1] == 1,5]))

# 여성 친인척 수 평균
mean(as.numeric(df6[df6[,1] == 2,5]))
sd(as.numeric(df6[df6[,1] == 2,5]))

######################################
# 2030 친인척 수 평균
mean(as.numeric(df6[df6[,2] == 2,5]))
sd(as.numeric(df6[df6[,2] == 2,5]))

# 40 친인척 수 평균
mean(as.numeric(df6[df6[,2] == 3,5]))
sd(as.numeric(df6[df6[,2] == 3,5]))

# 50 친인척 수 평균
mean(as.numeric(df6[df6[,2] == 4,5]))
sd(as.numeric(df6[df6[,2] == 4,5]))

# 60 친인척 수 평균
mean(as.numeric(df6[df6[,2] == 5,5]))
sd(as.numeric(df6[df6[,2] == 5,5]))
#####################################
# 1인 친인척 수 평균
mean(as.numeric(df6[df6[,3] == 1,5]))
sd(as.numeric(df6[df6[,3] == 1,5]))

# 2인이상 친인척 수 평균
mean(as.numeric(df6[df6[,3] == 2,5]))
sd(as.numeric(df6[df6[,3] == 2,5]))

#####################################

# 남성 지인 수 평균
mean(as.numeric(df6[df6[,1] == 1,6]))
sd(as.numeric(df6[df6[,1] == 1,6]))

# 여성 지인 수 평균
mean(as.numeric(df6[df6[,1] == 2,6]))
sd(as.numeric(df6[df6[,1] == 2,6]))

######################################
# 2030 지인 수 평균
mean(as.numeric(df6[df6[,2] == 2,6]))
sd(as.numeric(df6[df6[,2] == 2,6]))

# 40 지인 수 평균
mean(as.numeric(df6[df6[,2] == 3,6]))
sd(as.numeric(df6[df6[,2] == 3,6]))

# 50 지인 수 평균
mean(as.numeric(df6[df6[,2] == 4,6]))
sd(as.numeric(df6[df6[,2] == 4,6]))

# 60 지인 수 평균
mean(as.numeric(df6[df6[,2] == 5,6]))
sd(as.numeric(df6[df6[,2] == 5,6]))
#####################################
# 1인 지인 수 평균
mean(as.numeric(df6[df6[,3] == 1,6]))
sd(as.numeric(df6[df6[,3] == 1,6]))

# 2인이상 지인 수 평균
mean(as.numeric(df6[df6[,3] == 2,6]))
sd(as.numeric(df6[df6[,3] == 2,6]))

##########################################
# 배우자와의 만족도
# 데이터 생성
df6 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6], plan = survey[,76])
head(df6)
df6[,2] <- ifelse(df6[,2] == 1, 2, df6[,2])
df6[,3] <- ifelse(df6[,3] == 2, 2, ifelse(df6[,3] == 3,2, ifelse(df6[,3] == 4, 2, ifelse(df6[,3]==5,2, df6[,3]))))
df6 <- df6[df6[,4] != 6, ]

# 전체 배우자
table(df6[,4])
sum(table(df6[,4]))
round(table(df6[,4])/sum(table(df6[,4])) * 100 , 1)
mean(as.numeric(df6[,4]))
sd(as.numeric(df6[,4]))

#######################################
# 성별 별 배우자
# 빈도와 비율
table(df6[,1],df6[,4])
row1_sum <- sum(table(df6[,1], df6[,4])[1,])
ratio_row1 <- round((table(df6[,1], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,4])[2,])
ratio_row1 <- round((table(df6[,1], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 평균비교
group1 <- as.numeric(df6[df6[,1] == 1, 4])
group2 <- as.numeric(df6[df6[,1] == 2, 4])

mean(group1) # 남자 평균
sd(group1) # 남자 표편
mean(group2) # 여자 평균
sd(group2) # 여자 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)
########################################################
# 빈도와 비율
table(df6[,2], df6[,4])
row1_sum <- sum(table(df6[,2], df6[,4])[1,])
ratio_row1 <- round((table(df6[,2], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[2,])
ratio_row1 <- round((table(df6[,2], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[3,])
ratio_row1 <- round((table(df6[,2], df6[,4])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[4,])
ratio_row1 <- round((table(df6[,2], df6[,4])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 평균비교
# 나이대별 배우자(연령)
group2 <- as.numeric(df6[df6[,2] == 2, 4])
group3 <- as.numeric(df6[df6[,2] == 3, 4])
group4 <- as.numeric(df6[df6[,2] == 4, 4])
group5 <- as.numeric(df6[df6[,2] == 5, 4])

mean(group2) # 2030평균
sd(group2) # 2030표편
mean(group3) # 40평균
sd(group3) # 40표편
mean(group4) # 50평균
sd(group4) # 50표편
mean(group5) # 60평균
sd(group5) # 60표편

anova_data <- data.frame(
  value = c(group2, group3, group4, group5),
  group = factor(c(rep(2, length(group2)), 
                   rep(3, length(group3)), 
                   rep(4, length(group4)), 
                   rep(5, length(group5))))
)

anova_result <- aov(value ~ group, data = anova_data)
summary(anova_result)

########################################
# 자녀와의 관계 만족
# 데이터 생성
df6 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6], plan = survey[,77])
head(df6)
df6[,2] <- ifelse(df6[,2] == 1, 2, df6[,2])
df6[,3] <- ifelse(df6[,3] == 2, 2, ifelse(df6[,3] == 3,2, ifelse(df6[,3] == 4, 2, ifelse(df6[,3]==5,2, df6[,3]))))
df6 <- df6[df6[,4] != 6, ]

# 전체 자녀
table(df6[,4])
sum(table(df6[,4]))
round(table(df6[,4])/sum(table(df6[,4])) * 100 , 1)
mean(as.numeric(df6[,4]))
sd(as.numeric(df6[,4]))

#######################################
# 성별 별 자녀
# 빈도와 비율
table(df6[,1],df6[,4])
row1_sum <- sum(table(df6[,1], df6[,4])[1,])
ratio_row1 <- round((table(df6[,1], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,4])[2,])
ratio_row1 <- round((table(df6[,1], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 평균비교
group1 <- as.numeric(df6[df6[,1] == 1, 4])
group2 <- as.numeric(df6[df6[,1] == 2, 4])

mean(group1) # 남자 평균
sd(group1) # 남자 표편
mean(group2) # 여자 평균
sd(group2) # 여자 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)
########################################################
# 나이대별 자녀(연령)
# 빈도와 비율
table(df6[,2], df6[,4])
row1_sum <- sum(table(df6[,2], df6[,4])[1,])
ratio_row1 <- round((table(df6[,2], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[2,])
ratio_row1 <- round((table(df6[,2], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[3,])
ratio_row1 <- round((table(df6[,2], df6[,4])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[4,])
ratio_row1 <- round((table(df6[,2], df6[,4])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 평균비교
group2 <- as.numeric(df6[df6[,2] == 2, 4])
group3 <- as.numeric(df6[df6[,2] == 3, 4])
group4 <- as.numeric(df6[df6[,2] == 4, 4])
group5 <- as.numeric(df6[df6[,2] == 5, 4])

mean(group2) # 2030평균
sd(group2) # 2030표편
mean(group3) # 40평균
sd(group3) # 40표편
mean(group4) # 50평균
sd(group4) # 50표편
mean(group5) # 60평균
sd(group5) # 60표편

anova_data <- data.frame(
  value = c(group2, group3, group4, group5),
  group = factor(c(rep(2, length(group2)), 
                   rep(3, length(group3)), 
                   rep(4, length(group4)), 
                   rep(5, length(group5))))
)

anova_result <- aov(value ~ group, data = anova_data)
summary(anova_result)

###########################################
# 전반적 가족관계 만족
# 데이터 생성
df6 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6], plan = survey[,82])
head(df6)
df6[,2] <- ifelse(df6[,2] == 1, 2, df6[,2])
df6[,3] <- ifelse(df6[,3] == 2, 2, ifelse(df6[,3] == 3,2, ifelse(df6[,3] == 4, 2, ifelse(df6[,3]==5,2, df6[,3]))))
df6 <- df6[df6[,4] != 6, ]

# 전체 전반적 가족관계
table(df6[,4])
sum(table(df6[,4]))
round(table(df6[,4])/sum(table(df6[,4])) * 100 , 1)
mean(as.numeric(df6[,4]))
sd(as.numeric(df6[,4]))

#######################################
# 성별 별 전반적 가족관계
# 빈도와 비율
table(df6[,1],df6[,4])
row1_sum <- sum(table(df6[,1], df6[,4])[1,])
ratio_row1 <- round((table(df6[,1], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,4])[2,])
ratio_row1 <- round((table(df6[,1], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 평균비교
group1 <- as.numeric(df6[df6[,1] == 1, 4])
group2 <- as.numeric(df6[df6[,1] == 2, 4])

mean(group1) # 남자 평균
sd(group1) # 남자 표편
mean(group2) # 여자 평균
sd(group2) # 여자 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)
########################################################

# 나이대별 전반적 가족관계
# 빈도와 비율
table(df6[,2], df6[,4])
row1_sum <- sum(table(df6[,2], df6[,4])[1,])
ratio_row1 <- round((table(df6[,2], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[2,])
ratio_row1 <- round((table(df6[,2], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[3,])
ratio_row1 <- round((table(df6[,2], df6[,4])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[4,])
ratio_row1 <- round((table(df6[,2], df6[,4])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 평균비교
group2 <- as.numeric(df6[df6[,2] == 2, 4])
group3 <- as.numeric(df6[df6[,2] == 3, 4])
group4 <- as.numeric(df6[df6[,2] == 4, 4])
group5 <- as.numeric(df6[df6[,2] == 5, 4])

mean(group2) # 2030평균
sd(group2) # 2030표편
mean(group3) # 40평균
sd(group3) # 40표편
mean(group4) # 50평균
sd(group4) # 50표편
mean(group5) # 60평균
sd(group5) # 60표편

anova_data <- data.frame(
  value = c(group2, group3, group4, group5),
  group = factor(c(rep(2, length(group2)), 
                   rep(3, length(group3)), 
                   rep(4, length(group4)), 
                   rep(5, length(group5))))
)

anova_result <- aov(value ~ group, data = anova_data)
summary(anova_result)

######################################################
# 가구 별 전반적 가족
# 빈도와 비율
table(df6[,3],df6[,4])
row1_sum <- sum(table(df6[,3], df6[,4])[1,])
ratio_row1 <- round((table(df6[,3], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,4])[2,])
ratio_row1 <- round((table(df6[,3], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 평균비교
group1 <- as.numeric(df6[df6[,3] == 1, 4])
group2 <- as.numeric(df6[df6[,3] == 2, 4])

mean(group1) # 1인 평균
sd(group1) # 1인 표편
mean(group2) # 2인이상 평균
sd(group2) # 2인이상 표편

t_test_result <- t.test(group1, group2)
print(t_test_result)

################################################################################
# 8. 춘천시정신건강복지센터 인지도

# 복지센터를 아는지?
table(survey[,83])
round(table(survey[,83])/ sum(table(survey[,83])) * 100 , 1)


# 복지센터를 알게된 경로
table(survey[,84])
table(survey[,85])
table(survey[,86])
table(survey[,87])
table(survey[,88])
table(survey[,89])
table(survey[,90])

# 비율계산
rowsum <- sum(table(survey[,84]),
              table(survey[,85]),
              table(survey[,86]),
              table(survey[,87]),
              table(survey[,88]),
              table(survey[,89]),
              table(survey[,90]))

round(table(survey[,84])/434* 100,1)
round(table(survey[,85])/434* 100,1)
round(table(survey[,86])/434* 100,1)
round(table(survey[,87])/434* 100,1)
round(table(survey[,88])/434* 100,1)
round(table(survey[,89])/434* 100,1)
round(table(survey[,90])/434* 100,1)

######################################
# 춘천시 건강복지센터 인지도 표 20
df6 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6], plan = survey[,83])
head(df6)
df6[,2] <- ifelse(df6[,2] == 1, 2, df6[,2])
df6[,3] <- ifelse(df6[,3] == 2, 2, ifelse(df6[,3] == 3,2, ifelse(df6[,3] == 4, 2, ifelse(df6[,3]==5,2, df6[,3]))))

# 성별별 인지도
table(df6[,1],df6[,4])
row1_sum <- sum(table(df6[,1], df6[,4])[1,])
ratio_row1 <- round((table(df6[,1], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,4])[2,])
ratio_row1 <- round((table(df6[,1], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 나이대별 인지도
table(df6[,2], df6[,4])
row1_sum <- sum(table(df6[,2], df6[,4])[1,])
ratio_row1 <- round((table(df6[,2], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[2,])
ratio_row1 <- round((table(df6[,2], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[3,])
ratio_row1 <- round((table(df6[,2], df6[,4])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[4,])
ratio_row1 <- round((table(df6[,2], df6[,4])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 가구별 인지도
table(df6[,3],df6[,4])
row1_sum <- sum(table(df6[,3], df6[,4])[1,])
ratio_row1 <- round((table(df6[,3], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,4])[2,])
ratio_row1 <- round((table(df6[,3], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

############################################
# 복지센터에서 중요하게 다룰 사업
# 1순위
table(survey[,92])
round(table(survey[,92])/ sum(table(survey[,92])) * 100 , 1)
# 2순위
table(survey[,93])
round(table(survey[,93])/ sum(table(survey[,93])) * 100 , 1)
# 3순위
table(survey[,94])
round(table(survey[,94])/ sum(table(survey[,94])) * 100 , 1)

#############################################
# 전화서비스 인지도
# 전체 전화서비스 인지도(1577)
table(survey[,96])
round(table(survey[,96])/ sum(table(survey[,96])) * 100 , 1)
# 전체 전화서비스 이용(1577)
table(survey[,97])
round(table(survey[,97])/ sum(table(survey[,97])) * 100 , 1)
# 전체 전화서비스 인지도(1393)
table(survey[,98])
round(table(survey[,98])/ sum(table(survey[,98])) * 100 , 1)
# 전체 전화서비스 이용(1393)
table(survey[,99])
round(table(survey[,99])/ sum(table(survey[,99])) * 100 , 1)

###########################################
# 데이터 생성
df6 <- data.frame(sex = survey[,1], age = survey[,2], family = survey[,6], plan = survey[,96]
                  ,plan = survey[,97],plan = survey[,98],plan = survey[,99])
head(df6)
df6[,2] <- ifelse(df6[,2] == 1, 2, df6[,2])
df6[,3] <- ifelse(df6[,3] == 2, 2, ifelse(df6[,3] == 3,2, ifelse(df6[,3] == 4, 2, ifelse(df6[,3]==5,2, df6[,3]))))

# 성별별 인지도(1599)
table(df6[,1],df6[,4])
row1_sum <- sum(table(df6[,1], df6[,4])[1,])
ratio_row1 <- round((table(df6[,1], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,4])[2,])
ratio_row1 <- round((table(df6[,1], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 나이대별 인지도(1599)
table(df6[,2], df6[,4])
row1_sum <- sum(table(df6[,2], df6[,4])[1,])
ratio_row1 <- round((table(df6[,2], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[2,])
ratio_row1 <- round((table(df6[,2], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[3,])
ratio_row1 <- round((table(df6[,2], df6[,4])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,4])[4,])
ratio_row1 <- round((table(df6[,2], df6[,4])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 가구별 인지도(1599)
table(df6[,3],df6[,4])
row1_sum <- sum(table(df6[,3], df6[,4])[1,])
ratio_row1 <- round((table(df6[,3], df6[,4])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,4])[2,])
ratio_row1 <- round((table(df6[,3], df6[,4])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

##########################################
# 성별별 이용(1599)
table(df6[,1],df6[,5])
row1_sum <- sum(table(df6[,1], df6[,5])[1,])
ratio_row1 <- round((table(df6[,1], df6[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,5])[2,])
ratio_row1 <- round((table(df6[,1], df6[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 나이대별 이용(1599)
table(df6[,2], df6[,5])
row1_sum <- sum(table(df6[,2], df6[,5])[1,])
ratio_row1 <- round((table(df6[,2], df6[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,5])[2,])
ratio_row1 <- round((table(df6[,2], df6[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,5])[3,])
ratio_row1 <- round((table(df6[,2], df6[,5])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,5])[4,])
ratio_row1 <- round((table(df6[,2], df6[,5])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 가구별 이용(1599)
table(df6[,3],df6[,5])
row1_sum <- sum(table(df6[,3], df6[,5])[1,])
ratio_row1 <- round((table(df6[,3], df6[,5])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,5])[2,])
ratio_row1 <- round((table(df6[,3], df6[,5])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

####################################################
# 성별별 인지도(1393)
table(df6[,1],df6[,6])
row1_sum <- sum(table(df6[,1], df6[,6])[1,])
ratio_row1 <- round((table(df6[,1], df6[,6])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,6])[2,])
ratio_row1 <- round((table(df6[,1], df6[,6])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 나이대별 인지도(1393)
table(df6[,2], df6[,6])
row1_sum <- sum(table(df6[,2], df6[,6])[1,])
ratio_row1 <- round((table(df6[,2], df6[,6])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,6])[2,])
ratio_row1 <- round((table(df6[,2], df6[,6])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,6])[3,])
ratio_row1 <- round((table(df6[,2], df6[,6])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,6])[4,])
ratio_row1 <- round((table(df6[,2], df6[,6])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 가구별 인지도(1393)
table(df6[,3],df6[,6])
row1_sum <- sum(table(df6[,3], df6[,6])[1,])
ratio_row1 <- round((table(df6[,3], df6[,6])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,6])[2,])
ratio_row1 <- round((table(df6[,3], df6[,6])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

##########################################
# 성별별 이용(1393)
table(df6[,1],df6[,7])
row1_sum <- sum(table(df6[,1], df6[,7])[1,])
ratio_row1 <- round((table(df6[,1], df6[,7])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,1], df6[,7])[2,])
ratio_row1 <- round((table(df6[,1], df6[,7])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

# 나이대별 이용(1393)
table(df6[,2], df6[,7])
row1_sum <- sum(table(df6[,2], df6[,7])[1,])
ratio_row1 <- round((table(df6[,2], df6[,7])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,7])[2,])
ratio_row1 <- round((table(df6[,2], df6[,7])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,7])[3,])
ratio_row1 <- round((table(df6[,2], df6[,7])[3,] / row1_sum) * 100,1)
cat("3행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,2], df6[,7])[4,])
ratio_row1 <- round((table(df6[,2], df6[,7])[4,] / row1_sum) * 100,1)
cat("4행의 비율:\n")
print(ratio_row1)

# 가구별 이용(1393)
table(df6[,3],df6[,7])
row1_sum <- sum(table(df6[,3], df6[,7])[1,])
ratio_row1 <- round((table(df6[,3], df6[,7])[1,] / row1_sum) * 100,1)
cat("1행의 비율:\n")
print(ratio_row1)
row1_sum <- sum(table(df6[,3], df6[,7])[2,])
ratio_row1 <- round((table(df6[,3], df6[,7])[2,] / row1_sum) * 100,1)
cat("2행의 비율:\n")
print(ratio_row1)

############################################
# 알게된 경로
table(survey[,100])
table(survey[,101])
table(survey[,102])
table(survey[,103])
table(survey[,104])
table(survey[,105])
table(survey[,106])

# 비율계산
rowsum <- sum(table(survey[,100]),
              table(survey[,101]),
              table(survey[,102]),
              table(survey[,103]),
              table(survey[,104]),
              table(survey[,105]),
              table(survey[,106]))

round(table(survey[,100])/434* 100,1)
round(table(survey[,101])/434* 100,1)
round(table(survey[,102])/434* 100,1)
round(table(survey[,103])/434* 100,1)
round(table(survey[,104])/434* 100,1)
round(table(survey[,105])/434* 100,1)
round(table(survey[,106])/434* 100,1)







