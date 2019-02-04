# 성씨
ln <- read.csv("성씨.csv")
ln
library(ggplot2)
ggplot(data=ln, aes(x=lastName, y=people)) + geom_col() + coord_flip()
ggplot(data=ln, aes(x=reorder(lastName, ln$people), y=people)) + geom_bar(stat="identity")
ggplot(data=ln, aes(x=reorder(lastName, -ln$people), y=people)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90))
ggplot(data=ln, aes(x=reorder(lastName, -ln$people), y=people)) + geom_bar(stat="identity") + coord_flip()
p <- ggplot(data=ln, aes(x=reorder(lastName, -ln$people), y=people)) + geom_bar(stat="identity") + coord_flip()
p + labs(x = '인구수', y = '성씨', title = '우리나라의 성씨 그래프')

# 성씨 완성! 인구수
p <-ggplot(data=ln, aes(x=reorder(lastName, -ln$people), y=people)) + 
  geom_bar(stat="identity") + coord_flip() + 
  labs(x = '인구수', y = '성씨', title = '우리나라의 성씨 그래프') + 
  geom_text(aes(label=people), size = 2.5, hjust = 0, color = 'black')
p

# 성씨 파이그래프 다시
ln2 <- sort(ln$percent, decreasing = TRUE)
ln2

lastName1 <- ln$lastName[order()]
lastName1

ln3 <- ln[order(-ln$percent),]
ln3

# 성씨 완료! 퍼센트1
head <- head(ln3$percent, n = 10)
head
lbls <- head(ln3$lastName, n = 10)
lbls <- paste(lbls, ln3$percent)
lbls <- paste(lbls, "%", sep = "")
lbls
pie(head, labels = lbls, col = rainbow(length(head)), cex = 0.7)
# 성씨 완료! 퍼센트2
tail <- tail(ln3$percent, n = 11)
lbls <- tail(ln3$lastName, n = 11)
lbls <- paste(lbls, tail)
lbls <- paste(lbls, "%", sep = "")
lbls
pie(tail, labels = lbls, col = rainbow(length(tail)), cex = 0.7)

# 여행지 막대 그래프 완료 
tour <- read.csv("여행지-2015년 기준.csv")
tour2 <- tour[, c(2:7)]
tour2
barplot(as.matrix(tour2), main = "가고 싶은 여행지", beside = T, col = rainbow(nrow(tour2)), ylim = c(0, 80))
legend("topright", title = "여행지", fill = c("red", "orange", "yellow", "#33FF00", "#00FF66", "#00FFFF", "#0066FF", "#3300FF", "#CC00FF", "#FF0099"),
       c("프랑스", "하와이", "호주", "미국", "스위스", "이탈리아", "일본", "영국", "홍콩/마카오", "뉴질랜드"), cex = 0.5)
tour2 <- as.matrix(tour2)
tour2

tour
tour3 <- tour[, c(1, 8)]
tour3
tour3 <- as.matrix(tour3)
tour3
#barplot(as.matrix(tour3), main = "가고 싶은 여행지", beside = T, col = rainbow(nrow(tour3)), ylim = c(0, 80))
barplot(tour3$총합, beside = T, names.arg = tour3$nation, col = rainbow(nrow(tour3)))
text(0.7,200,"208.2")
text(1.9,170,"179.4")
text(3.1,160,"171.0")
text(4.3,160,"170.0")
text(5.5,150,"156.4")
text(6.7,150,"158.8")
text(7.8,140,"146.9")
text(9.1,120,"123.7")
text(10.3,120,"127.5")
text(11.5,105,"109.4")

# 계층 이동
library(plotrix)
move <- read.csv("계층 이동.csv")
move
lbls2 <- move$계층이동
lbls2 <- paste(lbls2, move$수치)
lbls2 <- paste(lbls2, "%", sep = "")
lbls2
pie3D(move$수치, labels = lbls2)
pie3D(move$수치)

# 오라클 (키)
library(RJDBC)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
mysql <- "select count(height_value)  from statUser where height_value < 150"
mysql2 <- "select count(height_value) from statUser where height_value >= 150 and height_value < 155"
mysql3 <- "select count(height_value) from statUser where height_value >= 155 and height_value < 160"
mysql4 <- "select count(height_value) from statUser where height_value >= 160 and height_value < 165"
mysql5 <- "select count(height_value) from statUser where height_value >= 165 and height_value < 170"
mysql6 <- "select count(height_value) from statUser where height_value >= 170 and height_value < 175"
mysql7 <- "select count(height_value) from statUser where height_value >= 175 and height_value < 180"
mysql8 <- "select count(height_value) from statUser where height_value >= 180 and height_value < 185"
mysql9 <- "select count(height_value) from statUser where height_value >= 185"
a <- dbGetQuery(conn, mysql)
b <- dbGetQuery(conn, mysql2)
c <- dbGetQuery(conn, mysql3)
d <- dbGetQuery(conn, mysql4)
e <- dbGetQuery(conn, mysql5)
f <- dbGetQuery(conn, mysql6)
g <- dbGetQuery(conn, mysql7)
h <- dbGetQuery(conn, mysql8)
i <- dbGetQuery(conn, mysql9)
#df <- data.frame(a, b, c, d, e, f, g, h, i)
#df
m <- matrix(c(a,b,c,d,e,f,g,h,i))
#m <- array(c(a,b,c,d,e,f,g,h,i))
m
plot1 <- plot(m, type = 'h', axes = FALSE, ann = FALSE, ylab = " ", xlab = " ", ylim = c(0, 20))
axis(1, at = 1:9, lab = c("150", "150~155", "155~160", "160~165", "165~170", "170~175", "175~180", "180~185", "185"), las = 2)
axis(2, ylim = c(0, 20))
points(m, pch = 19, col = "black")
title(main = "User Height", col.main = "blue", font.main = 4)
for (i in 1:9) {
  #  text(i, m[i], m[i], adj = c(0, 0))
  text(i, m[i], m[i], pos = 4)
}



# 사용자들의 성별 평균 키 막대 그래프
library(RJDBC)
library(ggplot2)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select user_gender, around(avg(height_value)) height from statUser group by user_gender"
q <- dbGetQuery(conn, q)
q
barplot(tapply(q$HEIGHT, q$USER_GENDER, mean), col = c("#821122", "pink"), ylim = c(0, 200))
b_plot <- barplot(tapply(q$HEIGHT, q$USER_GENDER, mean), col = c("#821122", "pink"), ylim = c(0, 200))
ggplot(data = q, aes(x = q$USER_GENDER, y = q$HEIGHT)) + geom_col() +labs(x= 'Gender', y = 'cm') + geom_text(aes(label=q$HEIGHT), size = 3, vjust=1.25, color = '#FFFFFF')

# 성씨
library(RJDBC)
library(plotrix)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select ln_value, count(ln_value) lnCount from statUser where ln_value is not null group by ln_value"
q <- dbGetQuery(conn, q)
q
q_labels1 <- q$LN_VALUE
q_labels1 <- paste(q_labels1, q$LNCOUNT)
q_labels1 <- paste(q_labels1, "People", sep="")
q_labels1
q$LN_VALUE
pie3D(q$LNCOUNT)
pie3D(r$RATIO, labels = r$JOB)

# 반려동물 막대
library(DBI)
library(rJava)
library(RJDBC)
library(plyr)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select pet_value, count(pet_value) pet from statUser where pet_value is not null group by pet_value"
q <- dbGetQuery(conn, q)
qd <- data.frame(q)
q2 <- "select petDB_id, petDB_value pet_value from petDB"
q2 <- dbGetQuery(conn, q2)
q2d <- data.frame(q2)
q2d
#q3 <- cbind(q, q2)
q3 <- merge(qd, q2d, by = 'PET_VALUE', all = FALSE)
q3 <- data.frame(merge(qd, q2d, by = 'PET_VALUE', all = FALSE))
#colnames(q3)
#str(q3)
#attach(q3)
#q4 <- q3[order(q3$PET_VALUE)]
library(plyr)
q4 <- arrange(q3, PETDB_ID)
q4
#ggplot(data = q4, aes(x = q4$PETDB_ID, y = q4$PET)) + geom_col() + geom_text(aes(label=q4$PET), size = 3, vjust=1.25, color = '#FFFFFF')
#ggplot(data = q4, aes(x = q4$PET_VALUE, y = q4$PET)) + geom_col() +labs(x= 'Pet Kind', y = 'People') + geom_text(aes(label=q4$PET), size = 3, vjust=1.25, color = '#FFFFFF')
#barplot(tapply(q4$PET, q4$PET_VALUE, mean))
barplot(q4$PET, beside = T, names.arg = q4$PET_VALUE, las = 2)
text(x = barplot, y = q4$PET * 0.95, labels = q4$PET, cex = 0.7)
#legend("topright", title = "여행지", fill = c("red", "orange", "yellow", "#33FF00", "#00FF66", "#00FFFF", "#0066FF", "#3300FF", "#CC00FF"),
#      c("", "고양이", "새", "토끼", "햄스터", "물고기", "거북이", "기타", "없음"), cex = 0.5)


# --------------
# 반려동물 다시
library(RJDBC)
library(plyr)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select pet_value, count(pet_value) pet from statUser where pet_value is not null group by pet_value"
q <- dbGetQuery(conn, q)
qd <- data.frame(q)
q2 <- "select petDB_id, petDB_value pet_value from petDB"
q2 <- dbGetQuery(conn, q2)
q2d <- data.frame(q2)
#q3 <- cbind(q, q2)
q3 <- merge(qd, q2d, by = 'PET_VALUE', all = FALSE)
q3 <- data.frame(merge(qd, q2d, by = 'PET_VALUE', all = FALSE))
colnames(q3)
str(q3)
attach(q3)
q4 <- arrange(q3, PETDB_ID)
barplot <- barplot(q4$PET, beside = T, names.arg = q4$PET_VALUE, las = 2)
text(x = barplot, y = q4$PET * 0.95, labels = q4$PET, cex = 0.7)

q4$PET

legend("topright", title = "여행지", fill = c("red", "orange", "yellow", "#33FF00", "#00FF66", "#00FFFF", "#0066FF", "#3300FF", "#CC00FF"),
       c("", "고양이", "새", "토끼", "햄스터", "물고기", "거북이", "기타", "없음"), cex = 0.5)


# 여행지
library(RJDBC)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select td.tourDB_id1, count(su.tourDB_id1) count, td.tourDB_nation 
from statUser su, tourDB td where su.tourDB_id1 = td.tourDB_id1
group by su.tourDB_id1, td.tourDB_id1, td.tourDB_nation"
q <- dbGetQuery(conn, q)
q
ggplot(data = q, aes(x = q$TOURDB_NATION, y = q$COUNT)) + geom_col() + labs(x = 'Nation', y = 'People') + geom_text(aes(label=q$COUNT), size = 3, vjust = 1.25, color = '#FFFFFF')

# 늙었다고
library(RJDBC)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select oldDB_id, count(oldDB_id) count from statUser where oldDB_id is not null group by oldDB_id order by oldDB_id"
q2 <- "select oldDB_id, oldDB_value from oldDB"
q <- dbGetQuery(conn, q)
q2 <- dbGetQuery(conn, q2)
q2
slices <- q$COUNT
lbls <- q$OLDDB_VALUE
slices <- paste(q2$OLDDB_VALUE, slices)
slices <- paste(slices,'People', sep = "")
slices
pie(q$COUNT, labels = slices, cex = 0.8, col = rainbow(length(q$COUNT)))

drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select o.oldDB_id, count(s.oldDB_id) count, o.oldDB_value from statUser s, oldDB o
where o.oldDB_id = s.oldDB_id
group by o.oldDB_id, s.oldDB_id, o.oldDB_value"
q <- dbGetQuery(conn, q)
q
ggplot(data = q, aes(x = q$OLDDB_VALUE, y = q$COUNT)) + geom_col() + coord_flip() + geom_bar(stat = "identity") + labs(x = 'old Value', y = 'People') + geom_text(aes(label = q$COUNT), size = 2.5, hjust = 2.5, color = "#FFFFFF")
ggplot(data = q, aes(x = q$OLDDB_VALUE, y = q$COUNT)) + geom_col() + coord_flip() + geom_bar(stat = "identity") + labs(x = ' ', y = 'People') + geom_text(aes(label = q$COUNT), size = 2.5, hjust = 2.5, color = "#FFFFFF")

# 여가시간
library(RJDBC)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select d.digitalDB_id, count(s.digitalDB_id) digitalCount, digitalDB_value from digitalDB d, statUser s 
where s.digitalDB_id is not null and d.digitalDB_id = s.digitalDB_id
group by d.digitalDB_id, s.digitalDB_id, digitalDB_value
order by digitalDB_id"
q <- dbGetQuery(conn, q)
q
plot(q$DIGITALCOUNT, axes = F, type = "h", ylab = " ", xlab = "", ylim = c(0, 20))
axis(1, at = 1:10, lab = c("Phone", "TV", "Computer", "Game", "Movie", "Cleaning", "Excercise", "Meeting", "Sleeping", "Book"), las = 2)
axis(2)
points(q$DIGITALCOUNT, pch = 19, col = "black")
for (i in 1:10) {
  text(i, q$DIGITALCOUNT[i], q$DIGITALCOUNT[i], pos = 4)
}


# SNS 
library(RJDBC)
library(ggplot2)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select m.mobileDB_id, count(s.mobileDB_id) mobileCount, mobileDB_value from statUser s, mobileDB m
where s.mobileDB_id is not null and s.mobileDB_id = m.mobileDB_id
group by m.mobileDB_id, s.mobileDB_id, mobileDB_value
order by mobileDB_id"
q <- dbGetQuery(conn, q)
q
ggplot(data = q, aes(x = q$MOBILEDB_VALUE, y = q$MOBILECOUNT)) + geom_bar(stat = 'identity') + coord_flip() + labs(y = 'People', x = 'Mobile Contents') + geom_text(aes(label=q$MOBILECOUNT), size = 3, hjust = 1.25, color = '#FFFFFF')

# 부자
library(RJDBC)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select tf.transDB_id, count(s.transDB_id) transCount, tf.transDB_value from statUser s, transferDB tf 
where s.transDB_id is not null and tf.transDB_id = s.transDB_id
group by tf.transDB_id, s.transDB_id, tf.transDB_value"
q <- dbGetQuery(conn, q)
q
ggplot(data = q, aes(x = q$TRANSDB_VALUE, y = q$TRANSCOUNT)) + geom_bar(stat = 'identity') + coord_flip() + labs(y = 'People', x = 'Transfer') + geom_text(aes(label=q$TRANSCOUNT), size = 3, hjust = 1.25, color = '#FFFFFF')

# 행복
library(RJDBC)
library(ggplot2)
library(MASS)
library(sqldf)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select s.happyDB_id, count(ha.happyDB_id) happyCount, ha.happyDB_value from statUser s, happyDB ha 
where ha.happyDB_id is not null and s.happyDB_id = ha.happyDB_id
group by s.happyDB_id, ha.happyDB_id, ha.happyDB_value"
q <- dbGetQuery(conn, q)
q
sapply(q, class)
People <- q$HAPPYCOUNT
happy <- q$HAPPYDB_VALUE
ggplot(data = q, aes(x="", y=People, fill = happy)) +
  facet_grid(facets = People) + geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y", clip = "on")

drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select s.happyDB_id, count(ha.happyDB_id) happyCount, ha.happyDB_value from statUser s, happyDB ha 
where ha.happyDB_id is not null and s.happyDB_id = ha.happyDB_id
group by s.happyDB_id, ha.happyDB_id, ha.happyDB_value"
q <- dbGetQuery(conn, q)
People <- q$HAPPYCOUNT
happy <- q$HAPPYDB_VALUE
People <- paste(happy, People)
People
pie(q$HAPPYCOUNT, labels = People, cex = 0.8, col = rainbow(length(q$HAPPYCOUNT)))






# 성씨 행복지수 상관관계 전처리
library(RJDBC)
library(ggplot2)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")
q <- "select s.ln_value, count(s.happyDB_id) happyCount, h.happyDB_value from statUser s, happyDB h
where h.happyDB_id = s.happyDB_id and ln_value is not null group by s.ln_value, h.happyDB_id, s.happyDB_id, h.happyDB_value"
q <- dbGetQuery(conn, q)
q
str(q)
list_ln <- q[, c(2:4)]
list_ln
#ggplot(data = list_ln, aes(x = list_ln$LN_VALUE, y = list_ln$HAPPYCOUNT, fill = list_ln$HAPPYDB_VALUE)) + geom_col() + coord_flip()
ggplot(data = list_ln, aes(x = list_ln$LN_VALUE, y = list_ln$HAPPYCOUNT, fill = list_ln$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = ' ', fill = ' ')
ggplot(data = q$HAPPYCOUNT, aes(x = q$LN_VALUE, y = q$HAPPYCOUNT, fill = q$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = ' ', fill = ' ')

# 성별 행복지수 분석
install.packages("dplyr")
library(dplyr)

mysql <- "select user_gender, happyDB_id from statUser"
r <- dbGetQuery(conn, mysql)

gen_happy <- r %>% filter(!is.na(HAPPYDB_ID)) %>% group_by(USER_GENDER, HAPPYDB_ID) %>% summarise(n = n())
gen_happy
gen_happy <- gen_happy %>% mutate(h = ifelse(HAPPYDB_ID==1, "아주 행복하다", ifelse(HAPPYDB_ID==2, "대체로 행복하다", ifelse(HAPPYDB_ID==3, "그저 그렇다", ifelse(HAPPYDB_ID==4, "대체로 불행하다", "아주 불행하다")))))
ggplot(data = gen_happy, aes(x=USER_GENDER, y = n, fill = h), xlab('명')) + geom_col() + coord_flip()

mysql <- "select s.user_gender, count(s.happyDB_id) happyCount, h.happyDB_value from statUser s, happyDB h
where s.happyDB_id = h.happyDB_id group by s.user_gender, s.happyDB_id, h.happyDB_value"
mysql <- dbGetQuery(conn, mysql)
mysql
ggplot(data = mysql, aes(x = mysql$USER_GENDER, y = mysql$HAPPYCOUNT, fill = mysql$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = ' ', fill = ' ')

# 연령대
sql1 <- "select user_age, count(*) ageCount, count(s.happyDB_id) happyCount, h.happyDB_value from statUser s, happyDB h
where user_age < 10 and s.happyDB_id is not null
group by user_age, s.happyDB_id, h.happyDB_value"
sql1 <- dbGetQuery(conn, sql1)
sql1
sql2 <- "select user_age, count(*) ageCount, count(s.happyDB_id) happyCount, h.happyDB_value from statUser s, happyDB h
where user_age  s.happyDB_id is not null
group by user_age, s.happyDB_id, h.happyDB_value"
sql2 <- dbGetQuery(conn, sql2)
sql2
ggplot(data = sql2, aes(x = sql2$USER_AGE, y = sql2$HAPPYCOUNT, fill = sql2$HAPPYDB_VALUE)) + geom_col() + coord_flip()


# 20대 총 수
totalCnt <- "select count(*) totalCnt from statUser where user_age >= 20 and user_age < 30"
totalCnt <- dbGetQuery(conn, totalCnt)
totalCnt

groupCnt <- "select s.happyDB_id, count(s.happyDB_id) Cnt, h.happyDB_value from statUser s, happyDB h
where s.happyDB_id = h.happyDB_id and user_age >= 20 and user_age < 30
group by s.happyDB_id, h.happyDB_value"
groupCnt <- dbGetQuery(conn, groupCnt)
groupCnt

library(dplyr)
groupCnt <- groupCnt %>% group_by(HAPPYDB_ID) %>% summarise(rate = groupCnt$CNT / totalCnt )
groupCnt
groupCnt$CNT

rate <- groupCnt$CNT / totalCnt
totalCnt
rate

# 반려동물
sql <- "select s.pet_value, count(s.happyDB_id) happyCnt, h.happyDB_value from statUser s, happyDB h 
where s.happyDB_id = h.happyDB_id and pet_value is not null group by s.pet_value, s.happyDB_id, h.happyDB_value"
sql <- dbGetQuery(conn, sql)
sql
ggplot(data = sql, aes(x = sql$PET_VALUE, y = sql$HAPPYCNT, fill = sql$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = ' ', fill = ' ')
ggplot(data = list_ln, aes(x = list_ln$LN_VALUE, y = list_ln$HAPPYCOUNT, fill = list_ln$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = ' ', fill = ' ')

# 여행지
tourSql <- "select t.tourDB_nation, count(s.happyDB_id) happyCount, h.happyDB_value from statUser s, happyDB h, tourDB t
where s.tourDB_id1 = t.tourDB_id1 and s.happyDB_id = h.happyDB_id and s.tourDB_id1 is not null and s.happyDB_id is not null
group by t.tourDB_nation, s.happyDB_id, h.happyDB_value"
tourSql <- dbGetQuery(conn, tourSql)
tourSql
ggplot(data = tourSql, aes(x = tourSql$TOURDB_NATION, y = tourSql$HAPPYCOUNT, fill = tourSql$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = ' ', fill = ' ')

# 여가
digitalSql <- "select d.digitalDB_value, count(s.happyDB_id) happyCount, h.happyDB_value from statUser s, happyDB h, digitalDB d
where s.digitalDB_id = d.digitalDB_id and s.happyDB_id = h.happyDB_id and s.digitalDB_id is not null and s.happyDB_id is not null
group by d.digitalDB_value, s.happyDB_id, h.happyDB_value"
digitalSql <- dbGetQuery(conn, digitalSql)
digitalSql
ggplot(data = digitalSql, aes(x = digitalSql$DIGITALDB_VALUE, y = digitalSql$HAPPYCOUNT, fill = digitalSql$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = ' ', fill = ' ')

# SNS
snsSql <- "select m.mobileDB_value, count(s.happyDB_id) happyCount, h.happyDB_value from statUser s, happyDB h, mobileDB m
where m.mobileDB_id = s.mobileDB_id and s.happyDB_id = h.happyDB_id and s.mobileDB_id is not null and s.happyDB_id is not null
group by m.mobileDB_value, s.happyDB_id, h.happyDB_value"
snsSql <- dbGetQuery(conn, snsSql)
snsSql
ggplot(data = snsSql, aes(x = snsSql$MOBILEDB_VALUE, y = snsSql$HAPPYCOUNT, fill = snsSql$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = ' ', fill = ' ')


library(RJDBC)
library(ggplot2)
library(MASS)
library(dplyr)
library(sqldf)
# 10대 행복지수 분석
sql1 <- "select happyDB_id, count(*) cnt from statUser where 
user_age >= 10 and user_age < 20 and s.happyDB_id is not null group by happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)

groupCnt
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h 
where h.happyDB_id = s.happyDB_id and user_age >= 10 and user_age < 20 and s.happyDB_id is not null 
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
#> 조인하고 나서 두개 적용
groupCnt1

# 20대 행복지수 분석
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h 
where h.happyDB_id = s.happyDB_id and user_age >= 20 and user_age < 30 and s.happyDB_id is not null 
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(age = '20대')
groupCnt2 <- groupCnt %>% mutate(pct=round(CNT/sum
                                           (CNT)*100, 1))
groupCnt2

# 30대 행복지수 분석
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h 
where h.happyDB_id = s.happyDB_id and user_age >= 30 and user_age < 40 and s.happyDB_id is not null 
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(age = '30대')
groupCnt3 <- groupCnt %>% mutate(pct=round(CNT/sum
                                           (CNT)*100, 1))
groupCnt3

# 40대 행복지수 분석
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h 
where h.happyDB_id = s.happyDB_id and user_age >= 40 and user_age < 50 and s.happyDB_id is not null 
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(age = '40대')
groupCnt4 <- groupCnt %>% mutate(pct=round(CNT/sum
                                           (CNT)*100, 1))
groupCnt4

# 50대 행복지수 분석
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h 
where h.happyDB_id = s.happyDB_id and user_age >= 50 and s.happyDB_id is not null 
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(age = '50대')
groupCnt5 <- groupCnt %>% mutate(pct=round(CNT/sum
                                           (CNT)*100, 1))
groupCnt5

# 세대별 결과 병합
groupResult <- rbind(groupCnt1, groupCnt2, groupCnt3, 
                     groupCnt4, groupCnt5)
ggplot(data = groupResult, aes(x=age, y=pct, fill=groupResult$HAPPYDB_VALUE)) + 
  geom_col() + coord_flip() + labs(x = 'age', y = 'percent', fill = ' ')



library(DBI)
library(rJava)
library(RJDBC)
library(dplyr)
library(ggplot2)
drv <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar")
conn <- dbConnect(drv, "jdbc:oracle:thin:@127.0.0.1:1521:xe", "scott", "tiger")


# 키 행복지수

# 남자 키별 행복지수
# 키별 행복지수 분석(150미만)
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h
where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value < 150 and s.user_gender = 'man'
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(height = '150미만')
groupCnt1 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))
groupCnt1

# 키별 행복지수 분석(150~160)
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h
where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 150 and s.height_value < 160 and s.user_gender = 'man'
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(height = '150~160')
groupCnt2 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))
groupCnt2

# 키별 행복지수 분석(160~170)
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h
where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 160 and s.height_value < 170 and s.user_gender = 'man'
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(height = '160~170')
groupCnt3 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))
groupCnt3

# 키별 행복지수 분석(170~180)
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h
where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 170 and s.height_value < 180 and s.user_gender = 'man'
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(height = '170~180')
groupCnt4 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))
groupCnt4

# 키별 행복지수 분석(180이상)
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h
where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 180 and s.user_gender = 'man'
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(height = '180이상')
groupCnt5 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))
groupCnt5

# 남자 키별 결과 병합
groupResult <- rbind(groupCnt2, groupCnt3, groupCnt4, groupCnt5)
ggplot(data = groupResult, aes(x=height, y=pct, fill=groupResult$HAPPYDB_VALUE)) + geom_col() + coord_flip() + ggtitle('Men') + xlab('Height(cm)') + ylab('Percent')


# 여자 키별 행복지수
# 키별 행복지수 분석(150미만)
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h
where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value < 150 and s.user_gender = 'woman'
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(height = '150미만')
groupCnt1 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))
groupCnt1

# 키별 행복지수 분석(150~160)
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h
where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 150 and s.height_value < 160 and s.user_gender = 'woman'
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(height = '150~160')
groupCnt2 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))

# 키별 행복지수 분석(160~170)
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h
where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 160 and s.height_value < 170 and s.user_gender = 'woman'
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(height = '160~170')
groupCnt3 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))
groupCnt3

# 키별 행복지수 분석(170~180)
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h
where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 170 and s.height_value < 180 and s.user_gender = 'woman'
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(height = '170~180')
groupCnt4 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))
groupCnt4

# 키별 행복지수 분석(180이상)
sql1 <- "select h.happyDB_value, count(*) cnt from statUser s, happyDB h
where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 180 and s.user_gender = 'woman'
group by h.happyDB_value, s.happyDB_id"
groupCnt <- dbGetQuery(conn, sql1)
groupCnt <- groupCnt %>% mutate(height = '180이상')
groupCnt5 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))
groupCnt5

# 여자 키별 결과 병합
groupResult <- rbind(groupCnt2, groupCnt3)
ggplot(data = groupResult, aes(x=height, y=pct, fill=groupResult$HAPPYDB_VALUE)) + geom_col() + coord_flip() + ggtitle('Women') + xlab('Height(cm)') + ylab('Percent')
