drop table statUser;

유저 - tourDB_id2 제거하고 tourDB_id1 외래키 연결
create table statUser (
	user_id number(30) primary key,
	user_place varchar2(100) not null,
	user_gender varchar2(100) not null,
	user_age number(10) not null,
	user_group varchar2(100) not null,
	pet_value varchar2(100),
	height_value number(30),
	ln_value varchar2(100),
	transDB_id number(10) references transferDB(transDB_id),
	digitalDB_id number(10) references digitalDB(digitalDB_id),
	happyDB_id	number(10) references happyDB(happyDB_id),
	tourDB_id1	number(10) references tourDB(tourDB_id1),
	oldDB_id	number(10) references oldDB(oldDB_id),
	mobileDB_id	number(10) references mobileDB(mobileDB_id),
	user_nick	varchar2(100)
);
select * from statUser;
나이DB
create table ageDB (
	ageDB_id	varchar2(30)	primary key,
	ageDB_value	number(10) not null
);
drop table ageDB;
select * from ageDB;


반려동물DB - 퍼센트 추가
create table petDB (
	petDB_id	number(10)	primary key,
	petDB_value	varchar2(100)	not null,
	petDB_percent number(10) not null
);
drop table petDB;

성씨DB - 인구수, 퍼센트 추가
create table lastNameDB (
	lnDB_id	number(10) primary key,
	lnDB_value	varchar2(100) not null,
	lnDB_people number(38) not null,
	lnDB_percent number(10) not null
);
drop table lastNameDB;
select * from lastNameDB;
alter table lastNameDB rename column lnDB_person to lnDB_people;

계층이동DB - 퍼센트 추가
create table transferDB (
	transDB_id	number(10) primary key,
	transDB_value	varchar2(100) not null,
	transDB_percent number(10) not null
);
drop table transferDB;

디지털 의존도 DB - 
create table digitalDB (
	digitalDB_id	number(10) primary key,
	digitalDB_value	varchar2(100) not null,
	digitalDB_percent number(10) not null
);
drop table digitalDB;

행복지수 DB
create table happyDB (
	happyDB_id	number(10) primary key,
	happyDB_value	varchar2(100) not null
);
select * from happyDB;
drop table happyDB;

행복지수 데이터 DB - 연령대/행복지수
create table happyData (
	happyData_id number(10) primary key,
	happyData_value number(10) not null
);

여행지DB - 아이디 / 나라
create table tourDB (
	tourDB_id1	number(10) primary key,
	tourDB_nation	varchar2(100) not null,
	tourDB_m20	number(10) not null,
	tourDB_m30	number(10) not null,
	tourDB_m40	number(10) not null,
	tourDB_f20	number(10) not null,
	tourDB_f30	number(10) not null,
	tourDB_f40	number(10) not null
);
drop table tourDB;

나이 들었을 때 DB
create table oldDB (
	oldDB_id	number(10) primary key,
	oldDB_value	varchar2(100) not null,
	oldDB_age20 number(10) not null,
	oldDB_age30 number(10) not null,
	oldDB_age40 number(10) not null
);
drop table oldDB;

소셜네트워크 컨텐츠 DB
create table mobileDB (
	mobileDB_id	number(10) primary key,
	mobileDB_value	varchar2(100) not null,
	mobileDB_person number(38) not null,
	mobileDB_percent number(10) not null
);
alter table mobileDB rename column mobileDB_person to mobileDB_people;
select * from mobileDB;
drop table mobileDB;