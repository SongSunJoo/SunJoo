유저
create table statUser (
	user_id 	varchar2(100) primary key not null,
	user_place 	varchar2(100) not null,
	user_gender varchar2(100) not null,
	user_age 	number(10) not null,
	user_group	varchar2(100) not null,
	pet_value	varchar2(100),
	height_value number(30),
	ln_value 	varchar2(100),
	transDB_id 	number(10) references transferDB(transDB_id),
	digitalDB_id number(10) references digitalDB(digitalDB_id),
	happyDB_id	number(10) references happyDB(happyDB_id),
	tourDB_id1	varchar2(100),
	tourDB_id2	varchar2(100),
	oldDB_id	number(10) references oldDB(oldDB_id),
	mobileDB_id	number(10) references mobileDB(mobileDB_id),
	user_nick	varchar2(100)
);
alter table statUser modify (user_id number(30));
alter table statUser modify (tourDB_id1 number(10));
select * from statUser;

insert into statUser values(user_ic=1,
							user_place="서울",
							user_gender="여",
							user_age=31,
							user_group="직장인",
							pet_value=null,
							height_value=null,
							ln_value=null,
							transDB_id=null,
							digitalDB_id=null,
							happyDB_id=null,,,,,);

insert into statUser (user_id,user_place,user_gender,user_age,user_group,
							pet_value,height_value,ln_value,transDB_id,digitalDB_id,
							happyDB_id,tourDB_id1,tourDB_id2,oldDB_id,mobileDB_id,user_nick) 

insert into statUser (user_id,user_place,user_gender,user_age,user_group)
			values   (1,'서울','여',31,'직장인');


나이DB
create table ageDB (
	ageDB_id	varchar2(100) primary key not null,
	ageDB_value	number(10)    not null
);

drop table ageDB;
insert into ageDB values ('여', 85.7);
insert into ageDB values ('남', 79.7);
select * from ageDB;

반려동물DB
create table petDB (
	petDB_id	number(10)	primary key not null,
	petDB_value	varchar2(100)	not null
);

성씨DB
create table lastNameDB (
	lnDB_id	number(10) primary key not null,
	lnDB_value	varchar2(100) not null
);

계층이동DB
create table transferDB (
	transDB_id	number(10) primary key not null,
	transDB_value	varchar2(100) not null
);

alter table transferDB add (transDB_percent number(10) not null)
select * from transferDB;

디지털 의존도 DB
create table digitalDB (
	digitalDB_id	number(10) primary key not null,
	digitalDB_value	varchar2(100) not null
);

alter table digitalDB add (digitalDB_percent number(10) not null);
select * from digitalDB;

insert into digitalDB values (1, '스마트폰을 한다', 72);
insert into digitalDB values (2, 'TV를 본다', 59);
insert into digitalDB values (3, '컴퓨터를 한다', 48);
insert into digitalDB values (4, '게임을 한다', 28);
insert into digitalDB values (5, '영화를 본다', 27);
insert into digitalDB values (6, '집(방)을 청소한다', 23);
insert into digitalDB values (7, '운동을 한다', 22);
insert into digitalDB values (8, '친구나 지인을 만난다', 21);
insert into digitalDB values (9, '(낮)잠을 잔다', 20);
insert into digitalDB values (10, '책을 읽는다', 20);


행복지수 DB
create table happyDB (
	happyDB_id	number(10) primary key not null,
	happyDB_value	varchar2(100) not null
);

여행지DB
create table tourDB (
	tourDB_id1	  number(10)    primary key not null,
	tourDB_nation varchar2(100) not null,
	tourDB_m20    number(10)    not null,
	tourDB_m30    number(10)    not null,
	tourDB_m40    number(10)    not null,
	tourDB_f20    number(10)    not null,
	tourDB_f30    number(10)    not null,
	tourDB_f40    number(10)    not null
);

insert into tourDB values (1, '프랑스', 0,0,0,0,0,0);
insert into tourDB values (2, '하와이', 0,0,0,0,0,0);
insert into tourDB values (3, '호주', 0,0,0,0,0,0);
insert into tourDB values (4, '미국', 0,0,0,0,0,0);
insert into tourDB values (5, '스위스', 0,0,0,0,0,0);
insert into tourDB values (6, '이탈리아', 0,0,0,0,0,0);
insert into tourDB values (7, '일본', 0,0,0,0,0,0);
insert into tourDB values (8, '영국', 0,0,0,0,0,0);
insert into tourDB values (9, '홍콩', 0,0,0,0,0,0);
insert into tourDB values (10, '뉴질랜드', 0,0,0,0,0,0);

select * from tourDB;

여행지1DB (대륙)
create table tour1DB (
	tourDB_id1	varchar2(100) primary key not null
);

select * from tour1DB;
drop table tour1DB;

여행지2DB (나라)
create table tour2DB (
	tourDB_id2	varchar2(100) primary key not null
);

나이 들었을 때 DB
create table oldDB (
	oldDB_id	number(10) 	  primary key not null,
	oldDB_value	varchar2(100) not null
);

select * from oldDB;
alter table oldDB modify (oldDB_age20 number(10));
alter table oldDB add (oldDB_age30 number(10) not null);
alter table oldDB add (oldDB_age40 number(10) not null);

insert into oldDB values (1, '늦게까지 노는게 피곤하고 다음날이 걱정될 때', 0,0,0);
insert into oldDB values (2, '교복입은 학생들이 예뻐보일 때', 0,0,0);
insert into oldDB values (3, '주변 사람들이 하나 둘 결혼할 때', 0,0,0);
insert into oldDB values (4, '새롭게 도전하는 것이 망설여질 때', 0,0,0);
insert into oldDB values (5, '군인을 아저씨라고 부르기 민망할 때', 0,0,0);
insert into oldDB values (6, '건강식품, 건강 프로그램은 찾게될 때', 0,0,0);
insert into oldDB values (7, '요즘 개그가 이해가 안될 때', 0,0,0);
insert into oldDB values (8, '낯선 사람이랑 얘기하는게 어색하지 않을 때', 0,0,0);
insert into oldDB values (9, '물건값을 깎는 것에 집착할 때', 0,0,0);

update oldDB set oldDB_value = '건강식품, 건강 프로그램을 찾게될 때' where oldDB_id=6;

소셜네트워크 컨텐츠 DB
create table mobileDB (
	mobileDB_id	number(10) primary key not null,
	mobileDB_value	varchar2(100) not null
);

select * from mobileDB;
alter table mobileDB add (mobileDB_people number(10) not null);
alter table mobileDB add (mobileDB_percent number(10) not null);
alter table mobileDB delete (mobileDB_person);
alter table mobileDB rename column mobileDB_person to mobileDB_people;


