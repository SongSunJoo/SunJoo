여론조사
create table poll(
	poll_id number(10) primary key,
	poll_title varchar2(100) not null,
	poll_cont1 varchar2(100) not null,
	poll_cont2 varchar2(100) not null,
	poll_cont1_cnt number default 0,
	poll_cont2_cnt number default 0,
	poll_startDate Date not null,
	poll_endDate Date not null
);

drop table poll;
update poll set poll_cont1_cnt = 0 , poll_cont2_cnt = 0; -- 테이블에 숫자(number는 null로 주면 안된다)
select * from poll;
select * from poll where poll_endDate > sysdate;
insert into poll values(1, '찍먹? 부먹?', '찍먹', '부먹', 0, 0, sysdate, sysdate+7);
insert into poll values(2, '보검? 동완?', '보검', '동완', 0, 0, sysdate, sysdate+7);
insert into poll values(3, '연석? 타타?', '연석', '타타', 0, 0, sysdate, sysdate+7);
insert into poll values(4, '물? 환타?', '물', '환타', 0, 0, sysdate, sysdate-5);