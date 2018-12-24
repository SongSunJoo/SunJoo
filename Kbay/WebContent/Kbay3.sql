create table knotice (
	n_num number(10) primary key,
	m_id  varchar2(100) references kmanager(m_id),
	n_subject varchar2(100) not null,
	n_content varchar2(100) not null,
	n_reg_date date not null
);

select * from knotice;

insert into knotice 
values (12, 'admin', 'Kbay 사이트 오픈12', 'Kbay 사이트를 새롭게 오픈하였습니다', sysdate, 'kbay2018');
insert into knotice 
values (13, 'admin', 'Kbay 사이트 오픈13', 'Kbay 사이트를 새롭게 오픈하였습니다', sysdate, 'kbay2018');
insert into knotice 
values (2, 'admin', 'Kbay 사이트 오픈2', 'Kbay 사이트를 새롭게 오픈하였습니다', sysdate, 'kbay2018');
insert into knotice 
values (3, 'admin', 'Kbay 사이트 오픈3', 'Kbay 사이트를 새롭게 오픈하였습니다', sysdate, 'kbay2018');
insert into knotice 
values (4, 'admin', 'Kbay 사이트 오픈4', 'Kbay 사이트를 새롭게 오픈하였습니다', sysdate, 'kbay2018');
insert into knotice 
values (5, 'admin', 'Kbay 사이트 오픈5', 'Kbay 사이트를 새롭게 오픈하였습니다', sysdate, 'kbay2018');
insert into knotice 
values (6, 'admin', 'Kbay 사이트 오픈6', 'Kbay 사이트를 새롭게 오픈하였습니다', sysdate, 'kbay2018');
insert into knotice 
values (7, 'admin', 'Kbay 사이트 오픈7', 'Kbay 사이트를 새롭게 오픈하였습니다', sysdate, 'kbay2018');
insert into knotice 
values (8, 'admin', 'Kbay 사이트 오픈8', 'Kbay 사이트를 새롭게 오픈하였습니다', sysdate, 'kbay2018');
insert into knotice 
values (9, 'admin', 'Kbay 사이트 오픈9', 'Kbay 사이트를 새롭게 오픈하였습니다', sysdate, 'kbay2018');
insert into knotice 
values (10, 'admin', 'Kbay 사이트 오픈10', 'Kbay 사이트를 새롭게 오픈하였습니다', sysdate, 'kbay2018');
insert into knotice 
values (11, 'admin', 'Kbay 사이트 오픈11', 'Kbay 사이트를 새롭게 오픈하였습니다', sysdate, 'kbay2018');

select * from knotice;

select * from (select a.*, rowNum rn from (
			select * from knotice order by n_reg_date desc) a)
			where rn between 1 and 10;

alter table knotice drop column m_pass;
			
select * from product order by p_num desc;

select * from kmember;
alter table kmember rename column buy_panalty to buy_penalty;

insert into kmember values ('admin', '1', 'admin', '010-123-4567', 'admin@naver.com', '서울', 0, 0, sysdate, 'n');


select * from member;


create table member (
	id varchar2(20) primary key,
	pass varchar2(20) not null,
	name varchar2(20) not null,
	birth date not null,
	email varchar2(20) not null,
	phone varchar2(20) not null,
	zipcode varchar2(20) not null,
	addr1 varchar2(20) not null,
	addr2 varchar2(20) not null,
	joindate date not null
);

insert into member values ('a1', '1','a1',sysdate,'a1@naver.com','010-123-4567',
  '16424','서울','강남',sysdate);
  
  
select * from kboard;

create table kboard (
	b_num number(10) primary key,
	b_subject varchar2(200) not null,
	b_content varchar2(200) not null,
	b_readcount number(10) not null, 
	b_ref number(10) not null, 
	b_re_step number(10) not null, 
	b_re_level number(10) not null, 
	b_reg_date date not null,
	b_pass varchar2(20) not null,
	id varchar2(20) not null	
);

insert into kboard values (
	1,'게시글1', '게시판을 오픈하였습니다', 0, 1, 0, 0, sysdate, 'admin', '1'
);

drop table kboard;

select * from kboard;

select * from product;
select * from auction;

select * from product where p_end_date <= sysdate;

select * from (select a.*, rowNum rn from 
	(select a_id from auction where p_num=6 order by a_reg_date desc) a) 
	where rn=1;

create table product_old as select * from product;
select * from product_old;
drop table product;
select * from product;

select * from auction;
drop table auction;

select * from kboard;

delete from product where p_num=55;
select * from AUCTION where p_num=99;

delete from AUCTION where p_num=55;

select * from (select a.*, rowNum rn from (select * from product where p_buyer = 'kbay2018' and p_auct_id = 1) a)
		where  rn between 1 and 10;

select * from product where p_name like '드론%';

update PRODUCT set p_auct_id = 1 where p_num=99;

select * from product where p_num=99;

select * from AUCTION_RESULT;
select * from KBOARD;

select * from product where p_end_date < (sysdate-1) and p_auct_id=0;
select * from product where to_date(p_end_date,'yyyy-mm-dd') < to_date(sysdate,'yyyy-mm-dd') and p_auct_id = 0;
select to_date(p_end_date, 'yyyy-mm-dd') from PRODUCT;

select * from product where p_buyer='kbay2018';
select * from product order by p_end_date desc;

select * from auction where p_num = 112;

update product set p_auct_id = 1 where p_buyer = 'kbay2018';
update product set p_buyer = 'kbay2018' where p_image = 'E23.jpg';
select * from product where p_buyer = 'kbay2018';






