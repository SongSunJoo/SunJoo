상품
create table product (
	p_num number(10) primary key not null,
	p_name varchar2(100) not null,
	p_used varchar2(100) not null,
	p_price number(30) not null,
	p_start_date date not null,
	p_end_date date not null,
	p_buyer varchar2(100),
	p_image varchar2(100) not null,
	id varchar2(100) not null constraint product_id_fk references kmember(id),
	p_auct_id number(10) not null constraint product_p_auct_id_fk references auction_result(p_auct_id),
	p_class_id1 varchar2(100) not null constraint product_p_class_id1_fk references productClass1(p_class_id1),
	p_class_id2 varchar2(100) not null constraint product_p_class_id2_fk references productClass2(p_class_id2)
);

회원
create table kmember (
	id		varchar2(100) primary key not null,	
	pass	varchar2(100) not null,
	name	varchar2(100) not null,
	tel		varchar2(100)	not null,
	email	varchar2(100) not null,
	addr	varchar2(100)	not null,
	sell_panalty	number(20),
	buy_panalty		number(20),
	reg_date		date not null,
	del		varchar2(100) not null
);

insert into kmember values ('admin', '1', 'admin', '010-123-4567', 'admin@naver.com', '서울', 0, 0, sysdate, 'n');
select * from kmember;

select * from product where id='admin';
select distinct(p_class_id1) from product;

경매
create table auction (
	a_num number(10) primary key,
	a_price number(30) not null,
	a_reg_date date not null,
	p_num number(10) not null,
	constraint auction_p_num_fk foreign key(p_num)
		references product(p_num)
);
alter table auction add (a_id varchar2(100));

insert into auction values (0, 1000, sysdate, 1, 'admin');
insert into auction values (1, 10000, sysdate, 5, 'admin');
insert into auction values (2, 10000, sysdate, 6, 'admin');
insert into auction values (3, 15000, sysdate, 5, 'admin');
insert into auction values (4, 10000, sysdate, 9, 'admin');
insert into auction values (5, 2000,  sysdate, 1, 'test');
insert into auction values (6, 20000, sysdate, 5, 'test');
insert into auction values (7, 20000, sysdate, 6, 'test');
insert into auction values (8, 2000,  sysdate, 5, 'test');
insert into auction values (9, 20000, sysdate, 9, 'test');

delete from auction where a_num=9;

select * from auction;
select * from product;

select max(a_price) from auction where p_num=1;

select distinct(p_num), p.p_name from auction a, product p where a.a_id = 'admin' and p.p_num=a.p_num;
select p.p_num, p.p_name, p_class_id1, p_class_id2, p_price, p_used, p_start_date, p_end_date, p_auct_id
from (select distinct(p_num) from auction where a_id = 'test') list, product p where p.p_num = list.p_num;

select *
from (select distinct(p_num) from auction where a_id = 'test') list, product p where p.p_num = list.p_num;

select max(a_price) from auction where p_num=2;

제약 조건 추가
alter table auction add constraint auction_p_num_fk foreign key(p_num)
	references product(p_num);

경매 결과
create table auction_result (
	p_auct_id number(10) primary key,
	p_auct_desc varchar2(100) not null
);

insert into auction_result values (0, '경매중');
insert into auction_result values (1, '판매종료');
insert into auction_result values (2, '판매취소');
insert into auction_result values (3, '구매취소');
insert into auction_result values (4, '유찰');

select * from auction_result;

상품 대분류
create table productClass1 (
	p_class_id1 varchar2(100) primary key not null,
	p_class_name1 varchar2(100) not null
);

insert into productClass1 values ('A', 'Fashion');
insert into productClass1 values ('B', 'Electric');
insert into productClass1 values ('C', 'Sports');
insert into productClass1 values ('D', 'Health&Beauty');
insert into productClass1 values ('E', 'Collection');

select * from productClass1;
상품 소분류
create table productClass2 (
	p_class_id2 varchar2(100) primary key not null,
	p_class_name2 varchar2(100) not null,
	p_class_id1 varchar2(100) not null,
	constraint productClass2_p_class_id1_fk foreign key(p_class_id1)
		references productClass1(p_class_id1)
);

alter table productClass2 drop constraint productClass2_p_class_id1_fk;
select * from productClass2;

insert into productClass2 values ('A1', '여성의류', 'A');
insert into productClass2 values ('A2', '남성의류', 'A');
insert into productClass2 values ('A3', '액세서리', 'A');
insert into productClass2 values ('B1', '스마트홈', 'B');
insert into productClass2 values ('B2', '컴퓨터', 'B');
insert into productClass2 values ('B3', '핸드폰', 'B');
insert into productClass2 values ('C1', '피트니스', 'C');
insert into productClass2 values ('C2', '캠핑용품', 'C');
insert into productClass2 values ('D1', '코스매틱', 'D');
insert into productClass2 values ('D2', '건강식품', 'D');
insert into productClass2 values ('E1', '피규어', 'E');
insert into productClass2 values ('E2', '빈티지', 'E');

delete * from productClass2;

select * from tab;
select * from auction_result;
select * from productClass1;
select * from productClass2;
select * from product;
select * from auction;
drop table auction_result;
drop table auction;
select * from kmember;

select nvl(max(p_num), 0)+1 from product;

select * from product;
select max(p_num) from product;

desc product;

drop table product;


alter table product drop constraint product_id_fk;
alter table product drop constraint product_p_auct_id_fk;
alter table product drop constraint product_p_class_id1_fk;
alter table product drop constraint product_p_class_id2_fk;

desc table product;

select * from product;
update product set p_buyer = 'test' where p_class_id1='A';
update product set p_buyer = 'test' where p_class_id1='C';

update product set p_auct_id = 1 where p_buyer='admin';

select * from product where p_buyer='admin';

select * from product where p_num = 2;

select * from auction;

delete from auction where a_id is null;

select * from product;

select * from (select a.*, rowNum rn from 
	(select *
	 from (select distinct(au.p_num) from auction au where au.a_id='admin') list, product p 
	 where p.p_num = list.p_num) a)
where  rn between 1 and 10;

select * from (select a.*, rowNum rn from 
	(select distinct(p_num) from auction where a_id='admin') a)
where  rn between 1 and 10;

select count(*) from (select distinct(p_num) from auction where a_id='admin');

select * from (select a.*, p.*, rowNum rn from 
			(select distinct(p_num) from auction where a_id='admin') a)
		where  rn between 1 and 10;
		
select distinct(p_num) from auction where a_id='admin';
		
select a.*, rowNum rn from 
	(select p.*
	 from (select distinct(au.p_num) from auction au where au.a_id='admin') list, product p 
	 where p.p_num = list.p_num) a;

select a.*, rowNum rn from 
	(select distinct(p_num) from auction where a_id='admin') a;

select * from kmember;

select * from kmanager;

select * from (select a.*, rowNum rn from 
			( select p_num, p_name, pc1.p_class_name1, pc2.p_class_name2, pc1.p_class_id1, 
			  pc2.p_class_id2, p_price, p_used, p_start_date, p_end_date 
			  from product p, productClass1 pc1, productClass2 pc2 
			  where p.p_class_id1 = pc1.p_class_id1 and p.p_class_id2 = pc2.p_class_id2 
			        and pc1.p_class_id1='A' and pc2.p_class_id2='A1' 
			  order by p_start_date desc) a) 
		where rn between 1 and 10;

select * from knotice;

		
		
		
		
		
		
		
		