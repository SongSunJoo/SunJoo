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
	id varchar2(100) not null references kmember(id),
	p_auct_id number(10) not null references auction_result(p_auct_id),
	p_class_id1 varchar2(100) not null references productClass1(p_class_id1),
	p_class_id2 varchar2(100) not null references productClass2(p_class_id2)
);

게시판
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

공지사항
create table knotice (
	n_num number(10) primary key,
	m_id  varchar2(100) references kmanager(m_id),
	n_subject varchar2(100) not null,
	n_content varchar2(100) not null,
	n_reg_date date not null
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

관리자
create table kmanager (
	m_id varchar2(100) primary key,
	m_pass varchar2(100) not null
);

경매
create table auction (
	a_num number(10) primary key,
	a_price number(30) not null,
	a_reg_date date not null,
	p_num number(10) not null references product(p_num),
	a_id varchar2(100) not null
);

경매 결과
create table auction_result (
	p_auct_id number(10) primary key,
	p_auct_desc varchar2(100) not null
);

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
	p_class_id1 varchar2(100) not null references productClass1(p_class_id1)
);

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


