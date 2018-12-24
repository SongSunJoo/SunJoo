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

select * from kmember;
insert into kmember values ('kbay2018', 'kbay2018', 'kbay', '010-2018-2018', 'kbay@kbay.com', 'seoul', null, null, sysdate, 'n');

--------------------------------------------------------


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

select * from productClass2;


--------------------------------------------------------


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

select * from product;
delete product where p_num=11;
insert into product values(10, '그릇', '중고', 30000, sysdate, sysdate+14 ,null, 'Collections_c1.png', 'kbay2018', 0, 'E', 'E2');
insert into product values(11, '칼', '중고', 50000, sysdate, sysdate+14 ,null, 'Collections_c2.png', 'kbay2018', 0, 'E', 'E2');
insert into product values(12, '목걸이', '중고', 15000, sysdate, sysdate+14 ,null, 'Collections_c3.png', 'kbay2018', 0, 'E', 'E2');
insert into product values(13, '청동상', '중고', 10000, sysdate, sysdate+14 ,null, 'Collections_c4.png', 'kbay2018', 0, 'E', 'E2');
insert into product values(14, '고미술품', '중고', 100000, sysdate, sysdate+14 ,null, 'Collections_c5.png', 'kbay2018', 0, 'E', 'E2');

insert into product values(15, '피규어', '중고', 30000, sysdate, sysdate+14 ,null, 'Collections_f1.png', 'kbay2018', 0, 'E', 'E1');
insert into product values(16, '미니마우스', '중고', 50000, sysdate, sysdate+14 ,null, 'Collections_f2.png', 'kbay2018', 0, 'E', 'E1');
insert into product values(17, '스타워즈', '중고', 15000, sysdate, sysdate+14 ,null, 'Collections_f3.png', 'kbay2018', 0, 'E', 'E1');
insert into product values(18, '나무', '중고', 10000, sysdate, sysdate+14 ,null, 'Collections_f4.png', 'kbay2018', 0, 'E', 'E1');
insert into product values(19, '아이언맨', '중고', 100000, sysdate, sysdate+14 ,null, 'Collections_f5.png', 'kbay2018', 0, 'E', 'E1');

insert into product values(20, '반도체', '새물건', 500000, sysdate, sysdate+14 ,null, 'Electronics_l1.jpg', 'kbay2018', 0, 'B', 'B2');
insert into product values(21, '삼성 노트북', '중고', 45000, sysdate, sysdate+14 ,null, 'Electronics_l2.jpg', 'kbay2018', 0, 'B', 'B2');
insert into product values(22, '애플 노트북', '중고', 60000, sysdate, sysdate+14 ,null, 'Electronics_l3.jpg', 'kbay2018', 0, 'B', 'B2');
insert into product values(23, 'MS 노트북', '중고', 150000, sysdate, sysdate+14 ,null, 'Electronics_l4.jpg', 'kbay2018', 0, 'B', 'B2');
insert into product values(24, 'LG 노트북', '새물건', 1000000, sysdate, sysdate+14 ,null, 'Electronics_l5.jpg', 'kbay2018', 0, 'B', 'B2');

insert into product values(25, '애플와치', '중고', 500000, sysdate, sysdate+14 ,null, 'Electronics_m1.jpg', 'kbay2018', 0, 'B', 'B3');
insert into product values(26, '삼성 갤럭시노트', '새물건', 450000, sysdate, sysdate+14 ,null, 'Electronics_m2.jpg', 'kbay2018', 0, 'B', 'B3');
insert into product values(27, '아이폰', '중고', 600000, sysdate, sysdate+14 ,null, 'Electronics_m3.jpg', 'kbay2018', 0, 'B', 'B3');
insert into product values(28, '삼성 갤럭시S', '새물건', 750000, sysdate, sysdate+14 ,null, 'Electronics_m4.jpg', 'kbay2018', 0, 'B', 'B3');
insert into product values(29, '아이폰 X', '중고', 6000000, sysdate, sysdate+14 ,null, 'Electronics_m5.jpg', 'kbay2018', 0, 'B', 'B3');

insert into product values(30, '삼성 TV', '중고', 500000, sysdate, sysdate+14 ,null, 'Electronics_t1.jpg', 'kbay2018', 0, 'B', 'B1');
insert into product values(31, 'LG TV', '새물건', 450000, sysdate, sysdate+14 ,null, 'Electronics_t2.jpg', 'kbay2018', 0, 'B', 'B1');
insert into product values(32, '후지필름 카메라 렌즈 미포함', '중고', 300000, sysdate, sysdate+14 ,null, 'Electronics_t3.jpg', 'kbay2018', 0, 'B', 'B1');
insert into product values(33, '후지필름 카메라 LX5', '새물건', 750000, sysdate, sysdate+14 ,null, 'Electronics_t4.jpg', 'kbay2018', 0, 'B', 'B1');
insert into product values(34, '오디오', '중고', 60000, sysdate, sysdate+14 ,null, 'Electronics_t5.jpg', 'kbay2018', 0, 'B', 'B1');

insert into product values(35, '검정퀼팅부츠', '새물건', 60000, sysdate, sysdate+14 ,null, 'Fashion_a1.jpg', 'kbay2018', 0, 'A', 'A3');
insert into product values(36, '털신발', '새물건', 6000, sysdate, sysdate+14 ,null, 'Fashion_a2.jpg', 'kbay2018', 0, 'A', 'A3');
insert into product values(37, '여자가죽부츠', '중고', 10000, sysdate, sysdate+14 ,null, 'Fashion_a3.jpg', 'kbay2018', 0, 'A', 'A3');
insert into product values(38, '나이키 블랙 오렌지 에디션', '중고', 600000, sysdate, sysdate+14 ,null, 'Fashion_a4.jpg', 'kbay2018', 0, 'A', 'A3');
insert into product values(39, '나이키 에어 화이트', '중고', 100000, sysdate, sysdate+14 ,null, 'Fashion_a5.jpg', 'kbay2018', 0, 'A', 'A3');

insert into product values(40, '빨간 후드티', '새물건', 15000, sysdate, sysdate+14 ,null, 'Fashion_m1.jpg', 'kbay2018', 0, 'A', 'A1');
insert into product values(41, '경량패딩', '새물건', 20000, sysdate, sysdate+14 ,null, 'Fashion_m2.jpg', 'kbay2018', 0, 'A', 'A1');
insert into product values(42, '리바이스 청자켓', '중고', 10000, sysdate, sysdate+14 ,null, 'Fashion_m3.jpg', 'kbay2018', 0, 'A', 'A1');
insert into product values(43, '가죽자켓', '중고', 30000, sysdate, sysdate+14 ,null, 'Fashion_m4.jpg', 'kbay2018', 0, 'A', 'A1');
insert into product values(44, '정장 자켓', '중고', 10000, sysdate, sysdate+14 ,null, 'Fashion_m5.jpg', 'kbay2018', 0, 'A', 'A1');

insert into product values(45, '검정 원피스', '중고', 15000, sysdate, sysdate+14 ,null, 'Fashion_w1.jpg', 'kbay2018', 0, 'A', 'A2');
insert into product values(46, '흰색원피스', '중고', 20000, sysdate, sysdate+14 ,null, 'Fashion_w2.jpg', 'kbay2018', 0, 'A', 'A2');
insert into product values(47, '꽃무늬 치마', '중고', 10000, sysdate, sysdate+14 ,null, 'Fashion_w3.jpg', 'kbay2018', 0, 'A', 'A2');
insert into product values(48, '노란색 남방', '중고', 3000, sysdate, sysdate+14 ,null, 'Fashion_w4.jpg', 'kbay2018', 0, 'A', 'A2');
insert into product values(49, '원피스', '새물건', 10000, sysdate, sysdate+14 ,null, 'Fashion_w5.jpg', 'kbay2018', 0, 'A', 'A2');

insert into product values(50, '안대', '새물건', 5000, sysdate, sysdate+14 ,null, 'Health & Beauty_h1.png', 'kbay2018', 0, 'D', 'D2');
insert into product values(51, '안마기', '새물건', 20000, sysdate, sysdate+14 ,null, 'Health & Beauty_h2.png', 'kbay2018', 0, 'D', 'D2');
insert into product values(52, '눈안마기', '중고', 10000, sysdate, sysdate+14 ,null, 'Health & Beauty_h3.png', 'kbay2018', 0, 'D', 'D2');
insert into product values(53, '비타민', '새물건', 30000, sysdate, sysdate+14 ,null, 'Health & Beauty_h4.png', 'kbay2018', 0, 'D', 'D2');
insert into product values(54, '영양제', '새물건', 10000, sysdate, sysdate+14 ,null, 'Health & Beauty_h5.png', 'kbay2018', 0, 'D', 'D2');

insert into product values(55, '쉐도우 10색', '새물건', 5000, sysdate, sysdate+14 ,null, 'Health & Beauty_m1.png', 'kbay2018', 0, 'D', 'D1');
insert into product values(56, '쉐도우 팔레트 366', '새물건', 20000, sysdate, sysdate+14 ,null, 'Health & Beauty_m2.png', 'kbay2018', 0, 'D', 'D1');
insert into product values(57, '립글로즈', '새물건', 10000, sysdate, sysdate+14 ,null, 'Health & Beauty_m3.png', 'kbay2018', 0, 'D', 'D1');
insert into product values(58, '하이라이트', '새물건', 30000, sysdate, sysdate+14 ,null, 'Health & Beauty_m4.png', 'kbay2018', 0, 'D', 'D1');
insert into product values(59, '아이라이너', '새물건', 10000, sysdate, sysdate+14 ,null, 'Health & Beauty_m5.png', 'kbay2018', 0, 'D', 'D1');

insert into product values(60, '낚시 모자', '새물건', 5000, sysdate, sysdate+14 ,null, 'Sports_c1.jpg', 'kbay2018', 0, 'C', 'C2');
insert into product values(61, '강아지 캠핑 텐트', '새물건', 20000, sysdate, sysdate+14 ,null, 'Sports_c2.jpg', 'kbay2018', 0, 'C', 'C2');
insert into product values(62, '캠핑텐트 4인용', '새물건', 10000, sysdate, sysdate+14 ,null, 'Sports_c3.jpg', 'kbay2018', 0, 'C', 'C2');
insert into product values(63, '캠핑 세트 ', '중고', 30000, sysdate, sysdate+14 ,null, 'Sports_c4.jpg', 'kbay2018', 0, 'C', 'C2');
insert into product values(64, '빈티지 캠핑 세트', '중고', 10000, sysdate, sysdate+14 ,null, 'Sports_c5.jpg', 'kbay2018', 0, 'C', 'C2');

insert into product values(65, '검정 자전거', '중고', 50000, sysdate, sysdate+14 ,null, 'Sports_f1.jpg', 'kbay2018', 0, 'C', 'C1');
insert into product values(66, '성인용 자전거', '중고', 20000, sysdate, sysdate+14 ,null, 'Sports_f2.jpg', 'kbay2018', 0, 'C', 'C1');
insert into product values(67, '테니스 라켓', '새물건', 10000, sysdate, sysdate+14 ,null, 'Sports_f3.jpg', 'kbay2018', 0, 'C', 'C1');
insert into product values(68, '스케이팅 신발 ', '중고', 30000, sysdate, sysdate+14 ,null, 'Sports_f4.jpg', 'kbay2018', 0, 'C', 'C1');
insert into product values(69, '골프 세트', '중고', 10000, sysdate, sysdate+14 ,null, 'Sports_f5.jpg', 'kbay2018', 0, 'C', 'C1');

--------------------------------------------------------2018.11.27

insert into product values(80, '흑백니트', '중고', 10000, sysdate, sysdate, null, 'A11.jpg', 'kbay2018', 0, 'A', 'A1');
insert into product values(81, '청바지', '중고', 10000, sysdate, sysdate+1, null, 'A12.jpg', 'kbay2018', 0, 'A', 'A1');
insert into product values(82, '반팔 카라티', '새물건', 10000, sysdate, sysdate+2, null, 'A13.jpg', 'kbay2018', 0, 'A', 'A1');
insert into product values(83, '메탈리카 티셔츠', '중고', 10000, sysdate, sysdate+3, null, 'A14.jpg', 'kbay2018', 0, 'A', 'A1');
insert into product values(84, '정장 자켓', '중고', 10000, sysdate, sysdate, null, 'A21.jpg', 'kbay2018', 0, 'A', 'A2');
insert into product values(85, '배색 남방', '중고', 10000, sysdate, sysdate+1, null, 'A22.jpg', 'kbay2018', 0, 'A', 'A2');
insert into product values(86, '원피스 티셔츠', '새물건', 10000, sysdate, sysdate+2, null, 'A23.jpg', 'kbay2018', 0, 'A', 'A2');
insert into product values(87, '여자 패딩', '새물건', 10000, sysdate, sysdate+3, null, 'A24.jpg', 'kbay2018', 0, 'A', 'A2');
insert into product values(88, '티셔츠', '중고', 10000, sysdate, sysdate+3, null, 'A25.jpg', 'kbay2018', 0, 'A', 'A2');
insert into product values(89, '파란 운동화', '중고', 10000, sysdate, sysdate, null, 'A31.jpg', 'kbay2018', 0, 'A', 'A3');
insert into product values(90, '시계', '중고', 10000, sysdate, sysdate+1, null, 'A32.jpg', 'kbay2018', 0, 'A', 'A3');
insert into product values(91, '반지', '새물건', 10000, sysdate, sysdate+2, null, 'A33.jpg', 'kbay2018', 0, 'A', 'A3');
insert into product values(92, '아디다스 운동화', '새물건', 10000, sysdate, sysdate+3, null, 'A34.jpg', 'kbay2018', 0, 'A', 'A3');

insert into product values(93, '날개 뱃찌', '중고', 10000, sysdate, sysdate+3, null, 'A35.jpg', 'kbay2018', 0, 'A', 'A3');
insert into product values(94, '드론', '중고', 10000, sysdate, sysdate+3, null, 'B11.jpg', 'kbay2018', 0, 'B', 'B1');
insert into product values(95, '캐논 DSLR', '중고', 10000, sysdate, sysdate+1, null, 'B12.jpg', 'kbay2018', 0, 'B', 'B1');
insert into product values(96, '액션캠', '새물건', 10000, sysdate, sysdate, null, 'B13.jpg', 'kbay2018', 0, 'B', 'B1');
insert into product values(97, '검정 드론 SLIR', '중고', 10000, sysdate, sysdate+2, null, 'B14.jpg', 'kbay2018', 0, 'B', 'B1');
insert into product values(98, '흰색 드론 SLRIDLC', '중고', 10000, sysdate, sysdate+4, null, 'B15.jpg', 'kbay2018', 0, 'B', 'B1');
insert into product values(99, '드론 ORKXLE', '중고', 10000, sysdate, sysdate, null, 'B16.jpg', 'kbay2018', 0, 'B', 'B1');
insert into product values(100, '아이폰', '중고', 10000, sysdate, sysdate+3, null, 'B31.jpg', 'kbay2018', 0, 'B', 'B3');
insert into product values(101, '수능 핸드폰', '중고', 10000, sysdate, sysdate+1, null, 'B32.jpg', 'kbay2018', 0, 'B', 'B3');
insert into product values(102, '화웨이 폰', '새물건', 10000, sysdate, sysdate, null, 'B33.jpg', 'kbay2018', 0, 'B', 'B3');
insert into product values(103, '갤럭시 SLIR', '중고', 10000, sysdate, sysdate+2, null, 'B34.jpg', 'kbay2018', 0, 'B', 'B3');
insert into product values(104, '아이폰 XS', '중고', 10000, sysdate, sysdate+4, null, 'B35.jpg', 'kbay2018', 0, 'B', 'B3');
insert into product values(105, '쿠션', '중고', 10000, sysdate, sysdate+4, null, 'C21.jpg', 'kbay2018', 0, 'C', 'C2');
insert into product values(106, '형광 쿠션', '중고', 10000, sysdate, sysdate, null, 'C22.jpg', 'kbay2018', 0, 'C', 'C2');
insert into product values(107, '시계', '중고', 10000, sysdate, sysdate, null, 'C23.jpg', 'kbay2018', 0, 'C', 'C2');
insert into product values(108, '화분', '새물건', 10000, sysdate, sysdate+1, null, 'C24.jpg', 'kbay2018', 0, 'C', 'C2');
insert into product values(109, '네일 드라이기', '새물건', 10000, sysdate, sysdate, null, 'D11.jpg', 'kbay2018', 0, 'D', 'D1');
insert into product values(110, '중국 도자기', '중고', 10000, sysdate, sysdate, null, 'E21.jpg', 'kbay2018', 0, 'E', 'E2');
insert into product values(111, '인어 공주 동상', '중고', 10000, sysdate, sysdate+1, null, 'E22.jpg', 'kbay2018', 0, 'E', 'E2');
insert into product values(112, '중국 접시', '중고', 10000, sysdate, sysdate+3, null, 'E23.jpg', 'kbay2018', 0, 'E', 'E2');
insert into product values(113, '옥 구슬', '중고', 50000, sysdate, sysdate+2, null, 'E24.jpg', 'kbay2018', 0, 'E', 'E2');
insert into product values(114, '용무늬 팬던트', '중고', 10000, sysdate, sysdate, null, 'E25.jpg', 'kbay2018', 0, 'E', 'E2');
insert into product values(115, '롤렉스 시계', '중고', 10000, sysdate, sysdate+3, null, 'A36.jpg', 'kbay2018', 0, 'A', 'A3');

--------------------------------------------------------

경매
create table auction (
	a_num number(10) primary key,
	a_price number(30) not null,
	a_reg_date date not null,
	p_num number(10) not null references product(p_num),
	a_id varchar2(100) not null
);

select * from auction;
insert into product values(10, 30000, sysdate, 10, null);


경매 결과
create table auction_result (
	p_auct_id number(10) primary key,
	p_auct_desc varchar2(100) not null
);

select * from auction_result;
insert into auction_result values(0, '경매중');
insert into auction_result values(1, '판매종료');
insert into auction_result values(2, '판매취소');
insert into auction_result values(3, '구매취소');
insert into auction_result values(4, '유찰');

--------------------------------------------------------


관리자
create table kmanager (
	m_id varchar2(100) primary key,
	m_pass varchar2(100) not null
);

select * from kmanager;


공지사항
create table knotice (
	n_num number(10) primary key,
	m_id  varchar2(100) references kmanager(m_id),
	n_subject varchar2(100) not null,
	n_content varchar2(100) not null,
	n_reg_date date not null
);

select * from knotice;


--------------------------------------------------------


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

drop table kboard;
select * from kboard;



