create table kmember (
	id varchar2(100) primary key,
	pass varchar2(100) not null,
	name varchar2(100) not null,
	tel	varchar2(100) not null,
	email varchar2(100) not null,
	addr varchar2(100) not null,
	sell_panalty number(20),
	buy_panalty	number(20),
	reg_date date not null,
	del	varchar2(100) not null
);

select * from kmember;