create table kmanager (
	m_id varchar2(100) primary key,
	m_pass varchar2(100) not null
);

desc kmanager;
insert into kmanager values('admin', 'kbay2018');
select * from kmanager;

create table notice (
	n_num number(10) primary key,
	m_id  varchar2(100) references kmanager(m_id),
	n_subject varchar2(100) not null,
	n_content varchar2(100) not null,
	n_reg_date date not null
);


delete table board;
desc board;
select * from board;

create table kboard (
	b_num number(10) primary key,
	b_subject varchar2(100) not null,
	b_content varchar2(255) not null,
	b_readcount number(10)  not null,
	b_ref number(10) not null,
	b_re_step number(10) not null,
	b_re_level number(10) not null,
	b_reg_date date not null,
	id number(10) references kmember(id) 
);

select * from kboard;

delete table notice;
drop table  notice;

desc notice;
select * from notice;