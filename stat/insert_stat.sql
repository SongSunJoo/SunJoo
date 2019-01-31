drop table statUser;

select * from ageDB;
insert into ageDB values('man', 79.7);
insert into ageDB values('woman', 85.7);

commit;

select * from petDB;
insert into petDB values(1, '개', 64);
insert into petDB values(2, '고양이', 9);
insert into petDB values(3, '새', 1);
insert into petDB values(4, '토끼', 1);
insert into petDB values(5, '햄스터', 1);
insert into petDB values(6, '물고기', 0.4);
insert into petDB values(7, '거북이', 0.2);
insert into petDB values(8, '기타', 1);
insert into petDB values(9, '키우고 싶은/좋아하는 동물 없음', 23);
commit;

select * from lastNameDB;
insert into lastNameDB values(1, '김', 10689959, 21.51);
insert into lastNameDB values(2, '이', 7306828, 14.7);
insert into lastNameDB values(3, '박', 4192074, 8.43);
insert into lastNameDB values(4, '최', 2333927, 4.7);
insert into lastNameDB values(5, '정', 2151879, 4.33);
insert into lastNameDB values(6, '강', 1176847, 2.37);
insert into lastNameDB values(7, '조', 1055567, 2.12);
insert into lastNameDB values(8, '윤', 1020547, 2.05);
insert into lastNameDB values(9, '장', 992721, 2);
insert into lastNameDB values(10, '임', 823921, 1.66);
insert into lastNameDB values(11, '오', 763281, 1.54);
insert into lastNameDB values(12, '한', 773404, 1.56);
insert into lastNameDB values(13, '신', 741081, 1.49);
insert into lastNameDB values(14, '추', 60483, 0.12);
insert into lastNameDB values(15, '권', 705941, 1.42);
insert into lastNameDB values(16, '황', 697171, 1.4);
insert into lastNameDB values(17, '안', 685639, 1.38);
insert into lastNameDB values(18, '송', 683494, 1.38);
insert into lastNameDB values(19, '유', 559110, 1.12);
insert into lastNameDB values(20, '홍', 558853, 1.12);
insert into lastNameDB values(21, '기타', 16592809, 33.38);
commit;

select * from transferDB;
insert into transferDB values(1, '확실하다 난 날아오를테다', 3);
insert into transferDB values(2, '날진 못해도 적당하게', 26.5);
insert into transferDB values(3, '그냥 그렇다', 38.8);
insert into transferDB values(4, '다음 생을 기약한다', 16.2);
insert into transferDB values(5, '모르겠다', 15.5);
commit;

select * from digitalDB;
insert into digitalDB values(1, '스마트폰을 한다', 71.9);
insert into digitalDB values(2, 'TV를 본다', 59.1);
insert into digitalDB values(3, '컴퓨터를 한다', 47.4);
insert into digitalDB values(4, '게임을 한다', 27.6);
insert into digitalDB values(5, '영화를 본다', 26.9);
insert into digitalDB values(6, '집(방) 청소', 22.5);
insert into digitalDB values(7, '운동을 한다', 22.5);
insert into digitalDB values(8, '친구나 지인을 만난다', 20.6);
insert into digitalDB values(9, '(낮)잠을 잔다', 20.2);
insert into digitalDB values(10, '책을 읽는다', 20.2);
commit;

select * from happyDB;
insert into happyDB values(1, '아주 행복하다');
insert into happyDB values(2, '대체로 행복하다');
insert into happyDB values(3, '보통이다');
insert into happyDB values(4, '대체로 불행하다');
insert into happyDB values(5, '아주 불행하다');
COMMIT;

insert into happyData values(10, 72.4)
insert into happyData values(20, 73.1)
insert into happyData values(30, 71.9)
insert into happyData values(40, 71.5)
insert into happyData values(50, 70.2)
insert into happyData values(60, 67.3)
insert into happyData values(70, 62)

select * from tourDB;
insert into tourDB values(1, '프랑스', 37.7, 29.5, 25.6, 44.2, 39.5, 31.7);
insert into tourDB values(2, '하와이', 22.3, 30.9, 31.3, 25.8, 36.1, 33);
insert into tourDB values(3, '호주', 19.2, 30.2, 37.4, 16.7, 32.7, 34.8);
insert into tourDB values(4, '미국', 36.2, 24.8, 27.8, 31.7, 23.1, 26.4);
insert into tourDB values(5, '스위스', 16.9, 27.5, 29.1, 31.7, 27.9, 23.3);
insert into tourDB values(6, '이탈리아', 22.3, 30.9, 31.3, 25.8, 36.1, 33);
insert into tourDB values(7, '일본', 36.2, 27.5, 26, 14.2, 19.7, 23.3);
insert into tourDB values(8, '영국', 23.1, 12.8, 14.1, 32.5, 24.5, 16.7);
insert into tourDB values(9, '홍콩/마카오', 22.3, 18.1, 25, 25, 17.7, 19.4);
insert into tourDB values(10, '뉴질랜드', 16.2, 14.8, 17.6, 20.8, 18.4, 21.6);

commit;

select * from oldDB;
insert into oldDB values(1, '늦게까지 노는게 피곤하고 다음날 걱정될 때', 23.4, 23.5, 28.8);
insert into oldDB values(2, '교복입은 학생들이 예뻐보일 때', 23.2, 5.1, 2.1);
insert into oldDB values(3, '주변 사람들이 하나 둘 결혼할 때', 18.8, 27.3, 4.7);
insert into oldDB values(4, '새롭게 도전하는것이 망설여질 때', 14.5, 20.5, 34);
insert into oldDB values(5, '군인을 아저씨라고 부르기 민망할 때', 7.9, 2, 1);
insert into oldDB values(6, '건강식품, 건강 프로그램 찾게 될 때', 3.6, 9.2, 17.3);
insert into oldDB values(7, '요즘 개그가 이해가 안될 때', 3, 2.4, 3.1);
insert into oldDB values(8, '낯선 사람이랑 얘기하는게 어색하지 않을 때', 3, 35.5, 6.3);
insert into oldDB values(9, '물건값을 깎는 것에 집착할 때', 2.6, 4.4, 2.6);
commit;

select * from mobileDB;
insert into mobileDB values(1, '밴드', 14100000, 23);
insert into mobileDB values(2, '인스타그램', 9100000, 15);
insert into mobileDB values(3, '페이스북', 8800000, 14);
insert into mobileDB values(4, '카카오스토리', 7750000, 12);
insert into mobileDB values(5, '네이버 카페', 7120000, 11);
insert into mobileDB values(6, '다음 카페', 4440000, 7);
insert into mobileDB values(7, '네이버 블로그', 4020000, 6);
insert into mobileDB values(8, '트위터', 2650000, 4);
insert into mobileDB values(9, '에브리타임', 2330000, 4);
insert into mobileDB values(10, '캐시슬라이드', 1950000, 3);
commit;