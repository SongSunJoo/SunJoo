package com.ch.stat.service;

import com.ch.stat.model.StatUser;

public interface StatUserService {

	int insert(StatUser su);
	
	int updateHeight(int user_id, int height_value);

	int updateName(int user_id, String lastName);

	int selectUserId();

	int updateAnimal(int user_id, String pet_value);

	int updateTour(int user_id, int tourDB_id1);

	int updateOld(int user_id, int oldDB_id);

	int updateDigital(int user_id, int digitalDB_id);

	int updateMobile(int user_id, int mobileDB_id);

	int updateTrans(int user_id, int transDB_id);

	int updateHappy(int user_id, int happyDB_id);

	int updateNick(int user_id, String user_nick);

	StatUser selectAll(int user_id);
}
