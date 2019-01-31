package com.ch.stat.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ch.stat.dao.StatUserDao;
import com.ch.stat.model.StatUser;

@Service
public class StatUserServiceImpl implements StatUserService{

	@Autowired
	private StatUserDao sud;

	@Override
	public int insert(StatUser su) {
		//System.out.println("spring test...");
		return sud.insert(su);
	}
	
	@Override
	public int updateHeight(int user_id, int height_value) {
		return sud.updateHeight(user_id, height_value);
	}

	@Override
	public int updateName(int user_id, String lastName) {
		return sud.updateName(user_id, lastName);
	}

	@Override
	public int selectUserId() {
		return sud.selectUserId();
	}

	@Override
	public int updateAnimal(int user_id, String pet_value) {
		return sud.updateAnimal(user_id, pet_value);
	}

	@Override
	public int updateTour(int user_id, int tourDB_id1) {
		return sud.updateTour(user_id, tourDB_id1);
	}

	@Override
	public int updateOld(int user_id, int oldDB_id) {
		return sud.updateOld(user_id, oldDB_id);
	}

	@Override
	public int updateDigital(int user_id, int digitalDB_id) {
		return sud.updateDigital(user_id, digitalDB_id);
	}

	@Override
	public int updateMobile(int user_id, int mobileDB_id) {
		return sud.updateMobile(user_id, mobileDB_id);
	}

	@Override
	public int updateTrans(int user_id, int transDB_id) {
		return sud.updateTrans(user_id, transDB_id);
	}

	@Override
	public int updateHappy(int user_id, int happyDB_id) {
		return sud.updateHappy(user_id, happyDB_id);
	}

	@Override
	public int updateNick(int user_id, String user_nick) {
		return sud.updateNick(user_id, user_nick);
	}

	@Override
	public StatUser selectAll(int user_id) {
		return sud.selectAll(user_id);
	}
}
