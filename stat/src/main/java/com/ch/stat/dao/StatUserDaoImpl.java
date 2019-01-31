package com.ch.stat.dao;

import java.util.HashMap;

import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.ch.stat.model.StatUser;

@Repository
public class StatUserDaoImpl implements StatUserDao {

	@Autowired
	private SqlSessionTemplate sst;

	@Override
	public int insert(StatUser su) {
		//System.out.println("Spring test...");
		return sst.insert("statUserns.insert", su);
	}
	
	@Override
	public int updateHeight(int user_id, int height_value) {
		HashMap<String, Object> hm = new HashMap<String, Object>();
		hm.put("user_id", user_id);
		hm.put("height_value", height_value);
		
		return sst.update("statUserns.updateHeight", hm);
	}


	@Override
	public int updateName(int user_id, String lastName) {
		HashMap<String, Object> hm = new HashMap<String, Object>();
		hm.put("user_id", user_id);
		hm.put("ln_value", lastName);
		
		return sst.update("statUserns.updateName", hm);
	}

	@Override
	public int selectUserId() {
		return sst.selectOne("statUserns.selectUserId");
	}

	@Override
	public int updateAnimal(int user_id, String pet_value) {
		HashMap<String, Object> hm = new HashMap<String, Object>();
		hm.put("user_id", user_id);
		hm.put("pet_value", pet_value);
		
		return sst.update("statUserns.updateAnimal", hm);
	}

	@Override
	public int updateTour(int user_id, int tourDB_id1) {
		HashMap<String, Object> hm = new HashMap<String, Object>();
		hm.put("user_id", user_id);
		hm.put("tourDB_id1", tourDB_id1);
		
		return sst.update("statUserns.updateTour", hm);
	}

	@Override
	public int updateOld(int user_id, int oldDB_id) {
		HashMap<String, Object> hm = new HashMap<String, Object>();
		hm.put("user_id", user_id);
		hm.put("oldDB_id", oldDB_id);
		
		return sst.update("statUserns.updateOld", hm);
	}

	@Override
	public int updateDigital(int user_id, int digitalDB_id) {
		HashMap<String, Object> hm = new HashMap<String, Object>();
		hm.put("user_id", user_id);
		hm.put("digitalDB_id", digitalDB_id);
		
		return sst.update("statUserns.updateDigital", hm);
	}

	@Override
	public int updateMobile(int user_id, int mobileDB_id) {
		HashMap<String, Object> hm = new HashMap<String, Object>();
		hm.put("user_id", user_id);
		hm.put("mobileDB_id", mobileDB_id);
		
		return sst.update("statUserns.updateMobile", hm);
	}

	@Override
	public int updateTrans(int user_id, int transDB_id) {
		HashMap<String, Object> hm = new HashMap<String, Object>();
		hm.put("user_id", user_id);
		hm.put("transDB_id", transDB_id);
		
		return sst.update("statUserns.updateTrans", hm);
	}

	@Override
	public int updateHappy(int user_id, int happyDB_id) {
		HashMap<String, Object> hm = new HashMap<String, Object>();
		hm.put("user_id", user_id);
		hm.put("happyDB_id", happyDB_id);
		
		return sst.update("statUserns.updateHappy", hm);
	}

	@Override
	public int updateNick(int user_id, String user_nick) {
		HashMap<String, Object> hm = new HashMap<String, Object>();
		hm.put("user_id", user_id);
		hm.put("user_nick", user_nick);
		
		return sst.update("statUserns.updateNick", hm);
	}

	@Override
	public StatUser selectAll(int user_id) {
		return sst.selectOne("statUserns.selectAll", user_id);
	}
}
