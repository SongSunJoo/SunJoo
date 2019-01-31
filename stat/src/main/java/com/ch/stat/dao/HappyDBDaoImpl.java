package com.ch.stat.dao;

import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class HappyDBDaoImpl implements HappyDBDao {
	@Autowired
	private SqlSessionTemplate sst;

	@Override
	public String selectHappy(int happyDB_id) {
		return sst.selectOne("happyDBns.selectHappy", happyDB_id);
	}
}
