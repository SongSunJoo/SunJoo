package com.ch.stat.dao;

import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class MobileDBDaoImpl implements MobileDBDao {
	@Autowired
	private SqlSessionTemplate sst;

	@Override
	public String selectMobile(int mobileDB_id) {
		return sst.selectOne("mobileDBns.selectMobile", mobileDB_id);
	}
}
