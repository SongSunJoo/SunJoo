package com.ch.stat.dao;

import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class OldDBDaoImpl implements OldDBDao {

	@Autowired
	private SqlSessionTemplate sst;

	@Override
	public String selectOld(int oldDB_id) {
		return sst.selectOne("oldDBns.selectOld", oldDB_id);
	}
}
