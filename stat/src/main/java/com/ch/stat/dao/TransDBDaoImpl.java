package com.ch.stat.dao;

import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class TransDBDaoImpl implements TransDBDao {
	@Autowired
	private SqlSessionTemplate sst;

	@Override
	public String selectTrans(int transDB_id) {
		return sst.selectOne("transDBns.selectTrans", transDB_id);
	}
}
