package com.ch.stat.dao;

import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class DigitalDBDaoImpl implements DigitalDBDao {
	
	@Autowired
	private SqlSessionTemplate sst;

	@Override
	public String selectDigital(int digitalDB_id) {
		return sst.selectOne("digitalDBns.selectDigital", digitalDB_id);
	}
}
