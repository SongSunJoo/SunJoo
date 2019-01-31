package com.ch.stat.dao;

import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class AgeDBDaoImpl implements AgeDBDao {

	@Autowired
	private SqlSessionTemplate sst;

	@Override
	public int select(String gender) {
		System.out.println("age daoImpl gender= "+gender);
		
		return sst.selectOne("ageDBns.select", gender);
	}
}
