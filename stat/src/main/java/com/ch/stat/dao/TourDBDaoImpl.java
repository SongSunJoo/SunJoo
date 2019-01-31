package com.ch.stat.dao;

import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class TourDBDaoImpl implements TourDBDao {

	@Autowired
	private SqlSessionTemplate sst;

	@Override
	public String selectNation(int tourDB_id1) {
		return sst.selectOne("tourDBns.selectNation", tourDB_id1);
	}
}
