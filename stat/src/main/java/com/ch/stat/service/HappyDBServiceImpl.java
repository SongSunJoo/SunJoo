package com.ch.stat.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ch.stat.dao.HappyDBDao;

@Service
public class HappyDBServiceImpl implements HappyDBService {
	@Autowired
	private HappyDBDao hpd;

	@Override
	public String selectHappy(int happyDB_id) {
		return hpd.selectHappy(happyDB_id);
	}
}
