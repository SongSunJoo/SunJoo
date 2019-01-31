package com.ch.stat.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ch.stat.dao.AgeDBDao;

@Service
public class AgeDBServiceImpl implements AgeDBService {

	@Autowired
	private AgeDBDao agd;

	@Override
	public int select(String gender) {
		return agd.select(gender);
	}
}
