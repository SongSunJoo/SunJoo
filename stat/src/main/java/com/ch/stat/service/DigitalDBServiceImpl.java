package com.ch.stat.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ch.stat.dao.DigitalDBDao;

@Service
public class DigitalDBServiceImpl implements DigitalDBService {
	
	@Autowired
	private DigitalDBDao dgd;

	@Override
	public String selectDigital(int digitalDB_id) {
		return dgd.selectDigital(digitalDB_id);
	}
}
