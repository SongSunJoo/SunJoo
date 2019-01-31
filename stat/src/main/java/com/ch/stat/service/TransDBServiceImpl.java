package com.ch.stat.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ch.stat.dao.TransDBDao;

@Service
public class TransDBServiceImpl implements TransDBService {
	@Autowired
	private TransDBDao trd;

	@Override
	public String selectTrans(int transDB_id) {
		return trd.selectTrans(transDB_id);
	}
}
