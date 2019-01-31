package com.ch.stat.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ch.stat.dao.OldDBDao;

@Service
public class OldDBServiceImpl implements OldDBService {

	@Autowired
	private OldDBDao odd;

	@Override
	public String selectOld(int oldDB_id) {
		return odd.selectOld(oldDB_id);
	}
}
