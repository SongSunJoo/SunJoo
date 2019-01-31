package com.ch.stat.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ch.stat.dao.MobileDBDao;

@Service
public class MobileDBServiceImpl implements MobileDBService {
	@Autowired
	private MobileDBDao mbd;

	@Override
	public String selectMobile(int mobileDB_id) {
		return mbd.selectMobile(mobileDB_id);
	}
}
