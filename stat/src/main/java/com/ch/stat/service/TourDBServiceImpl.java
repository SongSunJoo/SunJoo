package com.ch.stat.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ch.stat.dao.TourDBDao;

@Service
public class TourDBServiceImpl implements TourDBService {

	@Autowired
	private TourDBDao trd;
	
	@Override
	public String selectNation(int tourDB_id1) {
		return trd.selectNation(tourDB_id1);
	}

}
