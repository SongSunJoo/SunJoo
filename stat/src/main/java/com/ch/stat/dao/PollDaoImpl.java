package com.ch.stat.dao;

import java.util.Collection;
import java.util.HashMap;

import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.ch.stat.model.Poll;

@Repository
public class PollDaoImpl implements PollDao{

	@Autowired
	private SqlSessionTemplate sst;
	
	@Override
	public Poll select(Poll pl) {
		return sst.selectOne("pollns.selectPoll", pl);
	}

	@Override
	public int getTotal(Poll pl) {
		return sst.selectOne("pollns.getTotal", pl);
	}

	@Override
	public Collection<Poll> list(Poll pl) {
		return sst.selectList("pollns.list", pl);
	}

	@Override
	public int maxNum() {
		return sst.selectOne("pollns.maxNum");
	}

	@Override
	public int insert(Poll pl) {
		return sst.insert("pollns.insert", pl);
	}

	@Override
	public int select_latestPollId() {
		return sst.selectOne("pollns.select_latestPollId");
	}

/*	@Override
	public int updatePoll(Poll pl) {
		return sst.update("pollns.updatePoll", pl);
	}*/

	@Override
	public Collection<Poll> ongoinglist(Poll pl) {
		return sst.selectList("pollns.ongoinglist", pl);
	}

	@Override
	public int updatePoll(int poll_id, int poll_cont1_cnt, int poll_cont2_cnt) {
		HashMap<String, Object> hm = new HashMap<String, Object>();
		hm.put("poll_id", poll_id);
		hm.put("poll_cont1_cnt", poll_cont1_cnt);
		hm.put("poll_cont2_cnt", poll_cont2_cnt);
		return sst.update("pollns.updatePoll", hm);
	}
}
