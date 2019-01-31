package com.ch.stat.service;

import java.util.Collection;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ch.stat.dao.PollDao;
import com.ch.stat.model.Poll;

@Service
public class PollServiceImpl implements PollService {

	@Autowired
	private PollDao pld;
	
	@Override
	public Poll selectPoll(Poll pl) {
		return pld.select(pl);
	}

	@Override
	public int getTotal(Poll pl) {
		return pld.getTotal(pl);
	}

	@Override
	public Collection<Poll> list(Poll pl) {
		return pld.list(pl);
	}

	@Override
	public int maxNum() {
		return pld.maxNum();
	}

	@Override
	public int insert(Poll pl) {
		return pld.insert(pl);
	}

	@Override
	public int select_latestPollId() {
		return pld.select_latestPollId();
	}
	
/*	@Override
	public int updatePoll(Poll pl) {
		return pld.updatePoll(pl);
	}*/

	@Override
	public Collection<Poll> ongoinglist(Poll pl) {
		return pld.ongoinglist(pl);
	}

	@Override
	public int updatePoll(int poll_id, int poll_cont1_cnt, int poll_cont2_cnt) {
		return pld.updatePoll(poll_id, poll_cont1_cnt, poll_cont2_cnt);
	}
}
