package com.ch.stat.service;

import java.util.Collection;

import com.ch.stat.model.Poll;

public interface PollService {

	Poll selectPoll(Poll pl);
//	int updatePoll(Poll pl);
	int getTotal(Poll pl);
	Collection<Poll> list(Poll pl);
	int maxNum();
	int insert(Poll pl);
	int select_latestPollId();
	Collection<Poll> ongoinglist(Poll pl);
	int updatePoll(int poll_id, int poll_cont1_cnt, int poll_cont2_cnt);

}
