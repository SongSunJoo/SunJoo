package com.ch.stat.dao;
import java.util.Collection;

import com.ch.stat.model.Poll;
public interface PollDao {
	
	Poll select(Poll pl);

	int getTotal(Poll pl);

	Collection<Poll> list(Poll pl);

	int maxNum();

	int insert(Poll pl);

//	int updatePoll(Poll pl);

	int select_latestPollId();

	Collection<Poll> ongoinglist(Poll pl);

	int updatePoll(int poll_id, int poll_cont1_cnt, int poll_cont2_cnt);

}
