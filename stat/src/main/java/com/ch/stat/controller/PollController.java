package com.ch.stat.controller;
import java.util.Collection;
import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import com.ch.stat.model.Poll;
import com.ch.stat.service.PagingBean;
import com.ch.stat.service.PollService;

@Controller
public class PollController {

	@Autowired
	private PollService pls;

	@RequestMapping("poll_result")
	public String poll_result(Poll pl, String poll_cont, Model model, String pageNum) {
		System.out.println("poll_id="+pl.getPoll_id());
		System.out.println("Poll_cont="+poll_cont);
		int poll_cont1_cnt = pl.getPoll_cont1_cnt();
		int poll_cont2_cnt = pl.getPoll_cont2_cnt();
		
		System.out.println("poll_cont1_cnt = " + poll_cont1_cnt);
		System.out.println("poll_cont2_cnt = " + poll_cont2_cnt);
		
		if (poll_cont != null) {
			if (pl.getPoll_cont1().equals(poll_cont)) {
				poll_cont1_cnt++;
			}
			else if (pl.getPoll_cont2().equals(poll_cont)) {
				poll_cont2_cnt++;
			}
		}
		
/*		System.out.println("Poll_cont1="+pl.getPoll_cont1());
		System.out.println("Poll_cont2="+pl.getPoll_cont2());*/
		
		int result = 0;		int plid1 = 0;		int plid2 = 0;
		double poll_cont1_per = 0.00;		double poll_cont2_per = 0.00;
		
		result = pls.updatePoll(pl.getPoll_id(), poll_cont1_cnt, poll_cont2_cnt);
		
		Poll poll_result = pls.selectPoll(pl);
		
		plid1 = poll_result.getPoll_cont1_cnt();
		plid2 = poll_result.getPoll_cont2_cnt();
		System.out.println("count1 = " + plid1);
		System.out.println("count2 = " + plid2);
		
		int poll_total_cnt = plid1 + plid2;
		System.out.println("poll_total_cnt = " + poll_total_cnt);
		
		poll_cont1_per = 100*(double)(plid1)/(double)(poll_total_cnt);
		poll_cont2_per = 100*(double)(plid2)/(double)(poll_total_cnt);
		
		model.addAttribute("result", result);
		model.addAttribute("poll_result", poll_result);
		model.addAttribute("plid1", plid1);
		model.addAttribute("plid2", plid2);
		model.addAttribute("poll_cont1_per", Math.round(poll_cont1_per));
		model.addAttribute("poll_cont2_per", Math.round(poll_cont2_per));
		model.addAttribute("pageNum", pageNum);
		
		return "poll/poll_result";
	}
	
	@RequestMapping("poll_list")
	public String poll_list(Poll pl, Model model, String pageNum) {
		
		if (pageNum==null || pageNum.equals("")) {
			pageNum = "1";
		}
		
		int currentPage = Integer.parseInt(pageNum);
		int rowPerPage = 5;
		
		int total = pls.getTotal(pl);
		System.out.println("total="+total);
		
		int startRow = (currentPage - 1) * rowPerPage + 1;
		int endRow = startRow + rowPerPage - 1;
		
		pl.setStartRow(startRow);
		pl.setEndRow(endRow);
		
		Collection<Poll> list = pls.list(pl);
		int no = total - startRow + 1;
		PagingBean pb=new PagingBean(currentPage,rowPerPage,total);
		String[] tit = {"제목","내용1","내용2"};
		
		System.out.println("currentPage="+currentPage);
		model.addAttribute("list", list);
		model.addAttribute("poll", pl);
		model.addAttribute("tit",tit);
		model.addAttribute("no", no);
		model.addAttribute("pb", pb);
		model.addAttribute("total",total);
		return "poll/poll_list";
	}
	
	@RequestMapping("poll_insertForm")
	public String poll_insertForm(String pageNum, Model model) {

		model.addAttribute("pageNum", pageNum);
		
		return "poll/poll_insertForm";
	}
	
	@RequestMapping("poll_insert")
	public String poll_insert(Poll pl, String pageNum,
			Model model, HttpServletRequest request) {

		int number = pls.maxNum();
		pl.setPoll_id(number);
		
		int result = pls.insert(pl);
		Poll poll_result = pls.selectPoll(pl);
		
		model.addAttribute("result", result);
		model.addAttribute("pageNum",pageNum);
		model.addAttribute("poll", pl);
		model.addAttribute("poll_result", poll_result);
		
		return "poll/poll_insertResult";
	}
	
	@RequestMapping("poll_fun1")
	public String poll_fun1() {

		return "poll/poll_fun1";
	}
	
	@RequestMapping("poll_fun2")
	public String poll_fun2() {

		return "poll/poll_fun2";
	}
	
	@RequestMapping("poll_fun3")
	public String poll_fun3() {

		return "poll/poll_fun3";
	}
}