package com.ch.stat.model;

import java.sql.Date;

public class Poll {
	private int poll_id;
	private String poll_title;
	private String poll_cont1;
	private String poll_cont2;
	private int poll_cont1_cnt;
	private int poll_cont2_cnt;
	private Date poll_startDate;
	private Date poll_endDate;
	
	private int startRow;
	private int endRow;
	
	public int getPoll_id() {
		return poll_id;
	}
	public void setPoll_id(int poll_id) {
		this.poll_id = poll_id;
	}
	public String getPoll_title() {
		return poll_title;
	}
	public void setPoll_title(String poll_title) {
		this.poll_title = poll_title;
	}
	public String getPoll_cont1() {
		return poll_cont1;
	}
	public void setPoll_cont1(String poll_cont1) {
		this.poll_cont1 = poll_cont1;
	}
	public String getPoll_cont2() {
		return poll_cont2;
	}
	public void setPoll_cont2(String poll_cont2) {
		this.poll_cont2 = poll_cont2;
	}
	public int getPoll_cont1_cnt() {
		return poll_cont1_cnt;
	}
	public void setPoll_cont1_cnt(int poll_cont1_cnt) {
		this.poll_cont1_cnt = poll_cont1_cnt;
	}
	public int getPoll_cont2_cnt() {
		return poll_cont2_cnt;
	}
	public void setPoll_cont2_cnt(int poll_cont2_cnt) {
		this.poll_cont2_cnt = poll_cont2_cnt;
	}
	public Date getPoll_startDate() {
		return poll_startDate;
	}
	public void setPoll_startDate(Date poll_startDate) {
		this.poll_startDate = poll_startDate;
	}
	public Date getPoll_endDate() {
		return poll_endDate;
	}
	public void setPoll_endDate(Date poll_endDate) {
		this.poll_endDate = poll_endDate;
	}
	public int getStartRow() {
		return startRow;
	}
	public void setStartRow(int startRow) {
		this.startRow = startRow;
	}
	public int getEndRow() {
		return endRow;
	}
	public void setEndRow(int endRow) {
		this.endRow = endRow;
	}
}
