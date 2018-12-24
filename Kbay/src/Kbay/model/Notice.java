package Kbay.model;

import java.sql.Date;

public class Notice {
	private int    n_num;
	private String m_id;
	private String n_subject;
	private String n_content;
	private Date   n_reg_date;
	
	public int getN_num() {
		return n_num;
	}
	public void setN_num(int n_num) {
		this.n_num = n_num;
	}
	public String getM_id() {
		return m_id;
	}
	public void setM_id(String m_id) {
		this.m_id = m_id;
	}
	public String getN_subject() {
		return n_subject;
	}
	public void setN_subject(String n_subject) {
		this.n_subject = n_subject;
	}
	public String getN_content() {
		return n_content;
	}
	public void setN_content(String n_content) {
		this.n_content = n_content;
	}
	public Date getN_reg_date() {
		return n_reg_date;
	}
	public void setN_reg_date(Date n_reg_date) {
		this.n_reg_date = n_reg_date;
	}
	
}
