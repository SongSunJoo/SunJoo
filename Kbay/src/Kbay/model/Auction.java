package Kbay.model;

import java.sql.Date;

public class Auction {
	private int a_num;
	private int a_price;
	private Date a_reg_date;
	private int p_num;
	private String a_id;
	
	public String getA_id() {
		return a_id;
	}
	public void setA_id(String a_id) {
		this.a_id = a_id;
	}
	public int getA_num() {
		return a_num;
	}
	public void setA_num(int a_num) {
		this.a_num = a_num;
	}
	public int getA_price() {
		return a_price;
	}
	public void setA_price(int a_price) {
		this.a_price = a_price;
	}
	public Date getA_reg_date() {
		return a_reg_date;
	}
	public void setA_reg_date(Date a_reg_date) {
		this.a_reg_date = a_reg_date;
	}
	public int getP_num() {
		return p_num;
	}
	public void setP_num(int p_num) {
		this.p_num = p_num;
	}
}
