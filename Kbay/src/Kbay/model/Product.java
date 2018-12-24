package Kbay.model;

import java.sql.Date;

public class Product {
	private int p_num;
	private String p_name;
	private String p_used;
	private int p_price;
	private Date p_start_date;
	private Date p_end_date;
	private String p_buyer;
	private String p_image;
	private String id;
	private int p_auct_id;
	private String p_class_id1;
	private String p_class_id2;
	
	public int getP_num() {
		return p_num;
	}
	public void setP_num(int p_num) {
		this.p_num = p_num;
	}
	public String getP_name() {
		return p_name;
	}
	public void setP_name(String p_name) {
		this.p_name = p_name;
	}
	public String getP_used() {
		return p_used;
	}
	public void setP_used(String p_used) {
		this.p_used = p_used;
	}
	public int getP_price() {
		return p_price;
	}
	public void setP_price(int p_price) {
		this.p_price = p_price;
	}
	public Date getP_start_date() {
		return p_start_date;
	}
	public void setP_start_date(Date p_start_date) {
		this.p_start_date = p_start_date;
	}
	public Date getP_end_date() {
		return p_end_date;
	}
	public void setP_end_date(Date p_end_date) {
		this.p_end_date = p_end_date;
	}
	public String getP_buyer() {
		return p_buyer;
	}
	public void setP_buyer(String p_buyer) {
		this.p_buyer = p_buyer;
	}
	public String getP_image() {
		return p_image;
	}
	public void setP_image(String p_image) {
		this.p_image = p_image;
	}
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public int getP_auct_id() {
		return p_auct_id;
	}
	public void setP_auct_id(int p_auct_id) {
		this.p_auct_id = p_auct_id;
	}
	public String getP_class_id1() {
		return p_class_id1;
	}
	public void setP_class_id1(String p_class_id1) {
		this.p_class_id1 = p_class_id1;
	}
	public String getP_class_id2() {
		return p_class_id2;
	}
	public void setP_class_id2(String p_class_id2) {
		this.p_class_id2 = p_class_id2;
	}
}
