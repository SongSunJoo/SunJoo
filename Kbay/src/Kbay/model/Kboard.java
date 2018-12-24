package Kbay.model;
import java.sql.Date;
public class Kboard {
	private int b_num;
	private String b_subject;
	private String b_content;
	private int b_readcount;
	private int b_ref;
	private int b_re_step;
	private int b_re_level;
	private Date b_reg_date;
	private String id;
	private String b_pass;

	public int getB_num() {
		return b_num;
	}
	public void setB_num(int b_num) {
		this.b_num = b_num;
	}
	public String getB_subject() {
		return b_subject;
	}
	public void setB_subject(String b_subject) {
		this.b_subject = b_subject;
	}
	public String getB_content() {
		return b_content;
	}
	public void setB_content(String b_content) {
		this.b_content = b_content;
	}
	public int getB_readcount() {
		return b_readcount;
	}
	public void setB_readcount(int b_readcount) {
		this.b_readcount = b_readcount;
	}
	public int getB_ref() {
		return b_ref;
	}
	public void setB_ref(int b_ref) {
		this.b_ref = b_ref;
	}
	public int getB_re_step() {
		return b_re_step;
	}
	public void setB_re_step(int b_re_step) {
		this.b_re_step = b_re_step;
	}
	public int getB_re_level() {
		return b_re_level;
	}
	public void setB_re_level(int b_re_level) {
		this.b_re_level = b_re_level;
	}
	public Date getB_reg_date() {
		return b_reg_date;
	}
	public void setB_reg_date(Date b_reg_date) {
		this.b_reg_date = b_reg_date;
	}
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public String getB_pass() {
		return b_pass;
	}
	public void setB_pass(String b_pass) {
		this.b_pass = b_pass;
	}
}