package Kbay.model;
import java.sql.Date;
public class Kmember {
	private String id;
	private String pass;
	private String name;
	private String tel;
	private String email;
	private String addr;
	private int sell_penalty;
	private int buy_penalty;
	private Date reg_date;
	private String del;
	
	// getter는 보내주는 것, setter는 받는 것
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public String getPass() {
		return pass;
	}
	public void setPass(String pass) {
		this.pass = pass;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getTel() {
		return tel;
	}
	public void setTel(String tel) {
		this.tel = tel;
	}
	public String getEmail() {
		return email;
	}
	public void setEmail(String email) {
		this.email = email;
	}
	public String getAddr() {
		return addr;
	}
	public void setAddr(String addr) {
		this.addr = addr;
	}
	public int getSell_penalty() {
		return sell_penalty;
	}
	public void setSell_penalty(int sell_penalty) {
		this.sell_penalty = sell_penalty;
	}
	public int getBuy_penalty() {
		return buy_penalty;
	}
	public void setBuy_penalty(int buy_penalty) {
		this.buy_penalty = buy_penalty;
	}
	public Date getReg_date() {
		return reg_date;
	}
	public void setReg_date(Date reg_date) {
		this.reg_date = reg_date;
	}
	public String getDel() {
		return del;
	}
	public void setDel(String del) {
		this.del = del;
	}
}