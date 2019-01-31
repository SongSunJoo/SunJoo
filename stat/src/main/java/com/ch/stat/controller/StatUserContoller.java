package com.ch.stat.controller;

import java.util.Collection;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import com.ch.stat.model.Poll;
import com.ch.stat.model.StatUser;
import com.ch.stat.service.AgeDBService;
import com.ch.stat.service.DigitalDBService;
import com.ch.stat.service.HappyDBService;
import com.ch.stat.service.MobileDBService;
import com.ch.stat.service.OldDBService;
import com.ch.stat.service.PollService;
import com.ch.stat.service.StatUserService;
import com.ch.stat.service.TourDBService;
import com.ch.stat.service.TransDBService;

@Controller
public class StatUserContoller {
	
	@Autowired
	private StatUserService sus;
	
	@Autowired
	private AgeDBService    ags;
	
	@Autowired
	private TourDBService   trs;
	
	@Autowired
	private OldDBService    ods;
	
	@Autowired
	private DigitalDBService dgs;
	
	@Autowired
	private MobileDBService  mbs;
	
	@Autowired
	private TransDBService   tds;
	
	@Autowired
	private HappyDBService   hps;
	
	@Autowired
	private PollService		pls;
	
/*	@RequestMapping(value="survey1_main")
	public String main() {
		return "survey/survey1_main";
	}*/
	
	@RequestMapping("survey1_main")
	public String main(Poll pl, Model model) {
		/*int latestPollId = pls.select_latestPollId();
		pl.setPoll_id(latestPollId);*/
	
		Collection<Poll> ongoinglist = pls.ongoinglist(pl);
		
		model.addAttribute("ongoinglist", ongoinglist);
		
		return "survey/survey1_main";
	}
	
	@RequestMapping("survey1R_life")
	public String survey1R_life(String place, String gender, int age, String group, Model model) {
		//System.out.println("gender= "+gender);
		
		StatUser su = new StatUser();
		su.setUser_place(place);
		su.setUser_gender(gender);
		su.setUser_Age(age);
		su.setUser_Group(group);
		
		int result = sus.insert(su);
		
		int age_val = ags.select(gender);
		int rest_life = age_val - (age-1);
		
		int user_id = sus.selectUserId();
		
		model.addAttribute("result", result);
		model.addAttribute("user_id", user_id);
		model.addAttribute("age", age-1);
		model.addAttribute("rest_life", rest_life);
		
		return "survey/survey1R_life";
	}
	
	@RequestMapping("survey2_height")
	public String survey2_height(int user_id, Model model) {
		model.addAttribute("user_id", user_id);
		return "survey/survey2_height";
	}
	
	@RequestMapping("survey2R_height")
	public String survey2R_height(int user_id, int height_value, Model model) {
		
		System.out.println("user_id= " + user_id);
		System.out.println("height_value= " + height_value);
		
		int result = sus.updateHeight(user_id, height_value);
		
		model.addAttribute("user_id", user_id);
		model.addAttribute("height_value", height_value);
		//model.addAttribute("result", result);
		
		//return "survey/survey2R_height";
		return "survey/survey2R_heightR";
	}
	
	@RequestMapping("survey3_lastName")
	public String survey2_lastName(int user_id, Model model) {
		
		model.addAttribute("user_id", user_id);
		
		return "survey/survey3_lastName";
	}
	
	@RequestMapping("survey3R_lastName")
	public String survey3R_lastName(int user_id, String lastName, Model model) {
		int result = sus.updateName(user_id, lastName);
		
		model.addAttribute("user_id", user_id);
		model.addAttribute("lastName", lastName);
		model.addAttribute("result", result);
		    
		//return "survey/survey3R_lastName";
		return "survey/survey3R_lastNameR";
	}
	
	@RequestMapping("survey4_animal")
	public String survey4_animal(int user_id, Model model) {
		model.addAttribute("user_id", user_id);
		return "survey/survey4_animal";
	}
	
	@RequestMapping("survey4R_animal")
	public String survey4_animal(int user_id, String pet_value, Model model) {
		int result = sus.updateAnimal(user_id, pet_value);
		
		model.addAttribute("user_id", user_id);
		model.addAttribute("pet_value", pet_value);
		model.addAttribute("result", result);
		
		//return "survey/survey4R_animal";
		return "survey/survey4R_animalR";
	}
	
	@RequestMapping("survey5_tour")
	public String survey5_tour(int user_id, Model model) {
		model.addAttribute("user_id", user_id);
		return "survey/survey5_tour";
	}
	
	@RequestMapping("survey5R_tour")
	public String survey5R_tour(int user_id, int tourDB_id1, Model model) {
		int result = sus.updateTour(user_id, tourDB_id1);
		
		String tour_nation = trs.selectNation(tourDB_id1);
		
		model.addAttribute("user_id", user_id);
		model.addAttribute("tour_nation", tour_nation);
		model.addAttribute("result", result);
		
		System.out.println("tour_nation Start : " + tour_nation);
		
		//return "survey/survey5R_tour";
		return "survey/survey5R_tourR";
	}
	
	@RequestMapping("survey6_old")
	public String survey6_old(int user_id, Model model) {
		model.addAttribute("user_id", user_id);
		return "survey/survey6_old";
	}
	
	@RequestMapping("survey6R_old")
	public String survey6R_old(int user_id, int oldDB_id, Model model) {
		int result = sus.updateOld(user_id, oldDB_id);
		
		String old_value = ods.selectOld(oldDB_id);
		
		model.addAttribute("user_id", user_id);
		model.addAttribute("old_value", old_value);
		model.addAttribute("result", result);
		
		//return "survey/survey6R_old";
		return "survey/survey6R_oldR";
	}
	
	@RequestMapping("survey7_digital")
	public String survey7_digital(int user_id, Model model) {
		model.addAttribute("user_id", user_id);
		return "survey/survey7_digital";
	}
	
	@RequestMapping("survey7R_digital")
	public String survey7R_digital(int user_id, int digitalDB_id, Model model) {
		int result = sus.updateDigital(user_id, digitalDB_id);
		
		String digital_value = dgs.selectDigital(digitalDB_id);
		
		model.addAttribute("user_id", user_id);
		model.addAttribute("digital_value", digital_value);
		model.addAttribute("result", result);
		
		//return "survey/survey7R_digital";
		return "survey/survey7R_digitalR";
	}
	
	@RequestMapping("survey8_content")
	public String survey8_content(int user_id, Model model) {
		model.addAttribute("user_id", user_id);
		return "survey/survey8_content";
	}
	
	@RequestMapping("survey8R_content")
	public String survey8R_content(int user_id, int mobileDB_id, Model model) {
		int result = sus.updateMobile(user_id, mobileDB_id);
		
		String mobile_value = mbs.selectMobile(mobileDB_id);
		
		model.addAttribute("user_id", user_id);
		model.addAttribute("mobile_value", mobile_value);
		model.addAttribute("result", result);
		
		//return "survey/survey8R_content";
		return "survey/survey8R_contentR";
	}
	
	@RequestMapping("survey9_trans")
	public String survey9_trans(int user_id, Model model) {
		model.addAttribute("user_id", user_id);
		return "survey/survey9_trans";
	}
	
	@RequestMapping("survey9R_trans")
	public String survey9R_trans(int user_id, int transDB_id, Model model) {
		int result = sus.updateTrans(user_id, transDB_id);
		
		String trans_value = tds.selectTrans(transDB_id);
		
		model.addAttribute("user_id", user_id);
		model.addAttribute("trans_value", trans_value);
		model.addAttribute("result", result);
		
		//return "survey/survey9R_trans";
		return "survey/survey9R_transR";
	}
	
	@RequestMapping("survey10_happy")
	public String survey10_happy(int user_id, Model model) {
		model.addAttribute("user_id", user_id);
		return "survey/survey10_happy";
	}
	
	@RequestMapping("survey10R_happy")
	public String survey10R_happy(int user_id, int happyDB_id, Model model) {
		int result = sus.updateHappy(user_id, happyDB_id);
		
		String happy_value = hps.selectHappy(happyDB_id);
		
		model.addAttribute("user_id", user_id);
		model.addAttribute("happy_value", happy_value);
		model.addAttribute("result", result);
		
		//return "survey/survey10R_happy";
		return "survey/survey10R_happyR";
	}
	
	@RequestMapping("survey11_nick")
	public String survey11_nick(int user_id, Model model) {
		model.addAttribute("user_id", user_id);
		return "survey/survey11_nick";
	}
	
	@RequestMapping("survey11R_allYou")
	public String survey11R_allYou(int user_id, String user_nick, Model model) {
		int result = sus.updateNick(user_id, user_nick);
		
		StatUser su = new StatUser();
		su = sus.selectAll(user_id);
		
		int    age_value     = su.getUser_Age() - 1;
		int    rest_life     = ags.select(su.getUser_gender()) - age_value;
		
		String group_value   = su.getUser_Group();
		String trans_value   = tds.selectTrans(su.getTransDB_id());
		String digital_value = dgs.selectDigital(su.getDigitalDB_id());
		String happy_value   = hps.selectHappy(su.getHappyDB_id());
		String tour_value    = trs.selectNation(su.getTourDB_id1());
		String old_value     = ods.selectOld(su.getOldDB_id());
		String mobile_value  = mbs.selectMobile(su.getMobileDB_id());
		
		model.addAttribute("su", su);
		model.addAttribute("age_value", age_value);
		model.addAttribute("group_value", group_value);
		model.addAttribute("rest_life", rest_life);
		model.addAttribute("trans_value", trans_value);
		model.addAttribute("digital_value", digital_value);
		model.addAttribute("happy_value", happy_value);
		model.addAttribute("tour_value", tour_value);
		model.addAttribute("old_value", old_value);
		model.addAttribute("mobile_value", mobile_value);
		model.addAttribute("user_nick", user_nick);
		model.addAttribute("result", result);
		
		return "survey/survey11R_allYou";
	}
	
}
