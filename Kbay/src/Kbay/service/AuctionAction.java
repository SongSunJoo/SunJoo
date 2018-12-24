package Kbay.service;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.ProductInfoDao;
import Kbay.model.ProductInfo;

public class AuctionAction implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		/*HttpSession session = request.getSession();
		String id = (String) session.getAttribute("id");*/
		
		String id = "admin";
		
		ProductInfoDao pi = ProductInfoDao.getInstance();
		List<ProductInfo> list = pi.auctCheck();
		int list_cnt = pi.auctCheckCnt();
		
		System.out.println("list_cnt = " + list_cnt);
		
		int list_total = 0;
		int auct_total = 0;
		int p_num      = 0;
		int result     = 0;
		String a_id    = "";
		
		for (ProductInfo pp:list) {
			
			a_id    = "";
			list_total++;
			
			System.out.println("list_total = " + list_total);
			
			System.out.println("auct list p_num = " + pp.getP_num());
			
			p_num = pp.getP_num();
			a_id = pi.getA_ID(p_num);

			System.out.println("auct a_id = " + a_id);
			
			if (a_id == null || a_id.equals("")) {
				System.out.println("유찰");
				result = pi.setAuct4(p_num);
			}
			else {
				System.out.println("판매종료");
				result = pi.setAuct1(p_num, a_id);
			}
			
			if (result > 0)
				auct_total++;
		}
		
		request.setAttribute("list_total", list_total);
		request.setAttribute("auct_total", auct_total);
		
		return "admin/auction";
	}

}
