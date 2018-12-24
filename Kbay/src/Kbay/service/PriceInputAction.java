package Kbay.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import Kbay.dao.ProductInfoDao;
import Kbay.model.ProductInfo;

public class PriceInputAction implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		
		//System.out.println("price input action...");
		
		HttpSession session = request.getSession();
		String id = (String) session.getAttribute("id");
		
		if (id == null || id.equals(""))
			return "login/loginForm";
		
		//System.out.println("id = " + id);
		
		int p_num = Integer.parseInt(request.getParameter("p_num"));
		//System.out.println("p_num = " + p_num);
		
		int gubun = Integer.parseInt(request.getParameter("gubun"));
		//System.out.println("gubun = " + gubun);
		
		int pageNum = Integer.parseInt(request.getParameter("pageNum"));
		//System.out.println("pageNum = " + pageNum);
		
		ProductInfoDao pi = ProductInfoDao.getInstance();
		ProductInfo pInfo = null;
		
		int     max_price = pi.getAuctPrice(p_num);
		
		if (max_price == 0) {
			pInfo     = pi.getDetail(p_num);
			max_price = pInfo.getP_price();
		}
		
		request.setAttribute("max_price", max_price);
		request.setAttribute("id", id);
		request.setAttribute("p_num", p_num);
		request.setAttribute("gubun", gubun);
		request.setAttribute("pageNum", pageNum);
		
		return "product/priceInput";
	}

}
