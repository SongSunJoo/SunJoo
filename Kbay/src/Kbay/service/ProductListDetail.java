package Kbay.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.ProductInfoDao;
import Kbay.model.ProductInfo;

public class ProductListDetail implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		
		System.out.println("product List Detail...\n");
		
		int p_num = Integer.parseInt(request.getParameter("p_num"));
		/*int gubun = Integer.parseInt(request.getParameter("gubun"));*/
		int pageNum = Integer.parseInt(request.getParameter("pageNum"));
		
		System.out.println("p_num = " + p_num);
		
		ProductInfoDao pi = ProductInfoDao.getInstance();
		ProductInfo pInfo = pi.getDetail(p_num);
		
		int     max_price = pi.getAuctPrice(p_num);
		
		System.out.println("max_price = " + max_price);
		
		if (max_price > 0) {
			pInfo.setP_price(max_price);
		}
		
		String class1Name;
		String class2Name;
		String auctDesc;
		
		class1Name = pi.getClass1Name(pInfo.getP_class_id1());
		class2Name = pi.getClass2Name(pInfo.getP_class_id2());
		auctDesc   = pi.getAuctDesc(pInfo.getP_auct_id());
		
		System.out.println("pp auct_id = " + pInfo.getP_auct_id() + "\n");
		
		pInfo.setP_class_name1(class1Name);
		pInfo.setP_class_name2(class2Name);
		pInfo.setP_auct_desc(auctDesc);
		
		request.setAttribute("pInfo", pInfo);
		/*request.setAttribute("gubun", gubun);*/
		request.setAttribute("pageNum", pageNum);
			
		return "productlist/productListDetail";
	}

}
