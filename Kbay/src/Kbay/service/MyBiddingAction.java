package Kbay.service;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import Kbay.dao.KmemberDao;
import Kbay.dao.ProductDao;
import Kbay.dao.ProductInfoDao;
import Kbay.model.Kmember;
import Kbay.model.Product;
import Kbay.model.ProductInfo;

public class MyBiddingAction implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		HttpSession session = request.getSession();
		String id = (String) session.getAttribute("id");
		
		System.out.println("session id = " + id);
		
		if (id == null || id.equals(""))
			return "login/loginForm.do";
		
		int gubun = Integer.parseInt(request.getParameter("gubun"));
		System.out.println("gubun = " + gubun);
		
		String pageNum = request.getParameter("pageNum");
		System.out.println("pageNum = " + pageNum);
		
		//Paging
		int rowPerPage = 10;
		int pagePerBlock = 10;
		
		if (pageNum == null || pageNum.equals("")) {
			pageNum = "1";
		}
		
		int currentPage = Integer.parseInt(pageNum);
		int startRow    = (currentPage - 1) * rowPerPage + 1;
		int endRow      = startRow + rowPerPage - 1;
		
		System.out.println("startRow = " + startRow);
		System.out.println("endRow = "   + endRow);
		
		ProductInfoDao pi = ProductInfoDao.getInstance();
		List<ProductInfo> list = new ArrayList<>();
		
		int tot = 0;
		
		if (gubun == 1) {
			list = pi.list1(id, startRow, endRow);
			tot  = pi.listTotal1(id);
		}
		else if (gubun == 2) {
			list = pi.list2(id, startRow, endRow);
			tot  = pi.listTotal2(id);
		}
		else {
			list = pi.list3(id, startRow, endRow);
			tot  = pi.listTotal3(id);
		}
		
		int total = tot - startRow + 1;
		int startPage = currentPage - (currentPage - 1) % 10;
		int endPage = startPage + pagePerBlock - 1;
		int totPage = (int)Math.ceil((double)tot/rowPerPage);
		
		if (endPage > totPage) {
			endPage = totPage;
		}
		
		String class1Name;
		String class2Name;
		String auctDesc;
		int    max_price = 0;
		
		for (ProductInfo pp:list) {
			class1Name = pi.getClass1Name(pp.getP_class_id1());
			class2Name = pi.getClass2Name(pp.getP_class_id2());
			auctDesc   = pi.getAuctDesc(pp.getP_auct_id());
			
			
			pp.setP_class_name1(class1Name);
			pp.setP_class_name2(class2Name);
			pp.setP_auct_desc(auctDesc);
			
			max_price = pi.getAuctPrice(pp.getP_num());
			
			//System.out.println("max_price = " + max_price + "\n");
			
			if (max_price > 0) {
				pp.setP_price(max_price);
			}
		}
		
		request.setAttribute("total", total);
		request.setAttribute("list", list);
		request.setAttribute("startPage", startPage);
		request.setAttribute("endPage", endPage);
		request.setAttribute("totPage", totPage);
		request.setAttribute("pagePerBlock", pagePerBlock);
		request.setAttribute("currentPage", currentPage);
		
		request.setAttribute("list", list);
		request.setAttribute("gubun", gubun);
				
		return "product/myBidding";
	}

}
