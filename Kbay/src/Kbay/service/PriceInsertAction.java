package Kbay.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.AuctionDao;
import Kbay.dao.ProductInfoDao;
import Kbay.model.Auction;
import Kbay.model.ProductInfo;

public class PriceInsertAction implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		
		String a_id = request.getParameter("a_id");
		int a_price = Integer.parseInt(request.getParameter("newPrice"));
		int   p_num = Integer.parseInt(request.getParameter("p_num"));
		int pageNum = Integer.parseInt(request.getParameter("pageNum"));
		
		ProductInfoDao pi = ProductInfoDao.getInstance();
		ProductInfo pInfo = new ProductInfo();
		
		pInfo.setA_price(a_price);;
		pInfo.setP_num(p_num);
		pInfo.setA_id(a_id);
		
		int result = pi.insertAuct(pInfo);
		
		request.setAttribute("result", result);
		request.setAttribute("p_num", p_num);
		request.setAttribute("pageNum", pageNum);
				
		return "product/priceInsert";
	}

}
