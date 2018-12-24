package Kbay.service;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.ProductInfoDao;
import Kbay.model.ProductInfo;

public class PriceHistoryAction implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		
		int    p_num         = Integer.parseInt(request.getParameter("p_num"));
		int    gubun         = Integer.parseInt(request.getParameter("gubun"));
		String p_name        = request.getParameter("p_name");
		String p_class_name1 = request.getParameter("p_class_name1");
		String p_class_name2 = request.getParameter("p_class_name2");
		int    pageNum       = Integer.parseInt(request.getParameter("pageNum"));
		String p_image       = request.getParameter("p_image");
		
		ProductInfoDao pi = ProductInfoDao.getInstance();
		List<ProductInfo> list = pi.getAuctHistory(p_num);
		
		request.setAttribute("list", list);
		request.setAttribute("p_num", p_num);
		request.setAttribute("p_name", p_name);
		request.setAttribute("p_class_name1", p_class_name1);
		request.setAttribute("p_class_name2", p_class_name2);
		request.setAttribute("gubun", gubun);
		request.setAttribute("pageNum", pageNum);
		request.setAttribute("p_image", p_image);
		
		return "product/priceHistory";
	}

}
