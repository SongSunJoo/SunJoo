package Kbay.service;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.ProductInfoDao;
import Kbay.model.ProductInfo;

public class HotBiddingAction implements CommandProcess {
	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		int startNum = 1;
		int endNum = 5;
		
		ProductInfoDao pa = ProductInfoDao.getInstance();
		List<ProductInfo> hotBidding = pa.hotBidding(startNum, endNum);
		
		request.setAttribute("startNum", startNum);
		request.setAttribute("endNum", endNum);
		request.setAttribute("hotBidding", hotBidding);
		
		return "main/hotBidding";
	}

}
