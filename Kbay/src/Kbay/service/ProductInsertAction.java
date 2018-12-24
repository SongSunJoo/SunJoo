package Kbay.service;

import java.sql.Date;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import com.oreilly.servlet.MultipartRequest;
import com.oreilly.servlet.multipart.DefaultFileRenamePolicy;

import java.io.*;

import Kbay.dao.ProductDao;
import Kbay.dao.ProductInfoDao;
import Kbay.model.Product;
import Kbay.model.ProductInfo;

public class ProductInsertAction implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		try {
			request.setCharacterEncoding("utf-8");
		} catch (UnsupportedEncodingException e1) {}
		
		HttpSession session = request.getSession();
		String id = (String)session.getAttribute("id");
		
		if (id == null)	/*id = "admin";*/ {
			session.invalidate();
			return "kmember/loginForm.do";
		}
		
		String fileSave = "/fileSave";		
		int max = 1024 * 1024 * 10;	
		
		String real = request.getSession().getServletContext().getRealPath(fileSave);		
		System.out.println("real = " + real);
		
		MultipartRequest mr = null;
		try {
			mr = new MultipartRequest(request, real, max, "utf-8", new DefaultFileRenamePolicy());
		} catch (IOException e) {
			System.out.println("file error : " + e.getMessage());
		}
		
		String p_name 		= mr.getParameter("p_name");
		String p_class 		= mr.getParameter("class");
		int    p_price 		= Integer.parseInt(mr.getParameter("p_price"));
		String p_used 		= mr.getParameter("p_used");
		Date   p_start_date = Date.valueOf(mr.getParameter("p_start_date"));
		Date   p_end_date 	= Date.valueOf(mr.getParameter("p_end_date"));
		String p_class1     = p_class.substring(0,1);
				
		ProductInfoDao pi = ProductInfoDao.getInstance();
		ProductInfo pInfo = new ProductInfo();
		
		pInfo.setP_name(p_name);
		pInfo.setP_class_id1(p_class1);
		pInfo.setP_class_id2(p_class);
		pInfo.setP_price(p_price);
		pInfo.setP_used(p_used);
		pInfo.setP_buyer(id);
		pInfo.setP_auct_id(0);
		pInfo.setP_start_date(p_start_date);
		pInfo.setP_end_date(p_end_date);
		pInfo.setId(id);
		pInfo.setP_image(" ");
		
		int result = pi.insert(pInfo);	
		System.out.println("insert result = " + result + "\n");
		
		int p_num  = pi.selectMax();		
		System.out.println("p_num = " + p_num + "\n");
		
		String p_image = mr.getFilesystemName("p_image");		
		System.out.println("filename = " + p_image);
		
		//String p_image = p_num + "_" + filename; 
		
		File   file     = new File(real+"/"+ p_image);
		
		//String final_filename = p_num+"_"+filename;
		
		int result1 = pi.updateImgName(p_num, p_image);
		
		//System.out.println("full filename = " + real+"/"+p_num+"_"+filename);
		
		request.setAttribute("result1", result1);
		
		return "product/productInsert";
	}

}
