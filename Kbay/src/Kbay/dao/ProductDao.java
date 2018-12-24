package Kbay.dao;

import java.io.Reader;
import java.util.HashMap;
import java.util.List;

import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;

import Kbay.model.Kmember;
import Kbay.model.Product;

public class ProductDao {
	// singleton
	private static ProductDao instance = new ProductDao();
	private ProductDao() { }
	public static ProductDao getInstance() {
		return instance;
	}
	
	//Session 생성 DB 연결
	private static SqlSession session;
	static {
		try {
			Reader reader = Resources.getResourceAsReader("configuration.xml");
			SqlSessionFactory ssf = new SqlSessionFactoryBuilder().build(reader);
			session = ssf.openSession(true);
			reader.close();
		} catch (Exception e) {
			System.out.println("Product Session 생성 : " + e.getMessage());
		}
	}
	public List<Product> list1(String id) {
		return session.selectList("productns.list1", id);
	}
	/*public List<Kmember> list2(String id) {
		return session.selectList("productns.list2");
	}
	public List<Kmember> list3(String id) {
		return session.selectList("productns.list3");
	}*/
	public int insert(Product product) {
		int result = 0;
		
		result = session.insert("productns.insert", product);
		
		return result;
	}
	public int selectMax() {
		int p_num = 0;
		
		p_num = (int) session.selectOne("productns.selectMax");
		
		return p_num;
	}
	public int updateImgName(int p_num, String p_image) {
		int result = 0;
		HashMap<String, Object> hm = new HashMap<>();
		hm.put("p_num", p_num);
		hm.put("p_image", p_image);
		
		result = session.update("productns.updateImgName", hm);
		return result;
	}
	// New Item 목록
	public List<Product> newItem(int startNum, int endNum) {
		HashMap<String, Object> hm = new HashMap<>();
		hm.put("startNum", startNum);
		hm.put("endNum", endNum);
		System.out.println("ProductDao List startNum : " + startNum);
		System.out.println("ProductDao List endNum : " + endNum);
		return session.selectList("productns.newItem", hm);
	}
}
