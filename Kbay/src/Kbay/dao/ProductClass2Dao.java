package Kbay.dao;

import java.io.Reader;
import java.util.List;

import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;

import Kbay.model.ProductClass2;

public class ProductClass2Dao {
	//singleton
	private static ProductClass2Dao instance = new ProductClass2Dao();
	private ProductClass2Dao() { }
	public static ProductClass2Dao getInstance() {
		return instance;
	}
	
	//Session 생성, DB 연결
	private static SqlSession session;
	static {
		try {
			Reader reader = Resources.getResourceAsReader("configuration.xml");
			SqlSessionFactory ssf = new SqlSessionFactoryBuilder().build(reader);
			session = ssf.openSession(true);
			reader.close();
		} catch (Exception e) {
			System.out.println("ProductClass2Dao Session 생성 : " + e.getMessage());
		}
	}
	/*public List<ProductClass2> list(String class1) {
		return session.selectList("productClass2ns.list");
	}*/
}
