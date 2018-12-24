package Kbay.dao;

import java.io.Reader;

import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;

public class ProductClass1Dao {
	//singleton
	private static ProductClass1Dao instance = new ProductClass1Dao();
	private ProductClass1Dao() { }
	public static ProductClass1Dao getInstance() {
		return instance;
	}
	
	// Session 생성, DB 연결
	private static SqlSession session;
	static {
		try {
			Reader reader = Resources.getResourceAsReader("configuration.xml");
			SqlSessionFactory ssf = new SqlSessionFactoryBuilder().build(reader);
			session = ssf.openSession(true);
			reader.close();
		} catch (Exception e) {
			System.out.println("ProductClass1Dao Session 생성 : " + e.getMessage());
		}
	}
}
