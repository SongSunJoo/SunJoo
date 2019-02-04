package Kbay.dao;

import java.io.Reader;

import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;

import Kbay.model.KManager;

public class KManagerDao {
	
	/*singleton 정의*/
	private static KManagerDao instance = new KManagerDao();
	private KManagerDao() {}
	public static KManagerDao getInstance() {
		return instance;
	}
	
	private static SqlSession session;
	static {
		try {
			Reader reader = Resources.getResourceAsReader("configuration.xml");
			SqlSessionFactory ssf = new SqlSessionFactoryBuilder().build(reader);
			
			session = ssf.openSession(true);	// true: auto commit, 반드시 기술해야함
			reader.close();
		} catch (Exception e) {
			System.out.println("session 생성에러: "+e.getMessage());
		}
	}
	
	// 로그인 체크
	public KManager select(String m_id) {
		return (KManager) session.selectOne("kmanagerns.select", m_id);
	}
	
	
}
