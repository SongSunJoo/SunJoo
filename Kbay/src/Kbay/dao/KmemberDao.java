package Kbay.dao;
import java.io.Reader;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import Kbay.model.Kmember;
public class KmemberDao {
	// 싱글톤 생성
	// KmemberDao값을 instance값으로 새롭게 생성 
	private static KmemberDao instance = new KmemberDao();
	private KmemberDao() {}
	// instance값을 받아서 다시 instance값으로 보내줌
	public static KmemberDao getInstance() {
		return instance;
	}
	
	private static SqlSession session;
		static {
			try {
			// 파일을 읽어 옴
			Reader reader = Resources.getResourceAsReader("/configuration.xml");
			// 객체 생성 - 저장할 공간을 만들어 줌 
			// 위의 reader를 build하겠다
			SqlSessionFactory ssf = new SqlSessionFactoryBuilder().build(reader);
			session = ssf.openSession(true);
			// true를 쓰면 오토 커밋(자동 저장)
			reader.close(); 
			//읽은거 닫아줌
			} catch (Exception e) { // exception을 잡기 위해서 상위 양식 {
				System.out.println("Session 생성시 에러:"+e.getMessage());
			  }
		}
		
		
		/*---------------------Login---------------------------*/
		//LoginAction파일에서 jsp에서 받아 온 id와 db의 id가 같은지 체크
	public int check(String id, String pass) {
		// check의 결과를 o이라 함
		int result = 0;
		
		System.out.println("KmemberDao = " + id + ", " + pass);
		
		// kmember.xml의 kmemberns에서 id 중 하나를 가져와 dbPass라 함
		Kmember kmember = (Kmember) session.selectOne("kmemberns.select", id);
		
		System.out.println("Kmember select ok");
		
		// 만약 dbPass가 없거나 공란이면 result = -1 : id없음
		if(kmember.getPass() == null || kmember.getPass().equals("")) {
			result = -1;
		// 아니면 비밀번호가 dbPass와 같으면 result = 1 : 
		} else if(pass.equals(kmember.getPass()))
			result = 1;
		// id가 없지도 않고=id가 있고, pass도 맞으면 = 0 : 로그인 성공?
		else
			result = 0;
		return result;
		}

	public Kmember findId(String tel) {
		// 어떤 sql문만 선택할건지 because 하나만 선택하므로
		return (Kmember) session.selectOne("kmemberns.findId", tel);
		
	}
	public Kmember findPass(String id) {
		return (Kmember) session.selectOne("kmemberns.findPass", id);
	}
	
	/*---------------------Join---------------------------*/
	
	public int insert(Kmember kme) {
		return (int) session.insert("kmemberns.insert", kme);
	}
	public int IdConfirm(String id) {
		int result = 0;
		String str = (String) session.selectOne("kmemberns.idConfirm", id);
	// 만약 DB kmemberns에 id가 null이 아닐 경우(id가 있을 때), 결과값은 1
		if (str != null)
			result = 1;
		return result;
	}
	
	/*---------------------Update---------------------------*/
	public int update(Kmember kme) {
		return session.update("kmemberns.update", kme);
	}
	
	/*---------------------View---------------------------*/
	public Kmember select(String id) {
		return (Kmember) session.selectOne("kmemberns.select", id);
	}
	public int delete(String id) {
		return session.update("kmemberns.delete", id);
	}

}
