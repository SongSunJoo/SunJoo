package com.ch.stat.controller;

import java.io.File;

import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import com.github.rcaller.rStuff.RCaller;
import com.github.rcaller.rStuff.RCode;

@Controller
public class RController {
	@RequestMapping("survey2R_heightR")
	public String survey2R_heightR(int user_id, int height_value, Model model) {
		System.out.println("R Controller Start");

		RCaller caller = new RCaller();
		RCode code = new RCode();
		caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
		code.clear();
		caller.setRCode(code);

		try {
			File file;
			file = code.startPlot();
			System.out.println("graph : " + file.getPath());
			System.out.println(file);
			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			/*
			 * code.addRCode("data(\"cars\")"); code.addRCode("str(cars)");
			 * code.addRCode("plot(cars)");
			 */
//			code.addRCode("library(RJDBC)"); 
			code.addRCode(
					"drv<-JDBC(driverClass=\"oracle.jdbc.OracleDriver\",classPath=\"C:\\\\Program Files\\\\Java\\\\jdk1.8.0_181\\\\jre\\\\lib\\\\ext/ojdbc6.jar\")");
			code.addRCode("conn<-dbConnect(drv,\"jdbc:oracle:thin:@127.0.0.1:1521:xe\",\"scott\",\"tiger\")");
			code.addRCode("mysql<-\"select count(height_value)  from statUser where height_value < 150\"");
			code.addRCode(
					"mysql2<-\"select count(height_value) from statUser where height_value >= 150 and height_value < 155\"");
			code.addRCode(
					"mysql3<-\"select count(height_value) from statUser where height_value >= 155 and height_value < 160\"");
			code.addRCode(
					"mysql4<-\"select count(height_value) from statUser where height_value >= 160 and height_value < 165\"");
			code.addRCode(
					"mysql5<-\"select count(height_value) from statUser where height_value >= 165 and height_value < 170\"");
			code.addRCode(
					"mysql6<-\"select count(height_value) from statUser where height_value >= 170 and height_value < 175\"");
			code.addRCode(
					"mysql7<-\"select count(height_value) from statUser where height_value >= 175 and height_value < 180\"");
			code.addRCode(
					"mysql8<-\"select count(height_value) from statUser where height_value >= 180 and height_value < 185\"");
			code.addRCode("mysql9<-\"select count(height_value) from statUser where height_value >= 185\"");
			code.addRCode("a<-dbGetQuery(conn, mysql)");
			code.addRCode("b<-dbGetQuery(conn, mysql2)");
			code.addRCode("c<-dbGetQuery(conn, mysql3)");
			code.addRCode("d<-dbGetQuery(conn, mysql4)");
			code.addRCode("e<-dbGetQuery(conn, mysql5)");
			code.addRCode("f<-dbGetQuery(conn, mysql6)");
			code.addRCode("g<-dbGetQuery(conn, mysql7)");
			code.addRCode("h<-dbGetQuery(conn, mysql8)");
			code.addRCode("i<-dbGetQuery(conn, mysql9)");
			code.addRCode("m <- matrix(c(a,b,c,d,e,f,g,h,i))");
			code.addRCode(
					"plot(m, type = 'h', axes = FALSE, ann = FALSE, ylab = \" \", xlab = \" \", ylim = c(0, 20))");
			code.addRCode(
					"axis(1, at = 1:9, lab = c(\'150\', \'150~155\', \'155~160\', \'160~165\', \'165~170\', \'170~175\', \'175~180\', \'180~185\', \'185\'), las = 2)");
			code.addRCode("axis(2, ylim = c(0, 20))");
			code.addRCode("points(m, pch = 19, col = \"black\")");
			code.addRCode("title(main = \'User Height\', col.main = \'blue\', font.main = 4)");
			code.addRCode("for (i in 1:9) {\r\n" + 
					"  text(i, m[i], m[i], pos = 4)\r\n" + 
					"}");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			String sourceFilePath = file.getPath();
			System.out.println("sourceFilePath : " + sourceFilePath);
			String destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			String fileName = "HeightChart";
			File destinationPathObject = new File(destinationPath);
			File sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);
				}
				model.addAttribute("user_id", user_id);
				model.addAttribute("height_value", height_value);
//				model.addAttribute("destinationPath", destinationPath);
//				model.addAttribute("fileName", fileName);
//				model.addAttribute("sourceFilePath", sourceFilePath);
//				model.addAttribute("statusFileNameObject", statusFileNameObject);
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

		return "survey/survey2R_height";
	}

	@RequestMapping("survey3R_lastNameR")
	public String survey3R_lastNameR(int user_id, String lastName, Model model) {
		System.out.println("R Controller Start survey3R");

		RCaller caller = new RCaller();
		RCode code = new RCode();
		caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
		code.clear();
		caller.cleanRCode();

		try {
			File file;
			file = code.startPlot();
			System.out.println("getpath : " + file.getPath());
			System.out.println(file);

			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("plotrix");
			code.R_require("dplyr");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"q <- \"select ln_value, count(ln_value) lnCount from statUser where ln_value is not null group by ln_value\"");
			code.addRCode("q <- dbGetQuery(conn, q)");
			code.addRCode("q_labels1 <- q$LN_VALUE");
			code.addRCode("q_labels1 <- paste(q_labels1, q$LNCOUNT)");
//			code.addRCode("q_labels1 <- paste(q_labels1, \"People\", sep=\"\")");
			code.addRCode("pie3D(q$LNCOUNT, labels = q_labels1, explode = 0.1)");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			String sourceFilePath = file.getPath();
			System.out.println("sourceFilePath : " + sourceFilePath);
			String destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			String fileName = "LastNameChart";
			File destinationPathObject = new File(destinationPath);
			File sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
				model.addAttribute("user_id", user_id);
				model.addAttribute("lastName", lastName);
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

		return "survey/survey3R_lastName";

	}

	@RequestMapping("survey4R_animalR")
	public String survey4R_animalR(int user_id, String pet_value, Model model) {
		System.out.println("R Controller Start survey4R");

		RCaller caller = new RCaller();
		RCode code = new RCode();
		caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
		code.clear();
		caller.cleanRCode();

		try {
			File file;
			file = code.startPlot();
			System.out.println("getpath : " + file.getPath());
			System.out.println(file);

			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("plyr");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"q <- \"select pet_value, count(pet_value) pet from statUser where pet_value is not null group by pet_value\"");
			code.addRCode("q <- dbGetQuery(conn, q)");
			code.addRCode("qd <- data.frame(q)");
			code.addRCode("q2 <- \"select petDB_id, petDB_value pet_value from petDB\"");
			code.addRCode("q2 <- dbGetQuery(conn, q2)");
			code.addRCode("q2d <- data.frame(q2)");
			code.addRCode("q3 <- merge(qd, q2d, by = 'PET_VALUE', all = FALSE)");
			code.addRCode("q3 <- data.frame(merge(qd, q2d, by = 'PET_VALUE', all = FALSE))");
			code.addRCode("q4 <- arrange(q3, PETDB_ID)");
			code.addRCode("barplot <- barplot(q4$PET, beside = T, names.arg = q4$PET_VALUE, las = 2)");
			code.addRCode("text(x = barplot, y = q4$PET * 0.95, labels = q4$PET, cex = 0.7)");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			String sourceFilePath = file.getPath();
			System.out.println("sourceFilePath : " + sourceFilePath);
			String destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			String fileName = "PetChart";
			File destinationPathObject = new File(destinationPath);
			File sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
				model.addAttribute("user_id", user_id);
				model.addAttribute("pet_value", pet_value);
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

		return "survey/survey4R_animal";

	}

	@RequestMapping("survey5R_tourR")
	public String survey5R_tourR(int user_id, String tour_nation, Model model) {
		System.out.println("R Controller Start survey5R");

		RCaller caller = new RCaller();
		RCode code = new RCode();
		caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
		code.clear();
		caller.cleanRCode();

		try {
			File file;
			file = code.startPlot();
			System.out.println("getpath : " + file.getPath());
			System.out.println(file);

			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("ggplot2");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode("q <- \"select td.tourDB_id1, count(su.tourDB_id1) count, td.tourDB_nation \r\n"
					+ "from statUser su, tourDB td where su.tourDB_id1 = td.tourDB_id1\r\n"
					+ "group by su.tourDB_id1, td.tourDB_id1, td.tourDB_nation\"");
			code.addRCode("q <- dbGetQuery(conn, q)");
			code.addRCode(
					"ggplot(data = q, aes(x = q$TOURDB_NATION, y = q$COUNT)) + geom_col() + labs(x = 'Nation', y = 'People') + geom_text(aes(label=q$COUNT), size = 3, vjust = 1.25, color = '#FFFFFF')");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			String sourceFilePath = file.getPath();
			System.out.println("sourceFilePath : " + sourceFilePath);
			String destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			String fileName = "TourChart";
			File destinationPathObject = new File(destinationPath);
			File sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
				model.addAttribute("user_id", user_id);
				model.addAttribute("tour_nation", tour_nation);
				System.out.println("tour_nation R : " + tour_nation);
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

		return "survey/survey5R_tour";

	}

	@RequestMapping("survey6R_oldR")
	public String survey6R_oldR(int user_id, String old_value, Model model) {
		System.out.println("R Controller Start survey6R");

		RCaller caller = new RCaller();
		RCode code = new RCode();
		caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
		code.clear();
		caller.cleanRCode();

		try {
			File file;
			file = code.startPlot();
			System.out.println("getpath : " + file.getPath());
			System.out.println(file);

			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("ggplot2");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"q <- \"select o.oldDB_id, count(s.oldDB_id) count, o.oldDB_value from statUser s, oldDB o\r\n"
							+ "where o.oldDB_id = s.oldDB_id\r\n" + "group by o.oldDB_id, s.oldDB_id, o.oldDB_value\"");
			code.addRCode("q <- dbGetQuery(conn, q)");
			code.addRCode(
					"ggplot(data = q, aes(x = q$OLDDB_VALUE, y = q$COUNT)) + geom_col() + coord_flip() + geom_bar(stat = \"identity\") + labs(x = ' ', y = 'People') + geom_text(aes(label = q$COUNT), size = 2.5, hjust = 2.5, color = \"#FFFFFF\")");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			String sourceFilePath = file.getPath();
			System.out.println("sourceFilePath : " + sourceFilePath);
			String destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			String fileName = "OldChart";
			File destinationPathObject = new File(destinationPath);
			File sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
				model.addAttribute("user_id", user_id);
				model.addAttribute("old_value", old_value);
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

		return "survey/survey6R_old";

	}

	@RequestMapping("survey7R_digitalR")
	public String survey7R_digitalR(int user_id, String digital_value, Model model) {
		System.out.println("R Controller Start survey7R");

		RCaller caller = new RCaller();
		RCode code = new RCode();
		caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
		code.clear();
		caller.cleanRCode();

		try {
			File file;
			file = code.startPlot();
			System.out.println("getpath : " + file.getPath());
			System.out.println(file);

			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"q <- \"select d.digitalDB_id, count(s.digitalDB_id) digitalCount, digitalDB_value from digitalDB d, statUser s \r\n"
							+ "where s.digitalDB_id is not null and d.digitalDB_id = s.digitalDB_id\r\n"
							+ "group by d.digitalDB_id, s.digitalDB_id, digitalDB_value\r\n"
							+ "order by digitalDB_id\"");
			code.addRCode("q <- dbGetQuery(conn, q)");
			code.addRCode(
					"plot(q$DIGITALCOUNT, axes = F, type = \"h\", ylab = \"People\", xlab = \" \", ylim = c(0, 20))");
			code.addRCode(
					"axis(1, at = 1:10, lab = c(\"Phone\", \"TV\", \"Computer\", \"Game\", \"Movie\", \"Cleaning\", \"Excercise\", \"Meeting\", \"Sleeping\", \"Book\"), las = 2)");
			code.addRCode("axis(2)");
			code.addRCode("points(q$DIGITALCOUNT, pch = 19, col = \"black\")");
			code.addRCode("for (i in 1:10) {\r\n" + 
					"  text(i, q$DIGITALCOUNT[i], q$DIGITALCOUNT[i], pos = 4)\r\n" + 
					"}");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			String sourceFilePath = file.getPath();
			System.out.println("sourceFilePath : " + sourceFilePath);
			String destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			String fileName = "DigitalChart";
			File destinationPathObject = new File(destinationPath);
			File sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
				model.addAttribute("user_id", user_id);
				model.addAttribute("digital_value", digital_value);
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

		return "survey/survey7R_digital";

	}

	@RequestMapping("survey8R_contentR")
	public String survey8R_contentR(int user_id, String mobile_value, Model model) {
		System.out.println("R Controller Start survey8R");

		RCaller caller = new RCaller();
		RCode code = new RCode();
		caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
		code.clear();
		caller.cleanRCode();

		try {
			File file;
			file = code.startPlot();
			System.out.println("getpath : " + file.getPath());
			System.out.println(file);

			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("ggplot2");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"q <- \"select m.mobileDB_id, count(s.mobileDB_id) mobileCount, mobileDB_value from statUser s, mobileDB m\r\n"
							+ "where s.mobileDB_id is not null and s.mobileDB_id = m.mobileDB_id\r\n"
							+ "group by m.mobileDB_id, s.mobileDB_id, mobileDB_value\r\n" + "order by mobileDB_id\"");
			code.addRCode("q <- dbGetQuery(conn, q)");
			code.addRCode(
					"ggplot(data = q, aes(x = q$MOBILEDB_VALUE, y = q$MOBILECOUNT)) + geom_bar(stat = 'identity') + coord_flip() + labs(y = 'People', x = 'Mobile Contents') + geom_text(aes(label=q$MOBILECOUNT), size = 3, hjust = 1.25, color = '#FFFFFF')");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			String sourceFilePath = file.getPath();
			System.out.println("sourceFilePath : " + sourceFilePath);
			String destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			String fileName = "ContentChart";
			File destinationPathObject = new File(destinationPath);
			File sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
				model.addAttribute("user_id", user_id);
				model.addAttribute("mobile_value", mobile_value);
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

		return "survey/survey8R_content";

	}

	@RequestMapping("survey9R_transR")
	public String survey9R_transR(int user_id, String trans_value, Model model) {
		System.out.println("R Controller Start survey9R");

		RCaller caller = new RCaller();
		RCode code = new RCode();
		caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
		code.clear();
		caller.cleanRCode();

		try {
			File file;
			file = code.startPlot();
			System.out.println("getpath : " + file.getPath());
			System.out.println(file);

			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("ggplot2");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"q <- \"select tf.transDB_id, count(s.transDB_id) transCount, tf.transDB_value from statUser s, transferDB tf \r\n"
							+ "where s.transDB_id is not null and tf.transDB_id = s.transDB_id\r\n"
							+ "group by tf.transDB_id, s.transDB_id, tf.transDB_value\"");
			code.addRCode("q <- dbGetQuery(conn, q)");
			code.addRCode(
					"ggplot(data = q, aes(x = q$TRANSDB_VALUE, y = q$TRANSCOUNT)) + geom_bar(stat = 'identity') + coord_flip() + labs(y = 'People', x = ' ') + geom_text(aes(label=q$TRANSCOUNT), size = 3, hjust = 1.25, color = '#FFFFFF')");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			String sourceFilePath = file.getPath();
			System.out.println("sourceFilePath : " + sourceFilePath);
			String destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			String fileName = "TransChart";
			File destinationPathObject = new File(destinationPath);
			File sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
				model.addAttribute("user_id", user_id);
				model.addAttribute("trans_value", trans_value);
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

		return "survey/survey9R_trans";

	}

	@RequestMapping("survey10R_happyR")
	public String survey10R_happyR(int user_id, String happy_value, Model model) {
		System.out.println("R Controller Start survey10R");

		RCaller caller = new RCaller();
		RCode code = new RCode();
		caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
		code.clear();
		caller.cleanRCode();

		try {
			File file;
			file = code.startPlot();
			System.out.println("getpath : " + file.getPath());
			System.out.println(file);

			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"q <- \"select s.happyDB_id, count(ha.happyDB_id) happyCount, ha.happyDB_value from statUser s, happyDB ha where ha.happyDB_id is not null and s.happyDB_id = ha.happyDB_id\r\n"
							+ "group by s.happyDB_id, ha.happyDB_id, ha.happyDB_value\"");
			code.addRCode("q <- dbGetQuery(conn, q)");
			code.addRCode("People <- q$HAPPYCOUNT");
			code.addRCode("happy <- q$HAPPYDB_VALUE");
			code.addRCode("People <- paste(happy, People)");
			code.addRCode("pie(q$HAPPYCOUNT, labels = People, cex = 0.8, col = rainbow(length(q$HAPPYCOUNT)))");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			String sourceFilePath = file.getPath();
			System.out.println("sourceFilePath : " + sourceFilePath);
			String destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			String fileName = "HappyChart";
			File destinationPathObject = new File(destinationPath);
			File sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
				model.addAttribute("user_id", user_id);
				model.addAttribute("happy_value", happy_value);
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

		return "survey/survey10R_happy";
	}

	@RequestMapping("survey12_happyRel")
	public String survey12_happyRel() {
		System.out.println("R Controller Start survey12R");

		RCaller caller = new RCaller();
		RCode code = new RCode();
		caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
		code.clear();
		caller.cleanRCode();

		try {
			File file;
			file = code.startPlot();
			System.out.println("getpath : " + file.getPath());
			System.out.println(file);
			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("dplyr");
			code.R_require("ggplot2");
			// 1. 성별 행복 지수
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"mysql <- \"select s.user_gender, count(s.happyDB_id) happyCount, h.happyDB_value from statUser s, happyDB h\r\n"
							+ "where s.happyDB_id = h.happyDB_id group by s.user_gender, s.happyDB_id, h.happyDB_value\"");
			code.addRCode("mysql <- dbGetQuery(conn, mysql)");
			code.addRCode("mysql");
			code.addRCode(
					"ggplot(data = mysql, aes(x = mysql$USER_GENDER, y = mysql$HAPPYCOUNT, fill = mysql$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = 'people', fill = ' ')");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			String sourceFilePath = file.getPath();
			System.out.println("sourceFilePath : " + sourceFilePath);
			String destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			String fileName = "happyRel1";
			File destinationPathObject = new File(destinationPath);
			File sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
			}
			// 2. 연령대 행복지수
			caller = new RCaller();
			code = new RCode();
			caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
			code.clear();
			caller.cleanRCode();
			File file2;
			file2 = code.startPlot();
			System.out.println("getpath2 : " + file2.getPath());
			System.out.println(file2);
			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("dplyr");
			code.R_require("ggplot2");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h \r\n"
					+ "where h.happyDB_id = s.happyDB_id and user_age >= 10 and user_age < 20 and s.happyDB_id is not null \r\n"
					+ "group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(age = '10')");
			code.addRCode("groupCnt1 <- groupCnt %>% mutate(pct=round(CNT/sum\r\n" + "(CNT)*100, 1))");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h \r\n"
					+ "where h.happyDB_id = s.happyDB_id and user_age >= 20 and user_age < 30 and s.happyDB_id is not null \r\n"
					+ "group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(age = '20')");
			code.addRCode("groupCnt2 <- groupCnt %>% mutate(pct=round(CNT/sum\r\n" + "(CNT)*100, 1))");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h \r\n"
					+ "where h.happyDB_id = s.happyDB_id and user_age >= 30 and user_age < 40 and s.happyDB_id is not null \r\n"
					+ "group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(age = '30')");
			code.addRCode("groupCnt3 <- groupCnt %>% mutate(pct=round(CNT/sum\r\n" + "(CNT)*100, 1))");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h \r\n"
					+ "where h.happyDB_id = s.happyDB_id and user_age >= 40 and user_age < 50 and s.happyDB_id is not null \r\n"
					+ "group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(age = '40')");
			code.addRCode("groupCnt4 <- groupCnt %>% mutate(pct=round(CNT/sum\r\n" + "(CNT)*100, 1))");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h \r\n"
					+ "where h.happyDB_id = s.happyDB_id and user_age >= 50 and s.happyDB_id is not null \r\n"
					+ "group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(age = '50')");
			code.addRCode("groupCnt5 <- groupCnt %>% mutate(pct=round(CNT/sum\r\n" + "(CNT)*100, 1))");
			code.addRCode("groupResult <- rbind(groupCnt1, groupCnt2, groupCnt3, \r\n" + "groupCnt4, groupCnt5)");
			code.addRCode("ggplot(data = groupResult, aes(x=age, y=pct, fill=groupResult$HAPPYDB_VALUE)) + \r\n"
					+ "geom_col() + coord_flip() + labs(x = 'age', y = 'percent', fill = ' ')");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			sourceFilePath = file2.getPath();
			System.out.println("sourceFilePath2 : " + sourceFilePath);
			destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			fileName = "happyRel2";
			destinationPathObject = new File(destinationPath);
			sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
			}

			// 3. 키 행복지수 - 남자
			caller = new RCaller();
			code = new RCode();
			caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
			code.clear();
			caller.cleanRCode();
			File file3;
			file3 = code.startPlot();
			System.out.println("getpath3 : " + file3.getPath());
			System.out.println(file3);
			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("dplyr");
			code.R_require("ggplot2");
			code.addRCode("drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h\r\n" + 
					"where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value < 150 and s.user_gender = 'man'\r\n" + 
					"group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(height = '150')");
			code.addRCode("groupCnt1 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h\r\n" + 
					"where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 150 and s.height_value < 160 and s.user_gender = 'man'\r\n" + 
					"group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(height = '150~160')");
			code.addRCode("groupCnt2 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h\r\n" + 
					"where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 160 and s.height_value < 170 and s.user_gender = 'man'\r\n" + 
					"group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(height = '160~170')");
			code.addRCode("groupCnt3 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h\r\n" + 
					"where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 170 and s.height_value < 180 and s.user_gender = 'man'\r\n" + 
					"group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(height = '170~180')");
			code.addRCode("groupCnt4 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h\r\n" + 
					"where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 180 and s.user_gender = 'man'\r\n" + 
					"group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(height = '180')");
			code.addRCode("groupCnt5 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))");
			code.addRCode("groupResult <- rbind(groupCnt2, groupCnt3, groupCnt4, groupCnt5)");
			code.addRCode("ggplot(data = groupResult, aes(x=height, y=pct, fill=groupResult$HAPPYDB_VALUE)) + geom_col() + coord_flip() + ggtitle('Men') + labs(x = 'height(cm)', y = 'percent', fill = ' ')");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			sourceFilePath = file3.getPath();
			System.out.println("sourceFilePath3 : " + sourceFilePath);
			destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			fileName = "happyRel3";
			destinationPathObject = new File(destinationPath);
			sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
			}

			// 3. 키 행복지수 - 여
			caller = new RCaller();
			code = new RCode();
			caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
			code.clear();
			caller.cleanRCode();
			File file32;
			file32 = code.startPlot();
			System.out.println("getpath3-2 : " + file32.getPath());
			System.out.println(file32);
			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("dplyr");
			code.R_require("ggplot2");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h\r\n" + 
					"where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value < 150 and s.user_gender = 'woman'\r\n" + 
					"group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(height = '150')");
			code.addRCode("groupCnt1 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h\r\n" + 
					"where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 150 and s.height_value < 160 and s.user_gender = 'woman'\r\n" + 
					"group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(height = '150~160')");
			code.addRCode("groupCnt2 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h\r\n" + 
					"where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 160 and s.height_value < 170 and s.user_gender = 'woman'\r\n" + 
					"group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(height = '160~170')");
			code.addRCode("groupCnt3 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h\r\n" + 
					"where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 180 and s.user_gender = 'woman'\r\n" + 
					"group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("sql1 <- \"select h.happyDB_value, count(*) cnt from statUser s, happyDB h\r\n" + 
					"where h.happyDB_id = s.happyDB_id and s.height_value is not null and s.happyDB_id is not null and s.height_value >= 170 and s.height_value < 180 and s.user_gender = 'woman'\r\n" + 
					"group by h.happyDB_value, s.happyDB_id\"");
			code.addRCode("groupCnt <- dbGetQuery(conn, sql1)");
			code.addRCode("groupCnt <- groupCnt %>% mutate(height = '170~180')");
			code.addRCode("groupCnt4 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))");
			code.addRCode("groupCnt <- groupCnt %>% mutate(height = '180')");
			code.addRCode("groupCnt5 <- groupCnt %>% mutate(pct=round(CNT/sum(CNT)*100, 1))");
			code.addRCode("groupResult <- rbind(groupCnt2, groupCnt3)");
			code.addRCode("ggplot(data = groupResult, aes(x=height, y=pct, fill=groupResult$HAPPYDB_VALUE)) + geom_col() + coord_flip() + ggtitle('Women') + labs(x = 'height(cm)', y = 'percent', fill = ' ')");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			
		/*	 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			sourceFilePath = file32.getPath();
			System.out.println("sourceFilePath3-2 : " + sourceFilePath);
			destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			fileName = "happyRel3-2";
			destinationPathObject = new File(destinationPath);
			sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
			}

			// 4. 성씨 행복지수
			caller = new RCaller();
			code = new RCode();
			caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
			code.clear();
			caller.cleanRCode();
			File file4;
			file4 = code.startPlot();
			System.out.println("getpath4 : " + file4.getPath());
			System.out.println(file4);
			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("dplyr");
			code.R_require("ggplot2");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"q <- \"select h.happyDB_id,s.ln_value, count(s.happyDB_id) happyCount, h.happyDB_value from statUser s, happyDB h\r\n"
							+ "where h.happyDB_id = s.happyDB_id and ln_value is not null group by s.ln_value, h.happyDB_id, s.happyDB_id, h.happyDB_value\"");
			code.addRCode("q <- dbGetQuery(conn, q)");
			code.addRCode("q");
			code.addRCode("str(q)");
			code.addRCode("list_ln <- q[, c(2:4)]");
			code.addRCode("list_ln");
			code.addRCode(
					"ggplot(data = list_ln, aes(x = list_ln$LN_VALUE, y = list_ln$HAPPYCOUNT, fill = list_ln$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = 'people', fill = ' ')");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			sourceFilePath = file4.getPath();
			System.out.println("sourceFilePath4 : " + sourceFilePath);
			destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			fileName = "happyRel4";
			destinationPathObject = new File(destinationPath);
			sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
			}

			// 5. 반려동물 행복지수
			caller = new RCaller();
			code = new RCode();
			caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
			code.clear();
			caller.cleanRCode();
			File file5;
			file5 = code.startPlot();
			System.out.println("getpath5 : " + file5.getPath());
			System.out.println(file5);
			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("dplyr");
			code.R_require("ggplot2");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"sql <- \"select s.pet_value, count(s.happyDB_id) happyCnt, h.happyDB_value from statUser s, happyDB h \r\n"
							+ "where s.happyDB_id = h.happyDB_id and pet_value is not null group by s.pet_value, s.happyDB_id, h.happyDB_value\"");
			code.addRCode("sql <- dbGetQuery(conn, sql)");
			code.addRCode("sql");
			code.addRCode(
					"ggplot(data = sql, aes(x = sql$PET_VALUE, y = sql$HAPPYCNT, fill = sql$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = 'people', fill = ' ')");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			sourceFilePath = file5.getPath();
			System.out.println("sourceFilePath5 : " + sourceFilePath);
			destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			fileName = "happyRel5";
			destinationPathObject = new File(destinationPath);
			sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
			}

			// 6. 가고 싶은 여행지 행복지수
			caller = new RCaller();
			code = new RCode();
			caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
			code.clear();
			caller.cleanRCode();
			File file6;
			file6 = code.startPlot();
			System.out.println("getpath6 : " + file6.getPath());
			System.out.println(file6);
			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("dplyr");
			code.R_require("ggplot2");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"tourSql <- \"select t.tourDB_nation, count(s.happyDB_id) happyCount, h.happyDB_value from statUser s, happyDB h, tourDB t\r\n"
							+ "where s.tourDB_id1 = t.tourDB_id1 and s.happyDB_id = h.happyDB_id and s.tourDB_id1 is not null and s.happyDB_id is not null\r\n"
							+ "group by t.tourDB_nation, s.happyDB_id, h.happyDB_value\"");
			code.addRCode("tourSql <- dbGetQuery(conn, tourSql)");
			code.addRCode("tourSql");
			code.addRCode(
					"ggplot(data = tourSql, aes(x = tourSql$TOURDB_NATION, y = tourSql$HAPPYCOUNT, fill = tourSql$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = 'people', fill = ' ')");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			sourceFilePath = file6.getPath();
			System.out.println("sourceFilePath6 : " + sourceFilePath);
			destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			fileName = "happyRel6";
			destinationPathObject = new File(destinationPath);
			sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
			}

			// 7. 여가 행복지수
			caller = new RCaller();
			code = new RCode();
			caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
			code.clear();
			caller.cleanRCode();
			File file7;
			file7 = code.startPlot();
			System.out.println("getpath7 : " + file7.getPath());
			System.out.println(file7);
			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("dplyr");
			code.R_require("ggplot2");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"digitalSql <- \"select d.digitalDB_value, count(s.happyDB_id) happyCount, h.happyDB_value from statUser s, happyDB h, digitalDB d\r\n"
							+ "where s.digitalDB_id = d.digitalDB_id and s.happyDB_id = h.happyDB_id and s.digitalDB_id is not null and s.happyDB_id is not null\r\n"
							+ "group by d.digitalDB_value, s.happyDB_id, h.happyDB_value\"");
			code.addRCode("digitalSql <- dbGetQuery(conn, digitalSql)");
			code.addRCode("digitalSql");
			code.addRCode(
					"ggplot(data = digitalSql, aes(x = digitalSql$DIGITALDB_VALUE, y = digitalSql$HAPPYCOUNT, fill = digitalSql$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = 'people', fill = ' ')");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			sourceFilePath = file7.getPath();
			System.out.println("sourceFilePath7 : " + sourceFilePath);
			destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			fileName = "happyRel7";
			destinationPathObject = new File(destinationPath);
			sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
			}

			// 8. 여가 행복지수
			caller = new RCaller();
			code = new RCode();
			caller.setRscriptExecutable("C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe");
			code.clear();
			caller.cleanRCode();
			File file8;
			file8 = code.startPlot();
			System.out.println("getpath8 : " + file8.getPath());
			System.out.println(file8);
			code.R_require("DBI");
			code.R_require("rJava");
			code.R_require("RJDBC");
			code.R_require("dplyr");
			code.R_require("ggplot2");
			code.addRCode(
					"drv <- JDBC(driverClass=\"oracle.jdbc.OracleDriver\", classPath = \"C:/Program Files/Java/jdk1.8.0_181/jre/lib/ext/ojbdc6.jar\")");
			code.addRCode("conn <- dbConnect(drv, \"jdbc:oracle:thin:@127.0.0.1:1521:xe\", \"scott\", \"tiger\")");
			code.addRCode(
					"snsSql <- \"select m.mobileDB_value, count(s.happyDB_id) happyCount, h.happyDB_value from statUser s, happyDB h, mobileDB m\r\n"
							+ "where m.mobileDB_id = s.mobileDB_id and s.happyDB_id = h.happyDB_id and s.mobileDB_id is not null and s.happyDB_id is not null\r\n"
							+ "group by m.mobileDB_value, s.happyDB_id, h.happyDB_value\"");
			code.addRCode("snsSql <- dbGetQuery(conn, snsSql)");
			code.addRCode("snsSql");
			code.addRCode(
					"ggplot(data = snsSql, aes(x = snsSql$MOBILEDB_VALUE, y = snsSql$HAPPYCOUNT, fill = snsSql$HAPPYDB_VALUE)) + geom_col() + coord_flip() + labs(x = ' ', y = 'people', fill = ' ')");
			code.endPlot();
			caller.setRCode(code);
			caller.runOnly();
			/*
			 * test.FileMoving(file.getPath(),
			 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
			 */
			sourceFilePath = file8.getPath();
			System.out.println("sourceFilePath8 : " + sourceFilePath);
			destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
			fileName = "happyRel8";
			destinationPathObject = new File(destinationPath);
			sourceFilePathObject = new File(sourceFilePath);
			if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
			// both source and destination paths are available
			{
				// creating object for File class
				File statusFileNameObject = new File(destinationPath + "/" + fileName + ".png");
				if (statusFileNameObject.isFile())
				// Already file is exists in Destination path
				{
					statusFileNameObject.delete();
				}
				// File is not exists in Destination path.
				{
					// paste file from source to Destination path with fileName as value of fileName
					// argument
					FileUtils.copyFile(sourceFilePathObject, statusFileNameObject);

				}
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}
		return "survey/survey12_happyRel";
	}
}
