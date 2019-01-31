package com.ch.stat.model;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;

public class FileMove {
	
	static int serial = 0;
	public void FileMoving(String sourceFilePath,String destinationPath, String fileName) throws IOException {
		/*
		 * test.FileMoving(file.getPath(),
		 * "C:\\spring\\springSrc\\exR\\src\\main\\webapp\\WEB-INF\\img\\", "Rplot" );
		 */
/*		String sourceFilePath = file.getPath();
		System.out.println("sourceFilePath : " + sourceFilePath);
		String destinationPath = "C:\\spring\\springSrc\\.metadata\\.plugins\\org.eclipse.wst.server.core\\tmp1\\wtpwebapps\\stat\\fileSave";
		String fileName = "happyRel";*/
		File destinationPathObject = new File(destinationPath);
		File sourceFilePathObject = new File(sourceFilePath);
		if ((destinationPathObject.isDirectory()) && (sourceFilePathObject.isFile()))
		// both source and destination paths are available
		{
			// creating object for File class
			File statusFileNameObject = new File(destinationPath + "/" + fileName + (++serial) + ".png");
			System.out.println("serial = " + serial);
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
	}
}
