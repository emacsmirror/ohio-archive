/*
 *    JdeUtilities.java
 *    Copyright (C) 1999 Len Trigg (trigg@cs.waikato.ac.nz)
 *    Copyright (C) 1999 Paul Kinnucan (paulk@mathworks.com)
 *
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

package jde.util;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.zip.ZipFile;
import java.util.zip.ZipEntry;


/**
 * This class provides various utility methods.
 *
 * @author Len Trigg (trigg@cs.waikato.ac.nz) 
 * @author Paul Kinnucan (paulk@mathworks.com)
 * @version $Revision: 1.2 $
 */
public class JdeUtilities {

  /** Stores the list of all classes in the classpath */
  public static Vector CLASS_LIST = new Vector(500);
  
  /** Build the list of classes */
  static {

    // System.err.println("Making class list");
    buildClassList();

    //    System.err.println("Done (" + CLASS_LIST.size() + " classes)");

  }

  public static void buildClassList() {
    String classPath = System.getProperty("java.class.path");
    
    String classPathSeparator = File.pathSeparator;

    // For Java 2!
    String classPath2 = System.getProperty("sun.boot.class.path");
    if (classPath2 != null)
      classPath += classPathSeparator + classPath2;

    // Add all files from the extension directories to the classPath
    String extDirs = System.getProperty("java.ext.dirs");
    if (extDirs != null) {
      StringTokenizer st = new StringTokenizer(extDirs, classPathSeparator);

      // Iterate through extension directories
      while (st.hasMoreTokens()) {
	File[] extFiles = new File(st.nextToken()).listFiles();

	if (extFiles != null) {
	  // Iterate through files added them to classPath
	  for (int i = 0; i < extFiles.length; i++) {
	    classPath += classPathSeparator + extFiles[i].getPath();
	  }
	}
      }
    }

    // System.out.println("classpath = " + classPath);

    StringTokenizer st = new StringTokenizer(classPath, classPathSeparator);
    while (st.hasMoreTokens()) {
      String classPathEntry = st.nextToken();
      File classPathFile = new File(classPathEntry);
      if (classPathFile.exists()) {
	if (classPathEntry.toLowerCase().endsWith(".jar")) {
	  addClassesFromZip(CLASS_LIST, classPathFile);
	} else if (classPathEntry.toLowerCase().endsWith(".zip")) {
	  addClassesFromZip(CLASS_LIST, classPathFile);
	} else if (classPathFile.isDirectory()) {
	  addClassesFromDir(CLASS_LIST, classPathFile, classPathFile);
	}
      }
    }    
  }
  
    
  
  /**
   * Adds the classes from the supplied Zip file to the class list.
   *
   * @param classList the Vector to add the classes to
   * @param classPathFile the File to scan as a zip file
   */
  public static void addClassesFromZip(Vector classList,
				       File classPathFile) {
    // System.out.println("Processing jar/zip file: " + classPathFile);
    
    try {
      ZipFile zipFile = new ZipFile(classPathFile);
      Enumeration enum = zipFile.entries();
      while (enum.hasMoreElements()) {
	ZipEntry zipEntry = (ZipEntry)enum.nextElement();
	String current = zipEntry.getName();
	if (current.toLowerCase().endsWith(".class")) {
	  current = current.substring(0, current.length() - 6);
	  current = current.replace('/', '.');
	  current = current.replace('\\', '.');
	  classList.addElement(current);
	}
      }
    } catch (Exception ex) {
      System.err.println("Problem opening " + classPathFile + " with zip.");
    }
  }

  
  /**
   * Adds the classes from the supplied directory to the class list.
   *
   * @param classList the Vector to add the classes to
   * @param classPathFile the File to recursively scan as a directory
   */
  public static void addClassesFromDir(Vector classList,
				       File rootDir,
				       File currentDir) {
    
    String [] files = currentDir.list();
    for (int i = 0; i < files.length; i++) {
      String current = files[i];
      if (current.toLowerCase().endsWith(".class")) {
	current = current.substring(0, current.length() - 6);
	String rootPath = rootDir.getPath();
	String currentPath = currentDir.getPath();
	if (currentPath.indexOf(rootPath) != 0) {
	  System.err.println("currentPath doesn't start with rootPath!\n"
			     + "rootPath: " + rootPath + "\n"
			     + "currentPath: " + currentPath + "\n");
	} else {
	  String packageName = currentPath.substring(rootPath.length());
	  if (packageName.length() > 0) {
	    // Not the current directory
	    packageName = packageName.replace('\\', '.');
	    packageName = packageName.replace('/', '.');
	    classList.addElement(packageName.substring(1) + '.' + current);
	  } else {
	    // The current directory
	    classList.addElement(current);
	  }
	}
      } else {
	// Check if it's a directory to recurse into
	File currentFile = new File(currentDir, current);
	if (currentFile.isDirectory()) {
	  addClassesFromDir(classList, rootDir, currentFile);
	}
      }
    }
  }

  
  /**
   * Looks up an unqualified class name in the class path to find possible
   * fully qualified matches.
   *
   * @param className a value of type 'String'
   */
  public static void getQualifiedName(String className) {

    String qualifiedNameList = "(list";
    
    for (int i = 0; i < CLASS_LIST.size(); i++) {
      String testName = (String) CLASS_LIST.elementAt(i);
      
      if ((testName.length() > className.length() && testName.endsWith(className) &&
	   testName.charAt(testName.length() - className.length() - 1) == '.') ||
	  (testName.length() == className.length()) && testName.equals(className)) {
	
	// Avoid duplicates!
	testName = " \"" +  testName + "\"";
	if (qualifiedNameList.indexOf(testName) == -1)
          qualifiedNameList += testName;
      }
    }

    qualifiedNameList += ")";
    

    System.out.println(qualifiedNameList);
    System.out.flush();

  }


} // JdeUtilities

/*
 * $Log: JdeUtilities.java,v $
 * Revision 1.2  2000/06/22 02:27:25  paulk
 * Now adds Java extension classes to the class list. Thanks to Mark Gibson <mark@markg.co.uk>
 * for this enhancement.
 *
 * Revision 1.1.1.1  1999/08/20 01:19:51  paulk
 * Imported source
 *
 *
 */

// End of JdeUtilities.java
