/*
 *    Completion.java
 *    Copyright (C) 1999 Rodrigo Reyes (reyes@chez.com)
 *
 *    $Revision: 1.3 $
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

import java.lang.reflect.Modifier;
import java.lang.reflect.Field;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

/**
 * This class provides completion facilities.
 *
 * @author Rodrigo Reyes (reyes@chez.com) 
 */

public class Completion {

  private static void listClassInfo(Class c) 
  {
    System.out.println("(list ");

    Field[] fields = c.getFields();
    System.out.print("(list ");
    for (int index=0; index<fields.length; index++) {
      Field field = fields[index];
      if (Modifier.isPublic(field.getModifiers()))
  	 System.out.print("(list \""+field.getName()+"\" \""+
			  className(field.getType())+"\")");
    }
    System.out.println(")");

	
    Constructor[] constrs = c.getDeclaredConstructors();
    System.out.print("(list ");
    for (int index=0; index<constrs.length; index++) {
      Constructor constructor = constrs[index];
      if (Modifier.isPublic(constructor.getModifiers())) {
	System.out.print("(list \"");
	System.out.print(constructor.getName()+"\" ");
	listClassArray(constructor.getParameterTypes());
	System.out.print(")");
      }
      }
    System.out.print(")");

	
    Method[] methods = c.getMethods();
    System.out.println("(list ");
    for (int index=0; index<methods.length; index++) {
      Method method = methods[index];
      if (Modifier.isPublic(method.getModifiers())) {

	System.out.print("(list \"");
	System.out.print(method.getName()+"\" \"");
			
	System.out.print(className(method.getReturnType()) + "\" ");
			
	listClassArray(method.getParameterTypes());
	System.out.print(")");
      }
      }
    System.out.println(")");
	
    System.out.println(")");
    return;
  }
	
  public static void getClassInfo(String className)
  {
    try {
	    
      Class c = Class.forName(className);
      if (c != null)
	listClassInfo(c);
    } catch (ClassNotFoundException cnfe) { }
  }

  /**
   * Looks up an unqualified class name in the class path to find possible
   * fully qualified matches.
   *
   * @param className a value of type 'String'
   */
  public static void getClassInfo(String className, String[]imports) 
  {
    //	System.out.println("length : " + imports.length);
    for (int i=0; i<imports.length; i++)
      {
	String name = imports[i]+className;
	try {
	  Class c = Class.forName(name);
	  if (c != null)
	    {
	      listClassInfo(c);
	    }
	} catch (ClassNotFoundException cnfe) { }
		
      }
    System.out.println("nil");
  }

  static String className(Class c)
  {
    if (c.isArray())
      return c.getComponentType().getName() + "[]";
    else
      return c.getName();
  }
    
  static void listClassArray(Class[] classes)
  {
    //	System.out.println("(list ");
    for (int i=0; i<classes.length; i++)
      {
	System.out.print("\"");
	System.out.print(className(classes[i]));
	System.out.print("\" ");
      }
    //	System.out.println(")");
  }

} // Completion

/*
 * $Log: Completion.java,v $
 * Revision 1.3  2000/07/27 04:49:52  paulk
 * Now returns the type as well as name of each public field of a class. Thanks to Stephane Nicolas <s.nicolas@videotron.ca>.
 *
 * Revision 1.2  2000/02/09 04:48:41  paulk
 * Now uses Modifier.isPublic() to test whether a class's fields,
 * methods, and constructors are public and hence candidates for
 * completion. Now gets all fields and methods, not just those declared
 * by the class.
 *
 */

// End of Completion.java
