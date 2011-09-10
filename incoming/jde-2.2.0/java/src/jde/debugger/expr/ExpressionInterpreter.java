package jde.debugger.exp;

import jde.parser.visitor.ObjectDepthFirst;
import jde.parser.JavaParser;
import jde.parser.syntaxtree.*;
import jde.parser.ParseException;
import jde.parser.TokenMgrError;
import java.io.StringReader;
import com.sun.jdi.VirtualMachine;
import java.util.Vector;

public class ExpressionInterpreter extends ObjectDepthFirst {

  public ExpressionInterpreter(VirtualMachine vm) {
  }

  public Object eval(String expr) {
    JavaParser parser = new JavaParser(new StringReader(expr));
    System.out.println(expr);
    try {
      Node root = parser.Expression();
      System.out.println("Java Parser Version 1.1:  Java program parsed successfully.");
      Vector result = new Vector(1);
      root.accept(this, result);
      return result.get(0);
    }
    catch (ParseException e) {
      System.out.println(e.getMessage());
      System.out.println("Java Parser Version 1.1:  Encountered errors during parse.");
    }
    catch (TokenMgrError lexError) {
      System.out.println("Lexical error.");
    }

    return null;

  }

     /**
    * <PRE>
    * nodeToken -> &lt;IDENTIFIER&gt;
    * nodeListOptional -> ( "." &lt;IDENTIFIER&gt; )*
    * </PRE>
    */
   public Object visit(Name n, Object argu) {
      Object _ret=null;
      n.nodeToken.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      if (false) {
	throw new Error("Name not bound");
      } // end of if ()
      
      ((Vector) argu).add(n.nodeToken.tokenImage);
      return n.nodeToken.tokenImage;
   }

  public static void main(String args[]) {
    // System.out.println("Java Parser Version 1.1:  Reading from standard input . . .");
    // parser = new JavaParser(System.in);

    ExpressionInterpreter interpreter = new ExpressionInterpreter(null);

    try {
      Object result = interpreter.eval("msg");
      System.out.println("Result: " + result);
    }
    catch (Error error) {
      System.out.println(error);
    }
  }

  VirtualMachine vm = null;


}
