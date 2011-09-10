package jde.debugger.exp;

import jde.parser.visitor.ObjectDepthFirst;
import jde.parser.JavaParser;
import jde.parser.syntaxtree.*;
import jde.parser.ParseException;
import jde.parser.TokenMgrError;
import java.io.StringReader;

public class ExpressionVisitor extends ObjectDepthFirst {

  public static void main(String args[]) {
    JavaParser parser;
    System.out.println("Java Parser Version 1.1:  Reading from standard input . . .");
    // parser = new JavaParser(System.in);
    String expr = "2+a";
    parser = new JavaParser(new StringReader(expr));
    System.out.println(expr);
    try {
      Node root = parser.Expression();
      System.out.println("Java Parser Version 1.1:  Java program parsed successfully.");
      root.accept(new ExpressionVisitor(), null);
    }
    catch (ParseException e) {
      System.out.println(e.getMessage());
      System.out.println("Java Parser Version 1.1:  Encountered errors during parse.");
    }
    catch (TokenMgrError lexError) {
      System.out.println("Lexical error.");
    }
  }

  static JavaParser parser;

}
