//
// Generated by JTB 1.2.1
//

package jde.debugger.interpret.syntaxtree;

/**
 * Represents a grammar choice, e.g. ( A | B )
 */
public class NodeChoice implements Node {
   public NodeChoice(Node node) {
      this(node, -1);
   }

   public NodeChoice(Node node, int whichChoice) {
      choice = node;
      which = whichChoice;
   }

   public void accept(jde.debugger.interpret.visitor.Visitor v) {
      choice.accept(v);
   }
   public Object accept(jde.debugger.interpret.visitor.ObjectVisitor v, Object argu) {
      return choice.accept(v,argu);
   }

   public Node choice;
   public int which;
}
