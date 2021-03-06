//
// Generated by JTB 1.2.1
//

package jde.debugger.interpret.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeChoice -> "." "this"
 *       | "." AllocationExpression()
 *       | "[" Expression() "]"
 *       | "." &lt;IDENTIFIER&gt;
 *       | Arguments()
 * </PRE>
 */
public class PrimarySuffix implements Node {
   public NodeChoice nodeChoice;

   public PrimarySuffix(NodeChoice n0) {
      nodeChoice = n0;
   }

   public void accept(jde.debugger.interpret.visitor.Visitor v) {
      v.visit(this);
   }
   public Object accept(jde.debugger.interpret.visitor.ObjectVisitor v, Object argu) {
      return v.visit(this,argu);
   }
}

