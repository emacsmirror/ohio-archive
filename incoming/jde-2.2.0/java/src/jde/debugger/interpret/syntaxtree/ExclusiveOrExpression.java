//
// Generated by JTB 1.2.1
//

package jde.debugger.interpret.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * andExpression -> AndExpression()
 * nodeListOptional -> ( "^" AndExpression() )*
 * </PRE>
 */
public class ExclusiveOrExpression implements Node {
   public AndExpression andExpression;
   public NodeListOptional nodeListOptional;

   public ExclusiveOrExpression(AndExpression n0, NodeListOptional n1) {
      andExpression = n0;
      nodeListOptional = n1;
   }

   public void accept(jde.debugger.interpret.visitor.Visitor v) {
      v.visit(this);
   }
   public Object accept(jde.debugger.interpret.visitor.ObjectVisitor v, Object argu) {
      return v.visit(this,argu);
   }
}

