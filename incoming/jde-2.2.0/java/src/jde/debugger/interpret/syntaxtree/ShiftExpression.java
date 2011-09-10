//
// Generated by JTB 1.2.1
//

package jde.debugger.interpret.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * additiveExpression -> AdditiveExpression()
 * nodeListOptional -> ( ( "&lt;&lt;" | "&gt;&gt;" | "&gt;&gt;&gt;" ) AdditiveExpression() )*
 * </PRE>
 */
public class ShiftExpression implements Node {
   public AdditiveExpression additiveExpression;
   public NodeListOptional nodeListOptional;

   public ShiftExpression(AdditiveExpression n0, NodeListOptional n1) {
      additiveExpression = n0;
      nodeListOptional = n1;
   }

   public void accept(jde.debugger.interpret.visitor.Visitor v) {
      v.visit(this);
   }
   public Object accept(jde.debugger.interpret.visitor.ObjectVisitor v, Object argu) {
      return v.visit(this,argu);
   }
}
