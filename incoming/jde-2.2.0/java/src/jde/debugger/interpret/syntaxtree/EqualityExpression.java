//
// Generated by JTB 1.2.1
//

package jde.debugger.interpret.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * instanceOfExpression -> InstanceOfExpression()
 * nodeListOptional -> ( ( "==" | "!=" ) InstanceOfExpression() )*
 * </PRE>
 */
public class EqualityExpression implements Node {
   public InstanceOfExpression instanceOfExpression;
   public NodeListOptional nodeListOptional;

   public EqualityExpression(InstanceOfExpression n0, NodeListOptional n1) {
      instanceOfExpression = n0;
      nodeListOptional = n1;
   }

   public void accept(jde.debugger.interpret.visitor.Visitor v) {
      v.visit(this);
   }
   public Object accept(jde.debugger.interpret.visitor.ObjectVisitor v, Object argu) {
      return v.visit(this,argu);
   }
}

