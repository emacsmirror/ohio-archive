//
// Generated by JTB 1.2.1
//

package jde.debugger.interpret.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * conditionalOrExpression -> ConditionalOrExpression()
 * nodeOptional -> [ "?" Expression() ":" ConditionalExpression() ]
 * </PRE>
 */
public class ConditionalExpression implements Node {
   public ConditionalOrExpression conditionalOrExpression;
   public NodeOptional nodeOptional;

   public ConditionalExpression(ConditionalOrExpression n0, NodeOptional n1) {
      conditionalOrExpression = n0;
      nodeOptional = n1;
   }

   public void accept(jde.debugger.interpret.visitor.Visitor v) {
      v.visit(this);
   }
   public Object accept(jde.debugger.interpret.visitor.ObjectVisitor v, Object argu) {
      return v.visit(this,argu);
   }
}
