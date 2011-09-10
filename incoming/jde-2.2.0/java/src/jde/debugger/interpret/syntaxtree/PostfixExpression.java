//
// Generated by JTB 1.2.1
//

package jde.debugger.interpret.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * primaryExpression -> PrimaryExpression()
 * nodeOptional -> [ "++" | "--" ]
 * </PRE>
 */
public class PostfixExpression implements Node {
   public PrimaryExpression primaryExpression;
   public NodeOptional nodeOptional;

   public PostfixExpression(PrimaryExpression n0, NodeOptional n1) {
      primaryExpression = n0;
      nodeOptional = n1;
   }

   public void accept(jde.debugger.interpret.visitor.Visitor v) {
      v.visit(this);
   }
   public Object accept(jde.debugger.interpret.visitor.ObjectVisitor v, Object argu) {
      return v.visit(this,argu);
   }
}

