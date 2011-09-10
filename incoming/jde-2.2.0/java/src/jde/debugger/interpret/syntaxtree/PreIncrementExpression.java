//
// Generated by JTB 1.2.1
//

package jde.debugger.interpret.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeToken -> "++"
 * primaryExpression -> PrimaryExpression()
 * </PRE>
 */
public class PreIncrementExpression implements Node {
   public NodeToken nodeToken;
   public PrimaryExpression primaryExpression;

   public PreIncrementExpression(NodeToken n0, PrimaryExpression n1) {
      nodeToken = n0;
      primaryExpression = n1;
   }

   public PreIncrementExpression(PrimaryExpression n0) {
      nodeToken = new NodeToken("++");
      primaryExpression = n0;
   }

   public void accept(jde.debugger.interpret.visitor.Visitor v) {
      v.visit(this);
   }
   public Object accept(jde.debugger.interpret.visitor.ObjectVisitor v, Object argu) {
      return v.visit(this,argu);
   }
}

