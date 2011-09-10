//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeToken -> "++"
 * primaryExpression -> PrimaryExpression()
 * </PRE>
 */
public class PreIncrementExpression implements Node {
   private Node parent;
   public NodeToken nodeToken;
   public PrimaryExpression primaryExpression;

   public PreIncrementExpression(NodeToken n0, PrimaryExpression n1) {
      nodeToken = n0;
      if ( nodeToken != null ) nodeToken.setParent(this);
      primaryExpression = n1;
      if ( primaryExpression != null ) primaryExpression.setParent(this);
   }

   public PreIncrementExpression(PrimaryExpression n0) {
      nodeToken = new NodeToken("++");
      if ( nodeToken != null ) nodeToken.setParent(this);
      primaryExpression = n0;
      if ( primaryExpression != null ) primaryExpression.setParent(this);
   }

   public void accept(jde.parser.visitor.Visitor v) {
      v.visit(this);
   }
   public Object accept(jde.parser.visitor.ObjectVisitor v, Object argu) {
      return v.visit(this,argu);
   }
   public void setParent(Node n) { parent = n; }
   public Node getParent()       { return parent; }
}

