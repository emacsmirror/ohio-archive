//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * exclusiveOrExpression -> ExclusiveOrExpression()
 * nodeListOptional -> ( "|" ExclusiveOrExpression() )*
 * </PRE>
 */
public class InclusiveOrExpression implements Node {
   private Node parent;
   public ExclusiveOrExpression exclusiveOrExpression;
   public NodeListOptional nodeListOptional;

   public InclusiveOrExpression(ExclusiveOrExpression n0, NodeListOptional n1) {
      exclusiveOrExpression = n0;
      if ( exclusiveOrExpression != null ) exclusiveOrExpression.setParent(this);
      nodeListOptional = n1;
      if ( nodeListOptional != null ) nodeListOptional.setParent(this);
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
