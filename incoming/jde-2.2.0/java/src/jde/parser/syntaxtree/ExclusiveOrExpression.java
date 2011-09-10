//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * andExpression -> AndExpression()
 * nodeListOptional -> ( "^" AndExpression() )*
 * </PRE>
 */
public class ExclusiveOrExpression implements Node {
   private Node parent;
   public AndExpression andExpression;
   public NodeListOptional nodeListOptional;

   public ExclusiveOrExpression(AndExpression n0, NodeListOptional n1) {
      andExpression = n0;
      if ( andExpression != null ) andExpression.setParent(this);
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

