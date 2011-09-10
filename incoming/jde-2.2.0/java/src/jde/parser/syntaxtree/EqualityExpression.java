//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * instanceOfExpression -> InstanceOfExpression()
 * nodeListOptional -> ( ( "==" | "!=" ) InstanceOfExpression() )*
 * </PRE>
 */
public class EqualityExpression implements Node {
   private Node parent;
   public InstanceOfExpression instanceOfExpression;
   public NodeListOptional nodeListOptional;

   public EqualityExpression(InstanceOfExpression n0, NodeListOptional n1) {
      instanceOfExpression = n0;
      if ( instanceOfExpression != null ) instanceOfExpression.setParent(this);
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
