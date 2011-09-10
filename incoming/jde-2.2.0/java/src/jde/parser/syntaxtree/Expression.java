//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * conditionalExpression -> ConditionalExpression()
 * nodeOptional -> [ AssignmentOperator() Expression() ]
 * </PRE>
 */
public class Expression implements Node {
   private Node parent;
   public ConditionalExpression conditionalExpression;
   public NodeOptional nodeOptional;

   public Expression(ConditionalExpression n0, NodeOptional n1) {
      conditionalExpression = n0;
      if ( conditionalExpression != null ) conditionalExpression.setParent(this);
      nodeOptional = n1;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
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
