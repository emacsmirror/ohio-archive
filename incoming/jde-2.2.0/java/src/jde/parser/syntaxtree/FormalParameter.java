//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeOptional -> [ "final" ]
 * type -> Type()
 * variableDeclaratorId -> VariableDeclaratorId()
 * </PRE>
 */
public class FormalParameter implements Node {
   private Node parent;
   public NodeOptional nodeOptional;
   public Type type;
   public VariableDeclaratorId variableDeclaratorId;

   public FormalParameter(NodeOptional n0, Type n1, VariableDeclaratorId n2) {
      nodeOptional = n0;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      type = n1;
      if ( type != null ) type.setParent(this);
      variableDeclaratorId = n2;
      if ( variableDeclaratorId != null ) variableDeclaratorId.setParent(this);
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

