//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeChoice -> NestedClassDeclaration()
 *       | NestedInterfaceDeclaration()
 *       | MethodDeclaration()
 *       | FieldDeclaration()
 * </PRE>
 */
public class InterfaceMemberDeclaration implements Node {
   private Node parent;
   public NodeChoice nodeChoice;

   public InterfaceMemberDeclaration(NodeChoice n0) {
      nodeChoice = n0;
      if ( nodeChoice != null ) nodeChoice.setParent(this);
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

