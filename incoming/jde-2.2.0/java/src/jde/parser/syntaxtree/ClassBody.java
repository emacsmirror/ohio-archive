//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeToken -> "{"
 * nodeListOptional -> ( ClassBodyDeclaration() )*
 * nodeToken1 -> "}"
 * </PRE>
 */
public class ClassBody implements Node {
   private Node parent;
   public NodeToken nodeToken;
   public NodeListOptional nodeListOptional;
   public NodeToken nodeToken1;

   public ClassBody(NodeToken n0, NodeListOptional n1, NodeToken n2) {
      nodeToken = n0;
      if ( nodeToken != null ) nodeToken.setParent(this);
      nodeListOptional = n1;
      if ( nodeListOptional != null ) nodeListOptional.setParent(this);
      nodeToken1 = n2;
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
   }

   public ClassBody(NodeListOptional n0) {
      nodeToken = new NodeToken("{");
      if ( nodeToken != null ) nodeToken.setParent(this);
      nodeListOptional = n0;
      if ( nodeListOptional != null ) nodeListOptional.setParent(this);
      nodeToken1 = new NodeToken("}");
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
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
