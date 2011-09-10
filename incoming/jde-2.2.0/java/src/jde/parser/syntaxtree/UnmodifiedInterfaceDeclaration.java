//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeToken -> "interface"
 * nodeToken1 -> &lt;IDENTIFIER&gt;
 * nodeOptional -> [ "extends" NameList() ]
 * nodeToken2 -> "{"
 * nodeListOptional -> ( InterfaceMemberDeclaration() )*
 * nodeToken3 -> "}"
 * </PRE>
 */
public class UnmodifiedInterfaceDeclaration implements Node {
   private Node parent;
   public NodeToken nodeToken;
   public NodeToken nodeToken1;
   public NodeOptional nodeOptional;
   public NodeToken nodeToken2;
   public NodeListOptional nodeListOptional;
   public NodeToken nodeToken3;

   public UnmodifiedInterfaceDeclaration(NodeToken n0, NodeToken n1, NodeOptional n2, NodeToken n3, NodeListOptional n4, NodeToken n5) {
      nodeToken = n0;
      if ( nodeToken != null ) nodeToken.setParent(this);
      nodeToken1 = n1;
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
      nodeOptional = n2;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      nodeToken2 = n3;
      if ( nodeToken2 != null ) nodeToken2.setParent(this);
      nodeListOptional = n4;
      if ( nodeListOptional != null ) nodeListOptional.setParent(this);
      nodeToken3 = n5;
      if ( nodeToken3 != null ) nodeToken3.setParent(this);
   }

   public UnmodifiedInterfaceDeclaration(NodeToken n0, NodeOptional n1, NodeListOptional n2) {
      nodeToken = new NodeToken("interface");
      if ( nodeToken != null ) nodeToken.setParent(this);
      nodeToken1 = n0;
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
      nodeOptional = n1;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      nodeToken2 = new NodeToken("{");
      if ( nodeToken2 != null ) nodeToken2.setParent(this);
      nodeListOptional = n2;
      if ( nodeListOptional != null ) nodeListOptional.setParent(this);
      nodeToken3 = new NodeToken("}");
      if ( nodeToken3 != null ) nodeToken3.setParent(this);
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

