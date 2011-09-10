//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeToken -> "class"
 * nodeToken1 -> &lt;IDENTIFIER&gt;
 * nodeOptional -> [ "extends" Name() ]
 * nodeOptional1 -> [ "implements" NameList() ]
 * classBody -> ClassBody()
 * </PRE>
 */
public class UnmodifiedClassDeclaration implements Node {
   private Node parent;
   public NodeToken nodeToken;
   public NodeToken nodeToken1;
   public NodeOptional nodeOptional;
   public NodeOptional nodeOptional1;
   public ClassBody classBody;

   public UnmodifiedClassDeclaration(NodeToken n0, NodeToken n1, NodeOptional n2, NodeOptional n3, ClassBody n4) {
      nodeToken = n0;
      if ( nodeToken != null ) nodeToken.setParent(this);
      nodeToken1 = n1;
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
      nodeOptional = n2;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      nodeOptional1 = n3;
      if ( nodeOptional1 != null ) nodeOptional1.setParent(this);
      classBody = n4;
      if ( classBody != null ) classBody.setParent(this);
   }

   public UnmodifiedClassDeclaration(NodeToken n0, NodeOptional n1, NodeOptional n2, ClassBody n3) {
      nodeToken = new NodeToken("class");
      if ( nodeToken != null ) nodeToken.setParent(this);
      nodeToken1 = n0;
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
      nodeOptional = n1;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      nodeOptional1 = n2;
      if ( nodeOptional1 != null ) nodeOptional1.setParent(this);
      classBody = n3;
      if ( classBody != null ) classBody.setParent(this);
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

