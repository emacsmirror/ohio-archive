//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeToken -> "import"
 * name -> Name()
 * nodeOptional -> [ "." "*" ]
 * nodeToken1 -> ";"
 * </PRE>
 */
public class ImportDeclaration implements Node {
   private Node parent;
   public NodeToken nodeToken;
   public Name name;
   public NodeOptional nodeOptional;
   public NodeToken nodeToken1;

   public ImportDeclaration(NodeToken n0, Name n1, NodeOptional n2, NodeToken n3) {
      nodeToken = n0;
      if ( nodeToken != null ) nodeToken.setParent(this);
      name = n1;
      if ( name != null ) name.setParent(this);
      nodeOptional = n2;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      nodeToken1 = n3;
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
   }

   public ImportDeclaration(Name n0, NodeOptional n1) {
      nodeToken = new NodeToken("import");
      if ( nodeToken != null ) nodeToken.setParent(this);
      name = n0;
      if ( name != null ) name.setParent(this);
      nodeOptional = n1;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      nodeToken1 = new NodeToken(";");
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

