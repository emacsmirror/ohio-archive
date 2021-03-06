//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeToken -> "package"
 * name -> Name()
 * nodeToken1 -> ";"
 * </PRE>
 */
public class PackageDeclaration implements Node {
   private Node parent;
   public NodeToken nodeToken;
   public Name name;
   public NodeToken nodeToken1;

   public PackageDeclaration(NodeToken n0, Name n1, NodeToken n2) {
      nodeToken = n0;
      if ( nodeToken != null ) nodeToken.setParent(this);
      name = n1;
      if ( name != null ) name.setParent(this);
      nodeToken1 = n2;
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
   }

   public PackageDeclaration(Name n0) {
      nodeToken = new NodeToken("package");
      if ( nodeToken != null ) nodeToken.setParent(this);
      name = n0;
      if ( name != null ) name.setParent(this);
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

