//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeToken -> ";"
 * </PRE>
 */
public class EmptyStatement implements Node {
   private Node parent;
   public NodeToken nodeToken;

   public EmptyStatement(NodeToken n0) {
      nodeToken = n0;
      if ( nodeToken != null ) nodeToken.setParent(this);
   }

   public EmptyStatement() {
      nodeToken = new NodeToken(";");
      if ( nodeToken != null ) nodeToken.setParent(this);
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
