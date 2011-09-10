//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * name -> Name()
 * nodeListOptional -> ( "," Name() )*
 * </PRE>
 */
public class NameList implements Node {
   private Node parent;
   public Name name;
   public NodeListOptional nodeListOptional;

   public NameList(Name n0, NodeListOptional n1) {
      name = n0;
      if ( name != null ) name.setParent(this);
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
