-- $Id$

with Bc.Support.Nodes;

generic
package BC.Containers.Trees.Binary is

  type Binary_Tree is limited private;

  type Child_Branch is (Left, Right);

--   function Create (From : Binary_Tree) return Binary_Tree;

  function "=" (Left, Right : Binary_Tree) return Boolean;

  procedure Clear (Obj : in out Binary_Tree);
  procedure Insert (Obj : in out Binary_Tree;
                    Elem : in Item;
                    Child : in Child_Branch);
  procedure Append (Obj : in out Binary_Tree;
                    Elem : in Item;
                    Child : in Child_Branch;
                    After : in Child_Branch);
  procedure Remove (Obj : in out Binary_Tree; Child : in Child_Branch);
  procedure Share (Obj : in out Binary_Tree;
                   Share_With : in Binary_Tree;
                   Child : in Child_Branch := Right);
  procedure Swap_Child (Obj : in out Binary_Tree;
                        Swap_WIth : in out Binary_Tree;
                        Child : in Child_Branch);
  procedure Child (Obj : in out Binary_Tree; Child : in Child_Branch);
  procedure Left_Child (Obj : in out Binary_Tree);
  procedure Right_Child (Obj : in out Binary_Tree);
  procedure Parent (Obj : in out Binary_Tree);
  procedure Set_Item (Obj : in out Binary_Tree; Elem : in Item);

  function Has_Children (Obj : in Binary_Tree) return boolean;
  function Is_Null (Obj : in Binary_Tree) return boolean;
  function Is_Shared (Obj : in Binary_Tree) return boolean;
  function Is_Root (Obj : in Binary_Tree) return boolean;
  function Item_At (Obj : in Binary_Tree) return Item;

private

  package Nodes is new Bc.Support.Nodes(Item);
  use Nodes;

  procedure Purge (Node : in out Binary_Node_Ref);

  type Binary_Tree is new Limited_Controlled with record
    Rep : Binary_Node_Ref;
  end record;

  procedure Initialize (Obj : in out Binary_Tree);
  procedure Finalize (Obj : in out Binary_Tree);

end BC.Containers.Trees.Binary;
