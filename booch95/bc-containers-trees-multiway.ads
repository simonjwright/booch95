-- $Id$

with BC.Support.Nodes;

generic
package BC.Containers.Trees.Multiway is

  type Multiway_Tree is limited private;

  --    function Create(From : Multiway_Tree) return Multiway_Tree;

  function "=" (Left, Right : Multiway_Tree) return Boolean;

  procedure Clear (Obj : in out Multiway_Tree);
  procedure Insert (Obj : in out Multiway_Tree; Elem : in Item);

  procedure Append (Obj : in out Multiway_Tree;
                    Elem : in Item);
  procedure Append (Obj : in out Multiway_Tree;
                    Elem : in Item;
                    After : Natural);
  procedure Append (Obj : in out Multiway_Tree;
                    From_Tree : in out Multiway_Tree);

  procedure Remove (Obj : in out Multiway_Tree; Index : Natural);
  procedure Share (Obj : in out Multiway_Tree;
                   Share_With : in Multiway_Tree;
                   Child : Natural);
  procedure Swap_Child (Obj : in out Multiway_Tree;
                        Swap_WIth : in out Multiway_Tree;
                        Child : in Natural);
  procedure Child (Obj : in out Multiway_Tree; Child : in Natural);
  procedure Parent (Obj : in out Multiway_Tree);
  procedure Set_Item (Obj : in out Multiway_Tree; Elem : in Item);

  function Arity (Obj : Multiway_Tree) return Natural;

  function Has_Children (Obj : in Multiway_Tree) return Boolean;
  function Is_Null (Obj : in Multiway_Tree) return Boolean;
  function Is_Shared (Obj : in Multiway_Tree) return Boolean;
  function Is_Root (Obj : in Multiway_Tree) return Boolean;
  function Item_At (Obj : in Multiway_Tree) return Item;

private

  package Nodes is new Bc.Support.Nodes(Item);
  use Nodes;

  type Multiway_Tree is new Limited_Controlled with record
    Rep : Multiway_Node_Ref;
  end record;

  procedure Initialize (Obj : in out Multiway_Tree);
  procedure Finalize (Obj : in out Multiway_Tree);

end BC.Containers.Trees.Multiway;
