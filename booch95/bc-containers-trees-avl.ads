-- $Id$

with Bc.Support.Nodes;
generic
package BC.Containers.Trees.AVL is

  type AVL_Tree is limited private;

  type Is_Less_Than is access function (L,R : in Item) return Boolean;
  type Iteration_Function is access function (Elem: Item) return Boolean;

  type Child_Branch is (Left, Right);

  procedure Set_Less_Than_Func (Obj : in out AVL_Tree; F : Is_Less_Than);

  procedure Clear (Obj : in out AVL_Tree);
  procedure Insert (Obj : in out AVL_Tree;
                    Elem : in Item;
                    Already_Existed : out Boolean);
  procedure Delete (Obj : in out AVL_Tree; Elem : Item; Found : out Boolean);

  function Extent (Obj : in AVL_Tree) return Natural;

  function Is_Null (Obj : in AVL_Tree) return Boolean;
  function Is_Member (Obj : in AVL_Tree; Elem : Item) return Boolean;

  function Item_Of (Obj : AVL_Tree; Elem : Item ) return Item_Ptr;

  function Traverse (Obj : in AVL_Tree; Iter_Func : Iteration_Function)
                     return Boolean;

private

  package Nodes is new Bc.Support.Nodes (Item);
  use Nodes;

  type AVL_Tree is new Limited_Controlled with record
    Rep : AVL_Node_Ref;
    Size : Natural := 0;
    Less_Than : Is_Less_Than;
  end record;

  procedure Initialize (Obj : in out AVL_Tree);
  procedure Finalize (Obj : in out AVL_Tree);

end BC.Containers.Trees.AVL;
