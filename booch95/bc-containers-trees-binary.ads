--  The C++ Booch Components (Version 2.3)
--  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved.
--
--  BCBTree.h
--
--  This file contains the declaration of the binary tree.

with Bc.Support.Nodes;
generic
package BC.Trees.Binary is

   type Bin_Tree is private;

   type Child_Branch is (Left, Right);

   function Create(From : Bin_Tree) return Bin_Tree;

   function "="(Left, Right : Bin_Tree) return Boolean;

   procedure Clear(Obj : in out Bin_Tree);
   procedure Insert(Obj : in out Bin_Tree; Elem : in Item; 
					   Child : in Child_Branch);
   procedure Append(Obj : in out Bin_Tree; Elem : in Item; 
		    Child : in Child_Branch; After : in Child_Branch);
   procedure Remove(Obj : in out Bin_Tree; Child : in Child_Branch);
   procedure Share(Obj : in out Bin_Tree;  
		   Share_With : in Bin_Tree;
		   Child : in Child_Branch := Right);
   procedure Swap_Child(Obj : in out Bin_Tree; 
			Swap_WIth : in out Bin_Tree; 
			Child : in Child_Branch);
   procedure Child(Obj : in out allBin_Tree; Child : in Child_Branch);
   procedure Left_Child(Obj : in out Bin_Tree);
   procedure Right_Child(Obj : in out Bin_Tree);
   procedure Parent(Obj : in out Bin_Tree);
   procedure Set_Item(Obj : in out Bin_Tree; Elem : in Item);

   function Has_Children(Obj : in Bin_Tree) return boolean;
   function Is_Null(Obj : in Bin_Tree) return boolean;
   function Is_Shared(Obj : in Bin_Tree) return boolean;
   function Is_Root(Obj : in Bin_Tree) return boolean;
   function Item_At(Obj : in Bin_Tree) return Item;

private
   package Nodes is new Bc.Support.Nodes(Item); use Nodes;

   procedure Purge(Node : in out Binary_Node_Ref);

   type Bin_Tree is new Controlled with record
      Rep : Binary_Node_Ref;
   end record;

--    procedure Initialize;
--    procedure Finalize;
--    procedure Adjust;

end BC.Trees.Binary;
