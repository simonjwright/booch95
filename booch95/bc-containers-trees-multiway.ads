--  The C++ Booch Components (Version 2.3)
--  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved.
--
--  BCBTree.h
--
--  This file contains the declaration of the binary tree.
with BC.Support.Nodes;
generic
package BC.Containers.Trees.Multiway is

   type Mway_Tree is private;

   function Create(From : Mway_Tree) return Mway_Tree;

   function "="(Left, Right : Mway_Tree) return Boolean;

   procedure Clear(Obj : in out Mway_Tree);
   procedure Insert(Obj : in out Mway_Tree; Elem : in Item); 

   procedure Append(Obj : in out Mway_Tree; Elem : in Item); 
   procedure Append(Obj : in out Mway_Tree; Elem : in Item; After : Natural); 
   procedure Append(Obj : in out Mway_Tree; From_Tree : in out Mway_Tree); 

   procedure Remove(Obj : in out Mway_Tree; Index : Natural);
   procedure Share(Obj : in out Mway_Tree;  
		   Share_With : in Mway_Tree;
		   Child : Natural);
   procedure Swap_Child(Obj : in out Mway_Tree; 
			Swap_WIth : in out Mway_Tree; 
			Child : in Natural);
   procedure Child(Obj : in out Mway_Tree; Child : in Natural);
   procedure Parent(Obj : in out Mway_Tree);
   procedure Set_Item(Obj : in out Mway_Tree; Elem : in Item);

   function Arity (Obj : Mway_Tree) return Natural;

   function Has_Children(Obj : in Mway_Tree) return boolean;
   function Is_Null(Obj : in Mway_Tree) return boolean;
   function Is_Shared(Obj : in Mway_Tree) return boolean;
   function Is_Root(Obj : in Mway_Tree) return boolean;
   function Item_At(Obj : in Mway_Tree) return Item;

private
   package Nodes is new Bc.Support.Nodes(Item); use Nodes;

   type Mway_Tree is new Controlled with record
      Rep : Multiway_Node_Ptr;
   end record;

--    procedure Initialize;
--    procedure Finalize;
--    procedure Adjust;

end BC.Containers.Trees.Multiway;
