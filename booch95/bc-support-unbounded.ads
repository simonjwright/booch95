-- The Ada 95 Booch Components (Version 1.0 beta 1)
-- Copyright (C)1994-1997 Grady Booch and David Weller.  All Rights Reserved.
-- 
--      This program is free software; you can redistribute it
--      and/or modify it under the terms of the Ada Community
--      License which comes with this Library.
--
--      This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library 
--      for a copy.
--
-- Class denoting a list-based container

with BC.Support.Nodes;
generic
    type Item is private;
    type Item_Ptr is access all Item;
package BC.Support.Unbounded is

   type Unb_Node is private;

   function Create(From : Unb_Node) return Unb_Node;

   function "="(Left, Right : Unb_Node) return Boolean;

   procedure Clear  (Obj : in out Unb_Node);
   procedure Insert (Obj : in out Unb_Node; Elem : Item);
   procedure Insert (Obj : in out Unb_Node; Elem : Item; Before : Natural);
   procedure Append (Obj : in out Unb_Node; Elem : Item);
   procedure Append (Obj : in out Unb_Node; Elem : Item; After : Natural);
   procedure Remove (Obj : in out Unb_Node; From : Natural);
   procedure Replace(Obj : in out Unb_Node; Index : Positive; Elem : Item);
   function Length  (Obj : Unb_Node) return Natural;
   function First   (Obj : Unb_Node) return Item;
   function First   (Obj : access Unb_Node) return Item_Ptr;
   function Last    (Obj : Unb_Node) return Item;
   function Last    (Obj : access Unb_Node) return Item_Ptr;
   function Item_At (Obj : access Unb_Node; Index : Positive) return Item;
   function Item_At (Obj : access Unb_Node; Index : Positive) return Item_Ptr;
   function Location(Obj : access Unb_Node; Elem : Item; Start : Positive := 1) return Natural;

   type Unb_Node_Ref is access Unb_Node;

private

   package Nodes is new Bc.Support.Nodes(Item);
   
   type Unb_Node is record
      Rep   : Nodes.Node_Ref;
      Last  : Nodes.Node_Ref;
      Size  : Natural := 0;
      Cache : Nodes.Node_Ref;
      Cache_Index : Natural := 0;
   end record;

end BC.Support.Unbounded;


