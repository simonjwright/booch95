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
--  This file contains the declaration of the dynamic array-based class
--  used for the representation of dynamic structures.

with BC.Support.Nodes;
generic
   type Item is private;
   type Item_Ptr is access all Item;
package BC.Support.Dynamic is

   type Dyn_Node is private;
   -- An optimally-packed dynamic container whose items are stored on the heap

   type Dyn_Node_Ref is access Dyn_Node;

   function Create(From : Dyn_Node) return Dyn_Node_Ref;
   -- Construct a new Dynamic container that is identical to the given container

   function Create(Size : Positive := 10) return Dyn_Node_Ref;
   -- Construct a new dynamic container that has a chunk_Size set to the passed-in value

   function "="(Left, Right : Dyn_Node) return Boolean;
   -- Creation, Equality, and Inequality predefined

   procedure Clear  (Obj : in out Dyn_Node);
   -- Empty the container of all Items

   procedure Insert (Obj : in out Dyn_Node; Elem : Item);
   -- Add an item to the front of the container

   procedure Insert (Obj : in out Dyn_Node; Elem : Item; Before : Natural);
   -- Ass an item to the container, before the given index

   procedure Append (Obj : in out Dyn_Node; Elem : Item);
   -- Add an item to the end of the container

   procedure Append (Obj : in out Dyn_Node; Elem : Item; After : Natural);
   -- Add an item to the end of the container, after the given index

   procedure Remove (Obj : in out Dyn_Node; From : Natural);
   -- Remove the item at a given index

   procedure Replace(Obj : in out Dyn_Node; Index : Positive; Elem : Item);
   -- Replace the Item at Index with the new Elem

   function Length  (Obj : Dyn_Node) return Natural;
   -- Returns the number of items in the container

   function First   (Obj : Dyn_Node) return Item;
   function First   (Obj : access Dyn_Node) return Item_Ptr;
   -- Returns the Item at the front of the container

   function Last    (Obj : Dyn_Node) return Item;
   function Last    (Obj : access Dyn_Node) return Item_Ptr;
   -- Returns the item at the end of the container

   function Item_At (Obj : access Dyn_Node; Index : Positive) return Item;
   function Item_At (Obj : access Dyn_Node; Index : Positive) return Item_Ptr;
   -- Returns the item at the given index

   function Location(Obj : access Dyn_Node; Elem : Item; Start : Positive := 1) return Natural;
   -- Returns the first index in which the given item is found. Returns 0
   -- if unsuccessful.

   procedure Preallocate(Obj : in out Dyn_Node; New_Length : Natural := 10);
   -- Preallocate New_Length number of unused items for the container

   procedure Set_Chunk_Size(Obj: in out Dyn_Node; Size : Natural);
   -- Set the Chunk_Size for the container

   function Chunk_Size(Obj : in Dyn_Node) return Natural;
   -- returns the current Chunk_Size

   procedure Free(Obj : in out Dyn_Node_Ref);

private

   type Dyn_Arr is array(Positive range <>) of aliased Item;

   type Dyn_Arr_Ref is access all Dyn_Arr;

   type Dyn_Node is record
      Ref  : Dyn_Arr_Ref;
      Size : Natural := 0;
      Chunk_Size : Natural := 10;
   end record;

end BC.Support.Dynamic;


