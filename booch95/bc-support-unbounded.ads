--  Copyright (C) 1994-2001 Grady Booch, David Weller, Pat Rogers and
--  Simon Wright.
--  All Rights Reserved.
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Finalization;
with System.Storage_Pools;

generic
   type Item is private;
   with function "=" (L, R : Item) return Boolean is <>;
   type Item_Ptr is access all Item;
   Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;
package BC.Support.Unbounded is

   pragma Elaborate_Body;

   type Unb_Node is private;
   --  An unpacked container whose items are stored on the heap.
   --  Items are effectively indexed from 1.

   function "=" (Left, Right : Unb_Node) return Boolean;

   procedure Clear (Obj : in out Unb_Node);
   --  Empty the container of all Items

   procedure Insert (Obj : in out Unb_Node; Elem : Item);
   --  Add an item to the front of the container

   procedure Insert (Obj : in out Unb_Node; Elem : Item; Before : Positive);
   --  Add an item to the container, before the given index

   procedure Append (Obj : in out Unb_Node; Elem : Item);
   --  Add an item to the end of the container

   procedure Append (Obj : in out Unb_Node; Elem : Item; After : Positive);
   --  Add an item to the container, after the given index

   procedure Remove (Obj : in out Unb_Node; From : Positive);
   --  Remove the item at the given index

   procedure Replace (Obj : in out Unb_Node; Index : Positive; Elem : Item);
   --  Replace the item at index with the new elem

   function Length (Obj : Unb_Node) return Natural;
   --  Returns the number of items in the container

   function First (Obj : Unb_Node) return Item;
   function First (Obj : Unb_Node) return Item_Ptr;
   --  Returns the item at the front of the container

   function Last (Obj : Unb_Node) return Item;
   function Last (Obj : Unb_Node) return Item_Ptr;
   --  Returns the item at the end of the container

   function Item_At (Obj : Unb_Node; Index : Positive) return Item;
   function Item_At (Obj : Unb_Node; Index : Positive) return Item_Ptr;
   --  Returns the item at the given index

   function Location (Obj : Unb_Node; Elem : Item; Start : Positive := 1)
                     return Natural;
   --  Returns the first index in which the given item is
   --  found. Returns 0 if unsuccessful.

private

   type Node;
   type Node_Ref is access Node;
   for Node_Ref'Storage_Pool use Storage;
   type Node is record
      Element : Item;
      Next : Node_Ref;
      Previous : Node_Ref;
   end record;

   type Unb_Node is new Ada.Finalization.Controlled with record
      Rep : Node_Ref;
      Last : Node_Ref;
      Size : Natural := 0;
      Cache : Node_Ref;
      Cache_Index : Natural := 0; -- 0 means invalid
   end record;

   procedure Adjust (U : in out Unb_Node);
   procedure Finalize (U : in out Unb_Node);

end BC.Support.Unbounded;
