--  Copyright (C) 1994-2002 Grady Booch, David Weller, Pat Rogers and
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
with Ada.Streams;
with System.Storage_Pools;

generic
   type Item is private;
   with function "=" (L, R : Item) return Boolean is <>;
   type Item_Ptr is access all Item;
   Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;
   Initial_Size : Positive := 10;
package BC.Support.Dynamic is

   pragma Elaborate_Body;

   type Dyn_Node is private;
   --  An optimally-packed dynamic container whose items are stored on
   --  the heap

   function "=" (Left, Right : Dyn_Node) return Boolean;

   procedure Clear (Obj : in out Dyn_Node);
   --  Empty the container of all Items

   procedure Insert (Obj : in out Dyn_Node; Elem : Item);
   --  Add an item to the front of the container

   procedure Insert (Obj : in out Dyn_Node; Elem : Item; Before : Positive);
   --  Add an item to the container, before the given index

   procedure Append (Obj : in out Dyn_Node; Elem : Item);
   --  Add an item to the end of the container

   procedure Append (Obj : in out Dyn_Node; Elem : Item; After : Positive);
   --  Add an item to the end of the container, after the given index

   procedure Remove (Obj : in out Dyn_Node; From : Positive);
   --  Remove the item at a given index

   procedure Replace (Obj : in out Dyn_Node; Index : Positive; Elem : Item);
   --  Replace the Item at Index with the new Elem

   function Length (Obj : Dyn_Node) return Natural;
   --  Returns the number of items in the container

   function First (Obj : Dyn_Node) return Item;
   --  Returns the Item at the front of the container

   function Last (Obj : Dyn_Node) return Item;
   --  Returns the item at the end of the container

   function Item_At (Obj : Dyn_Node; Index : Positive) return Item;
   function Item_At (Obj : Dyn_Node; Index : Positive) return Item_Ptr;
   --  Returns the item at the given index

   function Location (Obj : Dyn_Node; Elem : Item; Start : Positive := 1)
                     return Natural;
   --  Returns the first index in which the given item is
   --  found. Returns 0 if unsuccessful.

   procedure Preallocate (Obj : in out Dyn_Node;
                          New_Length : Natural := Initial_Size);
   --  Preallocate New_Length number of unused items for the container

   procedure Set_Chunk_Size (Obj : in out Dyn_Node; Size : Natural);
   --  Set the Chunk_Size for the container

   function Chunk_Size (Obj : in Dyn_Node) return Natural;
   --  Returns the current Chunk_Size

private

   type Dyn_Arr is array (Positive range <>) of Item;

   type Dyn_Arr_Ref is access all Dyn_Arr;
   for Dyn_Arr_Ref'Storage_Pool use Storage;

   type Dyn_Node is new Ada.Finalization.Controlled with record
      Ref        : Dyn_Arr_Ref;
      Size       : Natural := 0;
      Chunk_Size : Natural := Initial_Size;
   end record;

   procedure Initialize (D : in out Dyn_Node);
   procedure Adjust (D : in out Dyn_Node);
   procedure Finalize (D : in out Dyn_Node);

   procedure Write_Dyn_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : Dyn_Node);

   procedure Read_Dyn_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : out Dyn_Node);

   for Dyn_Node'Write use Write_Dyn_Node;
   for Dyn_Node'Read use Read_Dyn_Node;

end BC.Support.Dynamic;
