--  Copyright (C) 1994-2002 Grady Booch, David Weller and Simon Wright.
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

with Ada.Streams;

generic
   type Item is private;
   with function "=" (L, R : Item) return Boolean is <>;
   type Item_Ptr is access all Item;
package BC.Support.Bounded is

   pragma Elaborate_Body;

   type Bnd_Node (Maximum_Size : Positive) is private;
   --  An optimally-packed static container whose items are stored on
   --  the stack.  Items are indexable starting at 1.  This is a
   --  supporting type.  As such, it is not intended to be used
   --  directly by the end-user.

   procedure Clear (Obj : in out Bnd_Node);
   --  Empty the container of all Items

   function "=" (Left, Right : Bnd_Node) return Boolean;

   procedure Insert (Obj : in out Bnd_Node; Elem : Item);
   --  Add an item to the front of the container

   procedure Insert (Obj : in out Bnd_Node; Elem : Item; Before : Positive);
   --  Add an item to the container, before the given index

   procedure Append (Obj : in out Bnd_Node; Elem : Item);
   --  Add an item to the end of the container

   procedure Append (Obj : in out Bnd_Node; Elem : Item; After : Positive);
   --  Add an item to the end of the container, after the given index

   procedure Remove (Obj : in out Bnd_Node; From : Positive);
   --  Remove the item at a given index

   procedure Replace (Obj : in out Bnd_Node; Index : Positive; Elem : Item);
   --  Replace the Item at Index with the new Elem

   function Available (Obj : Bnd_Node) return Natural;
   --  Returns available storage elements

   function Length (Obj : Bnd_Node) return Natural;
   --  Returns the number of items in the container

   function First (Obj : Bnd_Node) return Item;
   --  Returns the Item at the front of the container

   function Last (Obj : Bnd_Node) return Item;
   --  Returns the item at the end of the container

   function Item_At (Obj : Bnd_Node; Index : Positive) return Item;
   function Item_At (Obj : Bnd_Node; Index : Positive) return Item_Ptr;
   --  Returns the item at the given index

   function Location (Obj : Bnd_Node;
                      Elem : in Item;
                      Start : in Positive := 1) return Natural;
   --  Returns the first index in which the given item is
   --  found. Returns 0 if unsuccessful.

   pragma Inline (Insert);
   pragma Inline (Append);
   pragma Inline (Remove);
   pragma Inline (Replace);
   pragma Inline (Available);
   pragma Inline (Length);
   pragma Inline (First);
   pragma Inline (Last);
   pragma Inline (Item_At);
   pragma Inline (Location);

private

   subtype Elem_Range is Positive;
   type Elem_Array is array (Elem_Range range <>) of Item;

   subtype Size_Range is Natural;

   type Bnd_Node (Maximum_Size : Positive) is record
      Elems : Elem_Array (1 .. Maximum_Size);
      Start : Elem_Range := 1;
      Size : Size_Range := 0;
   end record;

   procedure Write_Bnd_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : Bnd_Node);

   procedure Read_Bnd_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : out Bnd_Node);

   for Bnd_Node'Write use Write_Bnd_Node;
   for Bnd_Node'Read use Read_Bnd_Node;

end BC.Support.Bounded;
