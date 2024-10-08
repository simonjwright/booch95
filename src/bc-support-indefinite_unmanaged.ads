--  Copyright 1994 Grady Booch
--  Copyright 2003-2014 Simon Wright <simon@pushface.org>
--  Copyright 2009 Martin Krischik

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with Ada.Finalization;
with Ada.Streams;
with BC.Support.Indefinite_Reference;

generic
   type Item (<>) is private;
   with function "=" (L, R : Item) return Boolean is <>;
   type Item_Ptr is access Item;
package BC.Support.Indefinite_Unmanaged is

   pragma Preelaborate;

   type Unm_Node is private;
   --  An unpacked container whose items are stored on the heap.
   --  Items are effectively indexed from 1.

   function "=" (Left, Right : Unm_Node) return Boolean;

   procedure Clear (Obj : in out Unm_Node);
   --  Empty the container of all Items

   procedure Insert (Obj : in out Unm_Node; Elem : Item);
   --  Add an item to the front of the container

   procedure Insert (Obj : in out Unm_Node; Elem : Item; Before : Positive);
   --  Add an item to the container, before the given index

   procedure Append (Obj : in out Unm_Node; Elem : Item);
   --  Add an item to the end of the container

   procedure Append (Obj : in out Unm_Node; Elem : Item; After : Positive);
   --  Add an item to the container, after the given index

   procedure Remove (Obj : in out Unm_Node; From : Positive);
   --  Remove the item at the given index

   procedure Replace (Obj : in out Unm_Node; Index : Positive; Elem : Item);
   --  Replace the item at index with the new elem

   function Length (Obj : Unm_Node) return Natural;
   --  Returns the number of items in the container

   function First (Obj : Unm_Node) return Item;
   --  Returns the item at the front of the container

   function Last (Obj : Unm_Node) return Item;
   --  Returns the item at the end of the container

   function Item_At (Obj : Unm_Node; Index : Positive) return Item;
   function Item_At (Obj : Unm_Node; Index : Positive) return Item_Ptr;
   --  Returns the item at the given index

   function Location (Obj : Unm_Node; Elem : Item; Start : Positive := 1)
                     return Natural;
   --  Returns the first index in which the given item is
   --  found. Returns 0 if unsuccessful.

private

   package IR is new Indefinite_Reference (T => Item, P => Item_Ptr);

   type Node;
   type Node_Ref is access Node;

   type Node is record
      Element : IR.Pointer;
      Next : Node_Ref;
      Previous : Node_Ref;
   end record;

   type Unm_Node is new Ada.Finalization.Controlled with record
      Rep : Node_Ref;
      Last : Node_Ref;
      Size : Natural := 0;
      Cache : Node_Ref;
      Cache_Index : Natural := 0; -- 0 means invalid
   end record;

   procedure Adjust (U : in out Unm_Node);
   procedure Finalize (U : in out Unm_Node);

   procedure Write_Unm_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : Unm_Node);

   procedure Read_Unm_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : out Unm_Node);

   for Unm_Node'Write use Write_Unm_Node;
   for Unm_Node'Read use Read_Unm_Node;

end BC.Support.Indefinite_Unmanaged;
