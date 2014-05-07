--  Copyright 1994 Grady Booch
--  Copyright 1998-2014 Simon Wright <simon@pushface.org>

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

with BC.Support.Dynamic;
with BC.Support.Hash_Tables;
with System.Storage_Pools;

generic
   with function Hash (V : Item) return Natural is <>;
   Buckets : Positive;
   Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;
   Initial_Size : Positive := 10;
package BC.Containers.Bags.Dynamic is

   pragma Preelaborate;

   --  A bag denotes a collection of items, drawn from some
   --  well-defined universe. A bag may contain duplicate items. A bag
   --  actually owns only one copy of each unique item: duplicates are
   --  counted, but are not stored with the bag.

   --  The hash function (the generic parameter Hash) determines the
   --  allocation of items to hash buckets. The value returned must
   --  not change during the lifetime of a given Item. The range of
   --  hash values need not be constrained to the number of buckets in
   --  the bag.

   --  The hash function must satisfy the condition that, for objects
   --  A and B, if A = B, then Hash (A) must equal Hash (B). The hash
   --  function should attempt to spread the set of possible items
   --  uniformly across the number of buckets. The quality of the hash
   --  function has a significant impact upon performance.

   type Bag is new Abstract_Bag with private;

   function Null_Container return Bag;

   procedure Clear (B : in out Bag);
   --  Empty the bag of all items.

   procedure Add (B : in out Bag; I : Item; Added : out Boolean);
   --  Add the item to the bag. If the item is not already a distinct
   --  member of the bag, copy the item and add it to the bag and set
   --  Added to True. If the item already exists, then increment the
   --  number of that item and set Added to False.

   procedure Remove (B : in out Bag; I : Item);
   --  If the item is not a member of the bag, raise
   --  BC.Not_Found. Otherwise, if there is exactly one of the item in
   --  the bag, remove the item in the bag; if there is more than one
   --  of the item in the bag, simply decrement its number.

   function Extent (B : Bag) return Natural;
   --  Return the number of distinct items in the bag.

   function Count (B : Bag; I : Item) return Natural;
   --  Return the number of times the item occurs in the bag.

   function Is_Empty (B : Bag) return Boolean;
   --  Return True if and only if there are no items in the bag.

   function Is_Member (B : Bag; I : Item) return Boolean;
   --  Return True if and only if the item exists in the bag.

   procedure Preallocate (B : in out Bag; Size : Positive);
   --  Allocates 'Size' additional storage elements for each bucket of
   --  the Bag

   procedure Set_Chunk_Size (B : in out Bag; Size : Positive);
   --  Establishes the Size each bucket of the Bag will grow if the
   --  Bag exhausts its current size.

   function Chunk_Size (B : Bag) return Positive;
   --  Returns the Chunk_Size.

   function New_Iterator (For_The_Bag : Bag) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Bag.

private

   package IC is new BC.Support.Dynamic (Item => Item,
                                         Item_Ptr => Item_Ptr,
                                         Storage => Storage,
                                         Initial_Size => Initial_Size);
   package Items is new BC.Support.Hash_Tables.Item_Signature
     (Item => Item,
      Item_Ptr => Item_Ptr,
      Eq => Containers."=",
      Hash => Hash,
      Item_Container => IC.Dyn_Node,
      Clear => IC.Clear,
      Insert => IC.Insert,
      Append => IC.Append,
      Remove => IC.Remove,
      Replace => IC.Replace,
      Length => IC.Length,
      Item_At => IC.Item_At,
      Access_Item_At => IC.Item_At,
      Location => IC.Location);

   type Positive_Ptr is access all Positive;
   for Positive_Ptr'Storage_Size use 0;
   package VC is new BC.Support.Dynamic (Item => Positive,
                                         Item_Ptr => Positive_Ptr,
                                         Storage => Storage,
                                         Initial_Size => Initial_Size);
   package Values is new BC.Support.Hash_Tables.Value_Signature
     (Value => Positive,
      Value_Ptr => Positive_Ptr,
      Eq => Standard."=",
      Value_Container => VC.Dyn_Node,
      Clear => VC.Clear,
      Insert => VC.Insert,
      Append => VC.Append,
      Remove => VC.Remove,
      Replace => VC.Replace,
      Length => VC.Length,
      Item_At => VC.Item_At,
      Access_Item_At => VC.Item_At,
      Location => VC.Location);

   package Tables is new BC.Support.Hash_Tables.Tables
     (Items => Items,
      Values => Values);

   type Bag is new Abstract_Bag with record
      Rep : Tables.Table (Number_Of_Buckets => Buckets);
   end record;

   procedure Attach (B : in out Bag; I : Item; C : Positive);

   procedure Detach (B : in out Bag; I : Item);

   procedure Set_Value (B : in out Bag; I : Item; C : Positive);

   --  Iterators

   type Dynamic_Bag_Iterator is new Bag_Iterator with null record;

   procedure Reset (It : in out Dynamic_Bag_Iterator);

   procedure Next (It : in out Dynamic_Bag_Iterator);

   function Is_Done (It : Dynamic_Bag_Iterator) return Boolean;

   function Current_Item (It : Dynamic_Bag_Iterator) return Item;

   function Current_Item_Ptr (It : Dynamic_Bag_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Dynamic_Bag_Iterator);

end BC.Containers.Bags.Dynamic;
