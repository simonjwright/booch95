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

with BC.Support.Bounded_Hash_Tables;

generic
   with function Hash (V : Item) return Natural is <>;
   Buckets : Positive;
   Maximum_Size : Positive;
package BC.Containers.Sets.Bounded is

   pragma Preelaborate;

   --  A set denotes a collection of items, drawn from some
   --  well-defined universe. A set may not contain duplicate items.

   --  The hash function (the generic parameter Hash) determines the
   --  allocation of items to hash buckets. The value returned must
   --  not change during the lifetime of a given Item. The range of
   --  hash values need not be constrained to the number of buckets in
   --  the set.

   --  The hash function must satisfy the condition that, for objects
   --  A and B, if A = B, then Hash (A) must equal Hash (B). The hash
   --  function should attempt to spread the set of possible items
   --  uniformly across the number of buckets. The quality of the hash
   --  function has a significant impact upon performance.

   type Unconstrained_Set
     (Number_Of_Buckets : Positive;
      Maximum_Size : Positive) is new Abstract_Set with private;

   subtype Set is Unconstrained_Set (Number_Of_Buckets => Buckets,
                                     Maximum_Size => Maximum_Size);

   function Null_Container return Unconstrained_Set;
   --  Note, this function has to be provided but the object returned
   --  is in fact a Set (ie, it is constrained).

   procedure Clear (S : in out Unconstrained_Set);
   --  Empty the set of all items.

   procedure Add (S : in out Unconstrained_Set; I : Item; Added : out Boolean);
   --  Add the item to the set. If the item is not already a distinct
   --  member of the set, copy the item and add it to the set and set
   --  Added to True. If the item already exists, then set Added to
   --  False.

   procedure Add (S : in out Unconstrained_Set; I : Item);
   --  Add the item to the set. If the item is not already a distinct
   --  member of the set, copy the item and add it to the set.

   procedure Remove (S : in out Unconstrained_Set; I : Item);
   --  If the item is not a member of the set, raise
   --  BC.Not_Found. Otherwise, remove the item from the set.

   function Available (S : Unconstrained_Set) return Natural;
   --  Return the number of unused slots in the set.

   function Extent (S : Unconstrained_Set) return Natural;
   --  Return the number of distinct items in the set.

   function Is_Empty (S : Unconstrained_Set) return Boolean;
   --  Return True if and only if there are no items in the set.

   function Is_Member (S : Unconstrained_Set; I : Item) return Boolean;
   --  Return True if and only if the item exists in the set.

   function New_Iterator
     (For_The_Set : Unconstrained_Set) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Set.

private

   package Items is new BC.Support.Bounded_Hash_Tables.Item_Signature
     (Item => Item,
      Item_Ptr => Item_Ptr,
      Eq => Containers."=",
      Hash => Hash);

   --  We need a dummy type for the Value component of the hash table.
   type Dummy is null record;
   type Dummy_Ptr is access all Dummy;
   package Values is new BC.Support.Bounded_Hash_Tables.Value_Signature
     (Value => Dummy,
      Value_Ptr => Dummy_Ptr,
      Eq => "=");

   package Tables is new BC.Support.Bounded_Hash_Tables.Tables
     (Items => Items,
      Values => Values);

   type Unconstrained_Set
     (Number_Of_Buckets : Positive;
      Maximum_Size : Positive)
   is new Abstract_Set with record
      Rep : Tables.Table (Number_Of_Buckets => Buckets,
                          Maximum_Size => Maximum_Size);
   end record;

   type Bounded_Set_Iterator is new Set_Iterator with null record;

   procedure Reset (It : in out Bounded_Set_Iterator);

   procedure Next (It : in out Bounded_Set_Iterator);

   function Is_Done (It : Bounded_Set_Iterator) return Boolean;

   function Current_Item (It : Bounded_Set_Iterator) return Item;

   function Current_Item_Ptr (It : Bounded_Set_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Bounded_Set_Iterator);

end BC.Containers.Sets.Bounded;
