--  Copyright (C) 1994-2001 Grady Booch and Simon Wright.
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

--  $Id$

with BC.Support.Dynamic;
with BC.Support.Hash_Tables;
with System.Storage_Pools;

generic
   with function Hash (K : Key) return Natural is <>;
   Buckets : Positive;
   type Storage_Manager (<>)
   is new System.Storage_Pools.Root_Storage_Pool with private;
   Storage : in out Storage_Manager;
   Initial_Size : Positive := 10;
package BC.Containers.Maps.Dynamic is

   pragma Elaborate_Body;

   --  A map denotes a collection forming a dictionary of domain/range
   --  pairs.

   --  The hash function (the generic parameter Hash) determines the
   --  allocation of pairs to hash buckets. The value returned must
   --  not change during the lifetime of a given Item. The range of
   --  hash values need not be constrained to the number of buckets in
   --  the map.

   --  The hash function must satisfy the condition that, for objects
   --  A and B, if A = B, then Hash (A) must equal Hash (B). The hash
   --  function should attempt to spread the set of possible items
   --  uniformly across the number of buckets. The quality of the hash
   --  function has a significant impact upon performance.

   type Map is new Abstract_Map with private;

   function Null_Container return Map;

   procedure Clear (M : in out Map);
   --  Empty the map of all key/item pairs.

   procedure Bind (M : in out Map; K : Key; I : Item);
   --  If the key already exists in the map, raise
   --  BC.Duplicate. Otherwise, add the key/item pair to the map.

   procedure Rebind (M : in out Map; K : Key; I : Item);
   --  If the key does not exist in the map, raise
   --  BC.Not_Found. Otherwise, change the key's binding to the given
   --  value.

   procedure Unbind (M : in out Map; K : Key);
   --  If the key does not exist in the map, raise
   --  BC.Not_Found. Otherwise, remove the key/item binding.

   function Extent (M : Map) return Natural;
   --  Return the number of key/item bindings in the map.

   function Is_Empty (M : Map) return Boolean;
   --  Return True if and only if there are no key/item bindings in
   --  the map; otherwise, return False.

   function Is_Bound (M : Map; K : Key) return Boolean;
   --  Return True if and only if there is a binding for the given key
   --  in the map; otherwise, return False.

   function Item_Of (M : Map; K : Key) return Item;
   --  If the key does not exist in the map, raises
   --  BC.Not_Found. Otherwise, return a copy of the item bound to the
   --  given key.

   procedure Preallocate (M : in out Map; Size : Positive);
   --  Allocates 'Size' additional storage elements for each bucket of
   --  the Map

   procedure Set_Chunk_Size (M : in out Map; Size : Positive);
   --  Establishes the Size each bucket of the Map will grow if the
   --  Map exhausts its current size.

   function Chunk_Size (M : Map) return Positive;
   --  Returns the Chunk_Size.

   function New_Iterator (For_The_Map : Map) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Map.

private

   package KC is new BC.Support.Dynamic (Item => Key,
                                         Item_Ptr => Key_Ptr,
                                         Storage_Manager => Storage_Manager,
                                         Storage => Storage,
                                         Initial_Size => Initial_Size);
   use KC;
   package Keys is new BC.Support.Hash_Tables.Item_Signature
     (Item => Key,
      Item_Container => KC.Dyn_Node);

   package IC is new BC.Support.Dynamic (Item => Item,
                                         Item_Ptr => Item_Ptr,
                                         Storage_Manager => Storage_Manager,
                                         Storage => Storage,
                                         Initial_Size => Initial_Size);
   use IC;
   package Items is new BC.Support.Hash_Tables.Value_Signature
     (Value => Item,
      Value_Ptr => Item_Ptr,
      Value_Container => IC.Dyn_Node);

   package Tables is new BC.Support.Hash_Tables.Tables
     (Items => Keys,
      Values => Items,
      Buckets => Buckets);

   type Map is new Abstract_Map with record
      Rep : Tables.Table;
   end record;

   procedure Attach (M : in out Map; K : Key; I : Item);

   function Number_Of_Buckets (M : Map) return Natural;

   function Length (M : Map; Bucket : Positive) return Natural;

   function Item_At
     (M : Map; Bucket, Index : Positive) return Item_Ptr;

   function Key_At
     (M : Map; Bucket, Index : Positive) return Key_Ptr;

end BC.Containers.Maps.Dynamic;
