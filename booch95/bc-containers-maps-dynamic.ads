-- Copyright (C) 1994-2000 Grady Booch and Simon Wright.
-- All Rights Reserved.
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

-- $Id$

with BC.Support.Dynamic;
with BC.Support.Hash_Tables;
with System.Storage_Pools;

generic
  with function Hash (V : Item) return Positive is <>;
  Buckets : Positive;
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Maps.Dynamic is

  pragma Elaborate_Body;

  -- A map denotes a collection forming a dictionary of domain/range
  -- pairs. Maps are cached, so that the most recently accessed
  -- domain/range pair can be found on the order of O(1).

  -- The hash function (the generic parameter Hash) determines the
  -- allocation of pairs to hash buckets. The value returned must not
  -- change during the lifetime of a given Item. The range of hash values
  -- need not be constrained to the number of buckets in the map.

  -- The hash function must satisfy the condition that, for objects A and
  -- B, if A = B, then Hash (A) must equal Hash (B). The hash function
  -- should attempt to spread the set of possible items uniformly across
  -- the number of buckets. The quality of the hash function has a
  -- significant impact upon performance.

  type Dynamic_Map is new Map with private;

  function Create (Size : Positive) return Dynamic_Map;
  -- Creates a new Dynamic Map each of whose buckets is preallocated for
  -- 'Size' elements

  procedure Clear (M : in out Dynamic_Map);
  -- Empty the map of all item/value pairs. The cached item/value pair is
  -- cleared.

  procedure Bind (M : in out Dynamic_Map; I : Item; V : Value);
  -- If the item already exists in the map, raise BC.Duplicate. Otherwise,
  -- add the item/value pair to the map. The cached item/value pair is set
  -- to this new binding.

  procedure Rebind (M : in out Dynamic_Map; I : Item; V : Value);
  -- If the item does not exist in the map, raise BC.Not_Found. Otherwise,
  -- change the item's binding to the given value. The cached item/value
  -- pair is set to this new binding.

  procedure Unbind (M : in out Dynamic_Map; I : Item);
  -- If the item does not exist in the map, raise BC.Not_Found. Otherwise,
  -- remove the item/value binding. The cached item/value pair is cleared.

  function Extent (M : Dynamic_Map) return Natural;
  -- Return the number of item/value bindings in the map.

  function Is_Empty (M : Dynamic_Map) return Boolean;
  -- Return True if and only if there are no item/value bindings in the
  -- map; otherwise, return False.

  function Is_Bound (M : Dynamic_Map; I : Item) return Boolean;
  -- Return True if and only if there is a binding for the given item in
  -- the map; otherwise, return False. The cached item/value pair is used
  -- to accelerate the search; if there is a cache hit, the time complexity
  -- of this operation is O(1).

  function Value_Of (M : Dynamic_Map; I : Item) return Value;
  -- If the item does not exist in the map, raises BC.Not_Found. Otherwise,
  -- return a constant pointer to the value bound to the given item. The
  -- cached item/value pair is used to accelerate the search; if there is a
  -- cache hit, the time complexity of this operation is O(1).

  procedure Preallocate (M : in out Dynamic_Map; Size : Positive);
  -- Allocates 'Size' additional storage elements for each bucket of the
  -- Map

  procedure Set_Chunk_Size (M : in out Dynamic_Map; Size : Positive);
  -- Establishes the Size each bucket of the Map will grow if the Map
  -- exhausts its current size.

  function Chunk_Size (M : Dynamic_Map) return Positive;
  -- Returns the Chunk_Size.

  function New_Iterator (For_The_Map : Dynamic_Map) return Iterator;
  -- Return a reset Iterator bound to the specific Map.

private

  package IC is new BC.Support.Dynamic (Item => Item,
                                        Item_Ptr => Item_Ptr,
                                        Storage_Manager => Storage_Manager,
                                        Storage => Storage);
  use IC;
  package Items is new BC.Support.Hash_Tables.Item_Signature
     (Item => Item,
      Item_Container => IC.Dyn_Node,
      Item_Container_Ptr => IC.Dyn_Node_Ref);

  package VC is new BC.Support.Dynamic (Item => Value,
                                        Item_Ptr => Value_Ptr,
                                        Storage_Manager => Storage_Manager,
                                        Storage => Storage);
  use VC;
  package Values is new BC.Support.Hash_Tables.Value_Signature
     (Value => Value,
      Value_Ptr => Value_Ptr,
      Value_Container => VC.Dyn_Node,
      Value_Container_Ptr => VC.Dyn_Node_Ref);

  package Tables is new BC.Support.Hash_Tables.Tables
     (Items => Items,
      Values => Values,
      Buckets => Buckets);

  type Dynamic_Map is new Map with record
    Rep : Tables.Table;
  end record;

  procedure Attach (M : in out Dynamic_Map; I : Item; V : Value);

  function Number_Of_Buckets (M : Dynamic_Map) return Natural;

  function Length (M : Dynamic_Map; Bucket : Positive) return Natural;

  function Item_At
     (M : Dynamic_Map; Bucket, Index : Positive) return Item_Ptr;

  function Value_At
     (M : Dynamic_Map; Bucket, Index : Positive) return Value_Ptr;

  type Dynamic_Map_Iterator (B : access Dynamic_Map'Class)
  is new Map_Iterator (B) with null record;

end BC.Containers.Maps.Dynamic;
