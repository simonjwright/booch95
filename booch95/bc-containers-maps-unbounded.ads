-- Copyright (C) 1994-2001 Grady Booch and Simon Wright.
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

with BC.Support.Unbounded;
with BC.Support.Hash_Tables;
with System.Storage_Pools;

generic
  with function Hash (V : Item) return Natural is <>;
  Buckets : Positive;
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Maps.Unbounded is

  pragma Elaborate_Body;

  -- A map denotes a collection forming a dictionary of domain/range
  -- pairs.

  -- The hash function (the generic parameter Hash) determines the
  -- allocation of pairs to hash buckets. The value returned must not
  -- change during the lifetime of a given Item. The range of hash values
  -- need not be constrained to the number of buckets in the map.

  -- The hash function must satisfy the condition that, for objects A and
  -- B, if A = B, then Hash (A) must equal Hash (B). The hash function
  -- should attempt to spread the set of possible items uniformly across
  -- the number of buckets. The quality of the hash function has a
  -- significant impact upon performance.

  type Unbounded_Map is new Map with private;

  function Null_Container return Unbounded_Map;

  function "=" (L, R : Unbounded_Map) return Boolean;
  -- Return True if the two Maps contain the same items bound to the
  -- same values.

  procedure Clear (M : in out Unbounded_Map);
  -- Empty the map of all item/value pairs.

  procedure Bind (M : in out Unbounded_Map; I : Item; V : Value);
  -- If the item already exists in the map, raise BC.Duplicate. Otherwise,
  -- add the item/value pair to the map.

  procedure Rebind (M : in out Unbounded_Map; I : Item; V : Value);
  -- If the item does not exist in the map, raise BC.Not_Found. Otherwise,
  -- change the item's binding to the given value.

  procedure Unbind (M : in out Unbounded_Map; I : Item);
  -- If the item does not exist in the map, raise BC.Not_Found. Otherwise,
  -- remove the item/value binding.

  function Extent (M : Unbounded_Map) return Natural;
  -- Return the number of item/value bindings in the map.

  function Is_Empty (M : Unbounded_Map) return Boolean;
  -- Return True if and only if there are no item/value bindings in the
  -- map; otherwise, return False.

  function Is_Bound (M : Unbounded_Map; I : Item) return Boolean;
  -- Return True if and only if there is a binding for the given item in
  -- the map; otherwise, return False.

  function Value_Of (M : Unbounded_Map; I : Item) return Value;
  -- If the item does not exist in the map, raises BC.Not_Found. Otherwise,
  -- return a constant pointer to the value bound to the given item.

  function New_Iterator (For_The_Map : Unbounded_Map) return Iterator'Class;
  -- Return a reset Iterator bound to the specific Map.

private

  package IC is new BC.Support.Unbounded (Item => Item,
                                          Item_Ptr => Item_Ptr,
                                          Storage_Manager => Storage_Manager,
                                          Storage => Storage);
  use IC;
  package Items is new BC.Support.Hash_Tables.Item_Signature
     (Item => Item,
      Item_Container => IC.Unb_Node,
      Item_Container_Ptr => IC.Unb_Node_Ref);

  package VC is new BC.Support.Unbounded (Item => Value,
                                          Item_Ptr => Value_Ptr,
                                          Storage_Manager => Storage_Manager,
                                          Storage => Storage);
  use VC;
  package Values is new BC.Support.Hash_Tables.Value_Signature
     (Value => Value,
      Value_Ptr => Value_Ptr,
      Value_Container => VC.Unb_Node,
      Value_Container_Ptr => VC.Unb_Node_Ref);

  package Tables is new BC.Support.Hash_Tables.Tables
     (Items => Items,
      Values => Values,
      Buckets => Buckets);

  type Table_P is access Tables.Table;
  for Table_P'Storage_Pool use Storage;

  type Unbounded_Map is new Map with record
    Rep : Table_P;
  end record;

  procedure Initialize (M : in out Unbounded_Map);

  procedure Adjust (M : in out Unbounded_Map);

  procedure Finalize (M : in out Unbounded_Map);

  procedure Attach (M : in out Unbounded_Map; I : Item; V : Value);

  function Number_Of_Buckets (M : Unbounded_Map) return Natural;

  function Length (M : Unbounded_Map; Bucket : Positive) return Natural;

  function Item_At
     (M : Unbounded_Map; Bucket, Index : Positive) return Item_Ptr;

  function Value_At
     (M : Unbounded_Map; Bucket, Index : Positive) return Value_Ptr;

end BC.Containers.Maps.Unbounded;
