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
package BC.Containers.Sets.Dynamic is

  pragma Elaborate_Body;

  -- A set denotes a collection of items, drawn from some well-defined
  -- universe. A set may not contain duplicate items.

  -- The hash function (the generic parameter Hash) determines the
  -- allocation of pairs to hash buckets. The value returned must not
  -- change during the lifetime of a given Item. The range of hash values
  -- need not be constrained to the number of buckets in the set.

  -- The hash function must satisfy the condition that, for objects A and
  -- B, if A = B, then Hash (A) must equal Hash (B). The hash function
  -- should attempt to spread the set of possible items uniformly across
  -- the number of buckets. The quality of the hash function has a
  -- significant impact upon performance.

  type Dynamic_Set is new Set with private;

  function Create (Size : Positive) return Dynamic_Set;
  -- Creates a new Dynamic Set each of whose buckets is preallocated for
  -- 'Size' elements

  procedure Clear (S : in out Dynamic_Set);
  -- Empty the set of all items.

  procedure Add (S : in out Dynamic_Set; I : Item; Added : out Boolean);
  -- Add the item to the set. If the item is not already a distinct member
  -- of the set, copy the item and add it to the set and set Added to
  -- True. If the item already exists, then set Added to False.

  procedure Remove (S : in out Dynamic_Set; I : Item);
  -- If the item is not a member of the set, raise BC.Not_Found. Otherwise,
  -- remove the item from the set.

  function Extent (S : Dynamic_Set) return Natural;
  -- Return the number of items in the set.

  function Is_Empty (S : Dynamic_Set) return Boolean;
  -- Return True if and only if there are no items in the set.

  function Is_Member (S : Dynamic_Set; I : Item) return Boolean;
  -- Return True if and only if the item exists in the set.

  procedure Preallocate (S : in out Dynamic_Set; Size : Positive);
  -- Allocates 'Size' additional storage elements for each bucket of the
  -- Set

  procedure Set_Chunk_Size (S : in out Dynamic_Set; Size : Positive);
  -- Establishes the Size each bucket of the Set will grow if the Set
  -- exhausts its current size.

  function Chunk_Size (S : Dynamic_Set) return Positive;
  -- Returns the Chunk_Size.

  function New_Iterator (For_The_Set : Dynamic_Set) return Iterator;
  -- Return a reset Iterator bound to the specific Set.

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

  -- We need a dummy type for the Value component of the hash table.
  type Boolean_Ptr is access all Boolean;
  package VC is new BC.Support.Dynamic (Item => Boolean,
                                        Item_Ptr => Boolean_Ptr,
                                        Storage_Manager => Storage_Manager,
                                        Storage => Storage);
  use VC;
  package Values is new BC.Support.Hash_Tables.Value_Signature
     (Value => Boolean,
      Value_Ptr => Boolean_Ptr,
      Value_Container => VC.Dyn_Node,
      Value_Container_Ptr => VC.Dyn_Node_Ref);

  package Tables is new BC.Support.Hash_Tables.Tables
     (Items => Items,
      Values => Values,
      Buckets => Buckets);

  type Dynamic_Set is new Set with record
    Rep : Tables.Table;
  end record;

  procedure Attach (S : in out Dynamic_Set; I : Item);

  procedure Detach (S : in out Dynamic_Set; I : Item);

  function Number_Of_Buckets (S : Dynamic_Set) return Natural;

  function Length (S : Dynamic_Set; Bucket : Positive) return Natural;

  function Item_At
     (S : Dynamic_Set; Bucket, Index : Positive) return Item_Ptr;

  type Dynamic_Set_Iterator (D : access Dynamic_Set'Class)
  is new Set_Iterator (D) with null record;

end BC.Containers.Sets.Dynamic;
