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

generic
  type Value is private;
package BC.Containers.Maps is

  pragma Elaborate_Body;

  -- A map denotes a collection forming a dictionary of domain/range
  -- pairs. Maps are cached, so that the most recently accessed
  -- domain/range pair can be found in times on the order of O(1).

  -- The parameter Item denotes the universe from which the map draws its
  -- domain; the parameter Value denotes the universe from which the map
  -- draws its range. The parameters Item and Value typically represent
  -- different types, although they may may represent the same
  -- types. Either may be a primitive type or user-defined.

  type Map is abstract new Container with private;

  function Are_Equal (L, R : Map'Class) return Boolean;
  -- Return True if and only if both maps have the same extent and the same
  -- item/value pairs; return False otherwise. The cached item/value pair
  -- is unaffected.
  -- Can't call this "=" because of the standard one for Map.

  procedure Clear (M : in out Map)
    is abstract;
  -- Empty the map of all item/value pairs. The cached item/value pair is
  -- cleared.

  procedure Bind (M : in out Map; I : Item; V : Value)
    is abstract;
  -- If the item already exists in the map, raise BC.Duplicate. Otherwise,
  -- add the item/value pair to the map. The cached item/value pair is set
  -- to this new binding.

  procedure Rebind (M : in out Map; I : Item; V : Value)
    is abstract;
  -- If the item does not exist in the map, raise BC.Not_Found. Otherwise,
  -- change the item's binding to the given value. The cached item/value
  -- pair is set to this new binding.

  procedure Unbind (M : in out Map; I : Item)
    is abstract;
  -- If the item does not exist in the map, raise BC.Not_Found. Otherwise,
  -- remove the item/value binding. The cached item/value pair is cleared.

  function Extent (M : Map) return Natural
    is abstract;
  -- Return the number of item/value bindings in the map.

  function Is_Empty (M : Map) return Boolean
    is abstract;
  -- Return True if and only if there are no item/value bindings in the
  -- map; otherwise, return False.

  function Is_Bound (M : Map; I : Item) return Boolean
    is abstract;
  -- Return True if and only if there is a binding for the given item in
  -- the map; otherwise, return False. The cached item/value pair is used
  -- to accelerate the search; if there is a cache hit, the time complexity
  -- of this operation is O(1).

  function Value_Of (M : Map; I : Item) return Value
    is abstract;
  -- If the item does not exist in the map, raises BC.Not_Found. Otherwise,
  -- return a constant pointer to the value bound to the given item. The
  -- cached item/value pair is used to accelerate the search; if there is a
  -- cache hit, the time complexity of this operation is O(1).

  -- Additional Iterator support

  function Current_Value (It : Iterator) return Value;
  -- Return a copy of the current Value.

  generic
    with procedure Apply (I : Item; V : Value; OK : out Boolean);
  procedure Visit (Using : in out Iterator);
  -- Call Apply with a copy of each Item/Value pair in the Container
  -- to which the iterator Using is bound. The iteration will
  -- terminate early if Apply sets OK to False.

  generic
    with procedure Apply (I : Item; V : in out Value; OK : out Boolean);
  procedure Modify (Using : in out Iterator);
  -- Call Apply for each Item/Value pair in the Container to which the
  -- iterator Using is bound. The Item is a copy, the Value is the
  -- actual content. The iteration will terminate early if Apply sets
  -- OK to False.

private

  type Map is abstract new Container with null record;

  type Value_Ptr is access all Value;
  for Value_Ptr'Storage_Size use 0;

  procedure Attach (M : in out Map; I : Item; V : Value);

  function Number_Of_Buckets (M : Map) return Natural;

  function Length (M : Map; Bucket : Positive) return Natural;

  function Item_At (M : Map; Bucket, Index : Positive) return Item_Ptr;

  function Value_At (M : Map; Bucket, Index : Positive) return Value_Ptr;

  type Map_Iterator (M : access Map'Class)
  is new Actual_Iterator (M) with record
    Bucket_Index : Natural := 0;
    Index : Natural := 0;
  end record;

  procedure Initialize (It : in out Map_Iterator);

  -- Overriding primitive supbrograms of the concrete actual Iterator.

  procedure Reset (It : in out Map_Iterator);

  procedure Next (It : in out Map_Iterator);

  function Is_Done (It : Map_Iterator) return Boolean;

  function Current_Item (It : Map_Iterator) return Item;

  function Current_Item (It : Map_Iterator) return Item_Ptr;

  procedure Delete_Item_At (It : Map_Iterator);

end BC.Containers.Maps;
