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

generic
  type Value is private;
package BC.Containers.Maps is

  pragma Elaborate_Body;

  -- A map denotes a collection forming a dictionary of domain/range
  -- pairs.

  -- The parameter Item denotes the universe from which the map draws its
  -- domain; the parameter Value denotes the universe from which the map
  -- draws its range. The parameters Item and Value typically represent
  -- different types, although they may may represent the same
  -- types. Either may be a primitive type or user-defined.

  type Map is abstract new Container with private;

  function Are_Equal (L, R : Map'Class) return Boolean;
  -- Return True if and only if both maps have the same extent and the same
  -- item/value pairs; return False otherwise.
  -- Can't call this "=" because of the standard one for Map.

  procedure Clear (M : in out Map)
    is abstract;
  -- Empty the map of all item/value pairs.

  procedure Bind (M : in out Map; I : Item; V : Value)
    is abstract;
  -- If the item already exists in the map, raise BC.Duplicate. Otherwise,
  -- add the item/value pair to the map.

  procedure Rebind (M : in out Map; I : Item; V : Value)
    is abstract;
  -- If the item does not exist in the map, raise BC.Not_Found. Otherwise,
  -- change the item's binding to the given value.

  procedure Unbind (M : in out Map; I : Item)
    is abstract;
  -- If the item does not exist in the map, raise BC.Not_Found. Otherwise,
  -- remove the item/value binding.

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
  -- the map; otherwise, return False.

  function Value_Of (M : Map; I : Item) return Value
    is abstract;
  -- If the item does not exist in the map, raises BC.Not_Found. Otherwise,
  -- return a constant pointer to the value bound to the given item.

  -- Additional Iterator support

  type Map_Iterator is new Iterator with private;

  function Current_Value (It : Map_Iterator) return Value;
  -- Return a copy of the current Value.

  generic
    with procedure Apply (I : Item; V : Value; OK : out Boolean);
  procedure Visit (Using : in out Map_Iterator'Class);
  -- Call Apply with a copy of each Item/Value pair in the Container
  -- to which the iterator Using is bound. The iteration will
  -- terminate early if Apply sets OK to False.

  generic
    with procedure Apply (I : Item; V : in out Value; OK : out Boolean);
  procedure Modify (Using : in out Map_Iterator'Class);
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

  -- The new subprograms for Map iteration (which allow access to the
  -- Value as well as the Item) require the inherited For_The_Container
  -- to in fact be in Map'Class. This must be the case since the only way
  -- of getting a Map_Iterator is by using one of the concrete forms'
  -- New_Iterator using eg
  --
  --   Iter : Map_Iterator'Class := Map_Iterator'Class (New_Iterator (M));
  --
  -- which GNAT fails at compilation time if M isn't actually a Map.
  type Map_Iterator is new Iterator with record
    Bucket_Index : Natural := 0;
    Index : Natural := 0;
  end record;

  -- Overriding primitive supbrograms of the concrete actual Iterator.

  procedure Reset (It : in out Map_Iterator);

  procedure Next (It : in out Map_Iterator);

  function Is_Done (It : Map_Iterator) return Boolean;

  function Current_Item (It : Map_Iterator) return Item;

  function Current_Item (It : Map_Iterator) return Item_Ptr;

  procedure Delete_Item_At (It : Map_Iterator);

end BC.Containers.Maps;
