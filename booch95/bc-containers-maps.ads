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

generic
   type Key is private;
package BC.Containers.Maps is

   pragma Elaborate_Body;

   --  A map denotes a collection forming a dictionary of domain/range
   --  pairs.

   --  The parameter Key denotes the universe from which the map draws
   --  its doamin; the parameter Item denotes the universe from which
   --  the map draws its range. The parameters Key and Item typically
   --  represent different types, although they may may represent the
   --  same types. Either may be a primitive type or user-defined.

   type Abstract_Map is abstract new Container with private;

   function Are_Equal (L, R : Abstract_Map'Class) return Boolean;
   --  Return True if and only if both maps have the same extent and
   --  the same key/item pairs; return False otherwise.
   --  Can't call this "=" because of the standard one for Map.

   procedure Clear (M : in out Abstract_Map)
      is abstract;
   --  Empty the map of all key/item pairs.

   procedure Bind (M : in out Abstract_Map; K : Key; I : Item)
      is abstract;
   --  If the key already exists in the map, raise
   --  BC.Duplicate. Otherwise, add the key/item pair to the map.

   procedure Rebind (M : in out Abstract_Map; K : Key; I : Item)
      is abstract;
   --  If the key does not exist in the map, raise
   --  BC.Not_Found. Otherwise, change the key's binding to the given
   --  value.

   procedure Unbind (M : in out Abstract_Map; K : Key)
      is abstract;
   --  If the key does not exist in the map, raise
   --  BC.Not_Found. Otherwise, remove the key/item binding.

   function Available (M : Abstract_Map) return Natural;
   --  Return the number of unused slots in the map.

   function Extent (M : Abstract_Map) return Natural
      is abstract;
   --  Return the number of key/item bindings in the map.

   function Is_Empty (M : Abstract_Map) return Boolean
      is abstract;
   --  Return True if and only if there are no key/item bindings in
   --  the map; otherwise, return False.

   function Is_Bound (M : Abstract_Map; K : Key) return Boolean
      is abstract;
   --  Return True if and only if there is a binding for the given key
   --  in the map; otherwise, return False.

   function Item_Of (M : Abstract_Map; K : Key) return Item
      is abstract;
   --  If the key does not exist in the map, raises
   --  BC.Not_Found. Otherwise, return a copy of the item bound to the
   --  given key.

   --  Additional Iterator support

   type Map_Iterator is new Iterator with private;

   function Current_Key (It : Map_Iterator'Class) return Key;
   --  Return a copy of the current Key.

   generic
      with procedure Apply (K : Key; I : Item; OK : out Boolean);
   procedure Visit (Using : in out Map_Iterator'Class);
   --  Call Apply with a copy of each Key/Item pair in the Container
   --  to which the iterator Using is bound. The iteration will
   --  terminate early if Apply sets OK to False.

   generic
      with procedure Apply (K : Key; I : in out Item; OK : out Boolean);
   procedure Modify (Using : in out Map_Iterator'Class);
   --  Call Apply for each Key/Item pair in the Container to which the
   --  iterator Using is bound. The Item is a copy, the Value is the
   --  actual content. The iteration will terminate early if Apply
   --  sets OK to False.

private

   type Abstract_Map is abstract new Container with null record;

   type Key_Ptr is access all Key;
   for Key_Ptr'Storage_Size use 0;

   procedure Attach (M : in out Abstract_Map; K : Key; I : Item);

   function Number_Of_Buckets (M : Abstract_Map) return Natural;

   function Length (M : Abstract_Map; Bucket : Positive) return Natural;

   function Item_At
     (M : Abstract_Map; Bucket, Index : Positive) return Item_Ptr;

   function Key_At (M : Abstract_Map; Bucket, Index : Positive) return Key_Ptr;

   --  The new subprograms for Map iteration (which allow access to
   --  the Key as well as the Item) require the inherited
   --  For_The_Container to in fact be in Map'Class. This must be the
   --  case since the only way of getting a Map_Iterator is by using
   --  one of the concrete forms' New_Iterator using eg
   --
   --   Iter : Map_Iterator'Class := Map_Iterator'Class (New_Iterator (M));
   --
   --  which GNAT fails at compilation time if M isn't actually a Map.
   type Map_Iterator is new Iterator with record
      Bucket_Index : Natural := 0;
      Index : Natural := 0;
   end record;

   --  Overriding primitive supbrograms of the concrete actual Iterator.

   procedure Reset (It : in out Map_Iterator);

   procedure Next (It : in out Map_Iterator);

   function Is_Done (It : Map_Iterator) return Boolean;

   function Current_Item (It : Map_Iterator) return Item;

   function Current_Item (It : Map_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Map_Iterator);

end BC.Containers.Maps;
