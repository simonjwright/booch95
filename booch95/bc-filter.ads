--  Copyright (C) 2001 Simon Wright.
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

with BC.Containers;
generic

   type Item is private;
   --  The type to be filtered.

   with package Source is new BC.Containers (Item);
   --  A Container instantiation.

   type From is new Source.Container with private;
   --  The Container type which contains the source Items.

   with package Target is new BC.Containers (Item);
   --  A Container instantiation (possibly different from Source).

   type To is new Target.Container with private;
   --  The Container type which is to contain the filtered Items.

   with function Pass (I : Item) return Boolean is <>;
   --  The function which determines whether an Item passes the filter
   --  or not.

   with procedure Clear (The_Container : in out To) is <>;
   --  A procedure to clear the destination Container.

   with procedure Add (To_The_Container : in out To; I : Item) is <>;
   --  A procedure to add an Item to the destination Container.

procedure BC.Filter (Input : From; Output : in out To);
